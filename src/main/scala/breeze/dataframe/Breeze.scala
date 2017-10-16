import SimpleApp.DF
import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.DenseVector
import breeze.linalg.support.CanSlice
import breeze.numerics.log
import shapeless._
import shapeless.labelled.{FieldType, KeyTag}

import scala.reflect.ClassTag

object trans extends App {

  trait Transformation[A, F] {
    type Ret

    def apply(v: A): Ret
  }
  object Transformation {
    type Aux[A, F, Ret0] = Transformation[A, F] { type Ret = Ret0 }

    implicit def ufuncTrans[A, F <: UFunc, Ret0](
        implicit impl: UFunc.UImpl[F, A, Ret0],
        ev: A <:!< KeyTag[_, _]): Aux[A, F, Ret0] =
      new Transformation[A, F] {
        override type Ret = Ret0
        override def apply(v: A): Ret = {
          impl(v)
        }
      }

    /** Specific transformation that maintains the KeyTag */
    implicit def columnTrans[K, V, F, Ret0](
        implicit t: Aux[V, F, Ret0]
    ): Aux[FieldType[K, V], F, FieldType[K, Ret0]] =
      new Transformation[FieldType[K, V], F] {
        override type Ret = FieldType[K, Ret0]

        override def apply(v: FieldType[K, V]): Ret =
          shapeless.labelled.field[K].apply[Ret0](t(v.asInstanceOf[V]))
      }

    implicit def hnilTrans[T <: HNil, F]: Aux[T, F, T] =
      new Transformation[T, F] {
        override type Ret = T
        override def apply(v: T): Ret = v
      }

    implicit def hlistTrans[H, T <: HList, F, HRes, TRes <: HList](
        implicit hTrans: Aux[H, F, HRes],
        tTrans: Aux[T, F, TRes]
    ): Aux[H :: T, F, HRes :: TRes] =
      new Transformation[H :: T, F] {
        override type Ret = HRes :: TRes

        override def apply(v: H :: T): HRes :: TRes =
          hTrans(v.head) :: tTrans(v.tail)
      }

    implicit def dfTrans[A <: HList, F, Ret0 <: HList](
        implicit t: Aux[A, F, Ret0]): Aux[DF[A], F, DF[Ret0]] =
      new Transformation[DF[A], F] {
        override type Ret = DF[Ret0]
        override def apply(v: DF[A]): Ret = DF(t(v.columns))
      }
  }

  trait Trans2[In1, In2, F] {
    type To
    def apply(a: In1, b: In2): To
  }
  object Trans2 {
    type Aux[In1, In2, F, To0] = Trans2[In1, In2, F] { type To = To0 }

    implicit def sliceTrans[From, Slice, To0](
        implicit canSlice: CanSlice[From, Slice, To0],
        ev: From <:!< KeyTag[_, _],
        ev2: To0 =:!= Any)
      : // TODO: strangely this is needed, (note: UImpl2 is contravariant in return type)
      Aux[From, Slice, Slice, To0] =
      new Trans2[From, Slice, Slice] {
        override type To = To0
        override def apply(a: From, b: Slice): To = {
          canSlice(a, b)
        }
      }

    implicit def ufuncTrans[In1, In2, F <: UFunc, Ret0](
        implicit impl: UFunc.UImpl2[F, In1, In2, Ret0],
        ev: In1 <:!< KeyTag[_, _],
        ev2: Ret0 =:!= Any) // TODO: strangely this is needed, (note: UImpl2 is contravariant in return type)
      : Aux[In1, In2, F, Ret0] =
      new Trans2[In1, In2, F] {
        override type To = Ret0
        override def apply(a: In1, b: In2): To = {
          impl(a, b)
        }
      }

    /** Specific transformation that maintains the KeyTag */
    implicit def columnTrans[K, V, In2, F, Ret0](
        implicit t: Aux[V, In2, F, Ret0]
    ): Aux[FieldType[K, V], In2, F, FieldType[K, Ret0]] =
      new Trans2[FieldType[K, V], In2, F] {
        override type To = FieldType[K, Ret0]

        override def apply(v: FieldType[K, V], b: In2): To =
          shapeless.labelled.field[K].apply[Ret0](t(v.asInstanceOf[V], b))
      }

    implicit def hnilTrans[From1 <: HNil, From2, F]: Aux[From1, From2, F, From1] =
      new Trans2[From1, From2, F] {
        override type To = From1
        override def apply(v: From1, b: From2): To = v
      }

    implicit def hlistTrans[H, T <: HList, From2, F, HRes, TRes <: HList](
        implicit hTrans: Aux[H, From2, F, HRes],
        tTrans: Aux[T, From2, F, TRes]
    ): Aux[H :: T, From2, F, HRes :: TRes] =
      new Trans2[H :: T, From2, F] {
        override type To = HRes :: TRes

        override def apply(v: H :: T, b: From2): HRes :: TRes =
          hTrans(v.head, b) :: tTrans(v.tail, b)
      }

    implicit def dfTrans[A <: HList, From2, F, Ret0 <: HList](
        implicit t: Aux[A, From2, F, Ret0]): Aux[DF[A], From2, F, DF[Ret0]] =
      new Trans2[DF[A], From2, F] {
        override type To = DF[Ret0]
        override def apply(v: DF[A], b: From2): To = DF(t(v.columns, b))
      }

  }

  def applyOn[In, F <: UFunc, Ret](v: In, f: F)(
      implicit impl: UFunc.UImpl[f.type, In, Ret]) = {
    f.apply[In, Ret](v)(impl)
  }

  implicit class TransOps[A](val v: A) {

    def trans[F](f: F)(implicit t: Transformation[A, F]): t.Ret =
      t(v)

    def trans2[F, In2](f: F, b: In2)(implicit t: Trans2[A, In2, F]): t.To =
      t(v, b)

    def myslice[Slice](s: Slice)(implicit t: Trans2[A, Slice, Slice]): t.To =
      trans2(s, s)(t)
  }

  import breeze.linalg._
  import breeze.numerics.log

  val x = DenseVector(1, 2)

  applyOn(1, log)
  println(applyOn(x, log))

  object stringify extends UFunc with MappingUFunc {
    def instance[T]: Impl[T, String] = (v: T) => "\"" + v.toString + "\""

    implicit val string = instance[String]
    implicit val int = instance[Int]
    implicit val double = instance[Double]
    implicit val float = instance[Float]
    implicit val bool = instance[Boolean]

  }

  object Add extends UFunc with MappingUFunc {

    implicit val int: Impl2[Int, Int, Int] = new Impl2[Int, Int, Int] {
      override def apply(v: Int, v2: Int): Int = v + v2
    }

  }

  println(applyOn(x, stringify))
  println(DenseVector(1, 2).trans(log))
  println((DenseVector(1, 3) :: DenseVector(4.0, 3.4) :: HNil).trans(stringify))

  import shapeless.record._
  import shapeless.labelled._

  import shapeless.syntax.singleton._
  val z = ("a" ->> DenseVector(1, 2) :: ("b" ->> DenseVector(1, 2)) :: HNil)
    .trans(stringify)

  println(z("a"))
  val vec = DenseVector(0, 1, 2, 3) :: HNil
  val ttt = "true"

  DenseVector(1,2) / DenseVector(2,3)

  println(Add(DenseVector(1, 2), 1))
  println(
    DenseVector(1, 2)
      .trans2(convert, Int)
      .trans2(Add, -1)
      .trans2(convert, Double))
  convert(convert(DenseVector(1, 2), Int), Double)
  println(vec.myslice(vec.head >:> 2).trans2(Add, 1))
//  SimpleApp.book.trans(stringify)

  // given a UFunc.Impl2[A,B,C], and a value b: B, create a Transformation[A,F,_]

}
