import SimpleApp.DF
import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.DenseVector
import breeze.numerics.log
import shapeless._
import shapeless.labelled.{FieldType, KeyTag}

object trans extends App {

  trait Transformation[A, F <: UFunc] {
    type Ret

    def apply(v: A, f: F): Ret
  }
  object Transformation {
    type Aux[A, F <: UFunc, Ret0] = Transformation[A, F] { type Ret = Ret0 }

    implicit def directTrans[A, F <: UFunc, Ret0](
        implicit impl: UFunc.UImpl[F, A, Ret0],
        ev: A <:!< KeyTag[_, _]): Aux[A, F, Ret0] =
      new Transformation[A, F] {
        override type Ret = Ret0
        override def apply(v: A, f: F): Ret = {
          f.apply[A, Ret](v)(impl.asInstanceOf[f.Impl[A, Ret]])
        }
      }

    /** Specific transformation that maintains the KeyTag */
    implicit def columnTrans[K, V, F <: UFunc, Ret0](
        implicit t: Aux[V, F, Ret0]
    ): Aux[FieldType[K, V], F, FieldType[K, Ret0]] =
      new Transformation[FieldType[K, V], F] {
        override type Ret = FieldType[K, Ret0]

        override def apply(v: FieldType[K, V], f: F): Ret =
          shapeless.labelled.field[K].apply[Ret0](t(v.asInstanceOf[V], f))
      }

    implicit def hnilTrans[T <: HNil, F <: UFunc]: Aux[T, F, T] =
      new Transformation[T, F] {
        override type Ret = T
        override def apply(v: T, f: F): Ret = v
      }

    implicit def hlistTrans[H, T <: HList, F <: UFunc, HRes, TRes <: HList](
        implicit hTrans: Aux[H, F, HRes],
        tTrans: Aux[T, F, TRes]
    ): Aux[H :: T, F, HRes :: TRes] =
      new Transformation[H :: T, F] {
        override type Ret = HRes :: TRes

        override def apply(v: H :: T, f: F): HRes :: TRes =
          hTrans(v.head, f) :: tTrans(v.tail, f)
      }

    implicit def dfTrans[A <: HList, F <: UFunc, Ret0 <: HList](
        implicit t: Aux[A, F, Ret0])
      : Aux[DF[A], F, DF[Ret0]] = new Transformation[DF[A], F] {
      override type Ret = DF[Ret0]
      override def apply(v: DF[A], f: F): Ret = DF(t(v.columns, f))
    }
  }

  def applyOn[In, F <: UFunc, Ret](v: In, f: F)(
      implicit impl: UFunc.UImpl[f.type, In, Ret]) = {
    f.apply[In, Ret](v)(impl)
  }

  implicit class TransOps[A](val v: A) {

    def trans[F <: UFunc](f: F)(implicit t: Transformation[A, F]): t.Ret =
      t(v, f)
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

  object slice extends MappingUFunc {

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

//  SimpleApp.book.trans(stringify)


}
