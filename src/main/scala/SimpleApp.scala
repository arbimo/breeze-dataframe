import breeze.linalg._
import shapeless._
import syntax.singleton._

import record._
import shapeless.labelled._
import shapeless.ops.record.{Keys, Remover, Selector, Values}

object SimpleApp extends App {



  val book =
    ("author" ->> "Benjamin Pierce") ::
    ("title"  ->> "Types and Programming Languages") ::
    ("id"     ->>  262162091) ::
    ("price"  ->>  44.11) ::
    HNil


  val book2 = book.remove("author")
  println(book.keys)

  type Col[Id, T] = FieldType[Id, DenseVector[T]]

  object Col {

  }

  trait Cols[A <: HList] {

//    def keys: Keys[A]


  }

  object Cols {
    def apply[A <: HList](implicit ev: Cols[A]): Cols[A] = ev

    implicit def hnilCols[L <: HNil]: Cols[L] = new Cols[L] {
//      override def keys: Keys[L] = Keys[L]
    }

    implicit def recCols[Id, C, T <: HList](
                                                      implicit tailCols: Cols[T]
//                                                      tailKeys: Keys[T],
//                                                      wit: Witness.Aux[Id]
                                                    )
    : Cols[Col[Id,C] :: T] = new Cols[Col[Id,C] :: T] {
//      override def keys: Keys[Col[Id, C] :: T] = Keys[Col[Id,C] :: T]
    }
  }



  case class DF[A <: HList](columns : A = HNil) {
    def keys(k: Keys[A]) = k()

    def apply(k: Witness)(implicit selector : Selector[A, k.T]): selector.Out = columns.apply(k)

    def +[T](k: Witness, data: DenseVector[T]) = DF(field[k.T](data) :: columns)
    def +[K,V](c: Col[K,V]) = DF(c :: columns)
  }

  implicit class DFOps[A <: HList : Cols](a : A) {

    def df: DF[A] = DF(a)

  }

  trait Format[A] {

    def format(a: A): String
  }

  object Format {


//    implicit def dfFormat[A <: HList : Cols](df: DF[A]): Format[DF[A]] = new Format[DF[A]] {
//      override def format(a: DF[A]): String =
//    }
  }

  trait Transform[A] {
    type B
    def apply(df : A) : B
  }
  object Transform {
    type Aux[A,B0] = Transform[A] { type B = B0 }
  }

  trait ToStringTransform[A] extends Transform[A] {
    type B
  }

  object ToStringTransform {
    type Aux[A,B0] = ToStringTransform[A] { type B = B0 }

    private def col2Strings[Id,T](c: Col[Id,T])(implicit witness: Witness.Aux[Id]) = {
      val raw = witness.value.toString +: c.map(_.toString).toScalaVector()
      val maxWidth = raw.map(_.length).max
      raw.map(_.padTo(maxWidth, ' '))
    }

    implicit val hnilTrans = new ToStringTransform[DF[HNil]] {
      type B = DF[HNil]
      override def apply(df: DF[HNil]): DF[HNil] = df
    }

    implicit def hlistTrans[Id,T, Tail <: HList, TailRes <: HList](
                                                  implicit tailTrans: ToStringTransform.Aux[Tail, TailRes],
      witness: Witness.Aux[Id])
    : ToStringTransform[DF[Col[Id,T] :: Tail]] = new ToStringTransform[DF[Col[Id,T] :: Tail]] {
      type B = DF[Col[Id,String] :: tailTrans.B]

      override def apply(df: DF[Col[Id, T] :: Tail]): B =
        DF(field[Id](df.columns.head.map(_.toString)) :: tailTrans(df.columns.tail))
    }
  }


  "a".narrow
  val z = Witness("a")
  type _a = z.T

  sealed trait K
  case object K1
  case object K2

//  val kv: Col[_a, Int] = field[_a](DenseVector(1, 2))
  val kv2: Col[K2.type, Int] :: HNil = K2 ->> DenseVector(1,2) :: HNil
  Cols[Col[K2.type, Int] :: HNil]
  Cols[Col[K1.type, Double] :: Col[K2.type, Int] :: HNil]

  val x = DF() +
    ("a" ->> DenseVector(1.0, 2.0)) +
    ("b" ->> DenseVector(4.0, 5.0))

  val y = x + ("c" ->> x("a") / x("b"))
  println(y)

//  kv.

//  Cols[HNil]
//  Cols[Col[, String] :: HNil]

//  val data =
//    ("author" ->> DenseVector("a", "b")) ::
//      ("year" ->> DenseVector(2000, 3000)) ::
//      HNil
//  val df = DF(data)
//  println(DF(data).keys)

//  println(df("year"))


//  println(new DFOps().keys)




//  case class DataFrame[A <: HList : Keys : Values](columns: A = HNil) {
//
//
//    def +[K,V](col: FieldType[K,V])(implicit keys: Keys[FieldType[K,V]::A]): DataFrame[FieldType[K,V] :: A] = DataFrame(col :: columns)
//
////    def -[V, O <: HList](k: Witness)(implicit remover: Remover.Aux[A, k.T, (V, O)]): DataFrame[O] = DataFrame(columns.remove(k)(remover)._2)
////
////    def cp(k: Witness)(implicit selector : Selector[A, k.T]): DataFrame[selector.Out :: A] = DataFrame(columns.apply(k) :: columns)
////    def map[In,Out](k: Witness, f: In => Out)(implicit selector : Selector[A, k.T]) = DataFrame(columns(k) :: columns)
////
//    def keys(implicit k: Keys[A]): k.Out = k()
//    def cols(implicit values: Values[A]): values.Out = values(columns)
//
//    def apply(k: Witness)(implicit selector : Selector[A, k.T]) = columns.apply(k)
//
////    def mapCol[(k: Witness)
////    def
//  }
//
////  val df = DataFrame() + ("pb" ->> Seq("docks", "blocks")) + ("success" ->> Seq(1, 2))
////  println(df.keys)
////  println(df.cols)
////  println(df("pb"))
////  println(df - "success")
////  println(df.cp("pb") - "pb")
////  val df3 = df.cp("pb")
////  val df4 = df3 - "pb"
////
////  println(df3)
////  println(df4)
////  println("end")

}