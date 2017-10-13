import breeze.linalg._
import shapeless._
import syntax.singleton._
import record._
import shapeless.labelled._
import shapeless.ops.record.{Keys, Remover, Selector, Values}
import trans.Trans2

object SimpleApp extends App {

  val book =
    ("author" ->> "Benjamin Pierce") ::
    ("title"  ->> "Types and Programming Languages") ::
    ("id"     ->>  262162091) ::
    ("price"  ->>  44.11) ::
    HNil


  val book2 = book.remove("author")
  println(book.keys)

  type Col[Id, T] = FieldType[Id, Vector[T]]

  case class DF[A <: HList](columns : A = HNil) {
    def keys(implicit k: Keys[A]) = k()

    def apply(k: Witness)(implicit selector : Selector[A, k.T]): selector.Out = columns.apply(k)

    def +[T](k: Witness, data: DenseVector[T]) = DF(field[k.T](data) :: columns)
    def +[T <: Col[_,_]](c: T): DF[T::A] = DF(c :: columns)

    def slice[Slice](s: Slice)(implicit t: Trans2[DF[A], Slice, Slice]): t.To = t(this, s)
  }

  implicit class DFOps[A <: HList](a : A) {

    def df: DF[A] = DF(a)

  }

  trait Format[A] {

    def format(a: A): String
  }


  "a".narrow
  val z = Witness("a")
  type _a = z.T

  val x = DF() +
    ("a" ->> DenseVector(1, 2, 3)) +
    ("b" ->> DenseVector(4, 5, 6))

  val y = x + ("c" ->> x("a") / x("b"))
  println(y)

  import trans._
  println(x.keys)
  println(x.trans(stringify))

  val a = DenseVector(1, 2, 3, 1)

  println(y.slice(y("a") <:< 3 | y("b") >:> 4))
//  val t = new SliceTrans[DenseVector[Int], BitVector, DenseVector[Int]](a <:< 3)
//  println(t)
//  println(a.trans()



}