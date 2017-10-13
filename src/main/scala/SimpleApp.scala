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

  case class DF[A <: HList](columns : A = HNil) {
    def keys(k: Keys[A]) = k()

    def apply(k: Witness)(implicit selector : Selector[A, k.T]): selector.Out = columns.apply(k)

    def +[T](k: Witness, data: DenseVector[T]) = DF(field[k.T](data) :: columns)
    def +[K,V](c: Col[K,V]) = DF(c :: columns)
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
    ("a" ->> DenseVector(1.0, 2.0)) +
    ("b" ->> DenseVector(4.0, 5.0))

  val y = x + ("c" ->> x("a") / x("b"))
  println(y)



}