package breeze.dataframe

import breeze.linalg._
import shapeless._
import syntax.singleton._
import record._
import shapeless.labelled._
import shapeless.ops.record.{Keys, Remover, Selector, Values}
import trans.{Trans2, stringify}

import scala.reflect.ClassTag

object SimpleApp extends App {

  val book =
    ("author" ->> "Benjamin Pierce") ::
      ("title" ->> "Types and Programming Languages") ::
      ("id" ->> 262162091) ::
      ("price" ->> 44.11) ::
      HNil

  val book2 = book.remove("author")
  println(book.keys)

  type Col[Id, T] = FieldType[Id, Vector[T]]

  case class DF[A <: HList](columns: A = HNil) {
    def keys(implicit k: Keys[A]) = k()

    def apply(k: Witness)(implicit selector: Selector[A, k.T]): selector.Out =
      columns.apply(k)

    def +[T](k: Witness, data: DenseVector[T]) = DF(field[k.T](data) :: columns)
    def +[T <: Col[_, _]](c: T): DF[T :: A] = DF(c :: columns)

    def slice[Slice](s: Slice)(implicit t: Trans2[DF[A], Slice, Slice]): t.To =
      t(this, s)

    def format(implicit f: Format[A]): String = {
      val strings = f.format(columns)
      val aligned = strings.map(col => {
        val maxWidth = col.map(_.length).max
        col.map(_.padTo(maxWidth + 1, ' '))
      })
      val sb = new StringBuilder
      for (line <- aligned.headOption.getOrElse(Seq()).indices) {
        for (col <- aligned.indices) {
          sb ++= aligned(col)(line)
        }
        sb += '\n'
      }
      sb.toString
    }

    def show(implicit f: Format[A]): this.type = { println(format); this }
  }

  implicit class DFOps[A <: HList](a: A) {

    def df: DF[A] = DF(a)

  }

  trait Format[A] {

    def format(a: A): Seq[Seq[String]]
  }
  object Format {

    implicit def hnil[T <: HNil]: Format[T] = _ => Seq()
    implicit def hlist[K, V, T <: HList](
        implicit f: Format[T],
        ev: V <:< Col[K,_],
        witness: Witness.Aux[K],
        tag: ClassTag[V]
                                        ): Format[V :: T] =
      (l: V :: T) => {
        f.format(l.tail) :+ (witness.value.toString +: l.head.toDenseVector
          .map(_.toString)
          .toScalaVector)
      }

  }

  def show[A](a: A)(implicit f: Format[A]) = println(f.format(a))

  "a".narrow
  val z = Witness("a")
  type _a = z.T

  val x = DF() +
    ("a" ->> DenseVector(1, 2, 3)) +
    ("b" ->> DenseVector(4, 5, 6))

  val y = x + ("c" ->> x("a") * x("b"))
  println(y)

  import trans._
  println(x.keys)
  println(x.trans(stringify))

  val a = DenseVector(1, 2, 3, 1)

  (y.slice(y("a") <:< 3 | y("b") >:> 4).show + ("AAA" ->> DenseVector(1,3, 4))).show
//  println(y.format)
  show("XXX" ->> DenseVector(1) :: HNil)
//  val t = new SliceTrans[DenseVector[Int], BitVector, DenseVector[Int]](a <:< 3)
//  println(t)
//  println(a.trans()

}
