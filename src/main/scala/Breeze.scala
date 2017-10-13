import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.DenseVector
import breeze.numerics.log
import shapeless._

object Breeze extends App {

  trait Transformation[A, F <: UFunc] {
    type Ret

    def apply(v: A, f: F): Ret
  }
  object Transformation {
    type Aux[A, F <: UFunc, Ret0] = Transformation[A, F] { type Ret = Ret0 }

    implicit def directTrans[A, F <: UFunc, Ret0](
        implicit impl: UFunc.UImpl[F, A, Ret0]) =
      new Transformation[A, F] {
        override type Ret = Ret0
        override def apply(v: A, f: F): Ret = {
          f.apply[A, Ret](v)(impl.asInstanceOf[f.Impl[A, Ret]])
        }
      }

    implicit def hnilTrans[F <: UFunc] = new Transformation[HNil, F] {
      override type Ret = HNil
      override def apply(v: HNil, f: F): Ret = HNil
    }

    implicit def hlistTrans[H, T <: HList, F <: UFunc, HRes, TRes <: HList](
        implicit hTrans: Transformation.Aux[H, F, HRes],
        tTrans: Transformation.Aux[T, F, TRes]
    ): Transformation.Aux[H :: T, F, HRes :: TRes] =
      new Transformation[H :: T, F] {
        override type Ret = HRes :: TRes

        override def apply(v: H :: T, f: F): HRes :: TRes =
          hTrans(v.head, f) :: tTrans(v.tail, f)
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
    def instance[T]: Impl[T, String] = (v: T) => v.toString

    implicit val string = instance[String]
    implicit val int = instance[Int]
    implicit val double = instance[Double]
    implicit val float = instance[Float]
    implicit val bool = instance[Boolean]

  }

  println(applyOn(x, stringify))
  println(DenseVector(1, 2).trans(log))
  println((DenseVector(1, 3) :: DenseVector(4.0, 3.4) :: HNil).trans(stringify))
}
