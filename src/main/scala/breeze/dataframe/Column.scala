import breeze.linalg._
import breeze.generic._
import breeze.linalg.{Vector => Vec}

import shapeless.labelled.KeyTag


case class Column[K,V  <: Vec[_]](key: K, data: V) extends KeyTag[K,Column[K,V]] {

}


object Column {


  implicit def ufunc1Deriv[K,V <: Vec[_],F,Out <: Vec[_]](implicit f: UFunc.UImpl[F,V,Out]): UFunc.UImpl[F,Column[K,V],Column[K,Out]] =
    new UFunc.UImpl[F,Column[K,V], Column[K,Out]] {
      def apply(in: Column[K,V]): Column[K,Out] = new Column(in.key, f(in.data))

    }

}
