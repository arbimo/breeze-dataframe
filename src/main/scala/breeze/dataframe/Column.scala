package breeze.dataframe

import breeze.linalg._
import breeze.generic._
import shapeless.Witness
import breeze.linalg.{Vector => Vec}

import shapeless.labelled.KeyTag


class Column[K,V  <: Vec[_]](val key: K, val data: V) extends KeyTag[K,Column[K,V]] {
  type Key = K
}



object Column {

  def apply[V <: Vec[_]](k : Witness, data: V): Column[k.T, V] =
    new Column(k.value, data)

  //object ConversionAsColumn {

    //implicit def columnAsVector[K, V <: Vec[_]](v: Column[K,V]): V = v.data
  //}
  //import ConversionAsColumn._


  implicit def ufunc1Deriv[K,V <: Vec[_],F,Out <: Vec[_]](implicit f: UFunc.UImpl[F,V,Out]):
      UFunc.UImpl[F,Column[K,V],Column[K,Out]] =
    new UFunc.UImpl[F,Column[K,V], Column[K,Out]] {
      def apply(in: Column[K,V]): Column[K,Out] = new Column(in.key, f(in.data))
    }


  implicit def ufunc2Deriv[K,V <: Vec[_], V2
    ,F,Out <: Vec[_]](
    implicit f: UFunc.UImpl2[F,V,V2,Out]):
      UFunc.UImpl2[F,Column[K,V],V2, Column[K,Out]] =
    new UFunc.UImpl2[F,Column[K,V], V2, Column[K,Out]] {
      def apply(in: Column[K,V], in2: V2): Column[K,Out] =
        new Column(in.key, f(in.data, in2))
    }


  implicit def ufunc2ColsDeriv[K,K2,V <: Vec[_], V2 <: Vec[_],
    F,Out <: Vec[_]] (
     implicit f: UFunc.UImpl2[F,V,V2,Out])
      :
      UFunc.UImpl2[F,Column[K,V],Column[K2,V2], Column[K,Out]] =
    new UFunc.UImpl2[F,Column[K,V], Column[K2,V2], Column[K,Out]] {
      def apply(in: Column[K,V], in2: Column[K2,V2]): Column[K,Out] =
        new Column(in.key, f(in.data, in2.data))
    }
}
