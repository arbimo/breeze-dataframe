package breeze.dataframe

import breeze.linalg._
import breeze.numerics._

import utest._

object ColumnTest extends TestSuite {

  val tests = Tests {
    val v1 = DenseVector(1, 2, 3)
    val v2 = DenseVector(1, 2, 3)
    val c1 = Column("a", v1)
    val c2 = Column("b", v2)

    "column creation" - {
      log(c1)

    }

    "ufuncs" - {
      import breeze.linalg.operators._
      import breeze.dataframe.trans.Add
      val x: Column[c1.Key, DenseVector[Int]] = Add(c1, 1)
      val y: Column[c1.Key, DenseVector[Int]] = OpAdd(c1, v1)
      val z: Column[c1.Key, DenseVector[Int]] = OpAdd(c1, c2)

      OpAdd(c1, c2)

    }

  }
}
