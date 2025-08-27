package apl

import com.mikadocs.kamin.{IntegerValue, MatrixDimensions, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

final class TransFunctionSuite extends FunSuite {
  // === tiny helpers ========================================================
  private def entry = functionDefinitionTable.lookupFunctionDefinition("trans").getOrElse(fail(s"Function not registered"))

  private def run(a: Value) = entry.function(null, Vector(a))

  private def i(x: Int) = IntegerValue(x)

  private def v(xs: Int*) = MatrixValue(xs, MatrixDimensions(if xs.length > 0 then 1 else 0, xs.length))

  private def m(rows: Int, cols: Int)(xs: Int*) = MatrixValue(xs, MatrixDimensions(rows, cols))
  
  // ---- Tests ----------------------------------------------------------------
  test("indx with an integer") {
    val a = i(10)

    val expected = i(10)
    assertEquals(run(a), Right(expected))
  }

  test("indx with a vector") {
    val a = v(10, 11, 25)

    val expected = v(10, 11, 25)
    assertEquals(run(a), Right(expected))
  }

  test("indx with a matrix") {
    val a = m(2, 3)(1, 2, 3, 4, 5, 6)

    val expected = m(3, 2)(1, 4, 2, 5, 3, 6)
    assertEquals(run(a), Right(expected))
  }
}
