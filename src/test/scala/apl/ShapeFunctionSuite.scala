package apl

import com.mikadocs.kamin.{IntegerValue, MatrixDimensions, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

final class ShapeFunctionSuite extends FunSuite {
  // === tiny helpers ========================================================
  private def entry = functionDefinitionTable.lookupFunctionDefinition("shape").getOrElse(fail(s"Function not registered"))
  private def run(a: Value) = entry.function(null, Vector(a))
  private def i(x: Int) = IntegerValue(x)
  private def v(xs: Int*) = MatrixValue(xs, MatrixDimensions(if xs.length > 0 then 1 else 0, xs.length))
  private def m(rows: Int, cols: Int)(xs: Int*) = MatrixValue(xs, MatrixDimensions(rows, cols))

  // ========================= Tests ========================================

  test("integer->vector: integer returns a 0-vector") {
    val data = i(10)
    val expected = v() // 0 vector
    assertEquals(run(data), Right(expected))
  }

  test("vector->vector: vector returns a 1-vector") {
    val data = v(10, 20, 30, 40)
    val expected = v(4)                // 4 columns
    assertEquals(run(data), Right(expected))
  }

  test("matrix->vector: matrix returns a 2-vector") {
    val data = m(3, 2)(1, 2, 3, 4, 5, 6)
    val expected = v(3, 2) // 3 rows of 2 columns
    assertEquals(run(data), Right(expected))
  }
}
