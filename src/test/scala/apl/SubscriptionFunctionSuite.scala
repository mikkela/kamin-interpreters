package apl

import com.mikadocs.kamin.{IntegerValue, MatrixDimensions, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

final class SubscriptionFunctionSuite extends FunSuite {
  // === tiny helpers ========================================================
  private def entry = functionDefinitionTable.lookupFunctionDefinition("[]").getOrElse(fail(s"Function not registered"))

  private def run2(a: Value, b: Value) = entry.function(null, Vector(a, b))

  private def i(x: Int) = IntegerValue(x)

  private def v(xs: Int*) = MatrixValue(xs, MatrixDimensions(1, xs.length))

  private def m(rows: Int, cols: Int)(xs: Int*) = MatrixValue(xs, MatrixDimensions(rows, cols))

  // ========================= Tests ========================================

  // --- Subscribe with an integer ----------------------------------------------------
  test("integer [] within a vector") {
    val a = v(10, 20, 30, 40)
    val b = i(3)
    val expected = v(30)
    assertEquals(run2(a, b), Right(expected))
  }

  test("integer [] within a matrix") {
    val a = m(3, 2)(10, 20, 30, 40, 50, 60)
    val b = i(3)
    val expected = v(50, 60)
    assertEquals(run2(a, b), Right(expected))
  }

  // --- Subscribe with a a vector ----------------------------------------------------
  test("vector [] within a vector") {
    val a = v(10, 20, 30, 40)
    val b = v(1, 3)
    val expected = v(10, 30)
    assertEquals(run2(a, b), Right(expected))
  }

  test("integer [] within a matrix") {
    val a = m(3, 2)(10, 20, 30, 40, 50, 60)
    val b = v(1, 3)
    val expected = m(2, 2)(10, 20, 50, 60)
    assertEquals(run2(a, b), Right(expected))
  }
}
