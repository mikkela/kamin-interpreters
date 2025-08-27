package apl

import com.mikadocs.kamin.{IntegerValue, MatrixDimensions, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

final class CatFunctionSuite extends FunSuite {
  // === tiny helpers ========================================================
  private def entry = functionDefinitionTable.lookupFunctionDefinition("cat").getOrElse(fail(s"Function not registered"))
  private def run2(a: Value, b: Value) = entry.function(null, Vector(a, b))
  private def i(x: Int) = IntegerValue(x)
  private def v(xs: Int*) = MatrixValue(xs, MatrixDimensions(1, xs.length))
  private def m(rows: Int, cols: Int)(xs: Int*) = MatrixValue(xs, MatrixDimensions(rows, cols))

  // ========================= Tests ========================================

  // --- Integer ----------------------------------------------------
  test("integer cat with an integer") {
    val a = i(10)
    val b = i(20)
    val expected = v(10, 20) 
    assertEquals(run2(a, b), Right(expected))
  }

  test("integer cat with a vector") {
    val a = i(10)
    val b = v(20, 30, 40)
    val expected = v(10, 20, 30, 40)
    assertEquals(run2(a, b), Right(expected))
  }

  test("integer cat with a matrix") {
    val a = i(10)
    val b = m(2,2)(20, 30, 40, 50)
    val expected = v(10, 20, 30, 40, 50)
    assertEquals(run2(a, b), Right(expected))
  }

  // --- Vector ----------------------------------------------------
  test("vector cat with an integer") {
    val a = v(10, 20)
    val b = i(20)
    val expected = v(10, 20, 20)
    assertEquals(run2(a, b), Right(expected))
  }

  test("vector cat with a vector") {
    val a = v(10, 20)
    val b = v(20, 30, 40)
    val expected = v(10, 20, 20, 30, 40)
    assertEquals(run2(a, b), Right(expected))
  }

  test("vector cat with a matrix") {
    val a = v(10, 20)
    val b = m(2, 2)(20, 30, 40, 50)
    val expected = v(10, 20, 20, 30, 40, 50)
    assertEquals(run2(a, b), Right(expected))
  }

  // --- Matrix ----------------------------------------------------
  test("matrix cat with an integer") {
    val a = m(2, 2)(10, 20, 30, 40)
    val b = i(20)
    val expected = v(10, 20, 30, 40, 20)
    assertEquals(run2(a, b), Right(expected))
  }

  test("matrix cat with a vector") {
    val a = m(2, 2)(10, 20, 30, 40)
    val b = v(20, 30, 40)
    val expected = v(10, 20, 30, 40, 20, 30, 40)
    assertEquals(run2(a, b), Right(expected))
  }

  test("matrix cat with a matrix") {
    val a = m(2, 2)(10, 20, 30, 40)
    val b = m(2, 2)(20, 30, 40, 50)
    val expected = v(10, 20, 30, 40, 20, 30, 40, 50)
    assertEquals(run2(a, b), Right(expected))
  }
}