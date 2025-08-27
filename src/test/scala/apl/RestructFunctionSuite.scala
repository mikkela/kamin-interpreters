package apl

import com.mikadocs.kamin.{IntegerValue, MatrixDimensions, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

final class RestructFunctionSuite extends FunSuite {
  // === tiny helpers ========================================================
  private def entry = functionDefinitionTable.lookupFunctionDefinition("restruct").getOrElse(fail(s"Function not registered"))
  private def run2(a: Value, b: Value) = entry.function(null, Vector(a, b))
  private def i(x: Int): IntegerValue = IntegerValue(x)
  private def v(xs: Int*) = if xs.isEmpty then MatrixValue.nullVector else MatrixValue(xs, MatrixDimensions(1, xs.length))
  private def m(rows: Int, cols: Int)(xs: Int*) = MatrixValue(xs, MatrixDimensions(rows, cols))

  // ========================= Tests ========================================

  // --- Integer ----------------------------------------------------

  test("integer: restructuring into an integer") {
    val shape = v()
    val data = i(10)
    val expected = i(10)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("integer: restructuring into a vector") {
    val shape = v(5)
    val data = i(10)
    val expected = v(10, 10, 10, 10, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("integer: restructuring into a matrix") {
    val shape = v(2, 3)
    val data = i(10)
    val expected = m(2, 3)(10, 10, 10, 10, 10, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  // --- Vector ----------------------------------------------------

  test("vector: restructuring into an integer") {
    val shape = v()
    val data = v(5, 10, 20, 30)
    val expected = i(5)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("vector: restructuring into same vector") {
    val shape = v(4)
    val data = v(5, 10, 20, 30)
    val expected = v(5, 10, 20, 30)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("vector: restructuring into shorter vector") {
    val shape = v(2)
    val data = v(5, 10, 20, 30)
    val expected = v(5, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("vector: restructuring into longer vector") {
    val shape = v(6)
    val data = v(5, 10, 20, 30)
    val expected = v(5, 10, 20, 30, 5, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  // --- Matrix ----------------------------------------------------

  test("matrix: restructuring into an integer") {
    val shape = v()
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = i(5)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into same length vector") {
    val shape = v(4)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = v(5, 10, 20, 30)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into a shorter vector") {
    val shape = v(3)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = v(5, 10, 20)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into a longer vector") {
    val shape = v(6)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = v(5, 10, 20, 30, 5, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into same shape matrix") {
    val shape = v(2, 2)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = m(2, 2)(5, 10, 20, 30)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into matrix with more columns") {
    val shape = v(2, 3)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = m(2, 3)(5, 10, 20, 30, 5, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into matrix with fewer columns") {
    val shape = v(2, 1)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = m(2, 1)(5, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into matrix with more rows") {
    val shape = v(3, 2)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = m(3, 2)(5, 10, 20, 30, 5, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  test("matrix: restructuring into matrix with fewer rows") {
    val shape = v(1, 2)
    val data = m(2, 2)(5, 10, 20, 30)
    val expected = m(1, 2)(5, 10)
    assertEquals(run2(shape, data), Right(expected))
  }

  // --- Errors --------------------------------------------------------------

  test("error: first argument must be a vector (reject non-vector matrix)") {
    val notVector = m(2, 2)(1, 2, 3, 4)      // rows=2, cols=2 -> not a vector
    val data      = v(10, 20)
    assertEquals(run2(notVector, data), Left("Invalid 1st argument. Must be a shape vector"))
  }

  test("error: first argument must be a shape vector (reject non-vector matrix)") {
    val notVector = v(1, 2, 3) // elements>2 -> not a shape vector
    val data = v(10, 20)
    assertEquals(run2(notVector, data), Left("Invalid 1st argument. Must be a shape vector"))
  }

  test("error: second argument must not be a null matrix") {
    assertEquals(run2(v(), MatrixValue.nullMatrix), Left("Invalid 2nd argument. It is a null matrix"))
  }
}
