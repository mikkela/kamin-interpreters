package apl

import com.mikadocs.kamin.{IntegerValue, MatrixDimensions, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

final class CompressFunctionSuite extends FunSuite {
  // === tiny helpers ========================================================
  private def entry = functionDefinitionTable.lookupFunctionDefinition("compress").getOrElse(fail(s"Function not registered"))
  private def run2(a: Value, b: Value) = entry.function(null, Vector(a, b))
  private def v(xs: Int*) = MatrixValue(xs, MatrixDimensions(1, xs.length))
  private def m(rows: Int, cols: Int)(xs: Int*) = MatrixValue(xs, MatrixDimensions(rows, cols))

  // ========================= Tests ========================================

  // --- Vector -> Vector ----------------------------------------------------

  test("vector->vector: basic mask keeps elements where mask != 0") {
    val mask = v(0, 1, 0, 2)                // length 4
    val data = v(10, 20, 30, 40)
    val expected = v(20, 40)                // pick idx 1 and 3
    assertEquals(run2(mask, data), Right(expected))
  }

  test("vector->vector: mask shorter than data => repeat") {
    val mask = v(1, 0)                      // will repeat to length 6 => [1,0,1,0,1,0]
    val data = v(10, 20, 30, 40, 50, 60)
    val expected = v(10, 30, 50)
    assertEquals(run2(mask, data), Right(expected))
  }

  test("vector->vector: mask longer than data => chop") {
    val mask = v(0, 1, 1, 0, 1)             // chopped to first 3 => [0,1,1]
    val data = v(7, 8, 9)
    val expected = v(8, 9)
    assertEquals(run2(mask, data), Right(expected))
  }

  test("vector->vector: all zeros => empty result with (0 x 0) dims") {
    val mask = v(0, 0, 0)
    val data = v(5, 6, 7)
    val expected = MatrixValue(Vector.empty, MatrixDimensions(0, 0))
    assertEquals(run2(mask, data), Right(expected))
  }

  // --- Vector -> Matrix (row filter, then flatten) ------------------------

  test("vector->matrix: repeat mask to rows; keep rows where mask != 0 (flattened)") {
    // 3x2 matrix:
    // [1,2]
    // [3,4]
    // [5,6]
    val data = m(3, 2)(1, 2, 3, 4, 5, 6)
    val mask = v(1, 0)                      // repeat to rows=3 => [1,0,1] => keep row1,row3
    val expected = v(1, 2, 5, 6)            // flattened kept rows
    assertEquals(run2(mask, data), Right(expected))
  }

  test("vector->matrix: chop mask to rows; keep matching rows only") {
    val data = m(3, 2)(1, 2, 3, 4, 5, 6)
    val mask = v(0, 1, 0, 1)                 // chop to 3 => [0,1,0] => keep only middle row
    val expected = v(3, 4)
    assertEquals(run2(mask, data), Right(expected))
  }

  test("vector->matrix: all zeros => empty vector (0 x 0)") {
    val data = m(2, 3)(9, 8, 7, 6, 5, 4)
    val mask = v(0)                          // repeat to rows=2 => [0,0]
    val expected = MatrixValue(Vector.empty, MatrixDimensions(0, 0))
    assertEquals(run2(mask, data), Right(expected))
  }

  // --- Errors --------------------------------------------------------------

  test("error: first argument must be a vector (reject non-vector matrix)") {
    val notVector = m(2, 2)(1, 2, 3, 4)      // rows=2, cols=2 -> not a vector
    val data      = v(10, 20)
    assertEquals(run2(notVector, data), Left("Invalid 1st argument. Must be a vector"))
  }

  test("error: second argument must be a matrix") {
    val mask = v(1, 0, 1)
    val notMatrix: Value = IntegerValue(42)
    assertEquals(run2(mask, notMatrix), Left("Invalid 2nd argument. Must be a matrix"))
  }
}
