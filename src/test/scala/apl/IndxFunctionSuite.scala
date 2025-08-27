package apl

import com.mikadocs.kamin.{IntegerValue, MatrixDimensions, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

final class IndxFunctionSuite extends FunSuite{
  // === tiny helpers ========================================================
  private def entry = functionDefinitionTable.lookupFunctionDefinition("indx").getOrElse(fail(s"Function not registered"))

  private def run(a: Value) = entry.function(null, Vector(a))

  private def i(x: Int) = IntegerValue(x)

  private def v(xs: Int*) = MatrixValue(xs, MatrixDimensions(if xs.length > 0 then 1 else 0, xs.length))

  private def m(rows: Int, cols: Int)(xs: Int*) = MatrixValue(xs, MatrixDimensions(rows, cols))

  // ---- Tests ----------------------------------------------------------------
  test("indx with a positive integer") {
    val a = i(10)

    val expected = v(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    assertEquals(run(a), Right(expected))
  }

  // --- Errors --------------------------------------------------------------

  test("error: argument must a positive integer (reject non-vector matrix)") {
    val notInteger = m(2, 2)(1, 2, 3, 4)
    assertEquals(run(notInteger), Left("Invalid type. Expected positive integer"))
  }

  test("error: argument must be a positive integer (reject zero") {
    val notPositive = i(0)
    assertEquals(run(notPositive), Left("Invalid type. Expected positive integer"))
  }
}
