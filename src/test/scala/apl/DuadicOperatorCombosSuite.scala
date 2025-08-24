package apl

import munit.FunSuite

import com.mikadocs.kamin.apl.functionDefinitionTable
import com.mikadocs.kamin.{Value, IntegerValue, MatrixValue} // and Dimensions if you need it elsewhere

final class DuadicOperatorCombosSuite extends FunSuite {

  // -- helpers ---------------------------------------------------------------

  /** Fetch the function entry for a symbol.
   * If your table doesn't have `get`, tweak this to use whatever it exposes.
   */
  private def entry(symbol: String) =
    functionDefinitionTable.lookupFunctionDefinition(symbol)
      .getOrElse(fail(s"Function not registered: $symbol"))

  /** Run a binary op. Env/evaluator is unused by these ops, so we pass null. */
  private def run2(symbol: String, a: Value, b: Value) =
    entry(symbol).function(null, Vector(a, b))

  /** Build a 1×n vector. */
  private def v(xs: Int*): MatrixValue =
    MatrixValue.vector(xs.toVector)

  // -- + ---------------------------------------------------------------------

  test("+ scalar/scalar") {
    assertEquals(run2("+", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(5)))
  }

  test("+ matrix/scalar") {
    assertEquals(run2("+", v(1, 2, 3), IntegerValue(10)), Right(v(11, 12, 13)))
  }

  test("+ scalar/matrix") {
    assertEquals(run2("+", IntegerValue(10), v(1, 2, 3)), Right(v(11, 12, 13)))
  }

  test("+ matrix/matrix (same shape)") {
    assertEquals(run2("+", v(1, 2, 3), v(4, 5, 6)), Right(v(5, 7, 9)))
  }

  test("+ matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2("+", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }

  // -- - ---------------------------------------------------------------------

  test("- scalar/scalar") {
    assertEquals(run2("-", IntegerValue(7), IntegerValue(4)), Right(IntegerValue(3)))
  }

  test("- matrix/scalar") {
    assertEquals(run2("-", v(1, 2, 3), IntegerValue(10)), Right(v(-9, -8, -7)))
  }

  test("- scalar/matrix") {
    assertEquals(run2("-", IntegerValue(10), v(1, 2, 3)), Right(v(9, 8, 7)))
  }

  test("- matrix/matrix (same shape)") {
    assertEquals(run2("-", v(5, 6, 7), v(1, 2, 3)), Right(v(4, 4, 4)))
  }

  test("- matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2("-", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }

  // -- * ---------------------------------------------------------------------

  test("* scalar/scalar") {
    assertEquals(run2("*", IntegerValue(6), IntegerValue(7)), Right(IntegerValue(42)))
  }

  test("* matrix/scalar") {
    assertEquals(run2("*", v(1, 2, 3), IntegerValue(10)), Right(v(10, 20, 30)))
  }

  test("* scalar/matrix") {
    assertEquals(run2("*", IntegerValue(10), v(1, 2, 3)), Right(v(10, 20, 30)))
  }

  test("* matrix/matrix (same shape)") {
    assertEquals(run2("*", v(1, 2, 3), v(4, 5, 6)), Right(v(4, 10, 18)))
  }

  test("* matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2("*", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }

  // -- / ---------------------------------------------------------------------

  test("/ scalar/scalar") {
    assertEquals(run2("/", IntegerValue(6), IntegerValue(3)), Right(IntegerValue(2)))
  }

  test("/ scalar/scalar division by zero -> Division with zero") {
    assertEquals(run2("/", IntegerValue(10), IntegerValue(0)), Left("Division with zero"))
  }

  test("/ matrix/scalar OK when scalar != 0") {
    assertEquals(run2("/", v(10, 20, 30), IntegerValue(10)), Right(v(1, 2, 3)))
  }

  test("/ matrix/scalar division by zero -> Division with zero") {
    assertEquals(run2("/", v(10, 20, 30), IntegerValue(0)), Left("Division with zero"))
  }

  test("/ scalar/matrix OK when no zeros") {
    assertEquals(run2("/", IntegerValue(100), v(5, 10, 20)), Right(v(20, 10, 5)))
  }

  test("/ scalar/matrix fails if any denominator is zero") {
    assertEquals(run2("/", IntegerValue(100), v(5, 0, 10)), Left("Division with zero"))
  }

  test("/ matrix/matrix same shape and no zeros in RHS") {
    assertEquals(run2("/", v(100, 200, 300), v(10, 20, 30)), Right(v(10, 10, 10)))
  }

  test("/ matrix/matrix fails if any RHS element is zero") {
    assertEquals(run2("/", v(100, 200, 300), v(10, 0, 30)), Left("Division with zero"))
  }

  test("/ matrix/matrix (shape mismatch -> Different shapes)") {
    assertEquals(run2("/", v(1, 2), v(1, 2, 3)), Left("Different shapes"))
  }

  // -- = ---------------------------------------------------------------------

  test("= scalar/scalar") {
    assertEquals(run2("=", IntegerValue(6), IntegerValue(7)), Right(IntegerValue(0)))
    assertEquals(run2("=", IntegerValue(7), IntegerValue(7)), Right(IntegerValue(1)))
  }

  test("= matrix/scalar") {
    assertEquals(run2("=", v(1, 2, 3), IntegerValue(2)), Right(v(0, 1, 0)))
  }

  test("= scalar/matrix") {
    assertEquals(run2("=", IntegerValue(1), v(1, 2, 3)), Right(v(1, 0, 0)))
  }

  test("= matrix/matrix (same shape)") {
    assertEquals(run2("=", v(1, 2, 3), v(2, 1, 3)), Right(v(0, 0, 1)))
  }

  test("* matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2("=", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }

  // -- < ---------------------------------------------------------------------

  test("< scalar/scalar") {
    assertEquals(run2("<", IntegerValue(6), IntegerValue(7)), Right(IntegerValue(1)))
    assertEquals(run2("<", IntegerValue(7), IntegerValue(6)), Right(IntegerValue(0)))
  }

  test("< matrix/scalar") {
    assertEquals(run2("<", v(1, 2, 3), IntegerValue(2)), Right(v(1, 0, 0)))
  }

  test("< scalar/matrix") {
    assertEquals(run2("<", IntegerValue(1), v(1, 2, 3)), Right(v(0, 1, 1)))
  }

  test("< matrix/matrix (same shape)") {
    assertEquals(run2("<", v(1, 2, 3), v(2, 1, 3)), Right(v(1, 0, 0)))
  }

  test("< matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2("<", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }

  // -- > ---------------------------------------------------------------------

  test("> scalar/scalar") {
    assertEquals(run2(">", IntegerValue(6), IntegerValue(7)), Right(IntegerValue(0)))
    assertEquals(run2(">", IntegerValue(7), IntegerValue(6)), Right(IntegerValue(1)))
  }

  test("> matrix/scalar") {
    assertEquals(run2(">", v(1, 2, 3), IntegerValue(2)), Right(v(0, 0, 1)))
  }

  test("> scalar/matrix") {
    assertEquals(run2(">", IntegerValue(1), v(0, 1, 2)), Right(v(1, 0, 0)))
  }

  test("> matrix/matrix (same shape)") {
    assertEquals(run2(">", v(1, 2, 3), v(2, 1, 3)), Right(v(0, 1, 0)))
  }

  test("> matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2(">", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }

  // -- max ---------------------------------------------------------------------

  test("max scalar/scalar") {
    assertEquals(run2("max", IntegerValue(6), IntegerValue(7)), Right(IntegerValue(7)))
  }

  test("max matrix/scalar") {
    assertEquals(run2("max", v(1, 2, 3), IntegerValue(2)), Right(v(2, 2, 3)))
  }

  test("max scalar/matrix") {
    assertEquals(run2("max", IntegerValue(1), v(0, 1, 2)), Right(v(1, 1, 2)))
  }

  test("max matrix/matrix (same shape)") {
    assertEquals(run2("max", v(1, 2, 3), v(2, 1, 3)), Right(v(2, 2, 3)))
  }

  test("max matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2("max", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }

  // -- min ---------------------------------------------------------------------

  test("min scalar/scalar") {
    assertEquals(run2("min", IntegerValue(6), IntegerValue(7)), Right(IntegerValue(6)))
  }

  test("min matrix/scalar") {
    assertEquals(run2("min", v(1, 2, 3), IntegerValue(2)), Right(v(1, 2, 2)))
  }

  test("min scalar/matrix") {
    assertEquals(run2("min", IntegerValue(1), v(0, 1, 2)), Right(v(0, 1, 1)))
  }

  test("min matrix/matrix (same shape)") {
    assertEquals(run2("min", v(1, 2, 3), v(2, 1, 3)), Right(v(1, 1, 3)))
  }

  test("min matrix/matrix (shape mismatch -> Shape mismatch)") {
    assertEquals(run2("min", v(1, 2), v(1, 2, 3)), Left("Shape mismatch"))
  }
}