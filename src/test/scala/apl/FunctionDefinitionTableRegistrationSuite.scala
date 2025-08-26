// src/test/scala/<yourpkg>/FunctionDefinitionTableRegistrationSuite.scala
// package <yourpkg>   // <- set your package if you use one

import munit.FunSuite

// Import your production types/instances:
import com.mikadocs.kamin.apl.functionDefinitionTable
import com.mikadocs.kamin.{Value, IntegerValue, MatrixValue} // and Dimensions if you need it elsewhere

final class FunctionDefinitionTableRegistrationSuite extends FunSuite {
  // ---- helpers -------------------------------------------------------------

  /** Look up a function entry by symbol.
   * Assumes your table exposes `get(symbol): Option[FunctionDefinitionEntry[Value]]`.
   * If your API differs (e.g., `lookup`, `apply`, etc.), just tweak this one method.
   */
  private def entry(symbol: String) =
    functionDefinitionTable
      .lookupFunctionDefinition(symbol)
      .getOrElse(fail(s"Not registered: $symbol"))

  /** Run an unary op with one argument.
  * We pass `null` for the environment; the ops you registered don't use it.
  */
  private def run(symbol: String, a: Value) =
    entry(symbol).function(null, Vector(a)) // env is unused by these ops

  /** Run a binary op with two arguments.
   * We pass `null` for the environment; the ops you registered don't use it.
   */
  private def run2(symbol: String, a: Value, b: Value) =
    entry(symbol).function(null, Vector(a, b)) // env is unused by these ops

  // Convenience matrix builders for vectors (1 × n)
  private def v(xs: Int*): MatrixValue = MatrixValue.vector(xs.toVector)

  // ---- registration presence & arity --------------------------------------

  test("registers +, -, /, *, =, <, >, max, min, +/, -/, */, //") {
    List("+", "-", "/", "*", "=", "<", ">", "max", "min", "or", "and", "+/", "-/", "*/", "//", "max/", "and/", "or/").foreach { s =>
      assert(functionDefinitionTable.lookupFunctionDefinition(s).nonEmpty, clues(s))
    }
  }

  test("all registered arithmetic operators have correct arity") {
    assertEquals(entry("+").numberOfArguments, 2)
    assertEquals(entry("-").numberOfArguments, 2)
    assertEquals(entry("/").numberOfArguments, 2)
    assertEquals(entry("*").numberOfArguments, 2)
    assertEquals(entry("=").numberOfArguments, 2)
    assertEquals(entry("<").numberOfArguments, 2)
    assertEquals(entry(">").numberOfArguments, 2)
    assertEquals(entry("max").numberOfArguments, 2)
    assertEquals(entry("min").numberOfArguments, 2)
    assertEquals(entry("and").numberOfArguments, 2)
    assertEquals(entry("or").numberOfArguments, 2)
    assertEquals(entry("+/").numberOfArguments, 1)
    assertEquals(entry("-/").numberOfArguments, 1)
    assertEquals(entry("*/").numberOfArguments, 1)
    assertEquals(entry("//").numberOfArguments, 1)
    assertEquals(entry("max/").numberOfArguments, 1)
    assertEquals(entry("and/").numberOfArguments, 1)
    assertEquals(entry("or/").numberOfArguments, 1)
  }

  // ---- light smoke tests to verify wiring (not re-testing mechanics) ------

  test("+ produces a scalar result (smoke)") {
    assertEquals(run2("+", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(5)))
  }

  test("- produces a scalar result (smoke)") {
    assertEquals(run2("-", IntegerValue(7), IntegerValue(4)), Right(IntegerValue(3)))
  }

  test("* produces a scalar result (smoke)") {
    assertEquals(run2("*", IntegerValue(6), IntegerValue(7)), Right(IntegerValue(42)))
  }

  test("/ has custom shape error 'Different shapes'") {
    val a = v(1, 2)       // shape (1 × 2)
    val b = v(1, 2, 3)    // shape (1 × 3)
    assertEquals(run2("/", a, b), Left("Different shapes"))
  }

  test("/ rejects division by zero (scalar/scalar)") {
    assertEquals(run2("/", IntegerValue(1), IntegerValue(0)), Left("Division with zero"))
  }

  test("/ rejects division by zero (scalar/matrix if any elem is 0)") {
    val denom = v(5, 0, 10)
    assertEquals(run2("/", IntegerValue(100), denom), Left("Division with zero"))
  }

  test("/ works on matrix/scalar when scalar != 0 (smoke)") {
    val m = v(10, 20, 30)
    assertEquals(run2("/", m, IntegerValue(10)), Right(v(1, 2, 3)))
  }

  test("= produces a scalar result (smoke)") {
    assertEquals(run2("=", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(0)))
  }

  test("< produces a scalar result (smoke)") {
    assertEquals(run2("<", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(1)))
  }

  test("> produces a scalar result (smoke)") {
    assertEquals(run2(">", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(0)))
  }

  test("max produces a scalar result (smoke)") {
    assertEquals(run2("max", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(3)))
  }

  test("min produces a scalar result (smoke)") {
    assertEquals(run2("min", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(2)))
  }

  test("and produces a scalar result (smoke)") {
    assertEquals(run2("and", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(1)))
  }

  test("or produces a scalar result (smoke)") {
    assertEquals(run2("or", IntegerValue(2), IntegerValue(3)), Right(IntegerValue(1)))
  }

  test("+/ produces a scalar result (smoke)") {
    assertEquals(run("+/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("-/ produces a scalar result (smoke)") {
    assertEquals(run("-/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("*/ produces a scalar result (smoke)") {
    assertEquals(run("*/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("// produces a scalar result (smoke)") {
    assertEquals(run("//", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("max/ produces a scalar result (smoke)") {
    assertEquals(run("max/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("and/ produces a scalar result (smoke)") {
    assertEquals(run("and/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("or/ produces a scalar result (smoke)") {
    assertEquals(run("or/", IntegerValue(2)), Right(IntegerValue(2)))
  }
}
