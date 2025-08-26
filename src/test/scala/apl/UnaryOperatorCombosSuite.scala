package apl

import com.mikadocs.kamin.{Dimensions, IntegerValue, MatrixValue, Value}
import com.mikadocs.kamin.apl.functionDefinitionTable
import munit.FunSuite

import scala.collection.immutable.Seq as run

class UnaryOperatorCombosSuite extends FunSuite {
  // -- helpers ---------------------------------------------------------------

  /** Fetch the function entry for a symbol.
   */
  private def entry(symbol: String) =
    functionDefinitionTable.lookupFunctionDefinition(symbol)
      .getOrElse(fail(s"Function not registered: $symbol"))
      
  /** Run a unary op. Env/evaluator is unused by these ops, so we pass null. */
  private def run(symbol: String, a: Value) =
    entry(symbol).function(null, Vector(a))
    
  // -- +/ ---------------------------------------------------------------------

  test("+/ scalar") {
    assertEquals(run("+/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("+/ vector") {
    assertEquals(run("+/", MatrixValue(Seq(1, 2, 3), Dimensions(1, 3))), Right(IntegerValue(6)))
  }

  test("+/ matrix") {
    assertEquals(run("+/", MatrixValue(Seq(1, 2, 3, 4, 5, 6), Dimensions(3, 2))), Right(MatrixValue(Seq(3, 7, 11), Dimensions(3, 1))))
  }

  // -- -/ ---------------------------------------------------------------------

  test("-/ scalar") {
    assertEquals(run("-/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("-/ vector") {
    assertEquals(run("-/", MatrixValue(Seq(1, 2, 3), Dimensions(1, 3))), Right(IntegerValue(-4)))
  }

  test("-/ matrix") {
    assertEquals(run("-/", MatrixValue(Seq(1, 2, 3, 4, 5, 6), Dimensions(3, 2))), Right(MatrixValue(Seq(-1, -1, -1), Dimensions(3, 1))))
  }

  // -- */ ---------------------------------------------------------------------

  test("*/ scalar") {
    assertEquals(run("*/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("*/ vector") {
    assertEquals(run("*/", MatrixValue(Seq(1, 2, 3), Dimensions(1, 3))), Right(IntegerValue(6)))
  }

  test("*/ matrix") {
    assertEquals(run("*/", MatrixValue(Seq(1, 2, 3, 4, 5, 6), Dimensions(3, 2))), Right(MatrixValue(Seq(2, 12, 30), Dimensions(3, 1))))
  }

  // -- // ---------------------------------------------------------------------

  test("// scalar") {
    assertEquals(run("//", IntegerValue(2)), Right(IntegerValue(2)))
    assertEquals(run("//", IntegerValue(0)), Right(IntegerValue(0)))
  }

  test("// vector") {
    assertEquals(run("//", MatrixValue(Seq(4, 2, 2), Dimensions(1, 3))), Right(IntegerValue(1)))
    assertEquals(run("//", MatrixValue(Seq(0, 2, 2), Dimensions(1, 3))), Right(IntegerValue(0)))
    assertEquals(run("//", MatrixValue(Seq(4, 2, 0), Dimensions(1, 3))), Left("Division with zero"))
  }

  test("// matrix") {
    assertEquals(run("//", MatrixValue(Seq(0, 1, 8, 4, 18, 6), Dimensions(3, 2))), Right(MatrixValue(Seq(0, 2, 3), Dimensions(3, 1))))
    assertEquals(run("//", MatrixValue(Seq(2, 1, 8, 0, 18, 6), Dimensions(3, 2))), Left("Division with zero"))
  }

  // -- and/ ---------------------------------------------------------------------

  test("and/ scalar") {
    assertEquals(run("and/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("and/ vector") {
    assertEquals(run("and/", MatrixValue(Seq(1, 2, 3), Dimensions(1, 3))), Right(IntegerValue(1)))
    assertEquals(run("and/", MatrixValue(Seq(1, 0, 3), Dimensions(1, 3))), Right(IntegerValue(0)))
  }

  test("and/ matrix") {
    assertEquals(run("and/", MatrixValue(Seq(1, 2, 3, 0, 0, 6), Dimensions(3, 2))), Right(MatrixValue(Seq(1, 0, 0), Dimensions(3, 1))))
  }

  // -- or/ ---------------------------------------------------------------------

  test("or/ scalar") {
    assertEquals(run("or/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("or/ vector") {
    assertEquals(run("or/", MatrixValue(Seq(1, 0, 3), Dimensions(1, 3))), Right(IntegerValue(1)))
    assertEquals(run("or/", MatrixValue(Seq(0, 0, 0), Dimensions(1, 3))), Right(IntegerValue(0)))
  }

  test("or/ matrix") {
    assertEquals(run("or/", MatrixValue(Seq(0, 0, 3, 0, 0, 6), Dimensions(3, 2))), Right(MatrixValue(Seq(0, 1, 1), Dimensions(3, 1))))
  }

  // -- max/ ---------------------------------------------------------------------

  test("max/ scalar") {
    assertEquals(run("max/", IntegerValue(2)), Right(IntegerValue(2)))
  }

  test("max/ vector") {
    assertEquals(run("max/", MatrixValue(Seq(1, 2, 3), Dimensions(1, 3))), Right(IntegerValue(3)))
  }

  test("max/ matrix") {
    assertEquals(run("max/", MatrixValue(Seq(1, 2, 3, 4, 6, 5), Dimensions(3, 2))), Right(MatrixValue(Seq(2, 4, 6), Dimensions(3, 1))))
  }
}
