package com.mikadocs.kamin
package apl

object functionDefinitionTable extends FunctionDefinitionTable[Value](e => APLEvaluator(e)):
  table.put("+", createBinaryOperation(_ + _))
  table.put("-", createBinaryOperation(_ - _))
  table.put("/", createBinaryOperation(
    op = _ / _,
    validate = (_, b) => if (b == 0) Left("Division with zero") else Right(()),
    shapeError = "Different shapes"
  ))
  table.put("*", createBinaryOperation(_ * _))
  table.put("=", createBinaryOperation((x, y) => if x == y then 1 else 0))
  table.put("<", createBinaryOperation((x, y) => if x < y then 1 else 0))
  table.put(">", createBinaryOperation((x, y) => if x > y then 1 else 0))
  table.put("max", createBinaryOperation(math.max(_, _)))
  table.put("min", createBinaryOperation(math.min(_, _)))
  table.put("and", createBinaryOperation((x, y) => if x != 0 && y != 0 then 1 else 0))
  table.put("or", createBinaryOperation((x, y) => if x != 0 || y != 0 then 1 else 0))

  table.put("+/", createAccumulatingOperation(_ + _))
  table.put("-/", createAccumulatingOperation(_ - _))
  table.put("*/", createAccumulatingOperation(_ * _))
  table.put("//", createAccumulatingOperation(
    op = _ / _,
    validate = s => if (s.length > 0 && s.tail.contains(0)) Left("Division with zero") else Right(())
  ))
  table.put("and/", createAccumulatingOperation((x, y) => if x != 0 && y != 0 then 1 else 0))
  table.put("or/", createAccumulatingOperation((x, y) => if x != 0 || y != 0 then 1 else 0))
  table.put("max/", createAccumulatingOperation((x, y) => if x > y then x else y))

  table.put("compress", FunctionDefinitionEntry(2,
    (env, arguments) =>
      val a = arguments.head
      val b = arguments(1)
      a match
        case am: MatrixValue if am.isVector =>
          b match
            case bm: MatrixValue => Right(MatrixValue.compress(am, bm))
            case _ => Left("Invalid 2nd argument. Must be a matrix")
        case _ => Left("Invalid 1st argument. Must be a vector")
  ))

  table.put("shape", FunctionDefinitionEntry(1,
    (env, arguments) =>
      val a = arguments.head
      a match
        case _: IntegerValue => Right(MatrixValue.nullMatrix)
        case m: MatrixValue => Right(m.shape)
  ))

  table.put("ravel", FunctionDefinitionEntry(1,
    (env, arguments) =>
      val a = arguments.head
      Right(ravel(a))
  ))

  table.put("restruct", FunctionDefinitionEntry(2,
    (env, arguments) =>
      val a = arguments.head
      val b = arguments(1)
      a match
        case am: MatrixValue if am.isShapeVector =>
          b match
            case IntegerValue(v) => Right(MatrixValue.restruct(am, MatrixValue.toMatrix(v)))
            case bm: MatrixValue if !bm.isNullMatrix => Right(MatrixValue.restruct(am, bm))
            case _ => Left("Invalid 2nd argument. It is a null matrix")
        case _ => Left("Invalid 1st argument. Must be a shape vector")

  ))

  table.put("cat", FunctionDefinitionEntry(2,
    (env, arguments) =>
      val a = ravel(arguments.head)
      val b = ravel(arguments(1))

      Right(MatrixValue.vector(a.value ++ b.value))
  ))

  table.put("indx", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case IntegerValue(v) if v > 0 =>
          Right(MatrixValue.index(v))
        case _ => Left("Invalid type. Expected positive integer")
  ))

  table.put("trans", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case v:IntegerValue => Right(v)
        case m:MatrixValue =>
          Right(m.transpose)
  ))


  table.put("[]", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (m:MatrixValue, i:IntegerValue) =>
          Right(simplify(m.subscription(MatrixValue.vector(Seq(i.value)))))
        case (m1: MatrixValue, m2: MatrixValue) if m2.isVector =>
          Right(simplify(m1.subscription(m2)))
        case _ => Left("Invalid type. Expected matrix as first argument and vector or integer as second")
  ))

  table.put("print", FunctionDefinitionEntry(1, (env, arguments) =>
    print(arguments.head.toString)
    Right(arguments.head)))

  private def simplify(v: Value): Value =
    v match
      case MatrixValue(value, dimensions) if dimensions.cols == 1 && dimensions.rows == 1 => IntegerValue(value.head)
      case _ => v

  private def ravel(value: Value): MatrixValue =
    value match
      case IntegerValue(v) => MatrixValue.toMatrix(v)
      case m: MatrixValue => m.ravel

  private def createAccumulatingOperation(
                                           op: (Int, Int) => Int,
                                           validate: Seq[Int] => Either[String, Unit] = _ => Right(())
                                         ) : FunctionDefinitionEntry =
    FunctionDefinitionEntry(1, (env, arguments) => {
      val v = arguments.head

      v match
        case IntegerValue(value) => Right(IntegerValue(value))
        case MatrixValue(value, dimensions) =>
          validate(value).map(_ =>
            simplify(
              MatrixValue(
                value.sliding(dimensions.cols, dimensions.cols).map(_.reduce(op)).toSeq, MatrixDimensions(dimensions.rows, 1)
              )
            )
          )
    })
  // Generic helper with optional per-pair validation and customizable shape error
  private def createBinaryOperation(
                                    op: (Int, Int) => Int,
                                    validate: (Int, Int) => Either[String, Unit] = (_, _) => Right(()),
                                    shapeError: String = "Shape mismatch"
                                  ): FunctionDefinitionEntry =
    FunctionDefinitionEntry(2, (env, arguments) => {
      val v1 = arguments.head
      val v2 = arguments(1)

      (v1, v2) match
        // scalar ∘ scalar
        case (IntegerValue(a), IntegerValue(b)) =>
          validate(a, b).map(_ => IntegerValue(op(a, b)))

        // matrix ∘ scalar
        case (MatrixValue(as, d1), IntegerValue(b)) =>
          // ensure all pairs (x,b) validate
          as.foldLeft[Either[String, Unit]](Right(())) { (acc, x) =>
            acc.flatMap(_ => validate(x, b))
          }.map(_ => MatrixValue(as.map(x => op(x, b)), d1))

        // scalar ∘ matrix
        case (IntegerValue(a), MatrixValue(bs, d2)) =>
          bs.foldLeft[Either[String, Unit]](Right(())) { (acc, y) =>
            acc.flatMap(_ => validate(a, y))
          }.map(_ => MatrixValue(bs.map(y => op(a, y)), d2))

        // matrix ∘ matrix (element-wise)
        case (m1 @ MatrixValue(as, d1), m2 @ MatrixValue(bs, d2)) =>
          if (m1.dimensions != m2.dimensions) Left(shapeError)
          else
            // validate pairwise first
            val validated = as.zip(bs).foldLeft[Either[String, Unit]](Right(())) {
              case (acc, (x, y)) => acc.flatMap(_ => validate(x, y))
            }
            validated.map(_ =>
              MatrixValue(as.zip(bs).map { case (x, y) => op(x, y) }, d1)
            )


        case _ =>
          Left("Invalid types")
      })

class APLEvaluator(val currentEnvironment: Environment[Value])
  extends Evaluator[Value]:
  private val valueExpressionEvaluator = ValueExpressionEvaluator[Value]()
  private val variableExpressionEvaluator = VariableExpressionEvaluator[Value](currentEnvironment)
  private val ifExpressionEvaluator = IfExpressionEvaluator[Value](this, ListValue.nil)
  private val whileExpressionEvaluator = WhileExpressionEvaluator[Value](this, ListValue.nil)
  private val setExpressionEvaluator = SetExpressionEvaluator[Value](this, currentEnvironment)
  private val beginExpressionEvaluator = BeginExpressionEvaluator[Value](this)
  private val operationExpressionEvaluator = OperationExpressionEvaluator[Value](this, currentEnvironment, functionDefinitionTable)
  override def visit(node: Node): Either[String, Value] =
    node match
      case n:ValueExpressionNode => valueExpressionEvaluator.evaluate(n)
      case n:VariableExpressionNode => variableExpressionEvaluator.evaluate(n)
      case n: IfExpressionNode => ifExpressionEvaluator.evaluate(n)
      case n:WhileExpressionNode => whileExpressionEvaluator.evaluate(n)
      case n: SetExpressionNode => setExpressionEvaluator.evaluate(n)
      case n: BeginExpressionNode => beginExpressionEvaluator.evaluate(n)
      case n: OperationExpressionNode => operationExpressionEvaluator.evaluate(n)

object APLEvaluator:
  val globalEnvironment: Environment[Value] = GlobalEnvironment[Value]()

  def apply(env: Environment[Value]): APLEvaluator =
    new APLEvaluator(env)

  def default(): APLEvaluator =
    new APLEvaluator(globalEnvironment)
