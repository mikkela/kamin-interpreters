package com.mikadocs.kamin
package apl

object functionDefinitionTable extends FunctionDefinitionTable[Value](e => APLEvaluator(e)):
  private def shape(v: MatrixValue) = v.dimensions

  putBinaryIntMatrixOp("+")(_ + _)
  putBinaryIntMatrixOp("-")(_ - _)
  putBinaryIntMatrixOp("/")(
    op = _ / _,
    validate = (_, b) => if (b == 0) Left("Division with zero") else Right(()),
    shapeError = "Different shapes"
  )
  putBinaryIntMatrixOp("*")(_ * _)
  putBinaryIntMatrixOp("=")((x, y) => if x == y then 1 else 0)
  putBinaryIntMatrixOp("<")((x, y) => if x < y then 1 else 0)
  putBinaryIntMatrixOp(">")((x, y) => if x > y then 1 else 0)
  putBinaryIntMatrixOp("max")(math.max(_, _))
  putBinaryIntMatrixOp("min")(math.min(_, _))

  table.put("car", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case ListValue(v) =>
          if v.nonEmpty then Right(v.head) else Left("The list is empty")
        case _ => Left("Invalid types")
  ))

  table.put("cdr", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case ListValue(v) =>
          if v.nonEmpty then Right(ListValue(v.tail)) else Left("The list is empty")
        case _ => Left("Invalid types")
  ))

  table.put("cons", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (head , tail: ListValue) =>
          Right(ListValue(head :: tail.value))
        case _ => Right(ListValue.nil)
  ))

  table.put("number?", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case IntegerValue(_) => Right(SymbolValue.T)
        case _ => Right(ListValue.nil)

  ))

  table.put("symbol?", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case SymbolValue(_) => Right(SymbolValue.T)
        case _ => Right(ListValue.nil)
  ))

  table.put("list?", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case ListValue(v) =>
          if v.nonEmpty then Right(SymbolValue.T) else Right(ListValue.nil)
        case _ => Right(ListValue.nil)
  ))

  table.put("null?", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case ListValue(v) =>
          if v.isEmpty then Right(SymbolValue.T) else Right(ListValue.nil)
        case _ => Right(ListValue.nil)
  ))

  table.put("print", FunctionDefinitionEntry(1, (env, arguments) =>
    print(arguments.head.toString)
    Right(arguments.head)))

  // Generic helper with optional per-pair validation and customizable shape error
  private def putBinaryIntMatrixOp(
                                    symbol: String
                                  )(
                                    op: (Int, Int) => Int,
                                    validate: (Int, Int) => Either[String, Unit] = (_, _) => Right(()),
                                    shapeError: String = "Shape mismatch"
                                  ): Unit =
    table.put(symbol,
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
            if (shape(m1) != shape(m2)) Left(shapeError)
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
    )

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
