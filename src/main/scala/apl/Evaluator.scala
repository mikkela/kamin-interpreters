package com.mikadocs.kamin
package apl

object functionDefinitionTable extends FunctionDefinitionTable[Value](e => APLEvaluator(e)):
  private def shape(v: MatrixValue) = v.dimensions

  table.put("+", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          Right(IntegerValue(v1.value + v2.value))
        case (v1: MatrixValue, v2: IntegerValue) =>
          Right(MatrixValue(v1.value.map(_ + v2.value), v1.dimensions))
        case (v1: IntegerValue, v2: MatrixValue) =>
          Right(MatrixValue(v2.value.map(_ + v1.value), v2.dimensions))
        case (v1: MatrixValue, v2: MatrixValue) if shape(v1) == shape(v2) =>
          Right(MatrixValue(v1.value.zip(v2.value).map{ case(x, y) => x + y}, v1.dimensions))
        case _ => Left("Invalid types")
  ))

  table.put("-", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          Right(IntegerValue(v1.value - v2.value))
        case (v1: MatrixValue, v2: IntegerValue) =>
          Right(MatrixValue(v1.value.map(_ - v2.value), v1.dimensions))
        case (v1: IntegerValue, v2: MatrixValue) =>
          Right(MatrixValue(v2.value.map(v1.value - _), v2.dimensions))
        case (v1: MatrixValue, v2: MatrixValue) if shape(v1) == shape(v2) =>
          Right(MatrixValue(v1.value.zip(v2.value).map{ case(x, y) => x - y}, v1.dimensions))
        case _ => Left("Invalid types")
  ))

  table.put("*", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          Right(IntegerValue(v1.value * v2.value))
        case (v1: MatrixValue, v2: IntegerValue) =>
          Right(MatrixValue(v1.value.map(_ * v2.value), v1.dimensions))
        case (v1: IntegerValue, v2: MatrixValue) =>
          Right(MatrixValue(v2.value.map(_ * v1.value), v2.dimensions))
        case (v1: MatrixValue, v2: MatrixValue) if shape(v1) == shape(v2) =>
          Right(MatrixValue(v1.value.zip(v2.value).map{ case(x, y) => x * y}, v1.dimensions))
        case _ => Left("Invalid types")
  ))

  table.put("/", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          if (v2.value != 0) Right(IntegerValue(v1.value / v2.value)) else Left("Division with zero")
        case (v1: MatrixValue, v2: IntegerValue) =>
          if (v2.value != 0) Right(MatrixValue(v1.value.map(_ / v2.value), v1.dimensions)) else Left("Division with zero")
        case (v1: IntegerValue, v2: MatrixValue) =>
          if (!v2.value.contains(0)) then Right(MatrixValue(v2.value.map(v1.value / _), v2.dimensions)) else Left("Division with zero")
        case (v1: MatrixValue, v2: MatrixValue) if shape(v1) == shape(v2) =>
          Right(MatrixValue(v1.value.zip(v2.value).map{ case(x, y) => x / y}, v1.dimensions))
        case (_: MatrixValue, _: MatrixValue) => Left("Different shapes")
        case _ => Left("Invalid types")
  ))

  table.put("=", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          if v1 == v2 then Right(SymbolValue.T) else Right(ListValue.nil)
        case (v1: SymbolValue, v2: SymbolValue) =>
          if v1 == v2 then Right(SymbolValue.T) else Right(ListValue.nil)
        case (v1: ListValue, v2: ListValue) =>
          if v1 == ListValue.nil && v2 == ListValue.nil then Right(SymbolValue.T) else Right(ListValue.nil)
        case _ =>
          Right(ListValue.nil)
  ))

  table.put("<", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          if v1 < v2 then Right(SymbolValue.T) else Right(ListValue.nil)
        case _ => Right(ListValue.nil)
  ))

  table.put(">", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          if v1 > v2 then Right(SymbolValue.T) else Right(ListValue.nil)
        case _ => Right(ListValue.nil)
  ))

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
