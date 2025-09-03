package com.mikadocs.kamin
package scheme

object functionDefinitionTable extends FunctionDefinitionTable[Value](e => SchemeEvaluator(e)):

  table.put("+", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) => Right(IntegerValue(v1.value + v2.value))
        case _ => Left("Invalid types")
  ))

  table.put("-", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) => Right(IntegerValue(v1.value - v2.value))
        case _ => Left("Invalid types")
  ))

  table.put("*", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) => Right(IntegerValue(v1.value * v2.value))
        case _ => Left("Invalid types")
  ))

  table.put("/", FunctionDefinitionEntry(2,
    (env, arguments) =>
      (arguments.head, arguments(1)) match
        case (v1: IntegerValue, v2: IntegerValue) =>
          if v2.value == 0 then
            Left("Division with zero")
          else
            Right(IntegerValue(v1.value + v2.value))
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
        case IntegerValue => Right(SymbolValue.T)
        case _ => Right(ListValue.nil)

  ))

  table.put("symbol?", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case SymbolValue => Right(SymbolValue.T)
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

  table.put("primop?", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case PrimitiveOperationValue(_) => Right(SymbolValue.T)
        case _ => Right(ListValue.nil)
  ))

  table.put("closure?", FunctionDefinitionEntry(1,
    (env, arguments) =>
      arguments.head match
        case ClosureValue => Right(SymbolValue.T)
        case _ => Right(ListValue.nil)
  ))

  table.put("print", FunctionDefinitionEntry(1, (env, arguments) =>
    print(arguments.head.toString)
    Right(arguments.head)))

class SchemeEvaluator(val currentEnvironment: Environment[Value])
  extends Evaluator[Value]:
  private val valueExpressionEvaluator = ValueExpressionEvaluator[Value]()
  private val variableExpressionEvaluator = VariableExpressionEvaluator[Value](currentEnvironment)
  private val ifExpressionEvaluator = IfExpressionEvaluator[Value](this, ListValue.nil)
  private val whileExpressionEvaluator = WhileExpressionEvaluator[Value](this, ListValue.nil)
  private val setExpressionEvaluator = SetExpressionEvaluator[Value](this, currentEnvironment)
  private val beginExpressionEvaluator = BeginExpressionEvaluator[Value](this)
  private val lambdaExpressionEvaluator = LambdaExpressionEvaluator[Value](this, currentEnvironment)
  private val functionCallExpressionEvaluator = FunctionCallExpressionEvaluator[Value](
    this, currentEnvironment, env => new SchemeEvaluator(env), functionDefinitionTable)
  private val sExpressionEvaluator = SExpressionEvaluator(this, currentEnvironment, functionDefinitionTable)
  override def visit(node: Node): Either[String, Value] =
    node match
      case n: ValueExpressionNode => valueExpressionEvaluator.evaluate(n)
      case n: VariableExpressionNode => variableExpressionEvaluator.evaluate(n)
      case n: IfExpressionNode => ifExpressionEvaluator.evaluate(n)
      case n: WhileExpressionNode => whileExpressionEvaluator.evaluate(n)
      case n: SetExpressionNode => setExpressionEvaluator.evaluate(n)
      case n: BeginExpressionNode => beginExpressionEvaluator.evaluate(n)
      case n: FunctionCallExpressionNode => functionCallExpressionEvaluator.evaluate(n)
      case n: SExpressionNode => sExpressionEvaluator.evaluate(n)

object SchemeEvaluator:
  val globalEnvironment: Environment[Value] = GlobalEnvironment[Value]()

  def apply(env: Environment[Value]): SchemeEvaluator =
    new SchemeEvaluator(env)

  def default(): SchemeEvaluator =
    new SchemeEvaluator(globalEnvironment)
