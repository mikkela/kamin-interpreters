package com.mikadocs.kamin
package lisp

object functionDefinitionTable extends FunctionDefinitionTable[Value](e => LispEvaluator(e)):
  
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

class LispEvaluator(val currentEnvironment: Environment[Value])
  extends Evaluator[Value]:
  private val valueExpressionEvaluator = ValueExpressionEvaluator[Value]()
  private val variableExpressionEvaluator = VariableExpressionEvaluator[Value](currentEnvironment)
  private val ifExpressionEvaluator = IfExpressionEvaluator[Value](this, ListValue.nil)
  private val whileExpressionEvaluator = WhileExpressionEvaluator[Value](this, ListValue.nil)
  private val setExpressionEvaluator = SetExpressionEvaluator[Value](this, currentEnvironment)
  private val beginExpressionEvaluator = BeginExpressionEvaluator[Value](this)
  private val operationExpressionEvaluator = OperationExpressionEvaluator[Value](this, currentEnvironment, functionDefinitionTable)
  private val sExpressionEvaluator = SExpressionEvaluator(this, currentEnvironment, functionDefinitionTable)
  override def visit(node: Node): Either[String, Value] =
    node match
      case n:VariableExpressionNode => variableExpressionEvaluator.evaluate(n)
      case n: IfExpressionNode => ifExpressionEvaluator.evaluate(n)
      case n:WhileExpressionNode => whileExpressionEvaluator.evaluate(n)
      case n: SetExpressionNode => setExpressionEvaluator.evaluate(n)
      case n: BeginExpressionNode => beginExpressionEvaluator.evaluate(n)
      case n: OperationExpressionNode => operationExpressionEvaluator.evaluate(n)
      case n: SExpressionNode => sExpressionEvaluator.evaluate(n)

object LispEvaluator:
  val globalEnvironment: Environment[Value] = GlobalEnvironment[Value]()

  def apply(env: Environment[Value]): LispEvaluator =
    new LispEvaluator(env)

  def default(): LispEvaluator =
    new LispEvaluator(globalEnvironment)
