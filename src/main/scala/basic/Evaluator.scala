package com.mikadocs.kamin
package basic

import scala.annotation.tailrec
import scala.collection.mutable

object functionDefinitionTable extends FunctionDefinitionTable[IntegerValue](e => BasicEvaluator(e)):
  table.put("+", FunctionDefinitionEntry(2, (env, arguments) => Right(IntegerValue(arguments.head.value + arguments(1).value))))
  table.put("-", FunctionDefinitionEntry(2, (env, arguments) => Right(IntegerValue(arguments.head.value - arguments(1).value))))
  table.put("*", FunctionDefinitionEntry(2, (env, arguments) => Right(IntegerValue(arguments.head.value * arguments(1).value))))
  table.put("/", FunctionDefinitionEntry(2, (env, arguments) =>
    if arguments(1).value != 0 then Right(IntegerValue(arguments.head.value / arguments(1).value)) else Left("Division with zero")))
  table.put("=", FunctionDefinitionEntry(2, (env, arguments) =>
    Right(IntegerValue(
      if arguments.head.value == arguments(1).value then 1 else 0
    ))))
  table.put("<", FunctionDefinitionEntry(2, (env, arguments) =>
    Right(IntegerValue(
      if arguments.head.value < arguments(1).value then 1 else 0
    ))))
  table.put(">", FunctionDefinitionEntry(2, (env, arguments) =>
    Right(IntegerValue(
      if arguments.head.value > arguments(1).value then 1 else 0
    ))))
  table.put("print", FunctionDefinitionEntry(1, (env, arguments) =>
    print(arguments.head.value)
    Right(arguments.head)))

class BasicEvaluator(val currentEnvironment: Environment[IntegerValue])
  extends Evaluator[IntegerValue]:
  private val zero = IntegerValue(0)
  private val valueExpressionEvaluator = ValueExpressionEvaluator[IntegerValue]()
  private val variableExpressionEvaluator = VariableExpressionEvaluator[IntegerValue](currentEnvironment)
  private val ifExpressionEvaluator = IfExpressionEvaluator[IntegerValue](this, zero)
  private val whileExpressionEvaluator = WhileExpressionEvaluator[IntegerValue](this, zero)
  private val setExpressionEvaluator = SetExpressionEvaluator[IntegerValue](this, currentEnvironment)
  private val beginExpressionEvaluator = BeginExpressionEvaluator[IntegerValue](this)
  private val operationExpressionEvaluator = OperationExpressionEvaluator[IntegerValue](this, currentEnvironment, functionDefinitionTable)
  
  override def visit(node: Node): Either[String, IntegerValue] =
    node match
      case n:ValueExpressionNode => valueExpressionEvaluator.evaluate(n)
      case n:VariableExpressionNode => variableExpressionEvaluator.evaluate(n)
      case n: IfExpressionNode => ifExpressionEvaluator.evaluate(n)
      case n:WhileExpressionNode => whileExpressionEvaluator.evaluate(n)
      case n: SetExpressionNode => setExpressionEvaluator.evaluate(n)
      case n: BeginExpressionNode => beginExpressionEvaluator.evaluate(n)
      case n: OperationExpressionNode => operationExpressionEvaluator.evaluate(n)

object BasicEvaluator:
  val globalEnvironment: Environment[IntegerValue] = GlobalEnvironment[IntegerValue]()

  def apply(env: Environment[IntegerValue]): BasicEvaluator =
    new BasicEvaluator(env)

  def default(): BasicEvaluator =
    new BasicEvaluator(globalEnvironment)

