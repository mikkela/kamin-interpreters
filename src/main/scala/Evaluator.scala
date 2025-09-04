package com.mikadocs.kamin

import scala.collection.mutable

trait Evaluator[TValue <: Value] extends NodeVisitor[Either[String, TValue]]
trait EvaluatorProducer[TValue <: Value] extends Function[Environment[TValue], Evaluator[TValue]]

class FunctionDefinitionTable[TValue <: Value](val evaluatorProducer: EvaluatorProducer[TValue]):
  type Function = (Environment[TValue], Seq[TValue]) => Either[String, TValue]
  case class FunctionDefinitionEntry(numberOfArguments: Int,
                                     function: Function)
  protected val table:mutable.HashMap[String, FunctionDefinitionEntry] = mutable.HashMap[String, FunctionDefinitionEntry]()

  def register(functionDefinition: FunctionDefinitionNode): Unit =
    table.put(functionDefinition.function,
      FunctionDefinitionEntry(
        functionDefinition.arguments.length,
        (globalEnvironment, parameters) =>
          val environment = PredefinedEnvironmentFrame[TValue](globalEnvironment, functionDefinition.arguments)
          functionDefinition.arguments.zip(parameters).foreach((k, v) => environment.set(k,v))
          evaluatorProducer.apply(environment).visit(functionDefinition.expression)
      ))

  def lookupFunctionDefinition(name: String): Option[FunctionDefinitionEntry] =
    table.get(name)

trait ExpressionNodeEvaluator[TExpressionNode <: ExpressionNode, TValue <: Value]:
  def evaluate(node: TExpressionNode): Either[String, TValue]

class ValueExpressionEvaluator[TValue <: Value]
  extends ExpressionNodeEvaluator[ValueExpressionNode, TValue]:
  override def evaluate(node: ValueExpressionNode): Either[String, TValue] =  
    node.value match
      case v:TValue => Right(v)
  

class VariableExpressionEvaluator[TValue <: Value](
                                               val environment: Environment[TValue]
                                             )
  extends ExpressionNodeEvaluator[VariableExpressionNode, TValue]:
  override def evaluate(node: VariableExpressionNode): Either[String, TValue] =
    environment.get(node.variable).toRight(s"Unknown variable: $node.variable")

class IfExpressionEvaluator[TValue <: Value](
                                              val evaluator: Evaluator[TValue],
                                              val falseValue: TValue
                                            )
  extends ExpressionNodeEvaluator[IfExpressionNode, TValue]:
  override def evaluate(node: IfExpressionNode): Either[String, TValue] =
    evaluator.visit(node.test) match
      case Right(v) =>
        if v.equals(falseValue) then evaluator.visit(node.alternative) else evaluator.visit(node.consequence)
      case Left(error) => Left(error)

class WhileExpressionEvaluator[TValue <: Value](
                                                 val evaluator: Evaluator[TValue],
                                                 val falseValue: TValue
                                               )
  extends ExpressionNodeEvaluator[WhileExpressionNode, TValue]:

  override def evaluate(node: WhileExpressionNode): Either[String, TValue] =
    evaluator.visit(node.test) match
      case Right(v) =>
        if v.equals(falseValue) then
          Right(falseValue)
        else
          evaluator.visit(node.body) match
            case Left(error) => Left(error)
            case Right(_) => evaluate(node)
      case Left(error) => Left(error)

class SetExpressionEvaluator[TValue <: Value] (
                                                val evaluator: Evaluator[TValue],
                                                val environment: Environment[TValue]
                                              )
  extends ExpressionNodeEvaluator[SetExpressionNode, TValue]:
  override def evaluate(node: SetExpressionNode): Either[String, TValue] =
    evaluator.visit(node.value) match
      case Right(v) =>
        environment.set(node.variable, v)
        Right(v)
      case Left(error) => Left(error)

class BeginExpressionEvaluator[TValue <: Value] (
                                                  val evaluator: Evaluator[TValue]
                                                )
  extends ExpressionNodeEvaluator[BeginExpressionNode, TValue]:
  override def evaluate(node: BeginExpressionNode): Either[String, TValue] =
    node.expressions.map(e => evaluator.visit(e)).last

class LambdaExpressionEvaluator[TValue <: Value] (
                                                   val evaluator: Evaluator[TValue],
                                                   val environment: Environment[TValue]
                                                 )
  extends ExpressionNodeEvaluator[LambdaExpressionNode, TValue]:
  override def evaluate(node: LambdaExpressionNode): Either[String, TValue] =
    Right(ClosureValue(node.arguments, node.expression, environment.asInstanceOf[Environment[Value]]).asInstanceOf[TValue])

trait ArgumentListEvaluator[TValue <: Value]:
  def evaluateParameters( evaluator: Evaluator[TValue],
                          parameters: Seq[ExpressionNode]
                              ): Either[String, List[TValue]] =
    parameters.foldLeft(Right(List.empty[TValue]): Either[String, List[TValue]]) { (acc, p) =>
      acc match
        case Left(error) => Left(error) // If there's already an error, keep it
        case Right(params) =>
          evaluator.visit(p) match
            case Left(error) => Left(error) // Stop and return the error if evaluation fails
            case Right(result) => Right(params :+ result) // Append result to the list if successful
    }

class FunctionCallExpressionEvaluator[TValue <: Value](val evaluator: Evaluator[TValue],
                                                       val environment: Environment[TValue],
                                                       val evaluatorProducer: EvaluatorProducer[TValue],
                                                       functionDefinitionTable: FunctionDefinitionTable[TValue])
  extends ExpressionNodeEvaluator[FunctionCallExpressionNode, TValue], ArgumentListEvaluator[TValue]:

  override def evaluate(node: FunctionCallExpressionNode): Either[String, TValue] = {
    evaluator.visit(node.function) match
      case Right(function) =>
        val evaluated = evaluateParameters(evaluator, node.parameters)
        evaluated match
          case Left(error) => Left(error)
          case Right(parameters) =>
            function match
              case f:PrimitiveOperationValue =>
                functionDefinitionTable.lookupFunctionDefinition(f.operation) match
                  case None => Left(s"Unknown operator: ${f.operation}")
                  case Some(functionDefinition) =>
                    if functionDefinition.numberOfArguments == parameters.length then
                      functionDefinition.function(environment, parameters)
                    else
                      Left(s"${f.operation}: invalid number of arguments")
              case f: ClosureValue =>
                if f.arguments.length == parameters.length then
                  val env = EnvironmentFrame[TValue](f.environment.asInstanceOf[Environment[TValue]])
                  f.arguments.zip(parameters).foreach((k, v) => env.set(k, v))
                  evaluatorProducer(env).visit(f.body)
                else
                  Left(s"Lambda expression: invalid number of arguments")
              case _ => Left("Failed")
  }

class OperationExpressionEvaluator[TValue <: Value] (
                                                      val evaluator: Evaluator[TValue],
                                                      val environment: Environment[TValue],
                                                      functionDefinitionTable: FunctionDefinitionTable[TValue]
                                                    )
  extends ExpressionNodeEvaluator[OperationExpressionNode, TValue], ArgumentListEvaluator[TValue]:


  override def evaluate(node: OperationExpressionNode): Either[String, TValue] =
    functionDefinitionTable.lookupFunctionDefinition(node.operator) match
      case None => Left(s"Unknown operator: $node.operator")
      case Some(functionDefinition) =>
        val evaluated = evaluateParameters(evaluator, node.parameters)
        evaluated match
          case Left(error) => Left(error)
          case Right(parameters) =>
            if functionDefinition.numberOfArguments == parameters.length then
              functionDefinition.function(environment, parameters)
            else
              Left(s"$node.operator: invalid number of arguments")

class SExpressionEvaluator(
                                                     val evaluator: Evaluator[Value],
                                                     val environment: Environment[Value],
                                                     functionDefinitionTable: FunctionDefinitionTable[Value]
                                                   )
  extends ExpressionNodeEvaluator[SExpressionNode, Value]:
  private def evaluateElements(elements: Seq[SExpressionNode]
                                ): Either[String, List[Value]] =
    elements.foldLeft(Right(List.empty[Value]): Either[String, List[Value]]) { (acc, p) =>
      acc match
        case Left(error) => Left(error) // If there's already an error, keep it
        case Right(params) =>
          evaluator.visit(p) match
            case Left(error) => Left(error) // Stop and return the error if evaluation fails
            case Right(result) => Right(params :+ result) // Append result to the list if successful
    }
    
  override def evaluate(node: SExpressionNode): Either[String, Value] =
    node.value match 
      case v: IntegerValue => Right(v)
      case v: SymbolValue => Right(v)
      case v: PrimitiveOperationValue => Right(v)
      case v: ClosureValue => Right(v)
      case e: Seq[SExpressionNode] =>
        val evaluated = evaluateElements(e)
        evaluated match
          case Left(error) => Left(error)
          case Right(elements) => Right(ListValue(elements))
      
