package com.mikadocs.kamin

trait Node

trait NodeVisitor[T]:
  def visit(node: Node): T

extension [T](node: Node) def visit(using v: NodeVisitor[T]): T = v.visit(node)

trait ExpressionNode extends Node

case class ValueExpressionNode(value: IntegerValue | MatrixValue | SExpressionNode | LambdaValue | ValueOperatorValue) extends ExpressionNode
case class SExpressionNode(value: IntegerValue | SymbolValue | Seq[SExpressionNode]) extends ExpressionNode:
  override def toString: String =
    value match {
      case v: IntegerValue => v.toString
      case v: SymbolValue => v.toString
      case v: Seq[SExpressionNode] => "(" + v.mkString(" ") + ")"
    }

case class VariableExpressionNode(variable: String) extends ExpressionNode
case class IfExpressionNode(test:ExpressionNode, consequence: ExpressionNode, alternative: ExpressionNode) extends ExpressionNode
case class WhileExpressionNode(test:ExpressionNode, body: ExpressionNode) extends ExpressionNode
case class SetExpressionNode(variable: String, value: ExpressionNode) extends ExpressionNode
case class BeginExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode
case class OperationExpressionNode(operator: String, parameters: Seq[ExpressionNode]) extends ExpressionNode
case class ExpressionListExpressionNode(expressions: Seq[ExpressionNode]) extends ExpressionNode
case class FunctionDefinitionNode(function: String, arguments: Seq[String], expression: ExpressionNode) extends Node
