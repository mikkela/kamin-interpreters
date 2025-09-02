package com.mikadocs.kamin

trait Printer extends NodeVisitor[String]
//trait PrinterProducer extends Function[Unit, Printer]

trait NodePrinter[TNode <: Node]:
  def printNodeTo(node: TNode, stringBuilder: StringBuilder): Unit

class ValueExpressionPrinter(val printer: Printer) extends NodePrinter[ValueExpressionNode]:
  override def printNodeTo(node: ValueExpressionNode, stringBuilder: StringBuilder): Unit =
    node.value match
      case v:IntegerValue => stringBuilder.append(v.value)
      case e:SExpressionNode => printer.visit(e)
      case v:MatrixValue => stringBuilder.append("'(" + v.value.mkString(" ") + ")")
      case v:ValueOperatorValue => stringBuilder.append(v.operator)
      case l:LambdaValue => stringBuilder.append("(lambda (" + l.args.mkString(" ") + ") " + printer.visit(l.body) + ")")

class VariableExpressionPrinter extends NodePrinter[VariableExpressionNode]:
  override def printNodeTo(node: VariableExpressionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append(node.variable)


class IfExpressionPrinter(val printer: Printer) extends NodePrinter[IfExpressionNode]:
  override def printNodeTo(node: IfExpressionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append("(if ")
    stringBuilder.append(printer.visit(node.test))
    stringBuilder.append(" ")
    stringBuilder.append(printer.visit(node.consequence))
    stringBuilder.append(" ")
    stringBuilder.append(printer.visit(node.alternative))
    stringBuilder.append(")")

class WhileExpressionPrinter(val printer: Printer) extends NodePrinter[WhileExpressionNode]:
  override def printNodeTo(node: WhileExpressionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append("(while ")
    stringBuilder.append(printer.visit(node.test))
    stringBuilder.append(" ")
    stringBuilder.append(printer.visit(node.body))
    stringBuilder.append(")")

class SetExpressionPrinter(val printer: Printer) extends NodePrinter[SetExpressionNode]:
  override def printNodeTo(node: SetExpressionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append("(set ")
    stringBuilder.append(node.variable)
    stringBuilder.append(" ")
    stringBuilder.append(printer.visit(node.value))
    stringBuilder.append(")")

class BeginExpressionPrinter(val printer: Printer) extends NodePrinter[BeginExpressionNode]:
  override def printNodeTo(node: BeginExpressionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append("(begin")
    node.expressions.foreach(
      e =>
        stringBuilder.append(" ")
        stringBuilder.append(printer.visit(e))
    )
    stringBuilder.append(")")

class ExpressionListExpressionPrinter(val printer: Printer) extends NodePrinter[ExpressionListExpressionNode]:
  override def printNodeTo(node: ExpressionListExpressionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append("(")
    var writtenFirst = false
    node.expressions.foreach(
      e =>
        if writtenFirst then stringBuilder.append(" ")
        stringBuilder.append(printer.visit(e))
        writtenFirst = true
    )
    stringBuilder.append(")")

class OperationExpressionPrinter(val printer: Printer) extends NodePrinter[OperationExpressionNode]:
  override def printNodeTo(node: OperationExpressionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append("( ")
    stringBuilder.append(node.operator)
    node.parameters.foreach(
      e =>
        stringBuilder.append(" ")
        stringBuilder.append(printer.visit(e))
    )
    stringBuilder.append(")")
    
class SExpressionPrinter(val printer: Printer) extends NodePrinter[SExpressionNode]:
  override def printNodeTo(node: SExpressionNode, stringBuilder: StringBuilder): Unit =
    node.value match 
      case n:IntegerValue => stringBuilder.append(n.value)
      case n:SymbolValue => stringBuilder.append(n.value)
      case s:Seq[SExpressionNode] =>
        stringBuilder.append("(")
        var writtenFirst = false
        s.foreach(
          e =>
            if writtenFirst then stringBuilder.append(" ")
            printNodeTo(e, stringBuilder)
            writtenFirst = true
        )
        stringBuilder.append(")")


class FunctionDefinitionPrinter(val printer: Printer) extends NodePrinter[FunctionDefinitionNode]:
  override def printNodeTo(node: FunctionDefinitionNode, stringBuilder: StringBuilder): Unit =
    stringBuilder.append("(define ")
    stringBuilder.append(node.function)
    stringBuilder.append(" (")
    var writtenFirst = false
    node.arguments.foreach(
      a =>
        if writtenFirst then stringBuilder.append(" ")
        stringBuilder.append(a)
        writtenFirst = true
    )
    stringBuilder.append(") ")
    stringBuilder.append(printer.visit(node.expression))
    stringBuilder.append(" )")
