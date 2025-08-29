package com.mikadocs.kamin
package apl

object aplPrinter extends Printer:
  private val valueExpressionPrinter = ValueExpressionPrinter(this)
  private val variableExpressionPrinter = VariableExpressionPrinter()
  private val ifExpressionPrinter = IfExpressionPrinter(this)
  private val whileExpressionPrinter = WhileExpressionPrinter(this)
  private val setExpressionPrinter = SetExpressionPrinter(this)
  private val beginExpressionPrinter = BeginExpressionPrinter(this)
  private val operationExpressionPrinter = OperationExpressionPrinter(this)
  private val functionDefinitionPrinter = FunctionDefinitionPrinter(this)

  override def visit(node: Node): String =
    val result = StringBuilder()
    node match
      case n:ValueExpressionNode =>
        valueExpressionPrinter.printNodeTo(n, result)
      case n:VariableExpressionNode =>
        variableExpressionPrinter.printNodeTo(n, result)
      case n:IfExpressionNode =>
        ifExpressionPrinter.printNodeTo(n, result)
      case n:WhileExpressionNode =>
        whileExpressionPrinter.printNodeTo(n, result)
      case n:SetExpressionNode =>
        setExpressionPrinter.printNodeTo(n, result)
      case n:BeginExpressionNode =>
        beginExpressionPrinter.printNodeTo(n, result)
      case n:OperationExpressionNode =>
        operationExpressionPrinter.printNodeTo(n, result)
      case n:FunctionDefinitionNode =>
        functionDefinitionPrinter.printNodeTo(n, result)
    result.toString()

object aplInterpreter extends Interpreter:

  override def interpret(program: String): EvaluationResult =
    aplParser.parse(Scanner().scan(KaminSourceReader(SourceReader(program)))).map(
      ast =>
        ast match
          case n: FunctionDefinitionNode =>
            functionDefinitionTable.register(n)
            n.function
          case n: ExpressionNode =>
            APLEvaluator(APLEvaluator.globalEnvironment).visit(ast) match
              case Left(error) => error
              case Right(result) => result.toString
    )

  override def parseAndPrint(program: String): ParseAndPrintResult =
    aplParser.parse(Scanner().scan(KaminSourceReader(SourceReader(program)))).map(
      ast => ast.visit(using aplPrinter)
    )




