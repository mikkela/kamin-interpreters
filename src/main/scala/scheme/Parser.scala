package com.mikadocs.kamin
package scheme

object schemeParser extends Parser[ExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    expressionParser.parse(tokens)

object expressionParser extends ExpressionParser:
  private val sExpressionParser = SExpressionParser()
  private val ifExpressionParser = IfExpressionParser(this)
  private val whileExpressionParser = WhileExpressionParser(this)
  private val setExpressionParser = SetExpressionParser(this)
  private val beginExpressionParser = BeginExpressionParser(this)
  private val lambdaExpressionParser = LambdaExpressionParser(this)
  private val functionCallExpressionParser = FunctionCallExpressionParser(this)
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    matchToken[IntegerToken, OperatorToken, NameToken, QuoteToken, LeftParenthesisToken](tokens) match
      case Some(t: IntegerToken) => Success(ValueExpressionNode(t.lexeme.toInt.toIntegerValue))
      case Some(t: OperatorToken) => Success(ValueExpressionNode(PrimitiveOperationValue(t.lexeme)))
      case Some(t: NameToken) => Success(VariableExpressionNode(t.lexeme))
      case Some(_: QuoteToken) => sExpressionParser.parse(tokens)
      case Some(_: LeftParenthesisToken) =>
        matchToken[IfToken, WhileToken, SetToken, BeginToken, LambdaToken](tokens) match
          case Some(_: IfToken) => ifExpressionParser.parse(tokens)
          case Some(_: WhileToken) => whileExpressionParser.parse(tokens)
          case Some(_: SetToken) => setExpressionParser.parse(tokens)
          case Some(_: BeginToken) => beginExpressionParser.parse(tokens)
          case Some(t: LambdaToken) => lambdaExpressionParser.parse(tokens)
          case None => functionCallExpressionParser.parse(tokens)
      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
