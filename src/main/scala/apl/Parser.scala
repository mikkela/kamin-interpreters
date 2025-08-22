package com.mikadocs.kamin
package apl

import basic.expressionParser.{handleUnmatchedToken, matchToken}

object aplParser extends Parser[ExpressionNode | FunctionDefinitionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode | FunctionDefinitionNode] =
    tokens.lookahead(2) match
      case Seq(t1: LeftParenthesisToken, t2: DefineToken) =>
        FunctionDefinitionParser(expressionParser).parse(tokens)
      case _ =>
        expressionParser.parse(tokens)

object expressionParser extends ExpressionParser:
  private val ifExpressionParser = IfExpressionParser(this)
  private val whileExpressionParser = WhileExpressionParser(this)
  private val setExpressionParser = SetExpressionParser(this)
  private val beginExpressionParser = BeginExpressionParser(this)
  private val operationExpressionParser = OperationExpressionParser(this)

  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ExpressionNode] =
    matchToken[IntegerToken, QuoteToken, NameToken, LeftParenthesisToken](tokens) match
      case Some(t: IntegerToken) => Success(ValueExpressionNode(t.lexeme.toInt.toIntegerValue))
      case Some(_: QuoteToken) => vectorConstParser.parse(tokens)
      case Some(t: NameToken) => Success(VariableExpressionNode(t.lexeme))
      case Some(_: LeftParenthesisToken) =>
        matchToken[IfToken, WhileToken, SetToken, BeginToken, OperatorToken, NameToken](tokens) match
          case Some(_: IfToken) => ifExpressionParser.parse(tokens)
          case Some(_: WhileToken) => whileExpressionParser.parse(tokens)
          case Some(_: SetToken) => setExpressionParser.parse(tokens)
          case Some(_: BeginToken) => beginExpressionParser.parse(tokens)
          case Some(t: OperatorToken) => operationExpressionParser.parse(prepend(t, tokens))
          case Some(t: NameToken) => operationExpressionParser.parse(prepend(t, tokens))
          case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

object vectorConstParser extends Parser[ValueExpressionNode]:
  private def parseList(
                         tokens: LookaheadIterator[Token],
                         initial: Seq[Int],
                         buildNode: Seq[Int] => ParserResult[ValueExpressionNode]
                       ): ParserResult[ValueExpressionNode] =
    matchToken[RightParenthesisToken, IntegerToken](tokens) match
      case Some(_: RightParenthesisToken) => buildNode(initial)
      case Some(i: IntegerToken) => parseList(tokens, initial.appended(i.lexeme.toInt), buildNode)
      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

  override def parse(tokens: LookaheadIterator[Token]): ParserResult[ValueExpressionNode] =
    matchToken[LeftParenthesisToken](tokens) match
      case Some(_: LeftParenthesisToken) =>
        parseList(tokens, Seq.empty[Int], v => Success(ValueExpressionNode(MatrixValue.vector(v))))
      case None =>
        handleUnmatchedToken(tokens.headOption)
