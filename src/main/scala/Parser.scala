package com.mikadocs.kamin

import scala.annotation.targetName
import scala.reflect.ClassTag

type ParserResult[T <: Node] = Result[T]

trait Parser[T <: Node]:
  def parse(tokens: LookaheadIterator[Token]): ParserResult[T]

  protected def handleUnmatchedToken(token: Option[Token], acceptUnfinished: Boolean = false): ParserResult[T] =
    if !token.isEmpty && token.get != EndOfFileToken then
      Failure(s"Parse error: ${token.get.lexeme} at position: ${token.get.position}")
    else
      if acceptUnfinished then
        Unfinished
      else
        Failure(s"Parse Error: Unexpected end of file")

  @targetName("matchToken")
  protected def matchToken[TokenType <: Token](tokens: LookaheadIterator[Token])
                                              (using ct: ClassTag[TokenType]): Option[TokenType] =
    if !tokens.hasNext then None
    else
      if ct.runtimeClass.isInstance(tokens.lookahead(1).head) then
        Some(tokens.next().asInstanceOf[TokenType])
      else
        None

  @targetName("match2Token")
  protected def matchToken[TokenTypeAlternative1 <: Token, TokenTypeAlternative2 <: Token](tokens: LookaheadIterator[Token])
                                                                                          (using ct1: ClassTag[TokenTypeAlternative1])
                                                                                          (using ct2: ClassTag[TokenTypeAlternative2]): Option[TokenTypeAlternative1 | TokenTypeAlternative2] =
    matchToken[TokenTypeAlternative1](tokens).
      orElse(matchToken[TokenTypeAlternative2](tokens))

  @targetName("match3Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token
  ]
  (tokens: LookaheadIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using ct2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2](tokens).
      orElse(matchToken[TokenTypeAlternative3](tokens))

  @targetName("match4Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token,
    TokenTypeAlternative4 <: Token
  ]
  (tokens: LookaheadIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using ct2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3])
  (using ct4: ClassTag[TokenTypeAlternative4]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3|TokenTypeAlternative4] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2, TokenTypeAlternative3](tokens).
      orElse(matchToken[TokenTypeAlternative4](tokens))

  @targetName("match5Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token,
    TokenTypeAlternative4 <: Token,
    TokenTypeAlternative5 <: Token
  ]
  (tokens: LookaheadIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using ct2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3])
  (using ct4: ClassTag[TokenTypeAlternative4])
  (using ct5: ClassTag[TokenTypeAlternative5]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3 | TokenTypeAlternative4 | TokenTypeAlternative5] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2, TokenTypeAlternative3, TokenTypeAlternative4](tokens).
      orElse(matchToken[TokenTypeAlternative5](tokens))
      
  @targetName("match6Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token,
    TokenTypeAlternative4 <: Token,
    TokenTypeAlternative5 <: Token,
    TokenTypeAlternative6 <: Token
  ]
  (tokens: LookaheadIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using c2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3])
  (using ct4: ClassTag[TokenTypeAlternative4])
  (using ct5: ClassTag[TokenTypeAlternative5])
  (using ct6: ClassTag[TokenTypeAlternative6]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3 | TokenTypeAlternative4 | TokenTypeAlternative5 | TokenTypeAlternative6] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2, TokenTypeAlternative3, TokenTypeAlternative4, TokenTypeAlternative5](tokens).
      orElse(matchToken[TokenTypeAlternative6](tokens))

  @targetName("match7Token")
  protected def matchToken[
    TokenTypeAlternative1 <: Token,
    TokenTypeAlternative2 <: Token,
    TokenTypeAlternative3 <: Token,
    TokenTypeAlternative4 <: Token,
    TokenTypeAlternative5 <: Token,
    TokenTypeAlternative6 <: Token,
    TokenTypeAlternative7 <: Token
  ]
  (tokens: LookaheadIterator[Token])
  (using ct1: ClassTag[TokenTypeAlternative1])
  (using c2: ClassTag[TokenTypeAlternative2])
  (using ct3: ClassTag[TokenTypeAlternative3])
  (using ct4: ClassTag[TokenTypeAlternative4])
  (using ct5: ClassTag[TokenTypeAlternative5])
  (using ct6: ClassTag[TokenTypeAlternative6])
  (using ct7: ClassTag[TokenTypeAlternative7]):
  Option[TokenTypeAlternative1 | TokenTypeAlternative2 | TokenTypeAlternative3 | TokenTypeAlternative4 | TokenTypeAlternative5 | TokenTypeAlternative6 | TokenTypeAlternative7] =
    matchToken[TokenTypeAlternative1, TokenTypeAlternative2, TokenTypeAlternative3, TokenTypeAlternative4, TokenTypeAlternative5, TokenTypeAlternative6](tokens).
      orElse(matchToken[TokenTypeAlternative7](tokens))

trait ExpressionParser extends Parser[ExpressionNode]

trait ExpressionListParser[TElement <: ExpressionNode, TResult <: ExpressionNode] extends Parser[TResult]:
  def parseList(
                 tokens: LookaheadIterator[Token],
                 initial: Seq[TElement],
                 buildNode: Seq[TElement] => ParserResult[TResult],
                 expressionParser: Parser[TElement]
               ): ParserResult[TResult] =
    matchToken[RightParenthesisToken, EndOfFileToken.type](tokens) match
      case Some(_: RightParenthesisToken) => buildNode(initial)
      case Some(EndOfFileToken) => Unfinished
      case None =>
        expressionParser.parse(tokens).flatMap { expr =>
          parseList(tokens, initial.appended(expr), buildNode, expressionParser)
        }

trait ArgumentListParser[T <: Node] extends Parser[T]:
  def parseList(
                 tokens: LookaheadIterator[Token],
                 initial: Seq[String],
                 buildNode: Seq[String] => ParserResult[T]
               ): ParserResult[T] =
    matchToken[RightParenthesisToken, NameToken](tokens) match
      case Some(_: RightParenthesisToken) => buildNode(initial)
      case Some(n: NameToken) => parseList(tokens, initial.appended(n.lexeme), buildNode)
      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

class FunctionDefinitionParser(val expressionParser: ExpressionParser)
  extends Parser[FunctionDefinitionNode], ArgumentListParser[FunctionDefinitionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[FunctionDefinitionNode] =
    (matchToken[LeftParenthesisToken](tokens), matchToken[DefineToken](tokens)) match
      case (Some(_), Some(_)) =>
        matchToken[NameToken](tokens) match
          case Some(function: NameToken) =>
            matchToken[LeftParenthesisToken](tokens) match
              case Some(_) =>
                parseList(
                  tokens,
                  Seq.empty[String],
                  arguments =>
                    expressionParser.parse(tokens).flatMap{
                      expression => Success(FunctionDefinitionNode(function.lexeme, arguments, expression))
                    }

                )

              case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
          case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
      case _ => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

class IfExpressionParser(val expressionParser: ExpressionParser) extends Parser[IfExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[IfExpressionNode] =
    // if keyword has been consumed
    expressionParser.parse(tokens).flatMap {
      test => expressionParser.parse(tokens).flatMap {
        consequence => expressionParser.parse(tokens).flatMap {
          alternative =>
            matchToken[RightParenthesisToken](tokens) match
              case Some(_) => Success(IfExpressionNode(test, consequence, alternative))
              case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)
        }
      }
    }


class WhileExpressionParser(val expressionParser: ExpressionParser) extends Parser[WhileExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[WhileExpressionNode] =
    // while keyword has been consumed
    expressionParser.parse(tokens).flatMap {
      test =>
        expressionParser.parse(tokens).flatMap {
          body =>
            matchToken[RightParenthesisToken](tokens) match
              case Some(_) => Success(WhileExpressionNode(test, body))
              case None => handleUnmatchedToken(tokens.headOption)

        }
    }

class SetExpressionParser(val expressionParser: ExpressionParser) extends Parser[SetExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[SetExpressionNode] =
    // set keyword has been consumed
    matchToken[NameToken](tokens) match
      case Some(variable: NameToken) =>
        expressionParser.parse(tokens).flatMap {
          value =>
            matchToken[RightParenthesisToken](tokens) match
              case Some(_) => Success(SetExpressionNode(variable.lexeme, value))
              case None => handleUnmatchedToken(tokens.headOption)
        }
      case _ => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

class BeginExpressionParser(val expressionParser: ExpressionParser) extends Parser[BeginExpressionNode], ExpressionListParser[ExpressionNode, BeginExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[BeginExpressionNode] =
    // begin keyword has been consumed
    parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(BeginExpressionNode(exprs)), expressionParser)

class OperationExpressionParser(val expressionParser: ExpressionParser) extends Parser[OperationExpressionNode], ExpressionListParser[ExpressionNode, OperationExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[OperationExpressionNode] =
    matchToken[OperatorToken, NameToken](tokens) match
      case Some(op: OperatorToken) =>
        parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(OperationExpressionNode(op.lexeme, exprs)), expressionParser)
      case Some(op: NameToken) =>
        parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(OperationExpressionNode(op.lexeme, exprs)), expressionParser)
      case None =>
        handleUnmatchedToken(tokens.headOption)

class SExpressionParser extends Parser[SExpressionNode], ExpressionListParser[SExpressionNode, SExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[SExpressionNode] = {
    matchToken[IntegerToken, NameToken, LeftParenthesisToken](tokens) match {
      case Some(t: IntegerToken) => Success(SExpressionNode(t.lexeme.toInt.toIntegerValue))
      case Some(t: NameToken) => Success(SExpressionNode(t.lexeme.toSymbolValue))
      case Some(_: LeftParenthesisToken) =>
        parseList(tokens, Seq.empty[SExpressionNode], exprs => Success(SExpressionNode(exprs)), this)
      case None =>
        handleUnmatchedToken(tokens.headOption)
    }
  }

class LambdaExpressionParser(val expressionParser: ExpressionParser)
  extends Parser[LambdaExpressionNode], ArgumentListParser[LambdaExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[LambdaExpressionNode] =
    // lambda keyword has been consumed
    matchToken[LeftParenthesisToken](tokens) match
      case Some(_) =>
        parseList(
          tokens,
          Seq.empty[String],
          arguments =>
            expressionParser.parse(tokens).flatMap {
              expression => Success(LambdaExpressionNode(arguments, expression))
            }
        )

      case None => handleUnmatchedToken(tokens.headOption, acceptUnfinished = true)

class FunctionCallExpressionParser(val expressionParser: ExpressionParser)
  extends Parser[FunctionCallExpressionNode], ExpressionListParser[ExpressionNode, FunctionCallExpressionNode]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[FunctionCallExpressionNode] =
    expressionParser.parse(tokens).flatMap {
      function =>
        parseList(tokens, Seq.empty[ExpressionNode], exprs => Success(FunctionCallExpressionNode(function, exprs)), expressionParser)
    }