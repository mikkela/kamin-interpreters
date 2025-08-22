package com.mikadocs.kamin

import munit.FunSuite
import scala.reflect.ClassTag

case class IdentifierToken(lexeme: String, position: SourcePosition) extends Token
case class NumberToken(lexeme: String, position: SourcePosition) extends Token


class TestParser extends Parser[Node]:
  override def parse(tokens: LookaheadIterator[Token]): ParserResult[Node] =
    Failure("Not implemented")

  // Exposing parseError for testing
  def testHandleUnmatchedToken(t: Option[Token], acceptUnfinished: Boolean): ParserResult[Node] =
    handleUnmatchedToken(t, acceptUnfinished)
  // Exposing matchToken for testing
  def testMatchToken[T <: Token](tokens: LookaheadIterator[Token])(using ct: ClassTag[T]): Option[T] =
    matchToken[T](tokens)

  def testMatchToken[T1 <: Token, T2 <: Token](tokens: LookaheadIterator[Token])
                                              (using ct1: ClassTag[T1])
                                              (using ct2: ClassTag[T2]): Option[T1 | T2] =
    matchToken[T1, T2](tokens)

class ParserTests extends FunSuite:
  test("matchToken should match the correct token type"):
    val tokens = LookaheadIterator[Token](Seq(IdentifierToken("x", createSourcePosition(1, 1)), NumberToken("42", createSourcePosition(1, 2))).iterator)
    val parser = new TestParser

    val result = parser.testMatchToken[IdentifierToken](tokens)

    assertEquals(result, Some(IdentifierToken("x", createSourcePosition(1, 1))))
    assertEquals(tokens.next(), NumberToken("42", createSourcePosition(1, 2))) // Ensure token was consumed

  test("matchToken should match the correct token types if more than one"):
    val tokens = LookaheadIterator[Token](Seq(IdentifierToken("x", createSourcePosition(1, 1)), NumberToken("42", createSourcePosition(1, 2))).iterator)
    val parser = new TestParser

    val result = parser.testMatchToken[NumberToken, IdentifierToken](tokens)

    assertEquals(result, Some(IdentifierToken("x", createSourcePosition(1, 1))))
    assertEquals(tokens.next(), NumberToken("42", createSourcePosition(1, 2))) // Ensure token was consumed

  test("matchToken should return None if the token does not match"):
    val tokens = LookaheadIterator[Token](Seq(NumberToken("42", createSourcePosition(1, 1))).iterator)
    val parser = new TestParser

    val result = parser.testMatchToken[IdentifierToken](tokens)

    assertEquals(result, None)
    assertEquals(tokens.next(), NumberToken("42", createSourcePosition(1, 1))) // Ensure token was NOT consumed

  test("matchToken should return None if no tokens are available"):
    val tokens = Seq().iterator
    val parser = new TestParser

    val result = parser.testMatchToken[IdentifierToken](tokens)

    assertEquals(result, None)

  test("handleUnmatchedToken - valid token failure"):
    val parser = new TestParser
    val token = Some(IdentifierToken("invalid", createSourcePosition(1, 1)))
    val result = parser.testHandleUnmatchedToken(token, acceptUnfinished = false)
    assertEquals(result, Failure("Parse error: invalid at position: 1.1"))

  test("handleUnmatchedToken - unexpected EOF token"):
    val parser = new TestParser
    val token = Some(EndOfFileToken)
    val result = parser.testHandleUnmatchedToken(token, acceptUnfinished = false)
    assertEquals(result, Failure("Parse Error: Unexpected end of file"))

  test("handleUnmatchedToken - unexpected EOF token - acceptUnfinished"):
    val parser = new TestParser
    val token = Some(EndOfFileToken)
    val result = parser.testHandleUnmatchedToken(token, acceptUnfinished = true)
    assertEquals(result, Unfinished)

  test("handleUnmatchedToken - unexpected EOF failure"):
    val parser = new TestParser
    val token = None
    val result = parser.testHandleUnmatchedToken(token, acceptUnfinished = false)
    assertEquals(result, Failure("Parse Error: Unexpected end of file"))

  test("handleUnmatchedToken - unexpected EOF failure - acceptUnfinished"):
    val parser = new TestParser
    val token = None
    val result = parser.testHandleUnmatchedToken(token, acceptUnfinished = true)
    assertEquals(result, Unfinished)
