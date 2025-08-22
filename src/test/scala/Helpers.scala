package com.mikadocs.kamin

// Helper classes for Tokens
case class AToken(position: SourcePosition) extends Token:
  override def lexeme: String = "A"

case class BToken(position: SourcePosition) extends Token:
  override def lexeme: String = "B"

case class TestToken(lexeme: String, position: SourcePosition) extends Token

// Helper for creating a simple SourcePosition
def createSourcePosition(l: Int, c: Int): SourcePosition = new SourcePosition:
  override def line: Int = l
  override def column: Int = c
  override def lineContents: String = ""

