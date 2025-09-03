package com.mikadocs.kamin

trait Token:
  def lexeme: String
  def position: SourcePosition

object EndOfFileToken extends Token:
  override def position: SourcePosition = ???
  override def lexeme: String = ???

case class ErrorToken(lexeme: String, position: SourcePosition) extends Token
trait WhiteSpaceToken extends Token
case class SpaceToken(position: SourcePosition) extends WhiteSpaceToken:
  override def lexeme: String = " "
case class TabToken(position: SourcePosition) extends WhiteSpaceToken:
  override def lexeme: String = "\t"
case class NewlineToken(position: SourcePosition) extends WhiteSpaceToken:
  override def lexeme: String = "\n"

case class NameToken(lexeme: String, position: SourcePosition) extends Token
case class IntegerToken(lexeme: String, position: SourcePosition) extends Token

abstract class PunctuationToken(val lexeme: String, val position: SourcePosition) extends Token

case class LeftParenthesisToken(override val position: SourcePosition) extends PunctuationToken(LeftParenthesisToken.leftParenthesis, position)
object LeftParenthesisToken:
  final val leftParenthesis = "("

case class RightParenthesisToken(override val position: SourcePosition) extends PunctuationToken(RightParenthesisToken.rightParenthesis, position)
object RightParenthesisToken:
  final val rightParenthesis = ")"

case class QuoteToken(override val position: SourcePosition) extends PunctuationToken(QuoteToken.quote, position)
object QuoteToken:
  final val quote = "'"

case class DollarToken(override val position: SourcePosition) extends PunctuationToken(DollarToken.dollar, position)
object DollarToken:
  final val dollar = "$"

case class HashToken(override val position: SourcePosition) extends PunctuationToken(HashToken.hash, position)
object HashToken:
  final val hash = "#"

abstract class KeywordToken(val lexeme: String, val position: SourcePosition) extends Token

case class DefineToken(override val position: SourcePosition) extends KeywordToken(DefineToken.define, position)
object DefineToken:
  final val define = "define"

case class IfToken(override val position: SourcePosition) extends KeywordToken(IfToken._if, position)
object IfToken:
  final val _if = "if"

case class WhileToken(override val position: SourcePosition) extends KeywordToken(WhileToken._while, position)
object WhileToken:
  final val _while = "while"

case class SetToken(override val position: SourcePosition) extends KeywordToken(SetToken.set, position)
object SetToken:
  final val set = "set"

case class BeginToken(override val position: SourcePosition) extends KeywordToken(BeginToken.begin, position)
object BeginToken:
  final val begin = "begin"

// Scheme + SASL
case class LambdaToken(override val position: SourcePosition) extends KeywordToken(LambdaToken.lambda, position)
object LambdaToken:
  final val lambda = "lambda"

// CLU
case class ClusterToken(override val position: SourcePosition) extends KeywordToken(ClusterToken.cluster, position)
object ClusterToken:
  final val cluster = "cluster"

case class RepToken(override val position: SourcePosition) extends KeywordToken(RepToken.rep, position)
object RepToken:
  final val rep = "rep"

// SmallTalk
case class ClassToken(override val position: SourcePosition) extends KeywordToken(ClassToken._class, position)
object ClassToken:
  final val _class = "class"

// PROLOG
case class InferToken(override val position: SourcePosition) extends KeywordToken(InferToken.infer, position)
object InferToken:
  final val infer = "infer"

case class FromToken(override val position: SourcePosition) extends KeywordToken(FromToken.from, position)
object FromToken:
  final val from = "from"

case class InferQuestionToken(override val position: SourcePosition) extends KeywordToken(InferQuestionToken.inferQuestion, position)
object InferQuestionToken:
  final val inferQuestion = "infer?"

abstract class OperatorToken(val lexeme: String, val position: SourcePosition) extends Token

case class PlusToken(override val position: SourcePosition) extends OperatorToken(PlusToken.plus, position)
object PlusToken:
  final val plus = "+"

case class MinusToken(override val position: SourcePosition) extends OperatorToken(MinusToken.minus, position)
object MinusToken:
  final val minus = "-"

case class StarToken(override val position: SourcePosition) extends OperatorToken(StarToken.star, position)
object StarToken:
  final val star = "*"

case class SlashToken(override val position: SourcePosition) extends OperatorToken(SlashToken.slash, position)
object SlashToken:
  final val slash = "/"

case class EqualToken(override val position: SourcePosition) extends OperatorToken(EqualToken.equal, position)
object EqualToken:
  final val equal = "="

case class LessToken(override val position: SourcePosition) extends OperatorToken(LessToken.less, position)
object LessToken:
  final val less = "<"

case class GreaterToken(override val position: SourcePosition) extends OperatorToken(GreaterToken.greater, position)
object GreaterToken:
  final val greater = ">"

case class PrintToken(override val position: SourcePosition) extends OperatorToken(PrintToken.print, position)
object PrintToken:
  final val print = "print"

// LISP + Scheme + SASL
case class ConsToken(override val position: SourcePosition) extends OperatorToken(ConsToken.cons, position)
object ConsToken:
  final val cons = "cons"

case class CarToken(override val position: SourcePosition) extends OperatorToken(CarToken.car, position)
object CarToken:
  final val car = "car"

case class CdrToken(override val position: SourcePosition) extends OperatorToken(CdrToken.cdr, position)
object CdrToken:
  final val cdr = "cdr"

case class NumberQuestionToken(override val position: SourcePosition) extends OperatorToken(NumberQuestionToken.numberQuestion, position)
object NumberQuestionToken:
  final val numberQuestion = "number?"

case class SymbolQuestionToken(override val position: SourcePosition) extends OperatorToken(SymbolQuestionToken.symbolQuestion, position)
object SymbolQuestionToken:
  final val symbolQuestion = "symbol?"

case class ListQuestionToken(override val position: SourcePosition) extends OperatorToken(ListQuestionToken.listQuestion, position)
object ListQuestionToken:
  final val listQuestion = "list?"

case class NullQuestionToken(override val position: SourcePosition) extends OperatorToken(NullQuestionToken.nullQuestion, position)
object NullQuestionToken:
  final val nullQuestion = "null?"

// Scheme + SASL
case class PrimopQuestionToken(override val position: SourcePosition) extends OperatorToken(PrimopQuestionToken.primopQuestion, position)
object PrimopQuestionToken:
  final val primopQuestion = "primop?"

case class ClosureQuestionToken(override val position: SourcePosition) extends OperatorToken(ClosureQuestionToken.closureQuestion, position)
object ClosureQuestionToken:
  final val closureQuestion = "closure?"

// APL
case class MaxToken(override val position: SourcePosition) extends OperatorToken(MaxToken.max, position)
object MaxToken:
  final val max = "max"

case class OrToken(override val position: SourcePosition) extends OperatorToken(OrToken.or, position)
object OrToken:
  final val or = "or"

case class AndToken(override val position: SourcePosition) extends OperatorToken(AndToken.and, position)
object AndToken:
  final val and = "and"

case class PlusSlashToken(override val position: SourcePosition) extends OperatorToken(PlusSlashToken.plusSlash, position)
object PlusSlashToken:
  final val plusSlash = "+/"

case class MinusSlashToken(override val position: SourcePosition) extends OperatorToken(MinusSlashToken.minusSlash, position)
object MinusSlashToken:
  final val minusSlash = "-/"

case class StarSlashToken(override val position: SourcePosition) extends OperatorToken(StarSlashToken.starSlash, position)
object StarSlashToken:
  final val starSlash = "*/"

case class SlashSlashToken(override val position: SourcePosition) extends OperatorToken(SlashSlashToken.slashSlash, position)
object SlashSlashToken:
  final val slashSlash = "//"

case class MaxSlashToken(override val position: SourcePosition) extends OperatorToken(MaxSlashToken.maxSlash, position)
object MaxSlashToken:
  final val maxSlash = "max/"

case class OrSlashToken(override val position: SourcePosition) extends OperatorToken(OrSlashToken.orSlash, position)
object OrSlashToken:
  final val orSlash = "or/"

case class AndSlashToken(override val position: SourcePosition) extends OperatorToken(AndSlashToken.andSlash, position)
object AndSlashToken:
  final val andSlash = "and/"

case class CompressToken(override val position: SourcePosition) extends OperatorToken(CompressToken.compress, position)
object CompressToken:
  final val compress = "compress"

case class ShapeToken(override val position: SourcePosition) extends OperatorToken(ShapeToken.shape, position)
object ShapeToken:
  final val shape = "shape"

case class RavelToken(override val position: SourcePosition) extends OperatorToken(RavelToken.ravel, position)
object RavelToken:
  final val ravel = "ravel"

case class RestructToken(override val position: SourcePosition) extends OperatorToken(RestructToken.restruct, position)
object RestructToken:
  final val restruct = "restruct"

case class CatToken(override val position: SourcePosition) extends OperatorToken(CatToken.cat, position)
object CatToken:
  final val cat = "cat"

case class IndxToken(override val position: SourcePosition) extends OperatorToken(IndxToken.indx, position)
object IndxToken:
  final val indx = "indx"

case class TransToken(override val position: SourcePosition) extends OperatorToken(TransToken.trans, position)
object TransToken:
  final val trans = "trans"

case class LeftSquareBracketRightSquareBracketToken(override val position: SourcePosition) extends OperatorToken(LeftSquareBracketRightSquareBracketToken.leftSquareBracketRightSquareBracket, position)
object LeftSquareBracketRightSquareBracketToken:
  final val leftSquareBracketRightSquareBracket = "[]"
