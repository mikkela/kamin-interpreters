package com.mikadocs.kamin

import Acceptance.{Accepted, Rejected}

private def isBasicSeparator(c: Char): Boolean = c.isWhitespace || c == '(' || c == ')'

class NameRule(val separators: Seq[String]) extends ScannerRule:
  override def accept(s: String): Acceptance = 
    if separators.exists(s.contains(_)) then Rejected
    else if s.forall(c => c.isLetter || !c.isWhitespace) then Accepted
    else Rejected
  override def apply(s: String, position: SourcePosition): Token = NameToken(s, position)

object IntegerRule extends ScannerRule:
  override def accept(s: String): Acceptance =
    if """-?\d+""".r matches s then
      Acceptance.Accepted
    else if "-".r matches s then
      Acceptance.Undecided
    else
      Acceptance.Rejected
  
  override def apply(s: String, position: SourcePosition): Token = IntegerToken(s, position)

object LeftParenthesisRule extends PredefinedStringMatchingRule(LeftParenthesisToken.leftParenthesis, p => LeftParenthesisToken(p))
object RightParenthesisRule extends PredefinedStringMatchingRule(RightParenthesisToken.rightParenthesis, p => RightParenthesisToken(p))
object QuoteRule extends PredefinedStringMatchingRule(QuoteToken.quote, p => QuoteToken(p))
object DollarRule extends PredefinedStringMatchingRule(DollarToken.dollar, p => DollarToken(p))
object HashRule extends PredefinedStringMatchingRule(HashToken.hash, p => HashToken(p))

object DefineRule extends PredefinedStringMatchingRule(DefineToken.define, p => DefineToken(p))
object IfRule extends PredefinedStringMatchingRule(IfToken._if, p => IfToken(p))
object WhileRule extends PredefinedStringMatchingRule(WhileToken._while, p => WhileToken(p))
object SetRule extends PredefinedStringMatchingRule(SetToken.set, p => SetToken(p))
object BeginRule extends PredefinedStringMatchingRule(BeginToken.begin, p => BeginToken(p))
object LambdaRule extends PredefinedStringMatchingRule(LambdaToken.lambda, p => LambdaToken(p))
object ClusterRule extends PredefinedStringMatchingRule(ClusterToken.cluster, p => ClusterToken(p))
object RepRule extends PredefinedStringMatchingRule(RepToken.rep, p => RepToken(p))
object ClassRule extends PredefinedStringMatchingRule(ClassToken._class, p => ClassToken(p))
object InferRule extends PredefinedStringMatchingRule(InferToken.infer, p => InferToken(p))
object FromRule extends PredefinedStringMatchingRule(FromToken.from, p => FromToken(p))
object InferQuestionRule extends PredefinedStringMatchingRule(InferQuestionToken.inferQuestion, p => InferQuestionToken(p))

object PlusRule extends PredefinedStringMatchingRule(PlusToken.plus, p => PlusToken(p))
object MinusRule extends PredefinedStringMatchingRule(MinusToken.minus, p => MinusToken(p))
object StarRule extends PredefinedStringMatchingRule(StarToken.star, p => StarToken(p))
object SlashRule extends PredefinedStringMatchingRule(SlashToken.slash, p => SlashToken(p))
object EqualRule extends PredefinedStringMatchingRule(EqualToken.equal, p => EqualToken(p))
object LessRule extends PredefinedStringMatchingRule(LessToken.less, p => LessToken(p))
object GreaterRule extends PredefinedStringMatchingRule(GreaterToken.greater, p => GreaterToken(p))
object PrintRule extends PredefinedStringMatchingRule(PrintToken.print, p => PrintToken(p))
object ConsRule extends PredefinedStringMatchingRule(ConsToken.cons, p => ConsToken(p))
object CarRule extends PredefinedStringMatchingRule(CarToken.car, p => CarToken(p))
object CdrRule extends PredefinedStringMatchingRule(CdrToken.cdr, p => CdrToken(p))
object NumberQuestionRule extends PredefinedStringMatchingRule(NumberQuestionToken.numberQuestion, p => NumberQuestionToken(p))
object SymbolQuestionRule extends PredefinedStringMatchingRule(SymbolQuestionToken.symbolQuestion, p => SymbolQuestionToken(p))
object ListQuestionRule extends PredefinedStringMatchingRule(ListQuestionToken.listQuestion, p => ListQuestionToken(p))
object NullQuestionRule extends PredefinedStringMatchingRule(NullQuestionToken.nullQuestion, p => NullQuestionToken(p))
object PrimopQuestionRule extends PredefinedStringMatchingRule(PrimopQuestionToken.primopQuestion, p => PrimopQuestionToken(p))
object ClosureQuestionRule extends PredefinedStringMatchingRule(ClosureQuestionToken.closureQuestion, p => ClosureQuestionToken(p))
object MaxRule extends PredefinedStringMatchingRule(MaxToken.max, p => MaxToken(p))
object OrRule extends PredefinedStringMatchingRule(OrToken.or, p => OrToken(p))
object AndRule extends PredefinedStringMatchingRule(AndToken.and, p => AndToken(p))
object CompressRule extends PredefinedStringMatchingRule(CompressToken.compress, p => CompressToken(p))
object ShapeRule extends PredefinedStringMatchingRule(ShapeToken.shape, p => ShapeToken(p))
object RavelRule extends PredefinedStringMatchingRule(RavelToken.ravel, p => RavelToken(p))
object RestructRule extends PredefinedStringMatchingRule(RestructToken.restruct, p => RestructToken(p))
object CatRule extends PredefinedStringMatchingRule(CatToken.cat, p => CatToken(p))
object IndxRule extends PredefinedStringMatchingRule(IndxToken.indx, p => IndxToken(p))
object TransRule extends PredefinedStringMatchingRule(TransToken.trans, p => TransToken(p))
object LeftSquareBracketRightSquareBracketRule extends PredefinedStringMatchingRule(LeftSquareBracketRightSquareBracketToken.leftSquareBracketRightSquareBracket, p => LeftSquareBracketRightSquareBracketToken(p))

object PlusSlashRule extends PredefinedStringMatchingRule(PlusSlashToken.plusSlash, p => PlusSlashToken(p))
object MinusSlashRule extends PredefinedStringMatchingRule(MinusSlashToken.minusSlash, p => MinusSlashToken(p))
object StarSlashRule extends PredefinedStringMatchingRule(StarSlashToken.starSlash, p => StarSlashToken(p))
object SlashSlashRule extends PredefinedStringMatchingRule(SlashSlashToken.slashSlash, p => SlashSlashToken(p))
object MaxSlashRule extends PredefinedStringMatchingRule(MaxSlashToken.maxSlash, p => MaxSlashToken(p))
object AndSlashRule extends PredefinedStringMatchingRule(AndSlashToken.andSlash, p => AndSlashToken(p))
object OrSlashRule extends PredefinedStringMatchingRule(OrSlashToken.orSlash, p => OrSlashToken(p))
