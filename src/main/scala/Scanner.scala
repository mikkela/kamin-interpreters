package com.mikadocs.kamin

import Acceptance.{Accepted, Rejected, Undecided}

trait Scanner:
  def scan(source: SourceReader): Iterator[Token]

extension (string: String)
  def add(sourceReading: SourceReading): String = string.appended(sourceReading.current)

abstract class FilteringScanner(val scanner: Scanner, val predicate: Token => Boolean) extends Scanner:
  private class TokenIterator(tokenIterator: Iterator[Token]) extends Iterator[Token]:
    private var nextToken: Option[Token] = None

    // Ensure `nextToken` is always the next valid token (not a WhiteSpaceToken)
    private def advance(): Unit =
      while (tokenIterator.hasNext && nextToken.isEmpty) {
        val token = tokenIterator.next()
        if predicate(token) then nextToken = Some(token)
      }

    override def hasNext: Boolean =
      if nextToken.isEmpty then advance()
      nextToken.isDefined

    override def next(): Token =
      val result = if !hasNext then EndOfFileToken else nextToken.get
      nextToken = None
      result

  override def scan(source: SourceReader): Iterator[Token] = TokenIterator(scanner.scan(source))

class WhiteSpaceSkippingScanner(scanner: Scanner) extends FilteringScanner(scanner, token => !token.isInstanceOf[WhiteSpaceToken])

enum Acceptance:
  case Accepted, Rejected, Undecided

trait ScannerRule:
  def accept(s: String): Acceptance

  def apply(s: String, position: SourcePosition): Token

class RuleBasedScanner(ruleSet: Seq[ScannerRule]) extends Scanner:
  private val rules = Seq(NewlineScannerRule, TabScannerRule, SpaceScannerRule) ++ ruleSet

  private class TokenIterator(var sourceReader: SourceReader) extends Iterator[Token]:
    private val lexemeScanner = LexemeScanner()
    private class ErrorHandler:
      private case class UndecidedState(undecidedText: String, sourceReader: SourceReader)
      extension (obj: Option[UndecidedState])
        private def text(): String = obj.map(_.undecidedText).getOrElse("")

      private def isNotAccepted(s: String): Boolean =
        rules.forall(_.accept(s) != Accepted)

      private def isUndecided(s: String): Boolean =
        rules.exists(_.accept(s) == Undecided)

      def apply(errorString: String, errorPosition: SourcePosition): Token =
        var error = errorString
        var reading: SourceReading = sourceReader.read
        var undecidedState:Option[UndecidedState] = None

        while !sourceReader.atEndOfSource && isNotAccepted(undecidedState.text().add(reading)) do
          if isUndecided(undecidedState.text().add(reading)) then
            undecidedState = Some(
              undecidedState.map(state => UndecidedState(state.undecidedText.add(reading), state.sourceReader))
                .getOrElse(UndecidedState("".add(reading), sourceReader))
            )
          else
            undecidedState = None
          error = error.add(reading)
          sourceReader = reading.next
          reading = sourceReader.read
        undecidedState.foreach(state => sourceReader = state.sourceReader)
        ErrorToken(error.substring(0, error.length - undecidedState.text().length), errorPosition)
    private class LexemeScanner:
      private case class AcceptedState(lexeme: String, sourceReader: SourceReader)

      private def isNotRejected(s: String): Boolean =
        rules.exists(_.accept(s) != Rejected)
      private def isAccepted(s: String): Boolean =
        rules.exists(_.accept(s) == Accepted)

      private def createToken(lexeme: String, position: SourcePosition): Token =
        rules.find(_.accept(lexeme) == Acceptance.Accepted).map(_.apply(lexeme, position)).getOrElse(ErrorToken(lexeme, position))

      def apply(): Token =
        var lexeme = ""
        var reading: SourceReading = sourceReader.read
        val initialPosition = sourceReader.position
        var acceptedState: Option[AcceptedState] = None
        while !sourceReader.atEndOfSource && isNotRejected(lexeme.add(reading)) do
          lexeme = lexeme.add(reading)
          sourceReader = reading.next
          reading = sourceReader.read
          if isAccepted(lexeme) then
            acceptedState = Some(AcceptedState(lexeme, sourceReader))

        acceptedState.map { state =>
          sourceReader = state.sourceReader
          createToken(state.lexeme, initialPosition)
        }.getOrElse(ErrorHandler()(lexeme, initialPosition))

    private var hasMore = true

    override def hasNext: Boolean  = hasMore
    override def next(): Token =
      if sourceReader.atEndOfSource then
        hasMore = false
        return EndOfFileToken

      lexemeScanner()

  override def scan(source: SourceReader): Iterator[Token] =

    new TokenIterator(source)

object SpaceScannerRule extends ScannerRule:
  override def accept(s: String): Acceptance = if s == " " then Accepted else Rejected
  override def apply(s: String, position: SourcePosition): Token = SpaceToken(position)

object TabScannerRule extends ScannerRule:
  override def accept(s: String): Acceptance = if s == "\t" then Accepted else Rejected
  override def apply(s: String, position: SourcePosition): Token = TabToken(position)

object NewlineScannerRule extends ScannerRule:
  override def accept(s: String): Acceptance = if s == "\n" then Accepted else Rejected
  override def apply(s: String, position: SourcePosition): Token = NewlineToken(position)

abstract class PredefinedStringMatchingRule(acceptedString: String, factory: SourcePosition => Token) extends ScannerRule:
  private val undecidedStrings: Seq[String => Boolean] = PredefinedStringMatchingRule.allSubstringsAsUndecided(acceptedString)
  override def accept(s: String): Acceptance =
    if s.equals(acceptedString) then
      Accepted
    else if undecidedStrings.exists(_.apply(s)) then
      Undecided
    else
      Rejected
  override def apply(s: String, position: SourcePosition): Token = factory(position)
object PredefinedStringMatchingRule:
  private def allSubstrings(s: String): Seq[String] =
    for {
      start <- 0 until s.length
      end <- (start + 1) to s.length
    } yield s.substring(start, end)

  def allSubstringsAsUndecided(s: String): Seq[String => Boolean] =
    allSubstrings(s).filter(p => !p.equals(s)).map(sub => (str: String) => str == sub)
