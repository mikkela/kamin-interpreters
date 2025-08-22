package com.mikadocs.kamin

import Acceptance.{Accepted, Rejected, Undecided}

class RuleBasedScannerTest extends munit.FunSuite {
  // Helper for creating a ScannerRule
  def createRule(
                  accepts: String => Boolean,
                  transform: String => String = identity
                ): ScannerRule = new ScannerRule {
    override def accept(s: String): Acceptance = if accepts(s) then Accepted else Rejected

    override def apply(s: String, position: SourcePosition): Token =
      TestToken(transform(s), position)
  }

  def createLookaheadRule(
                           initialPartAccept: String => Boolean,
                           lookaheadPartAccept: String => Boolean,
                           finalPart: String => Boolean
                         ): ScannerRule = new ScannerRule {
    override def accept(s: String): Acceptance =
      if initialPartAccept(s) then
        Accepted
      else
        if lookaheadPartAccept(s) then
          Undecided
        else
          if finalPart(s) then
            Accepted
          else
            Rejected

    override def apply(s: String, position: SourcePosition): Token =
      TestToken(s, position)
  }

  test("scans empty string into EoF Token. This token wiull be returned for ever") {
    val sut = RuleBasedScanner(Seq.empty)
    val iterator = sut.scan(SourceReader(""))
    assertEquals(iterator.hasNext, true)
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
    assertEquals(iterator.next(), EndOfFileToken)
  }

  test("scans the provided strean with a single matching rule matching a complete token") {
    val sut = RuleBasedScanner(Seq(createRule(_.startsWith("a"))))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("another", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with mutually exclusive rules, providing a stream of tokens") {
    val sut = RuleBasedScanner(Seq(
      createRule("an".contains),
      createRule("other".contains)
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("an", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TestToken("other", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with multiple matching rules, selects the longest match") {
    val sut = RuleBasedScanner(Seq(
      createRule("an".contains),
      createRule("another".contains)
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("another", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with multiple matching rules, selects the first matching rule when multiple longest match") {
    val sut = RuleBasedScanner(Seq(
      createRule("another".contains, _ + "!"),
      createRule("another".contains)
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("another!", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a single matching rule requiring look ahead") {
    val sut = RuleBasedScanner(Seq(createLookaheadRule(_.equals("a"), _.equals("a."), _.equals("a.b"))))
    val iterator = sut.scan(SourceReader("a.b"))

    assertEquals(iterator.next(), TestToken("a.b", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a single non-matching rule, the rejected characters are consumed and returned as an error") {
    val sut = RuleBasedScanner(Seq(createRule(_.startsWith("b"))))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), ErrorToken("another", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a two non-matching rule where the latter matches later in the string") {
    val sut = RuleBasedScanner(Seq(
      createRule(_.startsWith("b")),
      createRule("other".contains),
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), ErrorToken("an", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TestToken("other", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a first a matching then non-matching rules and then matching again") {
    val sut = RuleBasedScanner(Seq(
      createRule("an".contains),
      createRule("y".contains),
      createRule("her".contains),
    ))
    val iterator = sut.scan(SourceReader("another"))

    assertEquals(iterator.next(), TestToken("an", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), ErrorToken("ot", createSourcePosition(1, 3)))
    assertEquals(iterator.next(), TestToken("her", createSourcePosition(1, 5)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a single matching rule requiring look ahead that fails") {
    val sut = RuleBasedScanner(Seq(createLookaheadRule(_.equals("a"), _.equals("a."), _.equals("a.b"))))
    val iterator = sut.scan(SourceReader("a.c"))

    assertEquals(iterator.next(), TestToken("a", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), ErrorToken(".c", createSourcePosition(1, 2)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with a single matching rule requiring look ahead that initially fails and then succeeds") {
    val sut = RuleBasedScanner(Seq(createLookaheadRule(_.equals(""), _.equals("."), _.equals(".b"))))
    val iterator = sut.scan(SourceReader("a.b"))

    assertEquals(iterator.next(), ErrorToken("a", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TestToken(".b", createSourcePosition(1, 2)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans the provided stream with multiple matching one with look ahead (failing), the other, shorter doesn't") {
    val sut = RuleBasedScanner(Seq(
      createRule(_.equals("a")),
      createRule(_.equals(".")),
      createLookaheadRule(_.equals("a"), _.equals("."), _.equals(".b")),
    ))
    val iterator = sut.scan(SourceReader("a."))

    assertEquals(iterator.next(), TestToken("a", createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TestToken(".", createSourcePosition(1, 2)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }

  test("scans whitespaces into relevant tokens by default") {
    val sut = RuleBasedScanner(Seq.empty)
    val iterator = sut.scan(SourceReader(" \t \n "))

    assertEquals(iterator.next(), SpaceToken(createSourcePosition(1, 1)))
    assertEquals(iterator.next(), TabToken(createSourcePosition(1, 2)))
    assertEquals(iterator.next(), SpaceToken(createSourcePosition(1, 3)))
    assertEquals(iterator.next(), NewlineToken(createSourcePosition(1, 4)))
    assertEquals(iterator.next(), SpaceToken(createSourcePosition(2, 1)))
    assertEquals(iterator.next(), EndOfFileToken)
    assertEquals(iterator.hasNext, false)
  }
}
