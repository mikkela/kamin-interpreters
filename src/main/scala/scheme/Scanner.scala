package com.mikadocs.kamin
package scheme

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  QuoteRule,
  IfRule,
  WhileRule,
  BeginRule,
  SetRule,
  LambdaRule,
  PlusRule,
  MinusRule,
  StarRule,
  SlashRule,
  EqualRule,
  LessRule,
  GreaterRule,
  ConsRule,
  CarRule,
  CdrRule,
  NumberQuestionRule,
  SymbolQuestionRule,
  ListQuestionRule,
  NullQuestionRule,
  PrimopQuestionRule,
  ClosureQuestionRule,
  PrintRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis, QuoteToken.quote))
)))



