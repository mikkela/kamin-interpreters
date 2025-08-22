package com.mikadocs.kamin
package basic

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  DefineRule,
  IfRule,
  WhileRule,
  BeginRule,
  SetRule,
  PlusRule,
  MinusRule,
  StarRule,
  SlashRule,
  EqualRule,
  LessRule,
  GreaterRule,
  PrintRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis))
)))



