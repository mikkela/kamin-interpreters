package com.mikadocs.kamin
package prolog

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  InferRule,
  FromRule,
  InferQuestionRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis))
)))



