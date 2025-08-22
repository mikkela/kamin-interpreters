package com.mikadocs.kamin
package clu

class Scanner extends WhiteSpaceSkippingScanner(RuleBasedScanner(Seq(
  LeftParenthesisRule,
  RightParenthesisRule,
  DollarRule,
  DefineRule,
  IfRule,
  WhileRule,
  BeginRule,
  SetRule,
  ClusterRule,
  RepRule,
  PlusRule,
  MinusRule,
  StarRule,
  SlashRule,
  EqualRule,
  LessRule,
  GreaterRule,
  PrintRule,
  IntegerRule,
  NameRule(Seq(LeftParenthesisToken.leftParenthesis, RightParenthesisToken.rightParenthesis, DollarToken.dollar))
)))



