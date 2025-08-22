package com.mikadocs.kamin

type EvaluationResult = Result[String]
type ParseAndPrintResult = Result[String]

trait Interpreter:
  def interpret(progam: String): EvaluationResult
  def parseAndPrint(program: String): ParseAndPrintResult


