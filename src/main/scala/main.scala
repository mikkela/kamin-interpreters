package com.mikadocs.kamin

import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder

val languageMap: Map[String, Interpreter] = Map(
  "basic" -> basic.basicInterpreter,
  "lisp" -> lisp.lispInterpreter
)

@main
def Workbench(): Unit =
  val terminal = TerminalBuilder.terminal()
  val lineReader = LineReaderBuilder.builder()
    .terminal(terminal)
    .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
    .build()

  var interpreter = languageMap.head._2
  var continue = true
  var parseOrEvaluate: Either[Unit, Unit] = Right(())
  var program = StringBuilder()
  while continue do
    val input = if program.isEmpty then lineReader.readLine("->") else lineReader.readLine(">")
    if input.startsWith(":") then
      val parts = input.split("\\s+")
      if parts.nonEmpty then
        program.clear()
        parts.head.trim match
          case ":exit" =>
            continue = false
          case ":language" =>
            if parts.length == 2 then
              if languageMap.contains(parts(1)) then
                interpreter = languageMap(parts(1))
                println("Language is: " + parts(1))
              else
                println("Language not found: " + parts(1))

          case ":mode" =>
            if parts.length== 2 then
              parts(1) match
                case "parse" => parseOrEvaluate = Left(())
                case "evaluate" => parseOrEvaluate = Right(())

          case s =>
            println("Did not understand")
    else
      program.append(input)
      val result = if parseOrEvaluate.isLeft then
        interpreter.parseAndPrint(program.toString())
      else
        interpreter.interpret(program.toString())

      program.clear()
      result.handle(
        v => {
          println()
          println(v)
        },
        e => {
          println()
          println("Error: " + e)
        }
      )

