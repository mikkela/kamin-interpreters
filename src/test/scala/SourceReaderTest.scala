package com.mikadocs.kamin

class SourceReaderTest extends munit.FunSuite{
  test("SourceReader initial state") {
    val sourceString = "Hello, World!"
    val reader = SourceReader(sourceString)

    assertEquals(reader.position, new SourcePosition{
      override def line: Int = 1
      override def column: Int = 1
      override protected def lineContents: String = ???
    }) // Initial position must be (1, 1)
    assertEquals(reader.atEndOfSource, false) // Reader should not be at the end initially
  }

  test("SourceReader read returns correct character and updates offset") {
    val sourceString = "Hello"
    val reader = SourceReader(sourceString)

    val firstRead = reader.read
    assertEquals(firstRead.current, 'H')
    assertEquals(firstRead.next.position, new SourcePosition{
      override def line: Int = 1
      override def column: Int = 2
      override protected def lineContents: String = ???
    })
    assertEquals(firstRead.next.atEndOfSource, false)

    val secondRead = firstRead.next.read
    assertEquals(secondRead.current, 'e')
    assertEquals(secondRead.next.position, new SourcePosition{
      override def line: Int = 1
      override def column: Int = 3
      override protected def lineContents: String = ???
    })
  }

  test("SourceReader reads through entire string") {
    val sourceString = "abc"
    var currentReader = SourceReader(sourceString)
    val result = new StringBuilder

    while (!currentReader.atEndOfSource) {
      val reading = currentReader.read
      result.append(reading.current)
      currentReader = reading.next
    }

    assertEquals(result.toString, sourceString)
    assertEquals(currentReader.atEndOfSource, true)
  }

  test("SourceReader returns EndOfSource when reading past the end") {
    val sourceString = "abc"
    var currentReader = SourceReader(sourceString)

    // Read through the entire source
    while (!currentReader.atEndOfSource) {
      currentReader = currentReader.read.next
    }

    // At the end, it should return EndOfSource
    val endRead = currentReader.read
    assertEquals(endRead.current, SourceReader.EndOfSource)
    assertEquals(endRead.next, currentReader) // Should return itself at the end
  }

  test("SourceReader atEndOfSource should be true after consuming all characters") {
    val sourceString = "abc"
    var reader = SourceReader(sourceString)

    // Read through all characters
    while (!reader.atEndOfSource) {
      reader = reader.read.next
    }

    assertEquals(reader.atEndOfSource, true)
  }

  test("SourceReader handles empty input string") {
    val reader = SourceReader("")

    assertEquals(reader.atEndOfSource, true)
    val endRead = reader.read
    assertEquals(endRead.current, SourceReader.EndOfSource)
    assertEquals(endRead.next, reader)
  }

  test("SourceReader position tracks offset correctly") {
    val sourceString = "Line 1\nLine 2\nLine 3"
    val reader = SourceReader(sourceString)
    val firstRead = reader.read
    assertEquals(firstRead.next.position, new SourcePosition{
      override def line: Int = 1
      override def column: Int = 2
      override protected def lineContents: String = ???
    })

    // Read till second line
    val readerAfterFirstLine = Iterator.iterate(reader)(_.read.next).drop(7).next()
    assertEquals(readerAfterFirstLine.position, new SourcePosition{
      override def line: Int = 2
      override def column: Int = 1
      override protected def lineContents: String = ???
    })
  }
}
