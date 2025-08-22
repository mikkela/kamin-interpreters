package com.mikadocs.kamin

class OffsetSourcePositionTest extends munit.FunSuite{
  test("OffsetSourcePosition line and column calculations") {
    val source = "First line\nSecond line\nThird line"
    val pos1 = OffsetSourcePosition(source, 0) // Beginning of the first line
    val pos2 = OffsetSourcePosition(source, 11) // Beginning of the second line
    val pos3 = OffsetSourcePosition(source, 27) // Within the third line

    assertEquals(pos1.line, 1)
    assertEquals(pos1.column, 1)

    assertEquals(pos2.line, 2)
    assertEquals(pos2.column, 1)

    assertEquals(pos3.line, 3)
    assertEquals(pos3.column, 5) // "Third" starts at offset 20; 24 is the 5th character
  }

  test("OffsetSourcePosition lineContents") {
    val source = "First line\nSecond line\nThird line"
    val pos1 = OffsetSourcePosition(source, 0)
    val pos2 = OffsetSourcePosition(source, 11)

    assertEquals(pos1.lineContents, "First line")
    assertEquals(pos2.lineContents, "Second line")
  }

  test("OffsetSourcePosition < comparison") {
    val source = "First line\nSecond line\nThird line"
    val pos1 = OffsetSourcePosition(source, 0) // Beginning of the first line
    val pos2 = OffsetSourcePosition(source, 11) // Beginning of the second line

    assert(pos1 < pos2)
    assert(!(pos2 < pos1))
  }

  test("OffsetSourcePosition toString representation") {
    val source = "First line\nSecond line\nThird line"
    val pos1 = OffsetSourcePosition(source, 0)
    val pos2 = OffsetSourcePosition(source, 11)

    assertEquals(pos1.toString, "1.1")
    assertEquals(pos2.toString, "2.1")
  }

  test("OffsetSourcePosition longString visualization") {
    val source = "First line\nSecond line\nThird line"
    val pos1 = OffsetSourcePosition(source, 0) // Beginning of the first line
    val pos2 = OffsetSourcePosition(source, 15) // Within "Second line"

    val expectedPos1 =
      """First line
        |^""".stripMargin

    val expectedPos2 =
      """Second line
        |    ^""".stripMargin

    assertEquals(pos1.longString, expectedPos1)
    assertEquals(pos2.longString, expectedPos2)
  }

  test("OffsetSourcePosition with empty source") {
    val source = ""
    val pos = OffsetSourcePosition(source, 0)

    assertEquals(pos.line, 1)
    assertEquals(pos.column, 1)
    assertEquals(pos.lineContents, "")
  }

  test("OffsetSourcePosition handling of CRLF") {
    val source = "First line\r\nSecond line\r\nThird line"
    val pos1 = OffsetSourcePosition(source, 10) // "\r\n" in first line
    val pos2 = OffsetSourcePosition(source, 12) // Beginning of second line

    assertEquals(pos1.line, 1)
    assertEquals(pos1.column, 11) // CRLF treated as a single newline

    assertEquals(pos2.line, 2)
    assertEquals(pos2.column, 1)
  }
}
