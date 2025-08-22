package com.mikadocs.kamin

class LookaheadIteratorTest extends munit.FunSuite:
  test("lookahead without consuming"):
    val it = Iterator(1, 2, 3, 4, 5)
    val laIt = new LookaheadIterator(it)

    assertEquals(laIt.lookahead(1), Seq(1))
    assertEquals(laIt.lookahead(2), Seq(1, 2))
    assertEquals(laIt.lookahead(3), Seq(1, 2, 3))
    assertEquals(laIt.lookahead(10), Seq(1, 2, 3, 4, 5)) // Out of range

  test("lookahead and consume"):
    val it = Iterator(1, 2, 3, 4, 5)
    val laIt = new LookaheadIterator(it)

    assertEquals(laIt.lookahead(2), Seq(1, 2))
    assertEquals(laIt.next(), 1)
    assertEquals(laIt.lookahead(2), Seq(2, 3))
    assertEquals(laIt.next(), 2)
    assertEquals(laIt.next(), 3)
    assertEquals(laIt.lookahead(2), Seq(4, 5))
    assertEquals(laIt.next(), 4)
    assertEquals(laIt.next(), 5)
    assertEquals(laIt.lookahead(1), Seq())
    assert(!laIt.hasNext)

  test("hasNext behavior"):
    val it = Iterator(1, 2, 3)
    val laIt = new LookaheadIterator(it)

    assert(laIt.hasNext)
    assertEquals(laIt.next(), 1)
    assert(laIt.hasNext)
    assertEquals(laIt.next(), 2)
    assert(laIt.hasNext)
    assertEquals(laIt.next(), 3)
    assert(!laIt.hasNext)

class PrependTest extends munit.FunSuite:
  test("prepend to non-empty iterator"):
    val it = Iterator(2, 3, 4)
    val newIt = prepend(1, it)

    assertEquals(newIt.next(), 1)
    assertEquals(newIt.next(), 2)
    assertEquals(newIt.next(), 3)
    assertEquals(newIt.next(), 4)
    assert(!newIt.hasNext)

  test("prepend to empty iterator"):
    val it = Iterator.empty[Int]
    val newIt = prepend(1, it)

    assertEquals(newIt.next(), 1)
    assert(!newIt.hasNext)

  test("prepend does not modify original iterator"):
    val it = Iterator(2, 3, 4)
    val newIt = prepend(1, it)

    assertEquals(newIt.next(), 1)
    assertEquals(it.next(), 2) // Ensuring `it` remains usable
    assertEquals(it.next(), 3)
    assertEquals(it.next(), 4)
    assert(!it.hasNext)
