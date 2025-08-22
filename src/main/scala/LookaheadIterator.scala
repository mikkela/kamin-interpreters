package com.mikadocs.kamin

import scala.collection.mutable

implicit class LookaheadIterator[A](val iter: Iterator[A]) extends Iterator[A]:
  private val buffer = mutable.Queue.empty[A]

  private def ensureBuffered(n: Int): Unit =
    while (buffer.size < n && iter.hasNext) do
      buffer.enqueue(iter.next())

  def lookahead(n: Int): Seq[A] =
    ensureBuffered(n)
    buffer.iterator.take(n).toSeq

  def headOption: Option[A] =
    ensureBuffered(1)
    buffer.headOption
    
  override def hasNext: Boolean = buffer.nonEmpty || iter.hasNext

  override def next(): A =
    ensureBuffered(1)
    buffer.dequeue()

def prepend[T](head: T, tail: Iterator[T]): Iterator[T] =
  Iterator.single(head) ++ tail