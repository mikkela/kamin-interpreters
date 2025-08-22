package com.mikadocs.kamin

case class SourceReading(current: Char, next: SourceReader)

trait SourceReader:
  def read: SourceReading
  def position: SourcePosition
  def atEndOfSource: Boolean

object SourceReader:
  final val EndOfSource: Char = '\u001a'

  def apply(source: String): SourceReader = SourceReaderImpl(source, 0)

private class SourceReaderImpl(val source: String, private val offset: Int) extends SourceReader:
  private final val endOfSourceReading = SourceReading(SourceReader.EndOfSource, this)
  override def read: SourceReading =
    if !atEndOfSource then SourceReading(source(offset), SourceReaderImpl(source, offset + 1)) else endOfSourceReading
  override def position: SourcePosition = OffsetSourcePosition(source, offset)
  override def atEndOfSource: Boolean = offset >= source.length


trait SourcePosition:
  /** The line number referred to by the position; line numbers start at 1. */
  def line: Int

  /** The column number referred to by the position; column numbers start at 1. */
  def column: Int

  protected def lineContents: String
  override def toString = s"$line.$column"

  def longString: String = lineContents + "\n" + lineContents.take(column - 1).map { x => if (x == '\t') x else ' ' } + "^"
  def <(that: SourcePosition): Boolean =
    this.line < that.line ||
      this.line == that.line && this.column < that.column

  override def equals(other: Any) : Boolean=
    other match
      case that: SourcePosition => this.line == that.line && this.column == that.column
      case _ => false

import scala.collection.mutable.ArrayBuffer

case class OffsetSourcePosition(source: String, offset: Int) extends SourcePosition:
  /** An index that contains all line starts, including first line, and eof. */
  private lazy val index: Array[Int] =
    Option(OffsetSourcePosition.indexCache.get(source)) match
      case Some(index) => index
      case None =>
        val index = genIndex
        OffsetSourcePosition.indexCache.put(source, index)
        index

  private def genIndex: Array[Int] =
    val lineStarts = new ArrayBuffer[Int]
    lineStarts += 0
    for (i <- 0 until source.length)
      if (source.charAt(i) == '\n' ||
        (source.charAt(i) == '\r' && (i == (source.length - 1) || source.charAt(i + 1) != '\n'))) {
        lineStarts += (i + 1)
      }
    lineStarts += source.length
    lineStarts.toArray

  /** The line number referred to by the position; line numbers start at 1. */
  def line: Int =
    var lo = 0
    var hi = index.length - 1
    while (lo + 1 < hi)
      val mid = lo + ((hi - lo) / 2)
      if (offset < index(mid)) hi = mid
      else lo = mid
    lo + 1

  /** The column number referred to by the position; column numbers start at 1. */
  def column: Int = offset - index(line - 1) + 1

  def lineContents: String =
    val lineStart = index(line - 1)
    val lineEnd = index(line)
    val endIndex =
      if (lineStart < lineEnd - 1 && source.charAt(lineEnd - 2) == '\r' && source.charAt(lineEnd - 1) == '\n') {
        lineEnd - 2
      } else if (lineStart < lineEnd && (source.charAt(lineEnd - 1) == '\r' || source.charAt(lineEnd - 1) == '\n')) {
        lineEnd - 1
      } else {
        lineEnd
      }
    source.subSequence(lineStart, endIndex).toString

  override def toString = s"$line.$column"

  override def <(that: SourcePosition): Boolean = that match
    case OffsetSourcePosition(_, that_offset) =>
      this.offset < that_offset
    case _ =>
      this.line < that.line ||
        this.line == that.line && this.column < that.column

private object OffsetSourcePosition extends scala.runtime.AbstractFunction2[String, Int, OffsetSourcePosition] with SourcePositionCache

private trait SourcePositionCache:
  private lazy val indexCacheTL: ThreadLocal[java.util.Map[String, Array[Int]]] =
    // not DynamicVariable as that would share the map from parent to child :-(
    new ThreadLocal[java.util.Map[String, Array[Int]]] {
      override def initialValue = new java.util.WeakHashMap[String, Array[Int]]
    }

  def indexCache: java.util.Map[String, Array[Int]] = indexCacheTL.get
