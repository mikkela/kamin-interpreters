package com.mikadocs.kamin

import scala.collection.mutable
import scala.collection.mutable.Queue

class KaminSourceReader(val internal: SourceReader) extends SourceReader:
  override def position: SourcePosition = internal.position

  override def read: SourceReading =
    val reading = KaminSourceReader.read(internal)
    SourceReading(reading.current, KaminSourceReader(reading.next))

  override def atEndOfSource: Boolean = internal.atEndOfSource

object KaminSourceReader:
  private val buffer = mutable.Queue[SourceReading]()

  def read(from: SourceReader): SourceReading =
    if buffer.isEmpty then
      var reading = from.read
      buffer.enqueue(reading)
      if reading.current == ';' then
        buffer.clear()
        while reading.current != '\n' && !reading.next.atEndOfSource do
          reading = reading.next.read
        return if reading.next.atEndOfSource then reading.next.read else read(reading.next)

    buffer.dequeue()


