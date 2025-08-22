package com.mikadocs.kamin

trait Value

case class IntegerValue(value: Int) extends Value, Ordered[IntegerValue]:
  override def toString: String = value.toString

  override def compare(that: IntegerValue): Int =
    this.value.compare(that.value)

object IntegerValue:
  def unapply(v: IntegerValue): Option[Int] = Some(v.value)

implicit class IntegerValueExtension(val i: Int) extends AnyVal:
  def toIntegerValue: IntegerValue = IntegerValue(i)

case class DoubleValue(value: Double) extends Value:
  override def toString: String = value.toString

object DoubleValue:
  def unapply(v: DoubleValue): Option[Double] = Some(v.value)
  
case class StringValue(value: String) extends Value:
  override def toString: String = s"""$value"""

case class SymbolValue(value: String) extends Value:
  override def toString: String = value

object SymbolValue:
  val T: SymbolValue = SymbolValue("T")

case class ListValue(value: List[Value]) extends Value:
  override def toString: String = "(" + value.mkString(" ") + ")"

implicit class SymbolValueExtension(val s: String) extends AnyVal:
  def toSymbolValue: SymbolValue = SymbolValue(s)

object ListValue:
  val nil: ListValue = ListValue(List.empty)

case class Dimensions(rows: Int, cols: Int)

case class MatrixValue(value: Seq[Int], dimensions: Dimensions) extends Value:
  override def toString: String = ""

object MatrixValue:
  def vector(value: Seq[Int]): MatrixValue =
    MatrixValue(value = value, dimensions = Dimensions(1, value.length))
