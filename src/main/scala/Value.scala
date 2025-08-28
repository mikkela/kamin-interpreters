package com.mikadocs.kamin

import com.mikadocs.kamin.MatrixValue.vector

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

case class MatrixDimensions(rows: Int, cols: Int)

case class MatrixValue(value: Seq[Int], dimensions: MatrixDimensions) extends Value:
  override def toString: String = ""

  def isVector: Boolean = dimensions.rows == 1
  def isShapeVector: Boolean = isVector && dimensions.cols <= 2

  def isNullMatrix: Boolean = value.isEmpty

  def shape: MatrixValue =
    if isVector then vector(Seq(dimensions.cols))
    else vector(Seq(dimensions.rows, dimensions.cols))
  def ravel: MatrixValue =
    vector(value)
  def transpose : MatrixValue =
    if isVector then
      this
    else
      MatrixValue(value.sliding(dimensions.cols, dimensions.cols).toSeq.transpose.flatten, MatrixDimensions(dimensions.cols, dimensions.rows))

  def subscription(pVector: MatrixValue): MatrixValue = {
    require(pVector.isVector)
    if isVector then
      MatrixValue.vector(pVector.value.map(i => value(i - 1)))
    else {
      val rows = value.sliding(dimensions.cols, dimensions.cols).toSeq
      val picked = pVector.value.map(i => rows(i - 1)).flatten
      MatrixValue(picked, MatrixDimensions(pVector.value.length, dimensions.cols))
    }
  }

object MatrixValue:
  def vector(value: Seq[Int]): MatrixValue =
    MatrixValue(value = value, dimensions = MatrixDimensions(1, value.length))

  def toMatrix(value: Integer): MatrixValue = MatrixValue(Seq(value), MatrixDimensions(1, 1))
  def nullMatrix: MatrixValue = MatrixValue(Seq.empty, MatrixDimensions(0, 0))
  def nullVector: MatrixValue = MatrixValue(Seq.empty, MatrixDimensions(1, 0))

  def compress(controlMatrix: MatrixValue, value: MatrixValue): MatrixValue =
    if value.isVector then
      val v = repeatOrChop(controlMatrix.value, value.dimensions.cols)
      val temp = v.zip(value.value).filter((x, _) => x != 0).map((_, y) => y)
      MatrixValue(temp, MatrixDimensions(if (temp.length > 0) 1 else 0, temp.length))
    else
      val v = repeatOrChop(controlMatrix.value, value.dimensions.rows)
      val temp = v.zip(value.value.sliding(value.dimensions.cols, value.dimensions.cols)).filter((x, _) => x != 0).map((_, y) => y).flatten
      MatrixValue(temp, MatrixDimensions(if (temp.length > 0) 1 else 0, temp.length))

  def restruct(shape: MatrixValue, value: MatrixValue) =
    require(shape.isNullMatrix || shape.isShapeVector)
    if shape.isNullMatrix then
      IntegerValue(value.value.head)
    else if (shape.dimensions.cols == 1) then
      MatrixValue(repeatOrChop(value.value, shape.value.head), MatrixDimensions(1, shape.value.head))
    else
      MatrixValue(repeatOrChop(value.value, shape.value.head * shape.value(1)), MatrixDimensions(shape.value.head, shape.value(1)))

  def index(n: Int): MatrixValue =
    require(n > 0)
    MatrixValue.vector(1 to n)

  private def repeatOrChop[A](xs: Seq[A], n: Int): Seq[A] =
    require(n >= 0, "n must be >= 0")
    if (n <= xs.length) xs.take(n)
    else
      require(xs.nonEmpty, "cannot repeat an empty sequence to a positive length")
      Iterator.continually(xs).flatten.take(n).toSeq

