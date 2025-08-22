package com.mikadocs.kamin

sealed trait Result[+T]:
  def handle(success: T=>Unit, failure: String=>Unit, unfinished: ()=>Unit = ()=>{}): Unit = this match
    case Success(value) => success(value)
    case Failure(errorMessage) => failure(errorMessage)
    case Unfinished => unfinished()

  def flatMap[U](f: T => Result[U]): Result[U] = this match
    case Success(value) => f(value)
    case Failure(error) => Failure(error)
    case Unfinished => Unfinished

  def map[U](f: T => U): Result[U] = this match
    case Success(value) => Success(f(value))
    case Failure(error) => Failure(error)
    case Unfinished => Unfinished

object Result:
  def pure[T](value: T): Result[T] = Success(value)

case class Success[T](value: T) extends Result[T]
case class Failure(errorMessage: String) extends Result[Nothing]
case object Unfinished extends Result[Nothing]


