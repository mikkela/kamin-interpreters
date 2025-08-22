package com.mikadocs.kamin

import scala.collection.mutable

sealed trait Environment[V <: Value]:
  def get(key: String): Option[V]
  def set(key: String, value: V): Unit
  def contains(key: String): Boolean

class GlobalEnvironment[V <: Value] extends Environment[V]:
  private val scope = mutable.HashMap[String, Option[V]]()

  override def get(key: String): Option[V] =
    scope.get(key).flatten

  override def set(key: String, value: V): Unit =
    scope.put(key, Some(value))

  override def contains(key: String): Boolean = scope.contains(key)

class EnvironmentFrame[V <: Value](private val previousEnvironment: Environment[V]) extends Environment[V]:
  private val scope: mutable.Map[String, Option[V]] = mutable.Map.empty

  override def get(key: String): Option[V] =
    if scope.contains(key) then scope(key) else previousEnvironment.get(key)

  override def set(key: String, value: V): Unit =
    scope.put(key, Some(value))

  override def contains(key: String): Boolean = scope.contains(key)

class PredefinedEnvironmentFrame[V <: Value](private val previousEnvironment: Environment[V], keys: Seq[String]) extends Environment[V]:
  private val scope: mutable.Map[String, Option[V]] = mutable.Map.from(keys.map(_ -> None))

  override def get(key: String): Option[V] =
    if scope.contains(key) then scope(key) else previousEnvironment.get(key)

  override def set(key: String, value: V): Unit =
    if scope.contains(key) then scope.put(key, Some(value)) else previousEnvironment.set(key, value)

  override def contains(key: String): Boolean = scope.contains(key)
