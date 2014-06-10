package net.rfc1149.inf355

sealed trait Option[+A]

case class Some[+A](x: A) extends Option[A]

case object None extends Option[Nothing]

trait OptionModule {

  // types

  type Option

  type Some <: Option

  type None <: Option

  // injectors
  
  def some(x: Int): Some
  
  def none: None

  // catamorphism

  def fold[A](opt: Option)(ifNone: A, ifSome: Int => A): A

}

