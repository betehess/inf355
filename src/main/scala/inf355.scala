package net.rfc1149.inf355

import scala.language.higherKinds

trait OptionModule {
  type Option[+_]
  type Some[+A] <: Option[A]
  type None <: Option[Nothing]
  def some[A](x: A): Some[A]
  def none: None
  def fold[A, B](opt: Option[A])(ifNone: => B, ifSome: A => B): B
}


trait ScalaOption extends OptionModule {
  type Option[+A] = scala.Option[A]
  type Some[+A] = scala.Some[A]
  type None = scala.None.type
  def some[A](x: A): Some[A] = scala.Some(x)
  def none: None = scala.None
  def fold[A, B](opt: Option[A])(ifNone: => B, ifSome: A => B): B = opt match {
    case None => ifNone
    case Some(x) => ifSome(x)
  }
}

trait MyApp extends App with OptionModule {
  val opt = some(42)
  val s: String = fold(opt)("None", i => s"Some($i)")
  println(s)
}

object ScalaOptionApp extends MyApp with ScalaOption
