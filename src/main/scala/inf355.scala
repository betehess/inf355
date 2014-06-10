package net.rfc1149.inf355

import scala.language.higherKinds

trait OptionSig {
  type Option[+_]
  type Some[+A] <: Option[A]
  type None <: Option[Nothing]
}

trait Ops[Sig <: OptionSig] {
  def some[A](x: A): Sig#Some[A]
  def none: Sig#None
  def fold[A, B](opt: Sig#Option[A])(ifNone: => B, ifSome: A => B): B
}

trait ScalaOption extends OptionSig {
  type Option[+A] = scala.Option[A]
  type Some[+A] = scala.Some[A]
  type None = scala.None.type
}

object ScalaOption {
  implicit object Ops extends Ops[ScalaOption] {
    def some[A](x: A): ScalaOption#Some[A] = scala.Some(x)
    def none: ScalaOption#None = scala.None
    def fold[A, B](opt: ScalaOption#Option[A])(ifNone: => B, ifSome: A => B): B = opt match {
      case None => ifNone
      case Some(x) => ifSome(x)
    }
  }
  implicit val Show = new Show
}

class Show[Sig <: OptionSig](implicit ops: Ops[Sig]) {
  def show[A](opt: Sig#Option[A]): String =
    ops.fold(opt)("None", i => s"Some($i)")
}

class MyApp[Sig <: OptionSig](implicit ops: Ops[Sig], show: Show[Sig]) extends App {
  val opt = ops.some(42)
  println(show.show(opt))
}

object ScalaOptionApp extends MyApp[ScalaOption]
