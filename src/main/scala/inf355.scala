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
}

import java.util.Optional
import java.util.function.{ Function => F, Supplier }

trait Java8Option extends OptionSig {
  type Option[+A] = Optional[_ <: A]
  type Some[+A] = Optional[_ <: A]
  type None = Optional[Nothing]
}

object Java8Option {
  implicit object Ops extends Ops[Java8Option] {
    def some[A](x: A): Java8Option#Some[A] = Optional.of(x)
    def none: Java8Option#None = Optional.empty()
    def fold[A, B](opt: Java8Option#Option[A])(ifNone: => B, ifSome: A => B): B = {
      def f = new F[A, B] { def apply(a: A): B = ifSome(a) }
      def supplier = new Supplier[B] { def get(): B = ifNone }
      opt.map[B](f).orElseGet(supplier)
    }
  }
}


trait AnyOption extends OptionSig {
  type Option[+A] = Any
  type Some[+A] = Any
  type None = Null
}

object AnyOption {
  implicit object Ops extends Ops[AnyOption] {
    def some[A](x: A): AnyOption#Some[A] = x
    def none: AnyOption#None = null
    def fold[A, B](opt: AnyOption#Option[A])(ifNone: => B, ifSome: A => B): B = {
      if (opt == null)
        ifNone
      else
        ifSome(opt.asInstanceOf[A])
    }
  }
}


class Show[Sig <: OptionSig](implicit ops: Ops[Sig]) {
  def show[A](opt: Sig#Option[A]): String =
    ops.fold(opt)("None", i => s"Some($i)")
}

object Show {
  implicit def showInstance[Sig <: OptionSig](implicit ops: Ops[Sig]): Show[Sig] = new Show
}

class MyApp[Sig <: OptionSig](implicit ops: Ops[Sig]) extends App {
  val show: Show[Sig] = implicitly[Show[Sig]]
  val opt = ops.some(42)
  println(show.show(opt))
}

object ScalaOptionApp extends MyApp[ScalaOption]

object Java8OptionApp extends MyApp[Java8Option]

object AnyOptionApp extends MyApp[AnyOption]
