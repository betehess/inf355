package net.rfc1149.inf355

trait OptionModule {
  type Option
  type Some <: Option
  type None <: Option
  def some(x: Int): Some
  def none: None
  def fold[A](opt: Option)(ifNone: => A, ifSome: Int => A): A
}


trait ScalaOption extends OptionModule {
  type Option = scala.Option[Int]
  type Some = scala.Some[Int]
  type None = scala.None.type
  def some(x: Int): Some = scala.Some(x)
  def none: None = scala.None
  def fold[A](opt: Option)(ifNone: => A, ifSome: Int => A): A = opt match {
    case None => ifNone
    case Some(i) => ifSome(i)
  }
}

trait MyApp extends App with OptionModule {
  val opt = some(42)
  val s: String = fold(opt)("None", i => s"Some($i)")
  println(s)
}

object ScalaOptionApp extends MyApp with ScalaOption
