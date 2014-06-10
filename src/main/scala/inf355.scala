package net.rfc1149.inf355

sealed trait Expr

case class Num(i: Int) extends Expr

case class Mul(l: Expr, r: Expr) extends Expr

case class Sum(l: Expr, r: Expr) extends Expr

object Main extends App {

  def eval(e: Expr): Int = e match {
    case Num(i)    => i
    case Sum(l, r) => eval(l) + eval(r)
    case Mul(l, r) => eval(l) * eval(r)
  }

  val expr: Expr = Sum(Mul(Num(1), Num(2)), Num(3))

  println(eval(expr))

}
