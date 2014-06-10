package net.rfc1149.inf355

trait Expr

class Num(val i: Int) extends Expr

class Mul(val l: Expr, val r: Expr) extends Expr

class Sum(val l: Expr, val r: Expr) extends Expr

object Main extends App {

  val expr: Expr = new Sum(new Mul(new Num(1), new Num(2)), new Num(3))

  def eval(e: Expr): Int = e match {
    case num: Num => num.i
    case mul: Mul => eval(mul.l) * eval(mul.r)
    case sum: Sum => eval(sum.l) + eval(sum.r)
  }

  println(eval(expr))

}
