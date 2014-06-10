package net.rfc1149.inf355

trait Expr {
  def eval(): Int
}

class Num(val i: Int) extends Expr {
  def eval(): Int = i
}

class Mul(val l: Expr, val r: Expr) extends Expr {
  def eval(): Int = l.eval() * r.eval()
}

class Sum(val l: Expr, val r: Expr) extends Expr {
  def eval(): Int = l.eval() + r.eval()
}

object Main extends App {

  val expr: Expr = new Sum(new Mul(new Num(1), new Num(2)), new Num(3))

  println(expr.eval())

}
