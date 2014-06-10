package net.rfc1149.inf355

trait Expr {
  def accept[A](visitor: Visitor[A]): A
}

trait Visitor[A] {
  def visit(num: Num): A
  def visit(mul: Mul): A
  def visit(sum: Sum): A
}

class Num(val i: Int) extends Expr {
  def accept[A](visitor: Visitor[A]) = visitor.visit(this)
}

class Mul(val l: Expr, val r: Expr) extends Expr {
  def accept[A](visitor: Visitor[A]) = visitor.visit(this)
}

class Sum(val l: Expr, val r: Expr) extends Expr {
  def accept[A](visitor: Visitor[A]) = visitor.visit(this)
}

object Main extends App {

  class Evaluator extends Visitor[Int] {
    def visit(num: Num): Int = num.i
    def visit(mul: Mul): Int = mul.l.accept(this) * mul.r.accept(this)
    def visit(sum: Sum): Int = sum.l.accept(this) + sum.r.accept(this)
  }

  def eval(e: Expr): Int = e.accept(new Evaluator)

  val expr: Expr = new Sum(new Mul(new Num(1), new Num(2)), new Num(3))

  println(eval(expr))

}
