package net.rfc1149.inf355

trait Expr {
  def accept(visitor: Visitor): Unit
}

trait Visitor {
  def visit(num: Num): Unit
  def visit(mul: Mul): Unit
  def visit(sum: Sum): Unit
}

class Num(val i: Int) extends Expr {
  def accept(visitor: Visitor) = visitor.visit(this)
}

class Mul(val l: Expr, val r: Expr) extends Expr {
  def accept(visitor: Visitor) = visitor.visit(this)
}

class Sum(val l: Expr, val r: Expr) extends Expr {
  def accept(visitor: Visitor) = visitor.visit(this)
}

object Main extends App {

  class Evaluator(var acc: Int = 0) extends Visitor {
    def visit(num: Num): Unit = {
      acc = num.i
    }
    def visit(mul: Mul): Unit = {
      mul.l.accept(this)
      val leftValue = acc
      mul.r.accept(this)
      acc *= leftValue
    }
    def visit(sum: Sum): Unit = {
      sum.l.accept(this)
      val leftValue = acc
      sum.r.accept(this)
      acc += leftValue
    }
  }

  def eval(e: Expr): Int = {
    val evaluator = new Evaluator
    e.accept(evaluator)
    evaluator.acc
  }

  val expr: Expr = new Sum(new Mul(new Num(1), new Num(2)), new Num(3))

  println(eval(expr))

}
