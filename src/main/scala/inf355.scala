package net.rfc1149.inf355

// CATAMORPHISME

trait Expr {
  protected def accept[A](visitor: Visitor[A]): A
  def fold[A](
    ifNum: Num => A,
    ifSum: Sum => A,
    ifMul: Mul => A): A = {
    this.accept(new Visitor[A] {
      def visit(num: Num): A = ifNum(num)
      def visit(mul: Mul): A = ifMul(mul)
      def visit(sum: Sum): A = ifSum(sum)
    })
  }
}

trait Visitor[A] {
  def visit(num: Num): A
  def visit(mul: Mul): A
  def visit(sum: Sum): A
}

class Num(val i: Int) extends Expr {
  protected def accept[A](visitor: Visitor[A]) = visitor.visit(this)
}

class Mul(val l: Expr, val r: Expr) extends Expr {
  protected def accept[A](visitor: Visitor[A]) = visitor.visit(this)
}

class Sum(val l: Expr, val r: Expr) extends Expr {
  protected def accept[A](visitor: Visitor[A]) = visitor.visit(this)
}

object Main extends App {

  def eval(e: Expr): Int = e.fold(
    num => num.i,
    sum => eval(sum.l) + eval(sum.r),
    mul => eval(mul.l) * eval(mul.r)
  )

  val expr: Expr = new Sum(new Mul(new Num(1), new Num(2)), new Num(3))

  println(eval(expr))

}
