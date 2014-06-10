package net.rfc1149.inf355

class Num(val i: Int)

class Mul(val l: Any, val r: Any)

class Sum(val l: Any, val r: Any)

object Main extends App {

  val expr = new Sum(new Mul(new Num(1), new Num(2)), new Num(3))

  def eval(e: Any): Int = e match {
    case num: Num => num.i
    case mul: Mul => eval(mul.l) * eval(mul.r)
    case sum: Sum => eval(sum.l) + eval(sum.r)
  }

  println(eval(expr))

}
