package selfInterpretation

// todo: Fix it. At the moment ONLY COMBINATORS USING Rep WORK.
class Expr[T](val rep: Rep, val v: T)

object Expr {
    def eval[T](a: Expr[T]): Expr[T] = {
        new Expr[T](Interpreter.intp(a.rep), a.v)
    }

    def reflect[T](a: Expr[T]): T = a.v
    
    def `&`[T, K](a: Expr[T => K], b: Expr[T]): Expr[K] = {
        new Expr[K](`@`(a.rep, b.rep), a.v.apply(b.v))
    }

    // def reify[T](t: T): Expr[T] = ???

    val zero = new Expr(Zero, 0)
    val succ = new Expr(Succ, (x: Int) => x+1)
    val cond = new Expr(Cond, (x: Int, c: Any, f: (Int => Any)) => if (x == 0) c else f(x-1))
    val s = new Expr(S, (f: Any => Any => Any, g: Any => Any, x: Any) => f(x)(g(x)))
    val k = new Expr(K, (x: Any, y: Any) => x)
    val evl = new Expr(Eval, eval _)
    val reif = new Expr(Reif, reify _)
    val refl = new Expr(Refl, reflect _)

    implicit def reify(t: Int): Expr[Int] = t match {
        case 0 => zero
        case x => `&`(succ, reify(x-1)) 
    }

    //zt does not compile
    // val plus = `&`(`&`(s, `&`(k, s)), k)
}

object ExprTest extends App {
    import Expr._

    // println(eval(`&`(`&`(plus, reify(2)), reify(1))))
}