package selfInterpretation

object Interpreter {
    def intp(r: Rep): Rep = r match {
        // Reification transforms programs into data
        case `@`(Reif, x) => `@`(Reif, x)
        case `@`(x, y) => apply(`@`(intp(x), intp(y)))
        case _ => r
    }

    def apply(r: Rep): Rep = r match {
        case `@`(Succ, x) => `@`(Succ, x)
        case `@`(`@`(`@`(Cond, Zero), c), f) => 
            intp(c)
        case `@`(`@`(`@`(Cond, `@`(Succ, x)), c), f) =>
            intp(`@`(f, x))
        case `@`(`@`(`@`(S, f), g), x) => intp(`@`(`@`(f, x), `@`(g, x)))
        case `@`(`@`(K, x), y) => intp(x)
        case `@`(Y, f) => intp(`@`(f, `@`(Y, f)))
        case `@`(Eval, `@`(Reif, x)) => `@`(Reif, intp(x))
        // Reflection transforms data (Representation of programs) into programs 
        case `@`(Refl, `@`(Reif, x)) => intp(x)
        case _ => r
    }
}