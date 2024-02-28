package compileTimeSKI

// Source: https://michid.wordpress.com/2010/01/29/scala-type-level-encoding-of-the-ski-calculus/
trait Term {
    // Apply the term to a parameter
    type ap[x <: Term] <: Term
    // Simplify the term into canonical term
    type eval <: Term
}

// I combinator
trait I extends Term {
    type ap[x <: Term] = I1[x]
    type eval = I
}

trait I1[x <: Term] extends Term {
    type ap[y <: Term] = eval#ap[y]
    type eval = x#eval
}

// The K combinator
trait K extends Term {
  type ap[x <: Term] = K1[x]
  type eval = K
}

trait K1[x <: Term] extends Term {
  type ap[y <: Term] = K2[x, y]
  type eval = K1[x]
}

trait K2[x <: Term, y <: Term] extends Term {
  type ap[z <: Term] = eval#ap[z]
  type eval = x#eval
}

// The S combinator
trait S extends Term {
  type ap[x <: Term] = S1[x]
  type eval = S
}

trait S1[x <: Term] extends Term {
  type ap[y <: Term] = S2[x, y]
  type eval = S1[x]
}

trait S2[x <: Term, y <: Term] extends Term {
  type ap[z <: Term] = S3[x, y, z]
  type eval = S2[x, y]
}

trait S3[x <: Term, y <: Term, z <: Term] extends Term {
  type ap[v <: Term] = eval#ap[v]
  type eval = x#ap[z]#ap[y#ap[z]]#eval
}


// Constants
trait c extends Term {
  type ap[x <: Term] = c
  type eval = c
}
trait d extends Term {
  type ap[x <: Term] = d
  type eval = d
}
trait e extends Term {
  type ap[x <: Term] = e
  type eval = e
}

case class Equals[A >: B <:B , B]()

object TypeLevelEncodingCheck extends App{
    // Compile-time error
    // Equals[String, Int]

    // Ic -> c
    Equals[I#ap[c]#eval, c]
    
    // Kcd -> c
    Equals[K#ap[c]#ap[d]#eval, c]
    
    // KKcde -> d
    Equals[K#ap[K]#ap[c]#ap[d]#ap[e]#eval, d]
    
    // SIIIc -> Ic
    Equals[S#ap[I]#ap[I]#ap[I]#ap[c]#eval, c]
    
    // SKKc -> Ic
    Equals[S#ap[K]#ap[K]#ap[c]#eval, c]
    
    // SIIKc -> KKc
    Equals[S#ap[I]#ap[I]#ap[K]#ap[c]#eval, K#ap[K]#ap[c]#eval]
    
    // SIKKc -> K(KK)c
    Equals[S#ap[I]#ap[K]#ap[K]#ap[c]#eval, K#ap[K#ap[K]]#ap[c]#eval]
    
    // SIKIc -> KIc
    Equals[S#ap[I]#ap[K]#ap[I]#ap[c]#eval, K#ap[I]#ap[c]#eval]
    
    // SKIc -> Ic
    Equals[S#ap[K]#ap[I]#ap[c]#eval, c]
    
    // R = S(K(SI))K  (reverse)
    type R = S#ap[K#ap[S#ap[I]]]#ap[K]
    Equals[R#ap[c]#ap[d]#eval, d#ap[c]#eval]
}
