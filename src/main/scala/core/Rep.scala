package selfInterpretation

// Representation of SK terms
// An AST node represents either a combinator or the application
sealed trait Rep

sealed trait Number extends Rep
sealed trait BasicCombinator extends Rep
sealed trait ReflectionCombinator extends Rep
// left infix
case class `@`(a: Rep, b: Rep) extends Rep

case object Zero extends Number
case object Succ extends Number
case object Cond extends Number

case object S extends BasicCombinator
case object K extends BasicCombinator
case object Y extends BasicCombinator

// Metafunctions for metacircularity
case object Eval extends ReflectionCombinator
case object Reif extends ReflectionCombinator
case object Refl extends ReflectionCombinator