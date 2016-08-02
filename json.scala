// import matryoshka._
// import shapeless._
// import shapeless.ops.coproduct.Inject
// import atto._, Atto._
// import scalaz.Traverse, scalaz.syntax.applicative._

// object json extends Extras {

//   type JsonF[A] = 
//     Unit :+: Boolean :+: String :+: Double :+: List[A] :+: List[(String, A)] :+: CNil

//   implicit def FunctorJsonF: Traverse[JsonF] = null

//   // lift a coproduct element
//   def lift[T[_[_]], A](a: A)(
//     implicit cr: Corecursive[T], 
//              in: Inject[JsonF[T[JsonF]], A]
//   ): T[JsonF] =
//     cr.embed(Coproduct(a))

//   // a parser

//   def charToken(c: Char) = token(char(c))
//   val comma = charToken(',')
//   val colon = charToken(':')

//   def pair[T[_[_]]: Corecursive]: Parser[(String, T[JsonF])] = 
//     (token(stringLiteral) <* colon) ~ jexpr[T]

//   def jexpr[T[_[_]]: Corecursive]: Parser[T[JsonF]] =
//     token(stringLiteral)                   -| lift[T,String]                   |
//     braces(sepBy(pair, colon))             -| lift[T,List[(String, T[JsonF])]] |
//     squareBrackets(sepBy(jexpr[T], comma)) -| lift[T,List[T[JsonF]]]           |
//     token(double)                          -| lift[T,Double]                   |
//     token(string("null"))                  >| lift[T,Unit](())                 |
//     token(string("true"))                  >| lift[T,Boolean](true)            |
//     token(string("false"))                 >| lift[T,Boolean](false)

// }

