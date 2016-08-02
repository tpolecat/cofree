
import scalaz.{ Free => _, Cofree => _, _}, Scalaz._

object freex extends App {

  case class Fix[F[_]](unfix: F[Fix[F]])

  type Free[F[_], A] = Fix[λ[a => A \/ F[a]]]

  def free[F[_], A](a: A \/ F[Free[F, A]]): Free[F, A] = 
    Fix[λ[a => A \/ F[a]]](a)

  def pure[F[_], A](a: A): Free[F, A] = free(-\/(a))
  def roll[F[_], A](fa: F[Free[F,A]]): Free[F, A] = free(\/-(fa))

  // nested list
  def int(a: Int): Free[List, Int] = pure(a)
  def lst(as: Free[List, Int]*): Free[List, Int] = roll(as.toList)

  val data: Free[List, Int] = lst(int(1), lst(int(2), int(3)), int(4))

  implicit def freeMonad[F[_]: Functor]: Monad[Free[F, ?]] =
    new Monad[Free[F, ?]] {
      def point[A](a: => A): Free[F, A] = freex.pure(a)
      def bind[A, B](ffa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        ffa.unfix match {
          case -\/(a)  => f(a)
          case \/-(fffa) => roll(fffa.map(bind(_)(f)))
        }
    }

  def foldMap[M[_]: Monad, F[_], A](ffa: Free[F, A])(xf: F ~> M): M[A] =
    ffa.unfix match {
      case -\/(a)    => a.point[M]
      case \/-(fffa) => xf(fffa).flatMap(foldMap(_)(xf))
    }

  val prog = 
    for {
      a <- data
      b <- data
    } yield (a, b)

  println(foldMap(prog)(NaturalTransformation.refl))

}

object cofreex extends App {

  case class Fix[F[_]](unfix: F[Fix[F]])

  type Cofree[F[_], A] = Fix[λ[a => (A, F[a])]]


}