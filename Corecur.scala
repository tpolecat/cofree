
import doobie.imports._
import scalaz._, Scalaz._
import shapeless._
import shapeless.nat._
import shapeless.ops.nat._

trait Corecur[F[_], N <: Nat] {
  type Out[A]
  def unfold[A](a: A)(f: A => F[A]): Out[A]
  def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]): M[Out[A]]
}

object Corecur {
  def apply[F[_], N <: Nat](implicit ev: Corecur[F, N]): ev.type = ev

  type Aux[F[_], N <: Nat, A0[_]] =
    Corecur[F, N] {
      type Out[A] = A0[A]
    }

  implicit def Corecur1[F[_]: Functor]: Corecur.Aux[F, _1, F] =
    new Corecur[F, _1] {
      type Out[A] = F[A]
      def unfold[A](a: A)(f: A => F[A]): Out[A] = f(a)
      def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]) = f(a)
    }

  implicit def CorecurN[F[_]: Traverse, N <: Nat, P <: Nat](
    implicit p: Pred.Aux[N, P],
             c: Corecur[F, P]
  ) : Corecur.Aux[F, N, λ[α => F[c.Out[α]]]] =
    new Corecur[F, N] {
      type Out[A] = F[c.Out[A]]
      def unfold[A](a: A)(f: A => F[A]): Out[A] = f(a).map(c.unfold(_)(f))
      def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]) = f(a).flatMap(_.traverse(c.unfoldM(_)(f)))
    }

}
