
import doobie.imports._
import scalaz._, Scalaz._
import shapeless._
import shapeless.nat._
import shapeless.ops.nat._
import cofree._

object gen {

  trait Corecur[F[_], N <: Nat] {
    type Out[A]
    def unfold[A](a: A)(f: A => F[A]): Out[A]
    def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]): M[Out[A]]
    def fix[A](a: A)(f: A => F[A]): Fix[Out]
  }
  object Corecur {
    type Aux[F[_], N <: Nat, A0[_]] = Corecur[F, N] { type Out[A] = A0[A] }
    def apply[F[_], N <: Nat](implicit ev: Corecur[F, N]): ev.type = ev
  }

  implicit def CorecurZ[F[_]: Functor]: Corecur.Aux[F, _1, F] =
    new Corecur[F, _1] {
      type Out[A] = F[A]
      def unfold[A](a: A)(f: A => F[A]): Out[A] = f(a)
      def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]) = f(a)
      def fix[A](a: A)(f: A => F[A]) =
        Fix(unfold(a)(f).map(fix(_)(f)))
    }

  implicit def CorecurS[F[_]: Traverse, N <: Nat, P <: Nat](
    implicit p: Pred.Aux[N, P],
             c: Corecur[F, P]
  ) : Corecur.Aux[F, N, Lambda[a => F[c.Out[a]]]] =
    new Corecur[F, N] {
      type Out[A] = F[c.Out[A]]
      def unfold[A](a: A)(f: A => F[A]): Out[A] = f(a).map(c.unfold(_)(f))
      def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]) = f(a).flatMap(_.traverse(c.unfoldM(_)(f)))

      def fix[A](a: A)(f: A => F[A]): Fix[Out] = {
        val x: F[Fix[c.Out]] = f(a).map(a => c.fix(a)(f))
        ???
      }
    }

  def genReadFlat(id: Int, n: Nat)(
    implicit c: Corecur[ProfF, n.N]
  ): ConnectionIO[c.Out[Int]] =
    c.unfoldM(id)(readFlat)

  genReadFlat(1, 2)
}
