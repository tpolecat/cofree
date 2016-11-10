
import doobie.imports._
import scalaz._, Scalaz._
import shapeless._
import shapeless.nat._
import shapeless.ops.nat._

/**
 * Witness for bounded corecursion. Given a seed and coalgebra unfold `N` levels of `F`, where `N`
 * is a type-level natural number.
 */
trait Corecur[F[_], N <: Nat] {
  type Out[A] // F[A] when N =:= _1, F[F[A]] when N =:= _2, etc.
  def unfold[A](a: A)(f: A => F[A]): Out[A]
}

object Corecur {

  type Aux[F[_], N <: Nat, A0[_]] =
    Corecur[F, N] {
      type Out[A] = A0[A]
    }

  // Base case. We can always unfold one level (there is no _0 case).
  implicit def CorecurBase[F[_]]: Corecur.Aux[F, _1, F] =
    new Corecur[F, _1] {
      type Out[A] = F[A]
      def unfold[A](a: A)(f: A => F[A]): Out[A] = f(a)
    }

  // Inductive case. F must be a functor. If we already know how to unfold P levels and P is the
  // predecessor of N (i.e. N = P + 1), then we can unfold N levels.
  implicit def CorecurInductive[F[_]: Functor, N <: Nat, P <: Nat](
    implicit p: Pred.Aux[N, P],
             c: Corecur[F, P]
  ) : Corecur.Aux[F, N, λ[α => F[c.Out[α]]]] =
    new Corecur[F, N] {
      type Out[A] = F[c.Out[A]]
      def unfold[A](a: A)(f: A => F[A]): Out[A] = f(a).map(c.unfold(_)(f))
    }

}


//
// -------
//


/**
 * Witness for bounded Kleisli corecursion. This is the monadic version of Corecur. Here F must be
 * a traversable functor.
 */
trait CorecurM[F[_], N <: Nat] {
  type Out[A] // F[A] where N =:= _1, F[F[A]] where N =:= _2, etc.
  def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]): M[Out[A]]
}

object CorecurM {

  type Aux[F[_], N <: Nat, A0[_]] =
    CorecurM[F, N] {
      type Out[A] = A0[A]
    }

  implicit def CorecurMBase[F[_]]: CorecurM.Aux[F, _1, F] =
    new CorecurM[F, _1] {
      type Out[A] = F[A]
      def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]) = f(a)
    }

  // Here F must be a traversable functor.
  implicit def CorecurMInductive[F[_]: Traverse, N <: Nat, P <: Nat](
    implicit p: Pred.Aux[N, P],
             c: CorecurM[F, P]
    ) : CorecurM.Aux[F, N, λ[α => F[c.Out[α]]]] =
      new CorecurM[F, N] {
        type Out[A] = F[c.Out[A]]
        def unfoldM[M[_]: Monad, A](a: A)(f: A => M[F[A]]) = f(a).flatMap(_.traverse(c.unfoldM(_)(f)))
      }

}
