import atto._, Atto._
import scalaz._, Scalaz.{ char => _, _ }, scalaz.effect._
import doobie.imports._
// import matryoshka._
// import matryoshka.Recursive.ops._

object Extras extends Extras

trait Extras {

  case class Fix[F[_]](unfix: F[Fix[F]])

  // // Top-down fold+unfold
  // def extendPM[M[_]: Monad, F[_]: Traverse, A, B]
  //   (cfa: Cofree[F, A])(b: B)
  //   (f: (B, Cofree[F, A]) => M[B]): M[Cofree[F, B]] =
  //     f(b, cfa).flatMap { b => 
  //       cfa.tail.traverseU(extendPM(_)(b)(f)).map(Cofree(b, _))
  //     }

  // /** Extend, with a state variable that propagates with the branching structure. */
  // def extendP[F[_]: Functor, A, B](cfa: Cofree[F, A])(z : B)(f: (B, Cofree[F, A]) => B): Cofree[F, B] = {
  //   val b = f(z, cfa)
  //   Cofree(b, cfa.tail.map(extendP(_)(b)(f)))
  // }

  // // /** Extend, with a state variable that propagates with the branching structure. */
  // // def extendPx[T[_[_]]: Recursive, F[_]: Functor, B](cfa: T[F])(z : B)(f: (B, F[T[F]]) => B): Cofree[F, B] =
  // //   cfa.attributeTopDown(z)(f)

  // // is this useful on its own?
  // def extendPA[F[_]: Functor, A, B](cfa: Cofree[F, A])(z : B)(f: (B, A) => B): Cofree[F, B] =
  //   extendP(cfa)(z) { case (b, cfa) => f(b, cfa.head) }

  // def extendPI[F[_]: Functor, A, B](cfa: Cofree[F, A])(z : B)(f: B => B): Cofree[F, (A, B)] = {
  //   Cofree((cfa.head, z), cfa.tail.map(extendPA(_)((cfa.head, z)) { case ((_, b), a) => (a, f(b)) }))
  // }

  /** Dump a Cofree as a tree ... unprincipled, sorry. */
  def draw[F[_]: Traverse, A](fa: Cofree[F, A], indent: Int = 0): ConnectionIO[Unit] = 
    for {
      _ <- HC.delay(print(" " * indent))
      _ <- HC.delay(println(fa.head + " :< " + fa.tail))
      _ <- fa.tail.traverse(draw(_, indent + 1))
    } yield ()


  /** Dump a Cofree as a tree ... unprincipled, sorry. */
  def drawFix[F[_]: Traverse](fa: Fix[F], indent: Int = 0): ConnectionIO[Unit] = 
    for {
      _ <- HC.delay(print(" " * indent))
      _ <- HC.delay(println(fa.unfix))
      _ <- fa.unfix.traverse(drawFix(_, indent + 1))
    } yield ()

  // /** Pair each annotation with its distance from the root. */
  // def zipWithDepth[F[_]: Functor, A](cf: Cofree[F, A]): Cofree[F, (A, Int)] =
  //   extendPI(cf)(0)(_ + 1)

  // /** 
  //  * Pair each annotation A with the root annotation A0 and its depth in the tree; so the final
  //  * annotation `(a0, a, n)` means that `a` is a child of `a0` at a distance of `n`.
  //  */
  // def desc[F[_]: Functor, A](cf: Cofree[F, A]): Cofree[F, (A, A, Int)] =
  //   zipWithDepth(cf).map { case (a, n) => (cf.head, a, n) }

  // /**
  //  * Constuct a closure of all parent/child relationships and their relative distances; `(a, b, n)`
  //  * means that `b` is a child of `a` at distance `n`, where `n = 0` when `a = b`.
  //  */
  // def closure[F[_]: Functor: Foldable, A](cf: Cofree[F, A]): List[(A, A, Int)] =
  //   cf.extend(desc(_)).foldMap(_.toList)

  // def unfoldCM[M[_]: Monad, F[_]: Traverse, A](a: A)(f: A => M[F[A]]): M[Cofree[F, A]] =
  //   f(a).flatMap(_.traverse(unfoldCM(_)(f)).map(Cofree(a, _)))

  /* Parser that produces the current offset in the input. */
  val pos: Parser[Int] = {
    import Trampoline._
    import Parser._
    import Parser.Internal._
    new Parser[Int] {
      override def toString = "pos"
      def apply[R](st0: State, kf: Failure[R], ks: Success[Int,R]): TResult[R] =
        suspend(ks(st0,st0.pos))
    }
  }

}

