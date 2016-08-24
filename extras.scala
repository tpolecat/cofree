import atto._, Atto._
import doobie.imports._
import scalaz._, Scalaz._

/** Some helpers that would be distracting if they were in the main class :-\ */
trait Extras {

  /** Dump a Cofree as a tree. */
  def draw[F[_]: Traverse, A](fa: Cofree[F, A], indent: Int = 0): ConnectionIO[Unit] = 
    for {
      _ <- HC.delay(print(" " * indent))
      _ <- HC.delay(println(fa.head + " :< " + fa.tail))
      _ <- fa.tail.traverse(draw(_, indent + 1))
    } yield ()

  /** Parser that yields the current offset in the input. You are not expected to understand this. */
  val pos: Parser[Int] = {
    import Trambopoline._
    import Parser._
    import Parser.Internal._
    new Parser[Int] {
      override def toString = "pos"
      def apply[R](st0: State, kf: Failure[R], ks: Success[Int,R]): TResult[R] =
        suspend(ks(st0,st0.pos))
    }
  }

}

