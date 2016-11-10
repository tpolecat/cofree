import atto._, Atto._
import doobie.imports._, doobie.tsql._
import scalaz._, Scalaz._

/** Some helpers that would be distracting if they were in the main class. */
trait Extras {

  /** Draw a Cofree value to stdout. This uses universal .toString, sorry. . */
  def draw[F[_]: Traverse, A](fa: Cofree[F, A], indent: Int = 0): ConnectionIO[Unit] =
    for {
      _ <- HC.delay(print(" " * indent))
      _ <- HC.delay(println(fa.head + " :< " + fa.tail))
      _ <- fa.tail.traverse(draw(_, indent + 1))
    } yield ()

  // This is not inferable for mysterious reasons
  implicit val optionIntWrite = Write.option[JdbcType.JdbcInteger, String, Int]

}
