import atto._, Atto._, atto.compat.scalaz._
import scalaz._, Scalaz.{ char => _, _ }, scalaz.effect._

import doobie.imports._
import doobie.contrib.postgresql.pgtypes._

import doobie.tsql._
import doobie.tsql.postgres._

/** Companion code for the talk "Fun and Games with Fix, Cofree, and Doobie". */
object cofree extends Extras with SafeApp {

  /** Fixpoint type. */
  case class Fix[F[_]](unfix: F[Fix[F]])

  /** A data type for professors and their Ph.D. students. */
  case class ProfF[A](name: String, uni: String, year: Int, students: List[A]) {
    override def toString = s"ProfF($name, $uni, $year, «${students.length}»)"
  }
  object ProfF {

    // It is a traversable functor. For more on applicative and traversable functors see the
    // original paper by Conor McBride (http://strictlypositive.org/Idiom.pdf) and Scala examples
    // in the Cats documentation (http://typelevel.org/cats/tut/traverse.html).
    implicit val ProfFTraverse: Traverse[ProfF] =
      new Traverse[ProfF] {
        def traverseImpl[G[_]: Applicative, A, B](fa: ProfF[A])(f: A => G[B]): G[ProfF[B]] =
          fa.students.map(f).sequence.map(ss => fa.copy(students = ss))
      }

  }

  // Some test data. The hierarchy is represented with indentation.
  val data =
    """|Simeon Denis Poisson, École Polytechnique, 1800
       |  Gustav Peter Lejeune Dirichlet, Rheinische Friedrich-Wilhelms-Universität Bonn, 1827
       |    Rudolf Otto Sigismund Lipschitz, Universität Berlin, 1853
       |      C. Felix (Christian) Klein, Rheinische Friedrich-Wilhelms-Universität Bonn, 1868
       |        William Edward Story, Universität Leipzig, 1875
       |          Solomon Lefschetz, Clark University, 1911
       |            Albert William Tucker, Princeton University, 1932
       |              Marvin Lee Minsky, Princeton University, 1954
       |                Gerald Jay Sussman, Massachusetts Institute of Technology, 1973
       |                  Guy Lewis Steele, Massachusetts Institute of Technology, 1980
       |                    Philip Lee Wadler, Carnegie Mellon University, 1984
       |        C. L. Ferdinand (Carl Louis) Lindemann, Friedrich-Alexander-Universität Erlangen-Nürnberg, 1873
       |          David Hilbert, Universität Königsberg, 1885
       |            Wilhelm Ackermann, Georg-August-Universität Göttingen, 1925
       |            Haskell Curry, Georg-August-Universität Göttingen, 1930
       |            Hermann Weyl, Georg-August-Universität Göttingen, 1908
       |              Saunders Mac Lane, Georg-August-Universität Göttingen, 1934
       |                Steven Awodey, The University of Chicago, 1997
       |                William Howard, The University of Chicago, 1956
       |  Michel Chasles, École Polytechnique, 1814
       |    H. A. (Hubert Anson) Newton, Yale University, 1850
       |      E. H. (Eliakim Hastings) Moore, Yale University, 1885
       |        Oswald Veblen, The University of Chicago, 1903
       |          Alonzo Church, Princeton University, 1927
       |            Alan Mathison Turing, Princeton University, 1938
       |            Stephen Cole Kleene, Princeton University, 1934
       |              Robert Lee Constable, University of Wisconsin-Madison, 1968
       |                Robert William Harper, Cornell University, 1985
       |                  Benjamin Crawford Pierce, Carnegie Mellon University, 1991
       |""".stripMargin

  ///
  /// PARSING
  ///
  /// These parsers are written using atto (https://github.com/tpolecat/atto) would look similar
  /// with any monadic parsing library.
  ///

  /** Parser for a comma-terminated string. */
  val ctString: Parser[String] =
    takeWhile(_ != ',') <* token(char(','))

  /** Parser for un-annotated ProfF. */
  def prof(n: Int): Parser[Fix[ProfF]] =
    for {
      _    <- char(' ').replicateM(n)
      name <- ctString
      uni  <- ctString
      year <- int <* char('\n')
      ss   <- many(prof(n + 2))
    } yield Fix(ProfF(name, uni, year, ss))

  /** Parser for parse-position-annotated ProfF. */
  def posProf(n: Int): Parser[Cofree[ProfF, (Int, Int)]] =
    for {
      p0   <- pos
      _    <- char(' ').replicateM(n)
      name <- ctString
      uni  <- ctString
      year <- int <* char('\n')
      ss   <- many(posProf(n + 2))
      p1   <- pos
    } yield Cofree((p0, p1), ProfF(name, uni, year, ss))

  ///
  /// INSERT
  ///

  /** Insert a node with the given parent, disregarding children. */
  def insertNode(parent: Option[Int], p: ProfF[_]): ConnectionIO[Int] =
    sql"""
      INSERT INTO prof (parent, name, uni, year)
      VALUES ($parent, ${p.name}, ${p.uni}, ${p.year})
    """.update.withUniqueGeneratedKeys("id") // TODO: can't write option with tsql :-\

  /** Insert a tree rooted at `p` with an optional parent. */
  def insertTree(fp: Fix[ProfF], parent: Option[Int] = None): ConnectionIO[Cofree[ProfF, Int]] =
    for {
      h <- insertNode(parent, fp.unfix)
      t <- fp.unfix.traverse(insertTree(_, Some(h)))
    } yield Cofree(h, t)

  ///
  /// READ
  ///

  /** Read a ProfF with the given id, yielding a ProfF that references its children by id. */
  def readFlat(id: Int): ConnectionIO[ProfF[Int]] =
    for {
      data <- tsql"SELECT name, uni, year FROM prof WHERE id = $id".unique[(String, String, Int)]
      kids <- tsql"SELECT id FROM prof WHERE parent = $id".as[List[Int]]
    } yield ProfF(data._1, data._2, data._3, kids)

  /** Monadic cofree corecursion. */
  def unfoldCM[M[_]: Monad, F[_]: Traverse, A](id: A)(f: A => M[F[A]]): M[Cofree[F, A]] =
    f(id).flatMap(_.traverse(unfoldCM(_)(f)).map(Cofree(id, _)))

  /** Read an id-annotated tree. */
  def read(id: Int): ConnectionIO[Cofree[ProfF, Int]] =
    unfoldCM(id)(readFlat)

  ///
  /// READ AGAIN, with fancy SQL
  ///

  /** And the non-monadic version, for reference. */
  def unfoldC[F[_]: Functor, A](id: A)(f: A => F[A]): Cofree[F, A] =
    Cofree(id, f(id).map(unfoldC(_)(f)))

  def read2(id: Int): ConnectionIO[Cofree[ProfF, Int]] =
    tsql"""
      WITH RECURSIVE rec(id, parent, name, uni, year, students) AS(
       SELECT * FROM prof_closure WHERE id = $id
       UNION ALL SELECT p.* FROM prof_closure p, rec r
        WHERE r.id = p.parent
      ) SELECT id, name, uni, year, students
        FROM rec;
    """.as[Int => ProfF[Int]].map(unfoldC(id)(_))

  ///
  /// ENTRY POINT
  ///

  /** A transactor abstracts over connection pools and io-effect types. */
  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver",// driver
    "jdbc:postgresql:prof", // in-memory database
    "postgres", ""          // user, password
  )

  /** Our main method, via SafeApp */
  override def runc: IO[Unit] = {

    // Parse the data above
    val p: Fix[ProfF] =
      (prof(0) <~ endOfInput).parseOnly(data).option.get // yolo

    // Our database program
    val action: ConnectionIO[Unit] =
      for {

        // insert and draw
        t <- insertTree(p)
        // _ <- draw(t)

        // Read back and draw
        t <- read(t.head)
        // _ <- draw(t)

        // Again
        t <- read2(t.head)
        _ <- draw(t)

      } yield ()

    // our IO program
    action.transact(xa)

  }



}
