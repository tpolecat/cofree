import atto._, Atto._
import scalaz._, Scalaz.{ char => _, _ }, scalaz.effect._
// import matryoshka._
// import matryoshka.Recursive.ops._
// import matryoshka.Corecursive.ops._
import doobie.imports._

object cofree extends Extras with SafeApp {

  case class Fix[F[_]](unfix: F[Fix[F]]) {
    def toCofree(implicit ev: Functor[F]): Cofree[F, Unit] =
      Cofree((), unfix.map(_.toCofree))
  }

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


  case class ProfF[A](name: String, uni: String, year: Int, students: List[A]) {
    override def toString = s"ProfF($name, $uni, $year, «${students.length}»)"
  }
  object ProfF {
    implicit val ProfFTraverse: Traverse[ProfF] =
      new Traverse[ProfF] {
        def traverseImpl[G[_]: Applicative, A, B](fa: ProfF[A])(f: A => G[B]): G[ProfF[B]] =
          fa.students.traverse(f).map(ss => fa.copy(students = ss))
      }
  }

  ///
  /// PARSING
  ///

  val ctString: Parser[String] =
    takeWhile(_ != ',') <* token(char(','))

  def person(n: Int): Parser[Fix[ProfF]] =
    for {
      _    <- char(' ').replicateM(n)
      name <- ctString
      uni  <- ctString
      year <- int <* char('\n')
      ss   <- many(person(n + 2))
    } yield Fix(ProfF(name, uni, year, ss))

  def personPos(n: Int): Parser[Cofree[ProfF, (Int, Int)]] =
    for {
      p0   <- pos
      _    <- char(' ').replicateM(n)
      name <- ctString
      uni  <- ctString
      year <- int <* char('\n')
      ss   <- many(personPos(n + 2))
      p1   <- pos
    } yield Cofree((p0, p1), ProfF(name, uni, year, ss))

  ///
  /// DB OPERATIONS
  ///

  /** Create our schema. */
  def create: ConnectionIO[Unit] =
    sql"""

      CREATE TABLE prof (
        id     INTEGER IDENTITY,
        parent INTEGER     NULL,
        name   VARCHAR NOT NULL,
        uni    VARCHAR NOT NULL,
        year   INTEGER NOT NULL,
        FOREIGN KEY(parent) REFERENCES prof(id)        
      );

      CREATE TABLE closure (
        ancestor   INTEGER NOT NULL,
        descendant INTEGER NOT NULL,
        distance   INTEGER NOT NULL,
        FOREIGN KEY(ancestor) REFERENCES prof(id),      
        FOREIGN KEY(descendant) REFERENCES prof(id)   
      )

    """.update.run.void

  /** Insert a person with the given parent, disregarding children. */
  def insFlat(parent: Option[Int], p: ProfF[_]): ConnectionIO[Int] =
    sql"""
      INSERT INTO prof (parent, name, uni, year)
      VALUES ($parent, ${p.name}, ${p.uni}, ${p.year})
    """.update.withUniqueGeneratedKeys("id")

  def insFlat(parent: Int, p: ProfF[_]): ConnectionIO[Int] =
    insFlat(Some(parent), p)


  def insert(p: Cofree[ProfF, _]): ConnectionIO[Cofree[ProfF, Int]] =
    for {
      h <- insFlat(None, p.tail)
      t <- p.tail.traverseU(extendPM(_)(h) { (h, cf) => 
             insFlat(Some(h), cf.tail)
           })
    } yield Cofree(h, t)


  // /** Construct a program to insert a tree, yielding a tree annotated with primary keys. */
  // def ins[A](p: Cofree[ProfF, A]): ConnectionIO[Cofree[ProfF, Int]] =
  //   for {
  //     h <- insFlat(None, p.tail)
  //     t <- p.tail.traverseU(cf => insert[Cofree[?[_], A]](h, cf))
  //   } yield Cofree(h, t)

  // def insert[T[_[_]]: Recursive](parent: Int, p: T[ProfF]): ConnectionIO[Cofree[ProfF, Int]] =
  //   p.attributeTopDownM(parent)(insFlat)


  /** Read a person with the given id, yielding a person that references its children by id. */
  def readFlat(id: Int): ConnectionIO[ProfF[Int]] =
    for {
      data <- sql"SELECT name, uni, year FROM prof WHERE id = $id".query[(String, String, Int)].unique
      kids <- sql"SELECT id FROM prof WHERE parent = $id".query[Int].list
    } yield ProfF(data._1, data._2, data._3, kids)

  /** Read a primary-key-annotated tree. */
  def read(id: Int): ConnectionIO[Cofree[ProfF, Int]] =
     unfoldCM(id)(readFlat)

  // /** Construct a program to insert a closure. */
  // def insertC[F[_]: Foldable](cs: F[(Int, Int, Int)]): ConnectionIO[Int] = {    
  //   val sql = "INSERT INTO closure (ancestor, descendant, distance) VALUES (?, ?, ?)"
  //   Update[(Int, Int, Int)](sql).updateMany(cs)
  // }

  // /** Read a tree in one query using the closure. */
  // def readC(id: Int): ConnectionIO[Cofree[ProfF, Int]] =
  //   sql"""
  //     SELECT p.id, p.name, p.uni, p.year, p.parent 
  //     FROM prof p
  //     JOIN closure c ON p.id = c.descendant AND c.ancestor = $id
  //     ORDER BY distance ASC
  //   """.query[(Int, String, String, Int, Option[Int])]
  //      .list
  //      .map { rows =>
  //         def flat(n: Int): Option[ProfF[Int]] =
  //           rows.find(_._1 == n).map { case (id, name, uni, year, parent) =>
  //             ProfF(name, uni, year, rows.filter(_._5 == Some(id)).map(_._1))
  //           }
  //         attributeAna(id)(flat(_).get)
  //      }


  ///
  /// Entry Point
  ///

  override def runc: IO[Unit] = {

    val xa = DriverManagerTransactor[IO]("org.h2.Driver", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")
    val p: Fix[ProfF] = (person(0) <~ endOfInput).parseOnly(data).option.get

    val p0 = p.toCofree


    // // various ways to get to cofree
    // val p0 = Recursive[Fix].cata[ProfF, Cofree[ProfF, Unit]](p)(Cofree((), _))
    // val p1 = p.cata[Cofree[ProfF, Unit]](Cofree((), _))
    // val p2 = p.convertTo[Cofree[?[_], Unit]]

    val action = for {

      // Set up the db
      _ <- create

      // insert and read back
      t <- insert(p0)
      _ <- draw(t)
      t <- read(t.head)
      _ <- draw(t)

    } yield ()

    // our IO program
    action.transact(xa) 

  }

}


