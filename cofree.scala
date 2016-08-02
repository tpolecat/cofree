import atto._, Atto._
import scalaz._, Scalaz.{ char => _, _ }, scalaz.effect._
// import matryoshka._
// import matryoshka.Recursive.ops._
// import matryoshka.Corecursive.ops._
import doobie.imports._

/*

Ok so here are my goals:
1. Help you gain an intuition about some simple recursive types. Which will help you survive the
   next talk, about Matryoshka, which explores these ideas in a much more general way. For this talk
   I'm going to generalize a little bit but for the most part I'm going to keep things concrete
   enough that you can still get your hands on them.
2. Help you gain some confidence pushing on your types to see what happens. Beginners in particular
   because things like higher-kinded types and equational reasoning just aren't ideas that you have
   in your toolkit when your're starting out with functional programming. So I'll have a few 
   opportunities to do that.
3. Show you how boring doobie is. Which is good. Doobie is a pure functional database library that
   I work on, and we're going to do some database programming without ever really thinking about database 
   programming. Wr're going to treat database programs like any other data type and at some point
   you'll probably forget that we're doing database programming.


===

- introduce the problem
- derive fix and cofree
- show some other examples:
  - Fix[λ[a => (A, F[a])]] ~ Cofree[F, A]
  - Fix[λ[a => A \/ F[a]]] ~ Free[F, A]


 */

object cofree extends Extras with SafeApp {

  case class Fix[F[_]](unfix: F[Fix[F]])

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

    // traversable functors:
    // you have map like normal functors, but also .sequence which says
    // F[G[A]] => G[F[A]]
    // and map(f).sequence is .traverse(f)
    // also fa.sequence = fa.map(identity).sequence = fa.traverse(identity)
    // so this is a nice identity that you can find by equational reasoning; it must be true if the
    // first is true

    implicit val ProfFTraverse: Traverse[ProfF] =
      new Traverse[ProfF] {
        def traverseImpl[G[_]: Applicative, A, B](fa: ProfF[A])(f: A => G[B]): G[ProfF[B]] =
          fa.students    // List[A]
            .map(f)      // List[G[B]]
            .sequence    // G[List[B]] "we can do this because List is traversable"
            .map { ss => // we can map because G is a functor
              fa.copy(students = ss) // ss is a List[B], so this is a ProfF[B]
            }            // G[ProfF[B]] which is what we want
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


  /** Insert a node with the given parent, disregarding children. */
  def insertNode(parent: Option[Int], p: ProfF[_]): ConnectionIO[Int] =
    sql"""
      INSERT INTO prof (parent, name, uni, year)
      VALUES ($parent, ${p.name}, ${p.uni}, ${p.year})
    """.update.withUniqueGeneratedKeys("id")

  /** Insert a tree rooted at `p` with an optional parent. */
  def insertTree(fp: Fix[ProfF], parent: Option[Int] = None): ConnectionIO[Cofree[ProfF, Int]] =
    for {

      h <- insertNode(parent, fp.unfix)   // ConnectionIO[Int] "program that inserts the head and returns a new id"

      t <- fp.unfix                       // ProfF[Fix[ProfF]] "children are trees"
             .map(insertTree(_, Some(h))) // ProfF[ConnectionIO[Cofree[ProfF, Int]]] "ProfF whose children are programs that compute annotated trees"
             .sequence                    // ConnectionIO[ProfF[Cofree[ProfF, Int]]] "program that computes a ProfF whose children are annoted trees"
               // ^^ so this is why we needed to define a traversable functor
    } yield Cofree(h, t) 


  // so, notice that nothing we have done here particularly cares that we're using ProfF and ConnectionIO ...
  // it can just be any monad and any applicative functor, but I'm not going to generalize it for you; this
  // is the kind of thing matryoshka does and Greg will be talking about that stuff in a few minutes. I will
  // say that the option makes things less pretty than you might like … if the trees were infinite this would
  // be a little prettier.



  /** Read a person with the given id, yielding a person that references its children by id. */
  def readFlat(id: Int): ConnectionIO[ProfF[Int]] =
    for {
      data <- sql"SELECT name, uni, year FROM prof WHERE id = $id".query[(String, String, Int)].unique
      kids <- sql"SELECT id FROM prof WHERE parent = $id".query[Int].list
    } yield ProfF(data._1, data._2, data._3, kids)


  def read2(id: Int): ConnectionIO[Cofree[ProfF, Int]] =
    readFlat(id) // ConnectionIO[ProfF[Int]]
      .flatMap { pi =>
        pi.map(read2)         // ProfF[ConnectionIO[Cofree[ProfF, Int]]]
          .sequence          // ConnectionIO[ProfF[Cofree[ProfF, Int]]]
          .map(Cofree(id, _)) // ConnectionIO[Cofree[ProfF, Int]]
      }

  // and i will generalize this one because it's a little easier
  object hidden {

    def read3(id: Int)(f: Int => ConnectionIO[ProfF[Int]]): ConnectionIO[Cofree[ProfF, Int]] =
      f(id) // ConnectionIO[ProfF[Int]]
        .flatMap { pi =>
          pi.map(read3(_)(f))         // ProfF[ConnectionIO[Cofree[ProfF, Int]]]
            .sequence           // ConnectionIO[ProfF[Cofree[ProfF, Int]]]
            .map(Cofree(id, _)) // ConnectionIO[Cofree[ProfF, Int]]
        }

    def read4[M[_]: Monad](id: Int)(f: Int => M[ProfF[Int]]): M[Cofree[ProfF, Int]] =
      f(id) // M[ProfF[Int]]
        .flatMap { pi =>
          pi.map(read4(_)(f))         // ProfF[M[Cofree[ProfF, Int]]]
            .sequence           // M[ProfF[Cofree[ProfF, Int]]]
            .map(Cofree(id, _)) // M[Cofree[ProfF, Int]]
        }


    def read5[M[_]: Monad, F[_]: Traverse](id: Int)(f: Int => M[F[Int]]): M[Cofree[F, Int]] =
      f(id) // M[F[Int]]
        .flatMap { pi =>
          pi.map(read5(_)(f))   // F[M[Cofree[F, Int]]]
            .sequence           // M[F[Cofree[F, Int]]]
            .map(Cofree(id, _)) // M[Cofree[F, Int]]
        }


    def read6[M[_]: Monad, F[_]: Traverse, A](id: A)(f: A => M[F[A]]): M[Cofree[F, A]] =
      f(id) // M[F[A]]
        .flatMap { pi =>
          pi.traverse(read6(_)(f))  // M[F[Cofree[F, A]]]
            .map(Cofree(id, _))     // M[Cofree[F, A]]
        }

    def unfoldCM[M[_]: Monad, F[_]: Traverse, A](id: A)(f: A => M[F[A]]): M[Cofree[F, A]] =
      f(id).flatMap(_.traverse(unfoldCM(_)(f)).map(Cofree(id, _)))

    /** Read a primary-key-annotated tree. */
    def read(id: Int): ConnectionIO[Cofree[ProfF, Int]] =
      unfoldCM(id)(readFlat)

    // Ok and notice that none of this stuff cares at all that we're doing database programming.
    // There are no connections or resulsets or connection pools or threading or anything like that.
    // ConnectionIO is like any other monad; it's not special at all. It's just a value. It's just
    // data. So I think the beautiful thing about pure functional programing with well-understood
    // abstractions is that it lets you forget about all this extraneous crap and just focus on the
    // code in front of you.

  }


  ///
  /// Entry Point
  ///

  override def runc: IO[Unit] = {

    val xa = DriverManagerTransactor[IO]("org.h2.Driver", "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")
    val p: Fix[ProfF] = (person(0) <~ endOfInput).parseOnly(data).option.get

    val action = for {

      // Set up the db
      _ <- create

      // insert and read back
      t <- insertTree(p)
      _ <- draw(t)
      t <- read2(t.head)
      _ <- draw(t)

    } yield ()

    // our IO program
    action.transact(xa) 

  }

}


