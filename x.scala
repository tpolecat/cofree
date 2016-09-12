import scalaz._, Scalaz._

object blah extends App {

  val data = List(
    (1, "foo", 1,   "comment1"),
    (1, "foo", 2,   "comment2"),
    (2, "bar", 5,   "other"   ),
    (2, "bar", 6,   "other2"  ),
    (3, "bar", null, null     )
  )

  case class Article(title: String, comments: List[String])

  val as = data.foldMap {
    case (n, s, _, null) => Map((n, s) -> List.empty[String])
    case (n, s, _, s2  ) => Map((n, s) -> List(s2))
  } map {
    case ((_, s), ss) => Article(s, ss)
  }

  as.foreach(println)

}
