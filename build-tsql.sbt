// Connection information for doobie-tsql
scalacOptions ++= Seq(
  "-Xmacro-settings:doobie.driver=org.postgresql.Driver",
  "-Xmacro-settings:doobie.connect=jdbc:postgresql:prof",
  "-Xmacro-settings:doobie.user=postgres",
  "-Xmacro-settings:doobie.password="
)
