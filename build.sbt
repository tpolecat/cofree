scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-encoding", "UTF-8", // 2 args
  "-feature",                
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",       
  "-Ywarn-dead-code",       
  "-Ywarn-value-discard"
)

libraryDependencies ++= Seq(
  "org.scalaz"   %% "scalaz-core"       % "7.1.1",
  "org.scalaz"   %% "scalaz-effect"     % "7.1.1",
  "org.tpolecat" %% "doobie-core"       % "0.2.3",
  "org.tpolecat" %% "doobie-contrib-h2" % "0.2.3",
  "org.tpolecat" %% "atto-core"         % "0.4.2",
  "com.slamdata" %% "matryoshka-core"   % "0.4.0"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
