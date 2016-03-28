name := "picopickle-playground"
scalaVersion := "2.11.8"
// scalacOptions += "-Xlog-implicits"

libraryDependencies ++= Seq(
  "io.github.netvl.picopickle" %% "picopickle-core" % "0.2.1",
  "org.scalatest" %% "scalatest" % "2.2.5" % Test
)

