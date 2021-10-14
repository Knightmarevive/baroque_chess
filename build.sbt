name := "baroque_chess"

version := "0.2"

scalaVersion := "2.13.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0"
// libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.9"
libraryDependencies += "it.unimi.dsi" % "dsiutils" % "2.6.3"
// libraryDependencies += "it.unimi.dsi" % "fastutil-core" % "8.5.6"

run / connectInput := true

