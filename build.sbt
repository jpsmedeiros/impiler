name := "impiler-scala"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
                            "org.parboiled" %% "parboiled" % "2.1.5",
                            "org.scalatest" %% "scalatest" % "3.0.5" % "test"
                           )