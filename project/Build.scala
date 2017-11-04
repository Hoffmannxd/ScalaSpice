import sbt.Keys._
import sbt._
import xerial.sbt.Pack._

object Build extends Build {

  val moduleName = "spice"
  val NamePrefix = "scala-spice"

  name := NamePrefix + "."

  //Sbt-Pack
  val autoSettings = Seq(crossPaths := false, packGenerateWindowsBatFile := true, packJarNameConvention := "default")

  lazy val wrapper = Project(
    id = moduleName,
    base = file(moduleName)
  ).settings(Common.settings: _*)
    .settings(packAutoSettings ++ autoSettings)
    .settings(mainClass in Compile := Some(moduleName + ".Main"))
    .settings(libraryDependencies ++= Dependencies.generalDependencies)
    .settings(fork in run := true)

  fork in run := true
}