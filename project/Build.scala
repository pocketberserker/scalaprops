import sbt._, Keys._
import Common._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object build extends Build {

  private[this] val genName = "scalaprops-gen"
  private[this] val coreName = "scalaprops-core"
  private[this] val allName = "scalaprops-all"
  private[this] val scalazlawsName = "scalaprops-scalazlaws"
  private[this] val scalapropsName = "scalaprops"

  val scalazVersion = SettingKey[String]("scalazVersion")
  val shapelessVersion = SettingKey[String]("shapelessVersion")

  val modules: List[String] = (
    genName ::
    coreName ::
    allName ::
    scalazlawsName ::
    scalapropsName ::
    Nil
  )

  private[this] def module(id: String) =
    Project(id, file(id)).settings(commonSettings).settings(
      scalazVersion := "7.1.2",
      shapelessVersion := "2.1.0-2",
      initialCommands in console += "import scalaprops._, scalaz._",
      scalaJSStage in Global := FastOptStage
    ).enablePlugins(ScalaJSPlugin)

  lazy val gen = module("gen").settings(
    Generator.settings
  ).settings(
    name := genName,
    description := "pure functional random value generator",
    libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % scalazVersion.value
  )

  lazy val core = module("core").settings(
    name := coreName
  ).dependsOn(gen)

  lazy val scalazlaws = module("scalazlaws").settings(
    name := scalazlawsName
  ).dependsOn(core)

  lazy val scalaprops = module(scalapropsName).settings(
    name := scalapropsName,
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % "0.6.4",
    libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % scalazVersion.value,
    libraryDependencies ++= {
      val v = build.shapelessVersion.value
      if(scalaVersion.value.startsWith("2.10")) Seq(
        "com.github.japgolly.fork.shapeless" %%% "shapeless" % v % "test",
        compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
      ) else if(scalaVersion.value.startsWith("2.12")) {
        Nil
      } else Seq(
        "com.github.japgolly.fork.shapeless" %%% "shapeless" % v % "test"
      )
    },
    (sources in Test) := {
      val s = (sources in Test).value
      val useShapeless = Set("CofreeTest.scala", "FreeTest.scala")
      if(scalaVersion.value.startsWith("2.12")) {
        s.filterNot(f => useShapeless(f.getName))
      } else {
        s
      }
    },
    testFrameworks += new TestFramework("scalaprops.ScalapropsFramework"),
    parallelExecution in Test := false
  ).dependsOn(core, scalazlaws % "test")

  val root = Project("root", file(".")).settings(
    commonSettings
  ).settings(
    name := allName
  ).aggregate(
    gen, core, scalaprops, scalazlaws
  )
}
