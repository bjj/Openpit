import sbt._

class OpenPitProject(info: ProjectInfo) extends LWJGLProject(info)
{
    override def compileOptions = super.compileOptions ++
        Seq(Unchecked, Optimize)

    val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.6-SNAPSHOT" % "test" withSources()
    //val junit = "junit" % "junit" % "4.7" % "test"
    val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
}
