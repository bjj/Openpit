import sbt._
import de.element34.sbteclipsify._

class OpenPitProject(info: ProjectInfo) extends LWJGLProject(info) with Eclipsify
{
    override def compileOptions = super.compileOptions ++
        Seq(Unchecked, Optimize)

    val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.6-SNAPSHOT" % "test" withSources()
    val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.0" % "1.7" % "test"
    //val junit = "junit" % "junit" % "4.7" % "test"
    val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

    val jorbis = "com.projectdarkstar.ext.jorbis" % "jorbis" % "0.0.17"
    val darkstar = "Project DarkStar Repository" at "http://download.java.net/maven/2/"

    //val slick = "slick" % "slick" % "274"
    //val cokeandcode = "Coke and Code Repository" at "http://slick.cokeandcode.com/mavenrepo/"
    val slick = "org.newdawn.slick" % "org.newdawn.slick.openal" % "274" from "http://slick.cokeandcode.com/downloads/util/slick-util.jar"
}
