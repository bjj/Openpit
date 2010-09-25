import sbt._

class OpenPitProject(info: ProjectInfo) extends LWJGLProject(info)
{
	lazy val hi = task { println("Hello World"); None }
}
