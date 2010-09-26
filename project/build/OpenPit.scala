import sbt._

class OpenPitProject(info: ProjectInfo) extends LWJGLProject(info)
{
    override def compileOptions = super.compileOptions ++
	Seq(Unchecked, Optimize)
}
