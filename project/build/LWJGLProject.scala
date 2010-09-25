// from http://blog.misadventuregames.com/post/248744147/scala-and-lwjgl-with-sbt-updated

import sbt._

// Add automatic lwjgl native flavor detection to enable 'run' from sbt

abstract class LWJGLProject(info: ProjectInfo) extends DefaultProject(info) {

    def nativeLWJGL = {
	def version = "2.5"
        def os = System.getProperty("os.name").toLowerCase match {
            // suppose we could do something "clever" like only grab
            // the first 3 characters then match against that.
            case "linux" => ("linux",":")
            case "mac os x" => ("macosx",":")
            case "windows xp" => ("windows",";")
            case "windows vista" => ("windows",";")
            case "windows 7" => ("windows",";")
            case "sunos" => ("solaris",":")
            case _ => ("unknown","")
        }

        os._1 match {
            case "unknown" => ("","")
            case _ => (path("lib") / ("lwjgl-" + version) / "native" / os._1, os._2)
        }
    }

    override def fork = {
        val newPath = 
            System.getProperty("java.library.path") + nativeLWJGL._2 + nativeLWJGL._1
        forkRun(Seq("-Djava.library.path=" + newPath))
    }
}
