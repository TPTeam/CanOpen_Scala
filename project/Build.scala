import sbt._
import sbt.Keys._

object Build extends Build {

  lazy val project = Project(
    "CanOpen_Scala",
    file("."),
    settings = commonSettings ++ Seq(
      libraryDependencies ++= Seq(
        Dependency.Compile.akkaActor,
        Dependency.Compile.akkaRemote,
        Dependency.Compile.akkaKernel,
        Dependency.Compile.akkaSlf4j,
        Dependency.Compile.logbackClassic,
		Dependency.Compile.jna,
		Dependency.Compile.purejavacomm)
    )
  )

  def commonSettings = Defaults.defaultSettings ++ 
    Seq(
      organization := "com.tecniplast",
      version := "0.0.95",
      scalaVersion := Version.scala,
      scalacOptions ++= Seq(
        "-unchecked",
        "-deprecation",
        "-Xlint",
        "-language:_",
        //"-target:jvm-1.6",
        "-encoding", "UTF-8"
      ),
      unmanagedBase := baseDirectory.value / "external_libs",
      resolvers ++= 
      Seq(
		Opts.resolver.sonatypeReleases ,
		"sparetimelabs" at "http://www.sparetimelabs.com/maven2/"
      ),
      retrieveManaged := true
    )

  object Version {
    val scala = "2.11.4"
    val akka = "2.3.7"
  }
  
  
  object Dependency {

    object Compile {
      val akkaActor = 		"com.typesafe.akka" %% "akka-actor" % Version.akka
      val akkaSlf4j = 		"com.typesafe.akka" %% "akka-slf4j" % Version.akka
      val akkaRemote = 		"com.typesafe.akka" %% "akka-remote" % Version.akka
      val akkaKernel = 		"com.typesafe.akka" %% "akka-kernel" % Version.akka
      val logbackClassic = 	"ch.qos.logback" % "logback-classic" % "1.0.7"
	  val jna = 			"net.java.dev.jna" % "jna" % "3.5.2"
	  val purejavacomm = 	"com.sparetimelabs" % "purejavacomm" % "0.0.21" classifier ""
    }

  }
}
