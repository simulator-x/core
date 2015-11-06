libraryDependencies ++= Seq(
	compilerPlugin("org.scala-lang.plugins" % ("scala-continuations-plugin_" + scalaVersion.value) % "1.0.2"),
	"org.scala-lang" % "scala-library-all" % scalaVersion.value,
	"org.scalatest" %% "scalatest" % "3.0.0-M7", 
	"org.simplex3d" % "simplex3d-math-float_2.10" %"2.4.7",
	"org.simplex3d" % "simplex3d-math-double_2.10" %"2.4.7",
	"com.typesafe.akka" %% "akka-remote" %"2.3.10",
	"org.apache.logging.log4j" %"log4j-slf4j-impl" %"2.3",
	"org.apache.logging.log4j" %"log4j-core" %"2.3",
	"org.apache.logging.log4j" %"log4j-api" %"2.3",	  
	"javax.jmdns" %"jmdns" %"3.4.1", 
	"org.scoverage" %% "scalac-scoverage-plugin" % "1.1.1",
	"com.thoughtworks.xstream" % "xstream" % "1.4.8"
	// "com.github.romix.akka" %"akka-kryo-serialization_2.10" %"0.3.0",	
)

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "assets") ** "*.jar").classpath }

unmanagedJars in Compile +=
	Attributed.blank(
		file(scala.util.Properties.javaHome) / "/lib/jfxrt.jar")

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")

autoCompilerPlugins := true

scalacOptions += "-P:continuations:enable"

scalacOptions += "-deprecation"
 
scalacOptions += "-feature"

scalacOptions += "-language:postfixOps"

scalacOptions += "-language:existentials"

scalacOptions += "-language:implicitConversions"
