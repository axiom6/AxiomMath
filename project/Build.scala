import sbt._
//import sbt.Keys._

object Settings {
  val Name = "AxiomMath"
}

object Resolvers {
  val typesafe      = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  val sonatype      = "Sonatype Release"    at "https://oss.sonatype.org/content/repositories/releases"
  val mvnrepository = "MVN Repo"            at "http://mvnrepository.com/artifact"

  val allResolvers = Seq( typesafe, sonatype, mvnrepository )
}

object Version {
  val AxiomMath     = "0.0.0"
  val Scala         = "2.11.6"
 }

object Library {
  import Version._
//val sparkCore         = "org.apache.spark"     %% "spark-core"                         % Spark

}

object Dependencies {

  import Library._

  val AxiomMath = Seq(  )
}
