import mill._
import mill.scalalib._
import coursier.maven.MavenRepository
import publish._
import ammonite.ops._
import ammonite.ops.ImplicitWd._

object animesql extends ScalaModule {
  /** Non maven dependencies */
  def unmanagedClasspath = Agg(
    mill.modules.Util.download(
      "https://github.com/sguzman/BrotliExec/releases/download/v1.0.1/com.github.sguzman.brotli.brotliexec.1.0.1.jar",
      "com.github.sguzman.brotliexec.1.0.1.jar"
    )
  )

  /** Main class */
  def mainClass = Some("com.github.sguzman.anime.sql.Main")

  /** Name of project */
  def name = "AnimeScraperSql"

  /** Organization */
  def organization = "com.github.sguzman"

  /** Project version */
  def version = "1.0"

  /** Scala version */
  def scalaVersion = "2.12.4"

  /** Scalac parameters */
  def scalacOptions = Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  )


  /** Javac parameters */
  def javacOptions = Seq("-server")

  /** Resolvers */
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://repo1.maven.org/maven2"),
    MavenRepository("https://oss.sonatype.org/content/repositories/public"),
    MavenRepository("https://repo.typesafe.com/typesafe/releases"),
    MavenRepository("https://repo.scala-sbt.org/scalasbt/sbt-plugin-releases"),
    MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
    MavenRepository("https://jcenter.bintray.com")
  )

  /** SPBC Executable Download */
  def spbc = T{
    mkdir(T.ctx().dest)
    val wd = pwd
    interp.load.ivy("org.scalaj" %% "scalaj-http" % "2.3.0")
    import scalaj.http._

    val exec = Http("https://github.com/scalapb/ScalaPB/releases/download/v0.7.1/scalapbc-0.7.1.zip")
      .option(HttpOptions.followRedirects(true)).asBytes.body

    write(T.ctx().dest / "spbc.zip", exec)
    %%('unzip, T.ctx().dest / "spbc.zip", "-d", T.ctx().dest)

    %%('find, T.ctx().dest)
  }

  def protoc = T{
    val _ = spbc()
    val name = "animesql"
    val exec = pwd / "out" / name / "spbc" / "dest" / "scalapbc-0.7.1" / "bin" / "scalapbc"
    val protoFiles = %%('gfind, pwd / name / "proto", "-type", "f", "-name", "*.proto").out.lines

    protoFiles.foreach{a =>
      %%bash(exec, s"--proto_path=${pwd / name / "proto"}", a, s"--scala_out=${pwd / name / "src"}")
    }

    ls.rec(pwd / name / "proto").map(PathRef(_))
  }

  /** Ivy dependencies */
  def ivyDeps = Agg(
    ivy"net.ruippeixotog::scala-scraper:2.1.0",
    ivy"org.scalaj::scalaj-http:2.3.0",
    ivy"org.apache.commons:commons-lang3:3.7",
    ivy"com.outr::scribe:2.3.1",
    ivy"com.thesamet.scalapb::compilerplugin:0.7.1",
    ivy"com.thesamet.scalapb::scalapb-runtime:0.7.1",
    ivy"com.typesafe.slick::slick:3.2.3",
    ivy"org.slf4j:slf4j-nop:1.8.0-beta2",
    ivy"com.typesafe.slick::slick-hikaricp:3.2.3",
    ivy"org.postgresql:postgresql:42.2.2.jre7",
    ivy"com.typesafe::config:1.3.32"
  )

  def forkArgs = Seq("-Xmx4g")
}