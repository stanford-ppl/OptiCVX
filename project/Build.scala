import sbt._
import Keys._

object OptiCVXBuild extends Build {
    lazy val root = Project(id = "opticvx",
                            base = file("."))

    lazy val common = Project(id = "opticvx-common", base = file("partial/common"))

    lazy val model = Project(id = "opticvx-model", base = file("partial/model"))

    lazy val solvergen = Project(id = "opticvx-solvergen", base = file("partial/solvergen"))
}