import mill._
import mill.scalalib._
import $ivy.`com.goyeau::mill-scalafix::0.4.2`
import com.goyeau.mill.scalafix.ScalafixModule
import scalafmt._
import scalajslib._
import mill.scalajslib.api.ModuleKind
import mill.scalajslib.api._

object config {
  val scalaVersion = "3.6.2"
}

trait AppScalaModule extends ScalaModule with ScalafixModule with ScalafmtModule {
  def scalaVersion  = config.scalaVersion
  def scalacOptions = Seq("-Wunused:all")
}

trait AppScalaJSModule extends AppScalaModule with ScalaJSModule with ScalafixModule with ScalafmtModule {
  def scalaJSVersion = "1.17.0"
  def scalacOptions  = Seq("-Wunused:all")
}

object ballsort extends Module {

  object frontend extends AppScalaJSModule {
    def moduleKind = ModuleKind.ESModule

    def ivyDeps = Agg(
      ivy"io.github.kitlangton::animus::0.6.5",
      ivy"io.github.kitlangton::neotype::0.3.5",
      ivy"io.github.michalliss::foxxy-frontend::0.0.10"
    )

    object test extends ScalaJSTests with TestModule.ZioTest {
      override def ivyDeps = Agg(
        ivy"dev.zio::zio-test::2.1.13",
        ivy"dev.zio::zio-test-sbt::2.1.13",
        ivy"dev.zio::zio-test-magnolia::2.1.13"
      )
    }
  }

  object frontend_vite extends Module {
    def moduleDeps = Seq(frontend)

    def compile = T {
      val jsPath = frontend.fastLinkJS().dest.path

      if (!os.exists(frontend_vite.millSourcePath / "app")) {
        os.makeDir(frontend_vite.millSourcePath / "app")
      }
      os.copy(
        jsPath / "main.js",
        frontend_vite.millSourcePath / "app" / "main.js",
        replaceExisting = true
      )
      // os.copy(
      //   jsPath / "main.wasm",
      //   frontend_vite.millSourcePath / "app" / "main.wasm",
      //   replaceExisting = true
      // )
      // os.copy(
      //   jsPath / "__loader.js",
      //   frontend_vite.millSourcePath / "app" / "__loader.js",
      //   replaceExisting = true
      // )
    }
  }
}
