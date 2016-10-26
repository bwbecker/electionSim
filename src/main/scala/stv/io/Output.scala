package stv.io

import ca.bwbecker.io._
import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._
import stv.html._

import java.io.File

/**
  * Created by bwbecker on 2016-06-01.
  */
object Output {

  val dirs = new CachedMkdir


  /**
    * Copy the output of the less compiler to where we want it.
    */
  def copyLess(destDir: String): Unit = {
    import scala.language.implicitConversions

    import java.nio.file.StandardCopyOption.REPLACE_EXISTING
    import java.nio.file.Files.copy
    import java.nio.file.Paths.get

    implicit def toPath(filename: String) = get(filename)

    dirs.mkdirp(destDir + "/css/")
    copy("target/web/less/main/main.css", destDir + "/css/main.css", REPLACE_EXISTING)
  }


  def writeOverview(sims: List[Sim]): Unit = {

    dirs.mkdirp(s"${Main.outdir}/overview")

    val featuredSims = sims.filter(s => Main.featuredSystems.exists { f =>
      f.matches(s.params)
    })

    OverviewFeaturedHTML(featuredSims, sims.length, "Overview of Featured Simulations", "index.html").print
    OverviewAllHTML(sims, "Overview of All Simulations", "allSimulations.html").print
    AboutHTML(None).print
    ShortNameHTML.print
  }


  def writeHtml(params: Params, sim: Sim, doVoteSwingAnalysis: Boolean): Unit = {
    dirs.mkdirp(s"${Main.outdir}/${params.outDir}/")

    copyLess(Main.outdir)

    SummaryHTML(params, sim, doVoteSwingAnalysis).print
    RidingResultsHTML(params, sim).print
    RidingStatsHTML(params, sim).print
    RegionResultsHTML(params, sim).print
    RegionStatsHTML(params, sim).print
    ProvResultsHTML(params, sim).print
    ParametersHTML(params, sim).print
    AboutHTML(Some(sim)).print
  }
}
