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

    val erreSims = sims.filter(s ⇒ s.numMPs == 338 && s.newRidingsVec.forall(r ⇒ r.mapping.forall(or ⇒ or.pct == 100)))

    OverviewFeaturedHTML(featuredSims, sims.length, "Overview of Featured Simulations", "index.html").print
    OverviewSpecifiedSystemsHTML(sims, "Overview of All Simulations", "allSimulations.html", allSimsIntro).print
    OverviewSpecifiedSystemsHTML(erreSims, "Overview of ERRE Constrained Simulations", "erre.html", erreIntro).print
    AboutHTML(None).print
    ShortNameHTML.print
  }


  def writeHtml(params: Params, sim: Sim, doVoteSwingAnalysis: Boolean): Unit = {
    dirs.mkdirp(s"${Main.outdir}/${params.outDir}/")

    //    copyLess(Main.outdir)

    SummaryHTML(params, sim, doVoteSwingAnalysis).print
    RidingResultsHTML(params, sim).print
    RidingStatsHTML(params, sim).print
    RegionResultsHTML(params, sim).print
    RegionStatsHTML(params, sim).print
    ProvResultsHTML(params, sim).print
    ParametersHTML(params, sim).print
    AboutHTML(Some(sim)).print
  }

  private def allSimsIntro = div(
    h2("Introduction"),
    div(cls := "blockIndent")(
      p("""This page summarizes all of the modelling.  Other pages focus on the systems I find """,
        a(href := "index.html")("most interesting"), " and on modelling done at the request of the ",
        a(href := "erre.html")("ERRE Committee"), ".")
    )
  )

  private def erreIntro = div(
    h2("Introduction"),
    div(cls := "blockIndent")(
      p("""I presented my modelling work to the House of Commons Special Committee on Electoral Reform (ERRE)
    on 19-Oct-2016.
    The next day they passed a motion asking for more work within three constraints:"""),
      ol(li("Keep the composite Gallagher index as low as possible."),
        li("""Each projection must maintain the total number of Members for each Province and Territory, unchanged
            from the number allocated through the 2015 redistribution"""),
        li("""If any of the projections require a redistribution, the redistribution must be capable of being
            executed on an expedited basis, and as a result the only method of redistribution employed should be to
            merge existing ridings in sets of two, three, or more.""")
      ),
      p("""These are significant constraints, cutting to the core of the most common techniques for obtaining
    proportionality.  Pure multi-member systems such as STV meet these constraints easily, but anything that
    requires a top-up layer such as MMP or RU-PR is hard."""),
      p("""This page summarizes the systems developed for ERRE."""),
      p("""The report submitted to the Committee is """, a(href := "../ERRE_ModellingWithConstraints.pdf")("here"),
        """.""")
    ))
}
