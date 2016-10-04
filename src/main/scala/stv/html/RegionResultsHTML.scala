package stv.html

import ca.bwbecker.enrichments._
import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._

/**
  * Created by bwbecker on 2016-06-15.
  */
case class RegionResultsHTML(params: Params, sim: Sim) extends Page {

  protected val outDir: String = params.outDir
  protected val outFile: String = "regionResults.html"
  protected val pgTitle: String = s"Region Results for ${params.title} (${params.year} Data)"

  protected def content: TypedTag[String] = {
    div(
      descr,
      links,
      for {region ← sim.regions.sortBy(_.regionId)} yield {
        doRegion(region)
      }
    )
  }


  def descr: TypedTag[String] = {
    div(cls := "blockIndent")(
      p("""This page presents an analysis of each region -- the number of seats each party received in the
      region, the number they should have received, and various percentages."""),
      p("""The statistics are presented twice for models that have top-up seats in the region -- once
      that excludes the top-up seats and once including them.  This enables us to see the effect of
      including the top-up seats in the model."""),
      p("""Models that do not include top-up seats (FPTP, AV, STV) only present the statistics once."""),
      p("""The FPTP and AV electoral models don't even have the concept of a region.  In those cases the
      "regions" are the provinces.""")
    )
  }

  private def links: TypedTag[String] = {
    p(cls := "links")(
      for (region ← sim.regions.sortBy(_.regionId)) yield {
        span(a(href := s"#${region.regionId}")(region.regionId), " ")
      }
    )
  }


  def constituentRidings(targetRegion: String): Vector[String] = {


    val names = (for {
      prov ← sim.design.provinces
      region ← prov.regions
      if region.regionId == targetRegion
      newRiding ← region.ridings
    } yield {
      newRiding.mapping.map(or => or.name)
    }).flatten

    names.sorted.distinct
  }

  def doRegion(region: Region) = {

    val analysisWithoutTopups = new Analysis(
      sim.results.candidatesByRegion(region.regionId).filterNot(c ⇒ c.topupWinner),
      region.totalCandidates - region.topUpSeats)
    val analysisWithTopups = new Analysis(
      sim.results.candidatesByRegion(region.regionId),
      region.totalCandidates)

    assert(analysisWithoutTopups.allCandidates.length <= analysisWithTopups.allCandidates.length)
    assert(analysisWithoutTopups.elected.length <= analysisWithTopups.elected.length)

    div(cls := "region")(
      h2(id := region.regionId)("Region: " + region.regionId),
      div(cls := "blockIndent")(
        Analysis.comparativeStatsByPartyAsHTML(analysisWithoutTopups, analysisWithTopups),
        p(),
        table()(
          tr(td(cls := "topup")(
            h3(s"Topup Seats: (${region.topUpSeats})"),
            ol(
              for (c ← sim.results.topupByRegion(region.regionId)) yield {
                li(cls := c.party.toString)(c.party.toString)
              }
            )),
            td(cls := "constituentRidings")(
              h3("Constituent 2015 Ridings"),
              p(constituentRidings(region.regionId).mkString(", "))
            )
          ))
      )
    )
  }


}
