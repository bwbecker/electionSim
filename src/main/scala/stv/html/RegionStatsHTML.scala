package stv.html

import java.text.NumberFormat

import stv.Analysis.StatsByParty
import stv.Party._

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.{Party, Analysis, Params, Sim}
import ca.bwbecker.enrichments.RichBoolean

/**
  * Created by bwbecker on 2016-07-04.
  */
case class RegionStatsHTML(params: Params, sim: Sim) extends Page {

  protected val outDir: String = params.outDir
  protected val outFile: String = "regionStats.html"
  protected val pgTitle: String = s"Region Stats for ${params.title} (${params.year} Data)"

  // Column recording the Liberal overrepresentation totals
  private val LibOverRepColumn = 13
  private val totals = Array.fill[Double](16)(0.0)
  private val pctFmt = NumberFormat.getPercentInstance
  private val fmt = NumberFormat.getNumberInstance
  fmt.setMaximumFractionDigits(2)
  fmt.setMinimumFractionDigits(2)


  protected def content: TypedTag[String] = {


    div(cls := "blockInput")(
      definitions,
      doTable,
      (sim.design.is_proportional && this.totals(LibOverRepColumn) >= 10).option {
        libAdvantage
      }
    )

  }

  private def definitions: TypedTag[String] = {
    div(
      p("""Definitions:"""),
      ul(
        li("Local MPs: The number of MPs each party elected in local ridings in the specified region."),
        li("Topup Seats: The number of top-up seats won by each party in the specified region."),
        li(
          """Over/Under Representation: The difference in number of seats between what a party deserved,
            |based on first choice votes, and the number of seats they actually won.  Positive numbers indicate
            |more seats than deserved; negative numbers indicate fewer seats than deserved.""".stripMargin),
        li("""Gallagher:  The Gallagher Index of Disproportionality for the region.""")
      )
    )
  }

  private def doTable: TypedTag[String] = {
    val parties = List(Con, Bloc, Grn, Lib, NDP).zipWithIndex

    def seatsDeservedByOtherParties(statsByParty: Map[Party, StatsByParty]): Double = {
      val mainStream = parties.map(_._1)
      val others = statsByParty.filterNot { case (p, sbp) => mainStream.contains(p) }
      others.map { case (p, sbp) => sbp.mps - sbp.deservedMPs }.sum
    }

    val right = cls := "right"

    table(cls := "regionStats")(
      thead(
        tr(
          th()(),
          th(colspan := 5)("Local MPs"),
          th(colspan := 5)("Top-up Seats"),
          th(colspan := 6)("Over/Under Representation"),
          th()
        ),
        tr(
          th("Region Id"),
          for {i <- 1 to 3
               (p, idx) <- parties
          } yield {
            th(p.toString) // parties of interest, repeated 3 times
          },
          th("Oth"),
          th("Gallagher")
        )
      ),
      tbody(
        (for (region ← sim.regions.sortBy(r ⇒ r.ridings.head.province + r.regionId)) yield {
          val ridingSeats = region.ridings.map(r ⇒ r.districtMagnitude).sum
          //val analysis = Analysis(sim.results.candidatesByRegion(region.regionId), region.totalCandidates)
          val analysis = sim.results.analysisByRegion(Seq(region))
          val sbp = analysis.statsByParty.map(s => (s.party, s)).toMap.withDefault((p) => StatsByParty(p, 0, 0.0, 0, 0.0, 0.0, 0, 0))

          tr(
            td(a(href := s"regionResults.html#${region.regionId}")(region.regionId)),
            for ((p, i) <- parties) yield {
              val v = sbp(p).numRidingMPs
              totals(0 + i) = totals(0 + i) + v
              td(cls := p + " ltGray")(v)
            },
            for ((p, i) <- parties) yield {
              val v = sbp(p).numTopupSeats
              totals(5 + i) = totals(5 + i) + v
              td(cls := p.toString)(v)
            },
            for ((p, i) <- parties) yield {
              val v = sbp(p).mps - sbp(p).deservedMPs
              totals(10 + i) = totals(10 + i) + v
              td(cls := p + " ltGray")(fmt.format(v))
            }, {
              val v = seatsDeservedByOtherParties(sbp)
              totals(15) = totals(15) + v
              td(cls := "right ltGray")(fmt.format(v))
            },
            td(right)(f"${analysis.gallagherIndex * 100}%4.1f%%")
          )
        }) :+ tfoot(
          tr(cls := "totalsRow")(
            td(right)("Totals:"),
            for (i <- 0 to 9) yield {
              td(right)(totals(i).toInt)
            },
            for (i <- 10 to 15) yield {
              td(right)(fmt.format(totals(i)))
            }
          )
        )
      )
    )
  }


  private def libAdvantage: TypedTag[String] = {
    div(
      h3("Liberal Advantage"),
      p(
        s"""The above table shows an over-representation for the Liberals of
            |${fmt.format(this.totals(this.LibOverRepColumn))} seats.
            |Why?  There are several reasons:
         """.stripMargin),
      ol(
        li(p(
          """The Northwest Territories, Nunavut, and Yukon all elected Liberal MPs but don't have any
            |compensatory mechanism for non-Liberal voters.  There are no top-up seats or multi-member
            |ridings to help make results proportional.""".stripMargin)),
        li(p(
          """Several small regions such as New Brunswick and Nova Scotia were disproportionate enough
            |in the local seats that the compensatory mechanisms were overwhelmed and couldn't
            |adequately compensate.  Typically this is due to having only 1 top-up seat for the region
            |or a high proportion of single-member districts.
          """.stripMargin)
        ),
        li(
          p(
            """When a voter does not get their first choice, the Liberals benefit more than any other
              |party from the vote transfers.  A test in which NDP voter's second choice was typically
              |Conservative rather than Liberal showed the Conservatives with the over-representation
              |instead of the Liberals.
            """.stripMargin),
          p(
            """The model used for vote transfers is clearly inadequate.  It doesn't distinguish 3rd and
              |subsequent choices from 2nd choices.  It doesn't account for regional differences.  It's
              |based on relatively small sample sizes.  Real life will likely be different from this simulation.
            """.stripMargin)
        )
      )
    )
  }

}
