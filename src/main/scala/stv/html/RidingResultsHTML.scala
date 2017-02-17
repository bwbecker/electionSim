package stv.html

import scalatags.Text
import scalatags.Text.TypedTag
import scalatags.Text.all._

import ca.bwbecker.io._
import stv._
import stv.electionStrategy.StvRidingElectionStrategy

import java.io.File

/**
  * Created by bwbecker on 2016-06-15.
  */
case class RidingResultsHTML(params: Params, sim: Sim) extends Page {

  protected val outDir: String = params.outDir
  protected val outFile: String = "ridingResults.html"
  protected val pgTitle: String = s"Riding Results for ${params.title} (${params.year} Data)"


  protected def content: TypedTag[String] = {
    div(cls := "ridingResults")(
      descr,
      disclaimer,
      links,
      for (prov <- sim.design.provinces) yield {
        doProvince(prov)
      }
    )
  }

  private def constituentRidings(map: Seq[OldRiding]): TypedTag[String] = {
    ol(
      for {
        OldRiding(ridingId, pct, name) ← map.toList.sortBy(t ⇒ t.ridingId)
      } yield {
        li(f"${name}, $pct%3.1f")
      }
    )
  }


  private def descr = {
    val smSort = if (sim.params.electionStrat.sm.isInstanceOf[StvRidingElectionStrategy]) {
      "candidates sorted from the last selected/eliminated to the first."
    } else {
      "candidates sorted from most to fewest raw votes."
    }
    val mmSort = if (sim.params.electionStrat.mm.isInstanceOf[StvRidingElectionStrategy]) {
      "candidates sorted from the last selected/eliminated to the first."
    } else {
      "candidates sorted from most to fewest raw votes."
    }

    div(
      h3("Description"),
      //descriptions.get(params.name).getOrElse(div(p(s"Oops!  ${params.name} needs a description!")))
      div(cls := "blockIndent")(
        p(
          """This page shows the riding-by-riding result of simulating the election using the given
          model.  In most models (STV, most versions of MMP, RUP) existing ridings have been combined
          in various ways to make the new ridings.  These are listed in the "Composed from" column."""),
        p("""In other models (FPTP, AV) the existing ridings are used without modification."""),
        p(s"Single Member Elections: ${params.electionStrat.sm.name}; ", smSort),
        p(s"Multi Member Elections: ${params.electionStrat.mm.name}; ", mmSort)
      )
    )

  }

  private def disclaimer = div(
    h3("Disclaimer"),
    div(cls := "blockIndent")(
      p(
        """This data absolutely should not be used to make predictions about who will or will
      not win an actual election using this electoral system."""),
      p(
        """It's useful for understanding
      how votes transfer (or not, depending on the system) but it is not useful for making
      detailed preditions.  There are too many variables at play, particularly in how ridings
      have been combined.  In some models that has resulted in candidates being split between
      ridings, giving those candidates no chance of winning.  That wouldn't occur in a
      real election, of course.""")
    )
  )


  private def links: TypedTag[String] = {

    p(cls := "links")(
      for (provName ← ProvName.values) yield {
        span(a(href := s"#${provName}")(provName.entryName), " ")
      }
    )
  }


  def doRiding(riding: Riding) = {

    val cands = if (riding.districtMagnitude > 1) {
      sim.params.electionStrat.mm.sortCandidates(sim.results.candidatesByRiding(riding.ridingId))
    } else {
      sim.params.electionStrat.sm.sortCandidates(sim.results.candidatesByRiding(riding.ridingId))
    }


    val oldRidings = riding.mapping.sortBy(r ⇒ r.ridingId).zipWithIndex

    def cName(c: Candidate): String = {
      s"${
        oldRidings.find(t ⇒ t._1.ridingId == c.oldRidingId.toString).get._2 + 1
      }-${
        c.name
      }"
    }

    def optCheck(c: Candidate): Text.RawFrag = {
      (c.winner, c.seatType) match {
        case (true, SeatType.RidingSeat) if c.protect ⇒ raw("P")
        case (true, SeatType.RidingSeat)              ⇒ raw("&#10004;")
        case (true, SeatType.AdjustmentSeat)          ⇒ raw("A")
        case (_, _)                                   ⇒ raw("")
      }
    }

    for (c ← cands) yield {
      if (c == cands.head) {
        tr(
          td(rowspan := cands.length, cls := "ridingID firstRow")(riding.ridingId),
          td(rowspan := cands.length, cls := "mapsTo firstRow")(
            constituentRidings(riding.mapping),
            p(cls := "dataWarning")("This data can not be used to predict outcomes for individual candidates.")
          ),
          td(cls := s"${
            c.party
          } candName firstRow")(cName(c)),
          td(cls := s"${
            c.party
          } party firstRow")(c.party.toString),
          td(cls := "votes firstRow")(f"${
            c.votes
          }%,8.0f"),
          td(cls := "effVotes firstRow")(f"${
            c.effVotes
          }%,8.1f"),
          //td(cls := "firstRow")(c.order),
          td(cls := "winner firstRow")(optCheck(c))
        )
      } else {
        tr(
          td(cls := s"${
            c.party
          } candName")(cName(c)),
          td(cls := s"${
            c.party
          } party")(c.party.toString),
          td(cls := "votes")(f"${
            c.votes
          }%,8.0f"),
          td(cls := "effVotes")(f"${
            c.effVotes
          }%,8.1f"),
          //td(c.order),
          td(cls := "winner")(optCheck(c))
        )
      }
    }
  }

  def doProvince(prov: Province) = {
    div(h2(id := prov.prov.entryName)(prov.prov.longName),
      for (region ← prov.regions) yield {
        div(
          h3(s"Region: ${
            region.regionId
          }"),
          table(cls := "ridingSummary")(
            tr(th("Riding ID"),
              th("Composed from"),
              th("Candidate"),
              th("Party"),
              th("Votes"),
              th("Eff Votes")
            ),
            for (riding <- region.ridings) yield {
              doRiding(riding)
            }
          )
        )
      }
    )
  }

}
