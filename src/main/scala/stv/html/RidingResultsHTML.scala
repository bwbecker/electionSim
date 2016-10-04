package stv.html

import scalatags.Text.TypedTag
import scalatags.Text.all._

import ca.bwbecker.io._
import stv._

import java.io.File

/**
  * Created by bwbecker on 2016-06-15.
  */
case class RidingResultsHTML(params: Params, sim: Sim) extends Page {

  protected val outDir: String = params.outDir
  protected val outFile: String = "ridingResults.html"
  protected val pgTitle: String = s"Riding Results for ${params.title}"


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
    ul(
      for {
        OldRiding(ridingId, pct, name) ← map.toList.sortBy(t ⇒ t.ridingId)
      } yield {
        li(f"${name}, $pct%3.1f")
      }
    )
  }


  private def descr = {
    div(
      h3("Description"),
      //descriptions.get(params.name).getOrElse(div(p(s"Oops!  ${params.name} needs a description!")))
      div(cls := "blockIndent")(
        p(
          """This page shows the riding-by-riding result of simulating the election using the given
          model.  In most models (STV, most versions of MMP, RUP) existing ridings have been combined
          in various ways to make the new ridings.  These are listed in the "Composed from" column."""),
        p("""In other models (FPTP, AV) the existing ridings are used without modification.""")
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
    val cands = sim.results.candidatesByRiding(riding.ridingId).sortBy(c ⇒ (-c.votes, c.name))

    for (c ← cands) yield {
      if (c == cands.head) {
        tr(
          td(rowspan := cands.length, cls := "ridingID firstRow")(riding.ridingId),
          td(rowspan := cands.length, cls := "mapsTo firstRow")(
            constituentRidings(riding.mapping),
            p(cls := "dataWarning")("This data can not be used to predict outcomes for individual candidates.")
          ),
          td(cls := s"${c.party} candName firstRow")(c.name),
          td(cls := s"${c.party} party firstRow")(c.party.toString),
          td(cls := "votes firstRow")(f"${c.votes}%,8.0f"),
          td(cls := "effVotes firstRow")(f"${c.effVotes}%,8.1f"),
          td(cls := "winner firstRow")(if (c.winner) {
            raw("&#10004;")
          } else "")
        )
      } else {
        tr(
          td(cls := s"${c.party} candName")(c.name),
          td(cls := s"${c.party} party")(c.party.toString),
          td(cls := "votes")(f"${c.votes}%,8.0f"),
          td(cls := "effVotes")(f"${c.effVotes}%,8.1f"),
          td(cls := "winner")(if (c.winner) {
            raw("&#10004;")
          } else "")
        )
      }
    }
  }

  def doProvince(prov: Province) = {
    div(h2(id := prov.prov.entryName)(prov.prov.longName),
      for (region ← prov.regions) yield {
        div(
          h3(s"Region: ${region.regionId}"),
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
