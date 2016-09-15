package stv.html

import stv.Party._

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.{Sim, Params}

/**
  * Created by bwbecker on 2016-07-04.
  */
case class RidingStatsHTML(params: Params, sim: Sim) extends Page {

  protected val outDir: String = params.outDir
  protected val outFile: String = "ridingStats.html"
  protected val pgTitle: String = s"Riding Stats for ${params.title}"

  protected def content: TypedTag[String] = {
    val right = cls := "right"
    val left = cls := "left"

    val ridings = sim.newRidings.values.toList
    val numRidings = ridings.size
    val avgMPs = ridings.map(_.districtMagnitude).sum / numRidings.toDouble
    val avgArea = ridings.map(_.area).sum / numRidings.toDouble
    val avgPop = ridings.map(_.population).sum / numRidings.toDouble
    val avgPopPerMP = ridings.map(r ⇒ r.population / r.districtMagnitude).sum / numRidings.toDouble

    /**
      * Statistics on each riding:  DM, area, population, etc.
      */
    div(cls := "blockInput")(
      p("""Statistics for each new riding in the model."""),
      table(cls := "ridingStats")(
        thead(
          tr(th(colspan := 6)(),
            th(colspan := 5)("MPs Elected")),
          tr(th("Riding Name"),
            th("Province"),
            th("#MPs"),
            th("Area"),
            th("Population"),
            th("Pop/MP"),
            th("Con"),
            th("Bloc"),
            th("Grn"),
            th("Lib"),
            th("NDP")
          )
        ),

        tbody(
          (for ((riding_id, riding) ← sim.newRidings.toList.sortBy(t ⇒ t._2.province + t._2.ridingId)) yield {
            tr(
              td(left)(riding.name),
              td(left)(riding.province.toString),
              td(riding.districtMagnitude),
              td(f"${riding.area}%,10d"),
              td(f"${riding.population}%,10d"),
              td(f"${riding.population / riding.districtMagnitude}%,d"),
              td(cls := "Con")(riding.elected.count(_.party == Con)),
              td(cls := "Bloc")(riding.elected.count(_.party == Bloc)),
              td(cls := "Grn")(riding.elected.count(_.party == Grn)),
              td(cls := "Lib")(riding.elected.count(_.party == Lib)),
              td(cls := "NDP")(riding.elected.count(_.party == NDP))
            )
          }) :+ tfoot(
            tr(cls := "totalsRow")(
              td(colspan := 2)("Averages:"),
              td(right)(f"${avgMPs}%4.2f"),
              td(right)(f"${avgArea}%,4.0f"),
              td(right)(f"${avgPop}%,4.0f"),
              td(right)(f"${avgPopPerMP}%,4.0f")
            )
          )
        )
      )


    )


  }

}
