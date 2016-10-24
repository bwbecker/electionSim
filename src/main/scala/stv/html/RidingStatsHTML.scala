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
  protected val pgTitle: String = s"Riding Stats for ${params.title} (${params.year} Data)"

  protected def content: TypedTag[String] = {
    val right = cls := "right"
    val left = cls := "left"

    val ridings = sim.newRidingsVec
    val numRidings = ridings.size
    val avgMPs = ridings.map(_.districtMagnitude).sum / numRidings.toDouble
    val avgArea = ridings.map(_.area).sum / numRidings.toDouble
    val avgPop = ridings.map(_.population).sum / numRidings.toDouble
    val avgPopPerMP = ridings.map(r ⇒ r.population / r.districtMagnitude).sum / numRidings.toDouble

    val ridingByProv = sim.newRidingsVec.groupBy(r ⇒ r.province)
    val avgPopPerMPByProv = (for {
      (prov, ridingLst) ← ridingByProv
    } yield {
//      val popPerMP = ridingLst.map(r ⇒ r.population / r.districtMagnitude)
//      (prov, popPerMP.sum / popPerMP.length)
      val (pop, mps) = ridingLst.foldLeft((0, 0)){(a,r) ⇒ (a._1 + r.population, a._2 + r.districtMagnitude)}
      (prov, pop/mps.toDouble)
    }).toMap

    //println(pgTitle)
    //println(avgPopPerMPByProv)

    val sortByNum = "data-sort-method".attr := "number"
    /**
      * Statistics on each riding:  DM, area, population, etc.
      */
    div(cls := "blockInput")(
      p("""Statistics for each new riding in the model."""),
      table(id := "ridingStats", cls := "ridingStats")(
        thead(
          tr(th(colspan := 7)(),
            th(colspan := 5)("MPs Elected")),
          tr(th("Riding Name"),
            th("Prov"),
            th(sortByNum)("#MPs"),
            th(sortByNum)("Area"),
            th(sortByNum)("Pop"),
            th(sortByNum)("Pop/MP"),
            th(sortByNum)("Pop/MP Var"),
            th("Con"),
            th("Bloc"),
            th("Grn"),
            th("Lib"),
            th("NDP")
          )
        ),

        tbody(

          (for (riding ← sim.newRidingsVec.sortBy(t ⇒ t.province + t.ridingId)) yield {
            val elected = sim.results.electedByRiding(riding.ridingId)
            val popVariation = (riding.population / riding.districtMagnitude - avgPopPerMPByProv(riding.province)) /
              avgPopPerMPByProv(riding.province).toDouble

            tr(
              td(left)(riding.name),
              td(left)(riding.province.toString),
              td(riding.districtMagnitude),
              td(f"${riding.area}%,10d"),
              td(f"${riding.population}%,10d"),
              td(f"${riding.population / riding.districtMagnitude}%,d"),
              td(f"${popVariation * 100}%3.1f%%"),
              td(cls := "Con")(elected.count(_.party == Con)),
              td(cls := "Bloc")(elected.count(_.party == Bloc)),
              td(cls := "Grn")(elected.count(_.party == Grn)),
              td(cls := "Lib")(elected.count(_.party == Lib)),
              td(cls := "NDP")(elected.count(_.party == NDP))
            )
          })),

        tfoot(
          tr(cls := "totalsRow")(
            td(colspan := 2)("Averages:"),
            td(right)(f"${avgMPs}%4.2f"),
            td(right)(f"${avgArea}%,4.0f"),
            td(right)(f"${avgPop}%,4.0f"),
            td(right)(f"${avgPopPerMP}%,4.0f")
          )
        )
      ),

      script("""new Tablesort(document.getElementById('ridingStats'));""")
    )
  }

}
