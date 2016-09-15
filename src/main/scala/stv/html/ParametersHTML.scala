package stv.html


import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._

/**
  * Created by bwbecker on 2016-06-01.
  */
case class ParametersHTML(params: Params, sim: Sim) extends Page {

  protected val outDir = params.outDir
  protected val pgTitle = s"Parameters for ${params.title}"
  protected val outFile = "params.html"

  protected val content = div(
    div(cls := "blockIndent")(
      p(
        """This page includes the parameters used to run the model as well
          as the actual data.  Interested people could cut and paste the data
          into four spreadsheets, modify those spreadsheets, and then send them
          to me (bwbecker@golden.net) to produce the kind of data you see elsewhere
          on this site.""")
    ),
    ul(
      li(a(href := "#params")("Simulation Parameters")),
      li(a(href := "#ridingMapping")("Map Existing Ridings to New Ridings")),
      li(a(href := "#ridingData")("New Ridings")),
      li(a(href := "#regionMapping")("Map New Ridings to Regions")),
      li(a(href := "#regionData")("Region Data"))
    ),
    parameters,
    ridingMapping,
    prRiding,
    regionMapping,
    region
  )


  private def parameters = {
    div(id := "params")(
      h2("Simulation Parameters"),
      div(cls := "blockIndent")(
        table(
          thead(
            tr(th("Parameter"), th("Value"))
          ), tbody(
            tr(td("Parameter set name: "), td(params.name)),
            tr(td("Title: "), td(params.title)),
            tr(td("DB Model Name: "), td(params.designName.toString)),
            tr(td("Output directory: "), td(params.outDir)),
            params.voteAdjustment.map {
              va ⇒ tr(td("Vote adjustment: "),
                td(s"${va.toParty}.votes += ${va.pct} * ${va.fromParty}.votes"))
            },
            tr(td("Minium threshhold to earn top-up seats: "), td(params.threshholdForTopup)),
            tr(td("Single-Member Riding election type: "), td(params.singleMemberElectionStrategy.name)),
            tr(td("Multi-Member Riding election type: "), td(params.multiMemberElectionStrategy.name))
          ))
      )
    )
  }


  private def ridingMapping = {
    val csv = new StringBuffer()
    val model = sim.params.designName
    csv.append("model,riding_id,pr_riding_id,pct\n")
    for {(id, riding) ← sim.newRidings
         OldRiding(partRidingId, pct, name) ← riding.mapping
    } {
      csv.append(s"${model},${partRidingId},${id},${pct}\n")
    }

    div(id := "ridingMapping")(
      h2("Map Existing Ridings to New Ridings"),
      div(cls := "blockIndent")(
        p(
          """Map existing ridings (given with the 5 digit number assigned by Elections
          Canada) to new ridings.  Sometimes the mappings are one-to-one
          (eg FPTP, AV, one version of MMP) but more frequently several ridings are combined,
          perhaps with parts of other ridings."""),
        pre(csv.toString)
      )
    )
  }


  private def prRiding = {
    val csv = new StringBuffer()
    val model = sim.params.designName
    csv.append("model,pr_riding_id,district_mag\n")
    for {(id, riding) ← sim.newRidings} {
      csv.append(s"${model},${riding.ridingId},${riding.districtMagnitude}\n")
    }

    div(id := "ridingData")(
      h2("New Ridings"),
      div(cls := "blockIndent")(
        p("""Data for new ridings:  an identifier and the district magnitute (number of MPs)."""),
        pre(csv.toString)
      )
    )
  }


  private def regionMapping = {
    val csv = new StringBuffer()
    val model = sim.params.designName
    csv.append("model,region_id,pr_riding_id\n")
    for {region ← sim.regions
         riding ← region.ridings} {
      csv.append(s"${model},${region.regionId},${riding.ridingId}\n")
    }
    div(id := "regionMapping")(
      h2("Map New Ridings to Regions"),
      div(cls := "blockIndent")(
        p("""Map the new ridings into regions for the purpose of assigning top-up MPs. """),
        pre(csv.toString)
      )
    )
  }


  private def region = {
    val csv = new StringBuffer()
    val model = sim.params.designName
    csv.append("model,region_id,top_up_seats\n")
    for {region ← sim.regions} {
      csv.append(s"${model},${region.regionId},${region.topUpSeats}\n")
    }
    div(id := "regionData")(
      h2("Region Data"),
      div(cls := "blockIndent")(
        p(
          """Data for the regions: an identifier and the number of top-up seats.  For
      models without a regional layer (eg FPTP, AV), regions are formed for each
      province with no top-up seats."""),
        pre(csv.toString)
      )
    )
  }


}
