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
          as links to the actual data.  The input files may be interesting to
          those who want to really understand a simulation or to even
          produce a variant.""")
    ),
    parameters,
    h2("Input Files"),
    p("Input files can be found on GitHub:"),
    ul(
      li(a(href := s"https://github.com/bwbecker/electionSim/blob/master/json/candidates.json")("candidates.json"),
        ": Candidates from an actual election."),
      li(a(href := s"https://github.com/bwbecker/electionSim/blob/master/json/ridings.json")("ridings.json"),
        ": Information about each real riding (riding id, name, population, area)."),
      li(a(href := s"https://github.com/bwbecker/electionSim/blob/master/json/${params.designName}.json")(
        s"${params.designName}.json"), ": The riding structure used for this simulation -- mapping FPTP " +
        "ridings to new ridings, grouping the new ridings into regions, setting district magnitude and " +
        "the number of top-up seats, etc.")

    )
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


}
