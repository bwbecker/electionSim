package stv.html

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._
import stv.Analysis.StatsByParty
import stv.Party._
import stv.electionStrategy.ElectionStrategyEnum


/**
  * There are several overview pages, one with all the simulations and one with just the "featured" sims.
  * This is the common code.
  */
abstract class AbstractOverview extends Page {
  protected val outDir = "overview"
  override protected val includeThisModelMenu = false

  val pctFmt = java.text.NumberFormat.getPercentInstance
  val pct_dFmt = new java.text.DecimalFormat("#0.0%")
  val commaFmt = new java.text.DecimalFormat("###,###,###")
  val dd_dFmt = new java.text.DecimalFormat("##0.0")


  def resultsTable(sims: List[Sim]) = {
    div(id := "proportionality", cls := "results")(
      h2("Proportionality"),
      generateResultsTable(sims),
      resultsTableFootnotes
    )
  }

  /**
    * Turn a list of sims into table rows, properly sorted, using function f.
    *
    * @param sims
    * @param f
    * @return
    */
  protected def tableRows(sims: Seq[Sim], f: Sim => TypedTag[String]): Seq[TypedTag[String]] = {
    for (sim <- sims.sortBy(s => (s.design.design_name.entryName,
      s.params.electionStrat.sm.name,
      s.params.electionStrat.mm.name))) yield (f(sim))
  }


  protected def color(tGreen: Double, tYellow: Double)(v: Double): String = {
    if (Math.abs(v) < tGreen) "green" else if (Math.abs(v) < tYellow) "yellow" else "red"
  }

  protected def fmtOverRep(os: Option[StatsByParty]) = {
    val pct = os.map { s ⇒ s.pctMPs - s.pctVote }.getOrElse(0.0)
    val warn = color(0.05, 0.10)(pct)
    td(cls := "colPct0 " + warn)(pctFmt.format(pct))
  }

  protected def fmtGallagher(g: Double) = {
    td(cls := "colPct1 " + color(0.05, 0.10)(g))(pct_dFmt.format(g))
  }

  protected def fmtPrefParty(pct: Double) = {
    val color = if (pct < 0.50) "red"
    else if (pct < 0.75) "yellow"
    else "green"
    td(cls := "colPct0 " + color)(pctFmt.format(pct))
  }

  protected def generateResultsTable(sims: List[Sim]) = {
    val sortByNum = "data-sort-method".attr := "number"
    div(
      img(cls := "hdr")(src := "../img/ResultsTableHeader.svg"),
      table(id := "overview")(
        thead(
          tr(
            th(cls := "name")(""),
            th(sortByNum, cls := "num3")(""),
            th(sortByNum, cls := "num3")(""),
            th(sortByNum, cls := "colPct0")(""),
            th(sortByNum, cls := "colPct0")(""),
            th(sortByNum, cls := "colPct0")(""),
            th(sortByNum, cls := "colPct0")(""),
            th(sortByNum, cls := "colPct0")(""),
            th(sortByNum, cls := "colPct1")(""),
            th(sortByNum, cls := "colPct1")(""),
            th(sortByNum, cls := "colPct0")(""),
            th(sortByNum, cls := "colPct0")(""),
            th(cls := "shortName")("")
          )
        ),
        tbody(
          tableRows(sims, sim => {
            val statsByParty: Map[Party, StatsByParty] = sim.analysis.statsByParty.map(s ⇒ (s.party, s)).toMap

            tr(cls := "row")(
              td(cls := "name")(
                a(href := s"../${sim.params.outDir}/index.html")(
                  raw(sim.params.title.replace("(", "<br>(")))),
              td(cls := "num3")(sim.numRidingMPs),
              td(cls := "num3")(sim.numRegionalMPs),
              fmtOverRep(statsByParty.get(Lib)),
              fmtOverRep(statsByParty.get(Con)),
              fmtOverRep(statsByParty.get(NDP)),
              fmtOverRep(statsByParty.get(Bloc)),
              fmtOverRep(statsByParty.get(Grn)),
              fmtGallagher(sim.analysis.gallagherIndex),
              fmtGallagher(sim.compositeGallagher),
              fmtPrefParty(sim.pctVotersWithPreferredPartyLocally),
              fmtPrefParty(sim.pctVotersWithPreferredPartyRegionally),
              td(cls := "shortName")(sim.params.year)
            )
          }
          )
        )
      )
    )
  }


  def resultsTableFootnotes = div(cls := "blockIndent footnotes")(
    p("Footnotes"),
    ol(
      li(strong("Number of Local MPs"), " is the total number of MPs representing specific ridings.  " +
        "Those ridings may be either single-member ridings or multi-member."),
      li(strong("Number of Regional MPs"), " is the total number of MPs that represent multiple ridings. " +
        "This happens in systems with top-up seats such as MMP and RU-PR."),
      li(strong("Over-Representation by Party"), " is the percentage of MPs in Parliament minus the " +
        "percentage of the popular vote.  For example, in 2015 under FPTP the Liberals received 54.4% " +
        "of the seats but only 39.5% of the vote for an over-representation of (54.4 - 39.5) = 14.9%. " +
        " Negative numbers mean the party was under-represented."),
      li(strong("Gallagher Index"), " is a measure of disproportionality. " +
        "It combines both over and under-representation for each party into a single number.  Gallagher" +
        "indicies less than 5 are excellent."),
      li(strong("Gallagher Index 2015"), " is the Gallagher Index for the simulated 2015 election."),
      li(strong("Gallagher Index Composite"), " is the average of the Gallagher Indices for each province and " +
        "territory, weighted by its number of seats.  This corrects for a problem in calculating the " +
        "Gallagher Index for the nation as a whole, which can can hide regional disproportionalities " +
        "such as the significant over-representation of Conservatives in the Prairies offsetting the " +
        "over-representation of Liberals " +
        "in the Maritimes. "),
      li(strong("% Voters with Preferred Local MP"), " is the percentage of voters who have an MP " +
        "representing their riding from the same party as their first choice candidate.  Systems with " +
        "multi-member ridings will do better under this measure."),
      li(strong("% Voters with Preferred Regional MP"), " is the percentage of voters who have an MP " +
        "representing their region from the same party as their first choice candidate.  Systems with " +
        "top-up seats will do better under this measure."),
      li(strong("Short System Name"), " is a very consise ", a(href := "shortName.html")("abbreviation"),
        " of the key parameters for this simulation.  ")
    )
  )


  def observations = div(
    h2("Observations"),
    div(cls := "blockIndent")(
      ul(
        li(p(
          """FPTP in 2015 gave the Liberals an undeserved 15% over-representation of MPs,
          just like it did for the Conservatives in 2011.  """,
          a(href := "../fptp/index.html#cConLib")("These graphs"),
          """ shows that as the spread in the vote becomes larger, the spread in MPs
          grows even faster.  For every percent the Liberals can increase their vote,
          their share of MPs goes up by more than 3%."""
        )),

        li(p(
          """Many commentators have noted that AV favours centrist parties like the Liberals.
        These simulations illustrate just how true that is.  Replaying the 2015 election with
        AV gives the Liberals 24% more MPs than deserved based on the popular vote.""")),

        li(p(
          """Some MPs might consider keeping the current 338 local ridings and
        adding 10% - 15% more MPs to give a measure of proportionality. """,
          a(href := "../mmp-15pct/index.html")("These simulations"),
          """ and the table above show, however, that such a tepid response moves us towards proportionality
          but doesn't really get us there.  With similar design parameters (338 MPs in local ridings; 15% more MPs)
          the Rural-Urban Proportional (RUP) model does far better.""")),

        li(p(
          """Simulating swings in voter preferences shows that most of the proportional
        systems remain proportional even as the gap between the first-place and second-place
        finishers increases.  These can be seen in the """,
          a(href := "../rup-15pct/index.html#sensitivity")("Vote Swing Analysis graphs"),
          """ produced on the Summary page for each model as well as the "Average Gallagher Index"
          in the chart above."""),
          p(
            """The Gallagher Index is a measure of disproportionality.  Numbers
          below 5% are good.  The Average Gallagher Index is the average taken across many
          simulations done for the voter swing analysis."""),
          p("""Unfortunately, MMP with only a small top-up layer does not perform well on this measure.""")),

        li(p(
          """All of the parties are currently """, a(href := "../fptp/index.html#subsets")("disadvantaged by FPTP"),
          """.  The Liberals are disadvantaged
        in the prairie provinces while the Conservatives and NDP are disadvantaged in the eastern
        provinces and particularly in the Maritimes.  The Greens, of course, are disadvantaged nearly
        everywhere.  By the same token, a proportional system would advantage each party in different
        areas of the country."""))
      )
    )
  )

  def representationSummary(sims: List[Sim]) = {

    val propDescr = div(
      p("The Representation table focuses on how voters are represented by MP.")
    )


    val colName = cls := "colName"
    val colNumMPs = cls := "colNumMPs right"
    val colPct = cls := "colPct"
    val colArea = cls := "colArea"
    val coldd_d = cls := "coldd_d"

    def dFmtOrBlank(d: Double) = {
      if (d == 0.0) {
        ""
      } else {
        dd_dFmt.format(d)
      }
    }

    def repTable = table(
      thead(
        img(cls := "hdr")(src := "../img/PropertiesTableHeader.svg")
      ),
      tbody(
        tableRows(sims, sim ⇒
          tr(cls := "row")(
            td(colName)(a(href := s"../${sim.params.outDir}/index.html")(sim.params.title)),
            td(colNumMPs)(commaFmt.format(sim.numRidingMPs)),
            td(colNumMPs)(commaFmt.format(sim.numRegionalMPs)),
            td(colNumMPs)(sim.numMPs),

            td(coldd_d)(dd_dFmt.format(sim.avgMPsPerRiding)),
            td(coldd_d)(dFmtOrBlank(sim.avgTopUpMPsPerRegion)),
            td(coldd_d)(dFmtOrBlank(sim.avgTotalMPsPerRegion)),
            td(colArea)(commaFmt.format(sim.avgPopPerLocalMP)),
            td(colArea)(commaFmt.format(sim.medianLocalMPRidingArea)),
            //            fmtGallagher(sim.analysis.gallagherIndex),
            //            fmtGallagher(sim.compositeGallagher),
            td(cls := "shortName")()
          )
        )
      )
    )

    def footnotes = div(cls := "blockIndent footnotes")(
      p("Footnotes"),
      ol(
        li(strong("Number of Local MPs"), " is the total number of MPs representing a specific riding.  " +
          "That riding may be either a single-member riding or a multi-member riding."),
        li(strong("Number of Regional MPs"), " is the total number of MPs that represent multiple ridings. " +
          "This happens in systems with top-up seats such as MMP and RU-PR."),
        li(strong("Number of MPs"), " is the sum of the local and regional MPs, or how many seats in " +
          "Parliament is assumed by this model."),
        li(strong("Average Local MPs/Riding"), " is the average number of MPs representing a local riding.  For" +
          "systems that have single-member ridings everywhere such as FPTP and MMP, it will be 1.0.  " +
          "For systems that have at least some multi-member ridings such as STV and RU-PR it will be larger" +
          "than 1.0."),
        li(strong("Average Top-up Seats/Region"), " is useful for systems like MMP and RU-PR where it gives" +
          " the average number of seats in the top-up region."),
        li(strong("Average Total MPs/Region"), " is the average number of MPs representing a region -- the sum " +
          "of all the local MPs in that region plus the MPs in top-up seats for that region."),
        li(strong("Population/Local MP"), " is the total Canadian population divided by the number of local MPs."),
        li(strong("Area Represented by Median Local MP"), " is a measure of the area covered by a local MP.  In " +
          "this case 50% of the ridings are smaller than the area (given in square kilometeres) and 50% " +
          "of the ridings are larger."),
        li(strong("Short System Name"), " is a very consise ", a(href := "shortName.html")("abbreviation"),
          " of the key parameters for this simulation.  ")
      )
    )



    div(cls := "properties")(
      h2("Representation"),
      propDescr,
      repTable,
      footnotes
    )
  }

  def designDescriptions(sims: List[Sim]) = div(id := "designDescriptions", cls := "modelDescriptions")(
    h2("Riding Design Descriptions"),
    sims.map { s ⇒ s.design }.distinct.sortBy(d ⇒ d.design_name.entryName).map { d ⇒
      div(
        h3(d.design_name.entryName),
        div(cls := "blockIndent")(d.description)
      )
    }
  )

  def electionAlgorithmDescriptions(sims: List[Sim]) = {

    div(id := "electionStratDescr")(
      h2("Election Strategy Descriptions"),
      div(cls := "blockIndent")(
        p("""Election strategies are the specifics of how ballots are counted to determine which candidate fills
        a seat.  Each strategy has three parts:  how single-member ridings are handled, how multi-member ridings
        are handled, and finally how top-up or adjustment seats are handled.""")
      ),

      sims.flatMap { s ⇒ s.design.electionStrategies }.distinct.sortBy(es ⇒ es.entryName).map { es ⇒
        div(
          h3(es.entryName),
          div(cls := "blockIndent")(
            p(strong("Single-Member Ridings: "), es.sm.description),
            p(strong("Multi-Member Ridings: "), es.mm.description),
            p(strong("Top-up or Adjustments: "), es.topup.description)
          )
        )
      }

    )
  }

  //  def descriptions(sims: List[Sim]) = div(cls := "modelDescriptions")(
  //    h2("Descriptions"),
  //    for (param ← sims.map(_.params)) yield {
  //      div(
  //        h3(param.title),
  //        div(cls := "blockIndent")(
  //          p(param.description),
  //          p("Elections in single-member ridings are conducted with ", param.electionStrat.sm.name),
  //          p("Elections in multi-member ridings are conducted with ", param.electionStrat.mm.name)
  //        )
  //      )
  //    }
  //  )

  /**
    * Summarize the parameters for each model
    */
  def modelSummary(sims: List[Sim]) = {

    val header = thead(
      tr(td("Region"),
        td(),
        td("# Tot Seats"),
        td("% Seats"),
        td("Avg # Seats/Region"),
        td("Avg #Reg/Prov"),
        td("Avg Adjust Seats / Region"),
        td()
      ),
      tr(td("Riding"),
        td("Year"),
        td("# Tot Seats"),
        td("% Seats"),
        td("Avg # Seats/Riding"),
        td("% Single"),
        td("% Multiple"),
        td("Comp. Gallagher")
      )
    )

    val body = tbody(
      (for {
        (sim, idx) ← sims.sortBy(s ⇒ (
          s.params.designName.toString,
          s.params.electionStrat.sm.name,
          s.params.electionStrat.mm.name,
          s.params.year
          )).zipWithIndex
        ana = sim.analysis
        parms = sim.params
      } yield {
        val avgAdjSeatsPerRegion = if (parms.electionStrat == ElectionStrategyEnum.RcRUPR) {
          dd_dFmt.format(sim.avgTopUpMPsPerRegion)
        } else {
          ""
        }

        val rowCls = if (idx % 2 == 0) {"even"} else {"odd"}

        Seq(
          tr(cls := rowCls + " topRow")(
            td(cls := "name")(a(href := s"../${sim.params.outDir}/index.html")(sim.design.design_name.entryName)),
            td(),
            td(sim.numRegionalMPs),
            td(pctFmt.format(sim.numRegionalMPs / sim.numMPs.toDouble)),
            td(dd_dFmt.format(sim.avgTotalMPsPerRegion)),
            td(dd_dFmt.format(sim.avgRegionsPerProv)),
            td(avgAdjSeatsPerRegion),
            td()
          ),
          tr(cls := rowCls + " bottomRow")(
            td(parms.electionStrat.entryName),
            td(parms.year),
            td(sim.numRidingMPs),
            td(pctFmt.format(sim.numRidingMPs / sim.numMPs.toDouble)),
            td(dd_dFmt.format(sim.avgMPsPerRiding)),
            td(pctFmt.format(sim.pctSingleMbrSeats)),
            td(pctFmt.format(sim.pctMultiMbrSeats)),
            fmtGallagher(sim.compositeGallagher)
          )
        )
      })
    )

    div(id := "modelSummary", cls := "modelSummary")(
      h2("Model Summary"),
      div(cls := "blockIndent")(
        p("""Proportional electoral systems have many design parameters that can be tweaked.  This table has two
        rows for each model.  The bottom row applies to the riding; the top row applies to the region."""),
        p("""The first column of that table gives the name of the riding design (top) and the election algorithm
        used and the year of the election it's based on (bottom).  The riding design specifies a particular
        mapping from old (e.g. 2015) ridings to new
        ridings, how the new ridings are gathered into regions, and finally how the regions are gathered
        by province.  Riding designs are described in more detail at the bottom of this page and by following
        the riding design link.""")
      ),
      table(width := "100%")(header, body)
    )
  }


  protected def toc = div(cls := "blockIndent")(p("This page:"),
    ul(
      li(a(href := "#proportionality")("Proportionality of each model")),
      li(a(href := "#modelSummary")("Summary of each model's parameters")),
      li(a(href := "#designDescriptions")("Riding Design Descriptions")),
      li(a(href := "#electionStratDescr")("Election Strategy Descriptions"))
    )
  )


}


/** ************************************
  *
  * ************************************/
case class OverviewFeaturedHTML(sims: List[Sim],
                                numAllSims: Int,
                                val pgTitle: String,
                                val outFile: String) extends AbstractOverview {

  protected def content: TypedTag[String] = {
    div(cls := "overview")(
      introduction,
      resultsTable(sims),
      observations,
      modelSummary(sims),
      representationSummary(this.sims),
      designDescriptions(this.sims),
      electionAlgorithmDescriptions(this.sims),
      script("""new Tablesort(document.getElementById('overview'));""")
    )
  }

  private val whatsNew =
    div(cls := "whatsNew")(
      h3("What's New"),
      p(strong("2016-10-05"), ": Added a few simulations based on 2006, 2008, and 2011 elections. " +
        "See the second table on the ",
        a(href := "allSimulations.html")("All Systems"),
        " page."),
      p(strong("2016-10-12"), ": Added AV+ to the list of featured systems.  Several changes to the Vote " +
        "Swing Analysis graphs. Adjusted the Summary Statistics to account for constitutional considerations on " +
        "provinces for the Proportional MPs column."),
      p(strong("2016-10-30"), ": Added a ", a(href := "erre.html")("number of systems"),
        " at the request of the ERRE Committee that meet a specific set of constraints."),
      p(strong("2016-10-31"), ": Added an ", strong("exciting new system"), ", ",
        a(href := "../ru_multiples_rc2/index.html")("Riding-Centric Rural Urban Proportional"),
        ".  Follow the link to read it's many advantages."),
      p(strong("2017-02-26"), ": Added a made-in-Canada model that offers many advantages. See ",
        a(href := "../LPR_no_topup/index.html")("Local Proportional Representation (no topups)"), " and ",
        a(href := "../LPR_with_topups/index.html")("Local Proportional Represenation (with topups)"), " for ",
        "more details."
      )
    )


  private val introduction = div(
    h2("Introduction"),
    div(cls := "blockIndent")(
      whatsNew,
      p(
        s"""This web site presents the results of simulating ${numAllSims} electoral systems.
      Others have done individual simulations of their favourite systems, but this is believed
      to be the first which uses a consistent approach on so many.  This page features the
      ${sims.length} systems I view as most interesting.  For the """,
        a(href := "allSimulations.html")(s"full list of ${numAllSims} simulations"), """, please see the "All Systems"
        item in the "Overview" menu, above. """),
      p(s"""Each of these ${sims.length} systems is described at the bottom of this page."""),
      p(
        """There is a lot of data here.  It's not for the faint of heart.  Perhaps the best
        way to dive in is to start with the associated """,
        a(href := "../ModellingElections_en.pdf")("submission"), " to the Parlimentary Electoral Reform Committee."),
      p("""With respect to the Committee's first principle of Effectiveness and Legitimacy, the recommendations
        based on this modelling is that:""",
        ul(
          li("the Committee issue a preliminary report stating that the Alternative Vote would be a step backward" +
            " from FPTP and should not be considered further;"),
          li("the Committee strongly consider Rural-Urban PR, a highly proportional, made-in-Canada system that " +
            "effectively deals with our diverse riding sizes;"),
          li("if choosing STV, the Committee think carefully about whether having smaller multi-member ridings is" +
            " worth the decreased proportionality;"),
          li("if choosing MMP, the Committee should stipulate that FPTP (rather than AV) continues to be used in " +
            "the local riding elections;"),
          li("the Committee avoid MMP-Lite’s substantial increase in complexity for very little gain in " +
            "proportionality.")
        )),

      p(
        """It should be noted, of course, that all simulations make assumptions and that any change
      to the voting system would change how people vote.  So actual results will surely be
      somewhat different from any simulation.  Nevertheless, these simulations show us
      important properties about the systems we are considering that can help guide our decision-making.""")
    )
  )


}

/**
  * Created by bwbecker on 2016-06-29.
  */
case class OverviewSpecifiedSystemsHTML(sims: List[Sim],
                                        val pgTitle: String,
                                        val outFile: String,
                                        intro: TypedTag[String]
                                       ) extends AbstractOverview {

  protected def content: TypedTag[String] = {
    div(cls := "overview")(
      intro,
      toc,
      resultsTable(this.sims),
      modelSummary(sims),
      designDescriptions(this.sims),
      electionAlgorithmDescriptions(this.sims),
      script("""new Tablesort(document.getElementById('overview'));""")
    )
  }


}
