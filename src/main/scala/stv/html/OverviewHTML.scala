package stv.html

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._
import stv.Analysis.StatsByParty
import stv.Party._

/**
  * Created by bwbecker on 2016-06-29.
  */
case class OverviewHTML(sims: List[Sim], val pgTitle: String, val outFile: String) extends Page {
  protected val outDir = "overview"
  //  protected val outFile: String = "index.html"
  //  protected val pgTitle = "Overview of Simulations"
  override protected val includeThisModelMenu = false

  protected def content: TypedTag[String] = {
    div(cls := "overview")(
      comments,
      results,
      observations,
      properties,
      descriptions
    )
  }

  private val comments = div(
    h2("Comments"),
    div(cls := "blockIndent")(
      p(
        s"""This web site presents the results of simulating ${sims.length} electoral systems.
      Others have done individual simulations of their favourite systems, but this is believed
      to be the first which uses a consistent approach on so many."""),
      p(s"""Each of these ${sims.length} systems is described at the bottom of this page."""),
      p(
        """There is a lot of data here.  It's not for the faint of heart.  However, the major
      findings are presented immediately below in the """, em("Results"),
        """ table and the """, em("Observations"), " that follow it."),

      p(
        """Briefly, these simulations show that:""",
        ul(
          li("FPTP can be very unfair and AV (Alternative Vote) is even worse."),
          li("""Keeping our current ridings and adding a small layer of "top-up" MPs doesn't help much."""),
          li(
            """Single Transferable Vote (STV) and Mixed Member Proportional (MMP) can both perform
          very well provided there are enough MPs in either the multi-member ridings (STV) or the regional
          top-up layer (MMP)."""),
          li(
            """A combination of STV and MMP, called "Rural-Urban Proportional" or
          RUP, performs as well as STV and MMP but permits continued use of single-member
          ridings outside of large urban areas.""")
        )
      ),

      p(
        """It should be noted, of course, that all simulations make assumptions and that any change
      to the voting system would change how people vote.  So actual results will almost surely be
      somewhat different from any simulation.  Nevertheless, these simulations show us
      important properties about the systems we are considering that can help guide our decision-making.""")
    )
  )

  val pctFmt = java.text.NumberFormat.getPercentInstance
  val pct_dFmt = new java.text.DecimalFormat("#0.0%")
  val commaFmt = new java.text.DecimalFormat("###,###,###")
  val dd_dFmt = new java.text.DecimalFormat("##0.0")


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


  def results = {

    def color(tGreen: Double, tYellow: Double)(v: Double): String = {
      if (Math.abs(v) < tGreen) "green" else if (Math.abs(v) < tYellow) "yellow" else "red"
    }
    def fmtOverRep(s: StatsByParty) = {
      val pct = s.pctMPs - s.pctVote
      val warn = color(0.05, 0.10)(pct)
      td(cls := "colPct0 " + warn)(pctFmt.format(pct))
    }

    def fmtGallagher(g: Double) = {
      td(cls := "colPct1 " + color(0.05, 0.10)(g))(pct_dFmt.format(g))
    }

    def fmtPrefParty(pct: Double) = {
      val color = if (pct < 0.50) "red"
      else if (pct < 0.75) "yellow"
      else "green"
      td(cls := "colPct0 " + color)(pctFmt.format(pct))
    }

    def fmtTable = table(
      thead(
        img(cls := "hdr")(src := "../img/ResultsTableHeader.svg")
      ),
      tbody(
        tableRows(sims, sim => {
          val statsByParty: Map[Party, StatsByParty] = sim.analysis.statsByParty.map(s ⇒ (s.party, s)).toMap

          tr(cls := "row")(
            td(cls := "name")(
              a(href := s"../${sim.params.outDir}/index.html")(raw(sim.params.title.replace("(", "<br>(")))),
            td(cls := "num3")(sim.numRidingMPs),
            td(cls := "num3")(sim.numRegionalMPs),
            fmtOverRep(statsByParty(Lib)),
            fmtOverRep(statsByParty(Con)),
            fmtOverRep(statsByParty(NDP)),
            fmtOverRep(statsByParty(Bloc)),
            fmtOverRep(statsByParty(Grn)),
            fmtGallagher(sim.analysis.gallagherIndex),
            fmtGallagher(sim.compositeGallagher),
            fmtPrefParty(sim.pctVotersWithPreferredPartyLocally),
            fmtPrefParty(sim.pctVotersWithPreferredPartyRegionally),
            td(cls := "shortName")(sim.shortName)
          )
        }
        )
      )
    )

    def footnotes = div(cls := "footnotes")(
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


    div(cls := "results")(
      h2("Fairness"),
      fmtTable,
      footnotes
    )
  }


  def descriptions = div(cls := "modelDescriptions")(
    h2("Descriptions"),
    for (param ← sims.map(_.params)) yield {
      div(
        h3(param.title),
        param.description
      )
    }
  )

  def properties = {

    val propDescr = div(
      p("The Representation table focuses on how voters are represented by MP.")


      //      p(
      //        s"""This table summarizes two groups of properties about each of the ${
      //          sims.length
      //        } simulated systems."""),
      //      ul(
      //        li(strong("MPs:"),
      //          """ This part of the table shows how many MPs each model assumes and how they
      //  are distributed between local ridings and regional top-up MPs."""),
      //
      //        li(strong("District Magnitude:"),
      //          """ This shows how many MPs, on average, share a riding.  This will
      //  be 1 for FPTP, AV, and MMP;  somewhat larger for systems like STV and RUP.  It also shows the
      //  average number of top-up MPs in a region for MMP and RUP systems.""")
      //      )
    )


    val colName = cls := "colName"
    val colNumMPs = cls := "colNumMPs right"
    val colPct = cls := "colPct"
    val colArea = cls := "colArea"
    val coldd_d = cls := "coldd_d"

    def propertiesTable = table(
      thead(
        img(cls := "hdr")(src := "../img/PropertiesTableHeader.svg")
      ),
      tbody(
        tableRows(sims, sim ⇒
          tr(
            td(colName)(a(href := s"../${
              sim.params.outDir
            }/index.html")(sim.params.title)),
            td(colNumMPs)(sim.numRidingMPs),
            td(colNumMPs)(sim.numRegionalMPs),
            td(colNumMPs)(sim.numMPs),

            td(coldd_d)(dd_dFmt.format(sim.avgMPsPerRiding)),
            td(coldd_d)(dd_dFmt.format(sim.avgTopUpMPsPerRegion)),
            td(coldd_d)(dd_dFmt.format(sim.avgTotalMPsPerRegion)),
            td(colArea)(commaFmt.format(sim.avgPopPerLocalMP)),
            td(colArea)(commaFmt.format(sim.medianLocalMPRidingArea)),
            td(cls := "shortName")(sim.shortName)
          )
        )
      )
    )

    def footnotes = div(cls := "footnotes")(
      p("Footnotes"),
      ol(
        li(strong("Number of Local MPs"), " is the total number of MPs representing a specific riding.  " +
          "That riding may be either a single-member riding or a multi-member riding."),
        li(strong("Number of Regional MPs"), " is the total number of MPs that represent multiple ridings. " +
          "This happens in systems with top-up seats such as MMP."),
        li(strong("Number of MPs"), " is the sum of hte local and regional MPs, or how many seats in " +
          "Parliament is assumed by this model.")
      )
    )



    div(cls := "properties")(
      h2("Representation"),
      propDescr,
      propertiesTable,
      footnotes
    )
  }


  /**
    * Turn a list of sims into table rows, properly sorted, using function f.
    *
    * @param sims
    * @param f
    * @return
    */
  def tableRows(sims: Seq[Sim], f: Sim => TypedTag[String]): Seq[TypedTag[String]] = {
    for (sim <- sims.sortBy(s => (s.design.design_name.entryName,
      s.params.singleMemberElectionStrategy.name,
      s.params.multiMemberElectionStrategy.name))) yield (f(sim))
  }
}
