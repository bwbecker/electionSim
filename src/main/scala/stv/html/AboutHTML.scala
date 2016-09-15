package stv.html

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.{Sim, Params}

/**
 * Created by bwbecker on 2016-07-04.
 */
case class AboutHTML(sim:Option[Sim]) extends Page {

  val outDir = sim.map(_.params.outDir).getOrElse("overview")
  val outFile = "about.html"
  val pgTitle = "About Elections Modelling"
  override protected val includeThisModelMenu = sim.isDefined


  protected def content: TypedTag[String] = {
    div(
      h2("What it is"),
      p("""These pages are the output of a program that simulates a electoral systems.
          It can accomodate models that have ridings with 1 or more MPs and regions with
          0 or more "top-up" seats assigned to clusters of ridings.  That is, it can model:
        """,
        ul(
          li("First-Past-The-Post: 1 MP in each riding, no top-up seats"),
          li("Alternative Vote: 1 MP in each riding, no top-up seats"),
          li("Mixed Member Proportional: 1 MP in each riding with top-up seats"),
          li("Single Transferable Vote: multiple MPs in each riding, no top-up seats"),
          li("Hybrid models: 1 or more MPs in each riding with top-up seats.")
        )

      ),
      h2("How it works"),
      p("""The overall model is:""",
        ol(li("""Group existing ridings into new ridings, each with an
          assigned district magnitude (which may be as low as 1)"""),
          li("""Group the new ridings into compensation regions, giving each a
            number of compensatory or "top-up" seats (which may be as low as 0)"""),
          li("""Run an STV election in each of the ridings to determine the riding MPs"""),
          li("""For each region, determine which parties should be assigned the top-up seats."""))),


      p("""The STV election in step 3 has two alternatives.  Suppose there are several Liberals running
      in the same riding and it is determined that one is eliminated.  That candidate's votes are all
      split equally between the other Liberal candidates.
      """),

      p("""On the other hand, suppose that the last remaining Liberal is being eliminated.  In that
      case we use a "transfer function" that determines how votes are transferred to other candidates.
      In a FPTP election, there are no transfers.  In AV and STV elections, transfers are based on
      the best polling data we could find -- an """,
        a(href := "http://www.ekospolitics.com/wp-content/uploads/full_report_october_15_2015.pdf")("Ekos poll"),
      """ taken immediately before the 2015 election which asked about second choice preferences.  No
      attempt is made to correct for 3rd or 4th choices; the 2nd choice statistics are always used."""),

      h2("Data it produces"),
      p("The program produces the following results:"),
      ul(
        li(a(href := "index.html")("Summary"), """:  Summary statistics on overall
          proportionality, a unique vote swing analysis, statistics on various subsets of ridings
          (the East, the West, the Praries, etc), the distribution of area of the various multi-member ridings,
          distribution of district magnitudes, and how votes transfer between parties."""
        ),

        li(a(href := "ridingResults.html")("Riding Results"), """: The simulated results for each of the
        new ridings.  It shows which of the existing ridings were combined (if any) to form the new
        riding, who the 2015 candidates were that ran in that area, how many votes they received, and
        finally, who won the simulated election.""",
        p("When existing ridings are combined into new ridings, an existing riding may be split. ",
          a(href:= "../rup-338/ridingResults.html")("In this example"), """ the Alberta riding of
          Grande Prairie-Mackenzie was split between 5 different ridings, including Battle River-Crowfoot
          and Fort McMurray-Cold Lake.  The Conservative candidate from Grande Prairie-Mackenzie
           (Chris Warkentin) appears
          as a candidate in all five of the new ridings, but with only 20% of the votes he received
          in his original riding.  Obviously, Mr. Warkentin is not going to win any of the new ridings.
          But those votes are accounted for in the model.""")),

        li(a(href := "ridingStats.html")("Riding Statistics"), """: For each of the new ridings, this
        page gives the number of MPs, the area of the new riding in km""", sup("2"), """ the
        population, and the population per MP.  Averages are given at the bottom."""),

        li(a(href := "regionResults.html")("Region Results"), """: New ridings are gathered into
        regions which may (or may not) have top-up seats to help increase proportionality.  This
        page reports on the composition of those regions, the parties that won the top-up seats,
        and party statistics for the region, both with and without the top-up seats.""",
        p("""For models that don't have regions (FPTP, AV, STV), the second set of statistics are
        not meaningful and are not produced.  In these cases the regions """)),
        li(a(href := "")(""), """: """),
        li(a(href := "")(""), """: """),
        li(a(href := "")(""), """: """),
        li(a(href := "")(""), """: """),

        li(a(href := "riding.html")("FPTP Ridings"), """:  A summary of
          the 2015 FPTP election, riding by riding, listing each candidate, their
          party affiliation, and the number of votes received.  It also says which multi-member
          riding this riding has been assigned to.  It is possible to split an existing
          riding between several multi-member ridings, and so each one has an attached
          percentage."""),

        li(a(href := "newRidings.html")("STV Ridings"), """:  A summary of the
          new ridings -- the candidates in that riding, their party affiliation,
          and which candidate(s) won under the STV election for that riding."""),

        li(a(href := "regions.html")("Regions Summary"), """:  A summary of the
          compensation regions with and without the top-up MPs. """)
      ),

      h2("Program Inputs"),
      ul(
        li("The 2015 federal election results by candidate.",
          pre(cls := "input")("""riding_id,candidate_name,party_id,incumbent,elected,votes
10001,"Barnett, Lorraine E.",Con,f,f,4670
10001,"Byrne-Puumala, Krista",Grn,f,f,228
10001,"Andrews, Scott",Ind,t,f,7462
10001,"McDonald, Ken",Lib,f,t,23528
"""),
          p("""This input data typically does not change. However, one could
              change it to simulate
              models with data from previous elections.  Doing so would require some
              modifications to the program.""")
        ),

        li("The 2015 federal ridings.",
          pre(cls := "input")("""riding_id,prov,name,population,electors,area
10001,NL,Avalon,81540,66653,7303
10002,NL,Bonavista-Burin-Trinity,76704,61088,18961
10003,NL,Coast of Bays-Central-Notre Dame,78092,63621,43596
"""),
          p("This input data typically does not change.")
        ),

        li(
          "The mapping of existing ridings to new ridings.",
          pre(cls := "input")("""model,riding_id,pr_riding_id,pct
bwb_hybrid,48034,AB.34,100
bwb_hybrid,48005,AB.CalN,100
bwb_hybrid,48009,AB.CalN,100
bwb_hybrid,48010,AB.CalN,50
bwb_hybrid,48010,AB.CalS,50
"""),
          p("""The first field ("bwb_hybrid" in this example) is an identifier for
            the election model being run.  The second field is the Elections Canada identifier
            for an existing riding.  It must be one of the numbers in the first column of
            the ridings input.  The third column is an identifier for the new multi-member
            riding.  It's simply a string.  My convention has been to use the two letter abbreviation
            for the province (constitutionally, the new ridings can't cross provincial boundaries), a
            dot, and then either a number for singleton ridings or a very brief descriptive
            string for multi-member ridings.  The last field is the percentage of the 2015
            riding assigned to the given multi-member riding.""")
        ),

        li("Information about each region",
          pre(cls := "input")("""model,region_id,top_up_seats
bwb_hybrid,AB_R01,3
bwb_hybrid,ON_R01,3
bwb_hybrid,ON_R03,3
"""),
          p("""This is just how many top-up seats each region receives.  If this value
            is consistently 0, the result is a pure STV election simulation.""")
        ),

        li("The mapping of multi-member ridings to compensation regions.",
          pre(cls := "input")("""model,region_id,pr_riding_id
bwb_hybrid,AB_R02,AB.34
bwb_hybrid,AB_R01,AB.CalN
bwb_hybrid,AB_R01,AB.CalN
        """),
          p("""The second field is an identifier for a region and must appear in the regions
            input file.  The third is the identifier for a multi-member region. """)
        ),

        li("""In principle, the ratios used to transfer votes between candidates in the
          STV elections should be an input, but at the moment it's hard-coded.""")
      ),

      h2("Technical Details"),
      p("""The input data is loaded into a database to make generating some of the stats
        easier.  The program itself is written in Scala."""),
      p("""Given the dependence on a database
        and an unusual programming language, running it is not for the technically faint-of-heart.
        If you provide the input files, as described above, I'd be happy to include it here.
        """),

      h2(id := "contact")("Contact"),
      p(a(href := "mailto:mail@election-modelling.ca")("Byron Weber Becker"), "(mail@election-modelling.ca)")
    )
  }

}
