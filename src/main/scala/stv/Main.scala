package stv

import scalatags.Text
import scalatags.Text.all._
import ca.bwbecker.io.CachedMkdir
//import stv.html.OverviewHTML
import stv.io.{Input, Output}


/**
  * Created by bwbecker on 2016-05-29.
  */
object Main {

  val dbService = "election"

  val outdir = "html"

  val fptpDescr = div(
    p(a(href := "https://en.wikipedia.org/wiki/First-past-the-post_voting")("First-Past-The-Post"),
      """ (FPTP) is the electoral system that Canada uses now.  In each riding,
      the candidate with the most votes becomes the MP.  There are no regions or top-up seats.""")
  )

  val avDescr = div(
    p(a(href := "https://en.wikipedia.org/wiki/Instant-runoff_voting")("Alternative Vote"),
      """ (AV aka Instant-Runoff Voting) is a model which retains single-member ridings but elects the MP using
      a ranked ballot.  Ballots are counted by repeatedly removing the candidate with the fewest votes
       and transferring their votes to the remaining candidates based on each voter's ranking
      on their ballot.  This process ends when one candidate has at least 50% + 1 of the votes.""")
  )

  val stvMedDescr = div(
    p(
      """A classic """,
      a(href := "https://en.wikipedia.org/wiki/Single_transferable_vote")("Single Transferable Vote"),
      """ (STV) model with moderate-sized ridings of about 12 members
      each.  An STV system combines our existing single-member ridings into larger ridings with multiple
      members.  The total number of MPs would remain at 338."""),
    p(
      """Voters use a ranked ballot.  Their vote is initially allocated to their top-ranked candidate.
      As the count proceeds and it becomes clear that a candidate is assured victory or assured defeat,
      their excess votes are transferred to the remaining candidates.  This proceeds until all positions
      have been filled."""),
    p(
      """The grouping of existing ridings into multi-member ridings is based on work by """,
      a(href := "http://mech.ubc.ca/antony-hodgson/")("Antony Hodgson"),
      """, President of """, a(href := "https://fairvotingbc.com/")("Fair Voting BC"), ".")
  )

  val stvSmallDescr = div(
    p(
      """A classic """,
      a(href := "https://en.wikipedia.org/wiki/Single_transferable_vote")("Single Transferable Vote"),
      """ (STV) model with small ridings averaging just over 4 members
      each.  An STV system combines our existing single-member ridings into larger ridings with multiple
      members.  The total number of MPs would remain at 338."""),
    p(
      """Voters use a ranked ballot.  Their vote is initially allocated to their top-ranked candidate.
      As the count proceeds and it becomes clear that a candidate is assured victory or assured defeat,
      their excess votes are transferred to the remaining candidates.  This proceeds until all positions
      have been filled."""),
    p(
      """The grouping of existing ridings into multi-member ridings is based on work by """,
      a(href := "http://mech.ubc.ca/antony-hodgson/")("Antony Hodgson"),
      """, President of """, a(href := "https://fairvotingbc.com/")("Fair Voting BC"), ".")

  )

  val stvPlusDescr = div(
    p(
      """This model is the same as STV (Small Regions) except that a layer of top-up seats has been added
        |to increase proportionality.""".stripMargin),
    p(
      """The grouping of existing ridings into multi-member ridings is based on work by """,
      a(href := "http://mech.ubc.ca/antony-hodgson/")("Antony Hodgson"),
      """, President of """, a(href := "https://fairvotingbc.com/")("Fair Voting BC"), ".")

  )

  val mmp8avDescr = div(
    p("A ", a(href := "https://en.wikipedia.org/wiki/Mixed-member_proportional_representation")
    ("Mixed Member Proportional"),
      """ (MMP) model has single member ridings gathered
    into regions.  The "small regions" in this model's title indicates that these regions are
    composed of about 8 ridings, on average."""),
    p(
      """ Each region has a set of "top-up" or "compensatory" seats filled so that
          proportionality between the popular vote and the party affiliations of MPs
          is maintained."""),
    p(
      """ This simulation is based upon """,
      a(href := "http://wilfday.blogspot.ca/")("Wilf Day's work"),
      """ of dividing the ridings into
      regions and assigning the number of top - up MPs to each region.  It uses a
      ranked ballot in each riding (Alternative Vote) to select the local MP.  Other
      simulations use FPTP to choose the local MPs and still other simulations use
      larger regions.""")
  )

  val mmp8fptpDescr = div(
    p("A ", a(href := "https://en.wikipedia.org/wiki/Mixed-member_proportional_representation")
    ("Mixed Member Proportional"),
      """ (MMP) model has single member ridings gathered
      into regions.The "small regions" in this model's title indicates that these regions are
        composed of about 8 ridings, on average."""),
    p(
      """ Each region has a set of "top-up" or "compensatory" seats filled so that
          proportionality between the popular vote and the party affiliations of MPs
          is maintained."""),
    p(
      """ This simulation is based upon """,
      a(href := "http://wilfday.blogspot.ca/")("Wilf Day's work"),
      """ of dividing the ridings into
      regions and assigning the number of top - up MPs to each region. It uses
      a familiar First-Past-The-Post (FPTP) election in each riding to select the local MP. Other
      simulations use a ranked ballot (Alternative Vote) to choose the local MPs and
      still other simulations use
      larger regions.""")
  )

  val mmp14avDescr = div(
    p("A ", a(href := "https://en.wikipedia.org/wiki/Mixed-member_proportional_representation")
    ("Mixed Member Proportional"),
      """ (MMP) model has single member ridings gathered
      into regions.  The "Medium Regions" in this models title indicates that these regions are
      composed of about 14 ridings, on average."""),
    p(
      """ Each region has a set of "top-up" or "compensatory" seats filled so that
          proportionality between the popular vote and the party affiliations of MPs
          is maintained."""),
    p(
      """ This simulation is based upon """,
      a(href := "http://wilfday.blogspot.ca/")("Wilf Day's work"),
      """ of dividing the ridings into
      regions and assigning the number of top-up MPs to each region.It uses a
      ranked ballot in each riding(Alternative Vote) to select the local MP.Other
      simulations use FPTP to choose the local MPs and still other simulations use
      smaller regions.""")
  )

  val mmp14fptpDescr = div(
    p("A ", a(href := "https://en.wikipedia.org/wiki/Mixed-member_proportional_representation")
    ("Mixed Member Proportional"),
      """ (MMP) model has single member ridings gathered
      into regions.The "14" in this models title indicates that these regions are
      composed of about 14 ridings, on average."""),
    p(
      """ Each region has a set of "top-up" or "compensatory" seats filled so that
          proportionality between the popular vote and the party affiliations of MPs
          is maintained."""),
    p(
      """ This simulation is based upon """,
      a(href := "http://wilfday.blogspot.ca/")("Wilf Day's work"),
      """ of dividing the ridings into
      regions and assigning the number of top-up MPs to each region. It uses
      a familiar First-Past-The-Post (FPTP) election in each riding to select the local MP. Other
      simulations use a ranked ballot (Alternative Vote) to choose the local MPs and
      still other simulations use
      smaller regions.""")
  )

  val mmpLiteDescr = div(
    p(
      """Some MPs are apparently talking about an "MMP lite" approach
      in which our existing 338 ridings are kept intact but the House is enlarged by
      10 - 15% to provide top-up seats that bring a measure of proportionality."""),
    p(
      """ This simulation is based upon """,
      a(href := "http://wilfday.blogspot.ca/")("Wilf Day's work"),
      """ of dividing the ridings into
      regions and assigning the number of top-up MPs to each region.  It uses
      a familiar First-Past-The-Post (FPTP) election in each riding to select the local MP.""")
  )

  val rup338Descr =
    div(
      p("The ", a(href := "../RuralUrbanProportional_20160706.pdf")("Rural-Urban Proportional"),
        """ (RU-PR) model uses multi-member ridings in metropolitan
        areas and single member ridings in non-metropolitan and rural areas.  It adds a
        small layer of top-up MPs. Together, these two techniques yield excellent
        proportionality across a wide range of electoral results."""),

      p(
        """ This version of RUP enlarges ridings by about 15% to keep the total number
        of MPs in the House of Commons at the current 338. """,
        a(href := "../rup-15pct/index.html")("Another simulation"),
        """ of this
        model adds 15% more MPs to keep the current ridings the same size.""")
    )

  val rup338ListDescr =
    div(
      p("The ", a(href := "../RuralUrbanProportional_20160706.pdf")("Rural-Urban Proportional"),
        """ (RUP) model uses multi-member ridings in metropolitan
        areas and single member ridings in non-metropolitan and rural areas.  It adds a
        small layer of top-up MPs. Together, these two techniques yield excellent
        proportionality across a wide range of electoral results."""),

      p("Uses ListPR to determine winners."),

      p(
        """ This version of RUP enlarges ridings by about 15% to keep the total number
        of MPs in the House of Commons at the current 338. """,
        a(href := "../rup-15pct/index.html")("Another simulation"),
        """ of this
        model adds 15% more MPs to keep the current ridings the same size.""")
    )

  val rup15PctDescr = div(
    p("The ", a(href := "../RuralUrbanProportional_20160706.pdf")("Rural-Urban Proportional"),
      """ (RUP) model uses multi-member ridings in metropolitan
      areas and single member ridings in non-metropolitan and rural areas. It adds a
      small layer of top-up MPs. Together, these two techniques yield excellent
      proportionality across a wide range of electoral results."""),

    p(
      """This version of RUP enlarges the House of Commons by about 15% (about
      50 additional MPs) so that ridings remain at their current size. """,
      a(href := "../rup-338/index.html")("Another simulation"),
      """ of this
      model enlarges the ridings to keep the number of MPs constant at 338.""")
  )



  val featuredSystems = List(
    /*
      Params("bwb_hybrid2013", "Hybrid 2013 w/ 2nd Choice Data", "bwb_hybrid", "html/bwb_hybrid_2013",
        p("Hybrid model based on 2015 results with 2013 transfer probabilities."),
        XferProb2013),

      Params("bwb_hybrid2015", "Hybrid w/ Ekos Choice Data", "bwb_hybrid", "html/bwb_hybrid_2015",
        p("Hybrid model based on 2015 results with Ekos transfer probabilities."),
        Ekos),

      Params("mmp_av", "Mixed Member Proportional (AV)", "mmp", "html/mmp_av",
        p("MMP simulated with 2015 election results and AV in the ridings."),
        XferProbLeger2015),

      Params("mmp_fptp", "Mixed Member Proportional (FPTP)", "mmp", "html/mmp_fptp",
        p("MMP simulated with 2015 election results and FPTP in the ridings."),
        XferProbFPTP),
    */

    Params("fptp", "First-Past-The-Post", "FPTP",
      DesignName.fptp, s"fptp", fptpDescr, FptpElectionStrategy, NotApplicableElectionStrategy),

    Params("av", "Alternative Vote", "AV",
      DesignName.fptp, s"av", avDescr, AvElectionStrategy, NotApplicableElectionStrategy),

    Params("stv_med", "Single Transferable Vote (Medium-sized Regions)", "STV (Medium Regions)",
      DesignName.stv_med, s"stv_med", stvMedDescr, AvElectionStrategy, EkosStvElectionStrategy),

    Params("stv_small", "Single Transferable Vote (Small Regions)", "STV (Small Regions)",
      DesignName.stv_small, s"stv_small", stvSmallDescr, AvElectionStrategy, EkosStvElectionStrategy),


    Params("mmp-8-fptp", "Mixed Member Proportional (Small Regions)", "MMP (Small Regions)",
      DesignName.mmp_small, s"mmp-8-fptp", mmp8fptpDescr, FptpElectionStrategy, NotApplicableElectionStrategy),


    Params("mmpLite", "Mixed Member Proportional (Lite)", "MMP-Lite",
      DesignName.mmp_enlargeP, s"mmp-15pct", mmpLiteDescr, FptpElectionStrategy, NotApplicableElectionStrategy),

    Params("rup-338", "Rural-Urban PR (More Singles, 338 Seats)", "RU-PR (More Singles, 338 Seats)",
      DesignName.ru_singles, s"rup-338", rup338Descr, AvElectionStrategy, ListElectionStrategy),

    Params("rup-15pct", "Rural-Urban PR (More Singles, 389 Seats)", "RU-PR (More Singles, 389 Seats)",
      DesignName.ru_enlargeP, s"rup-15pct", rup15PctDescr, FptpElectionStrategy, EkosStvElectionStrategy),

    Params("rup-stv", "Rural-Urban PR (Few Singles)", "RU-PR (Few Singles, 390 Seats)",
      DesignName.ru_multiples, s"rup-stv", stvPlusDescr, AvElectionStrategy, EkosStvElectionStrategy)
  )

  val variantSystems = List(
    Params("mmp-8-av", "MMP (Small Regions, AV)", "MMP-Small-AV",
      DesignName.mmp_small, s"mmp-8-av", mmp8avDescr, AvElectionStrategy, EkosStvElectionStrategy),
    Params("mmp-14-av", "MMP (Medium Regions, AV)", "MMP-Medium-AV",
      DesignName.mmp_med, s"mmp-14-av", mmp14avDescr, AvElectionStrategy, EkosStvElectionStrategy),

    Params("mmp-14-fptp", "MMP (Medium Regions, FPTP)", "MMP-Medium-FPTP",
      DesignName.mmp_med, s"mmp-14-fptp", mmp14fptpDescr, FptpElectionStrategy, NotApplicableElectionStrategy),

    Params("rup-338-list", "Rural-Urban PR (More Singles, 338 Seats, ListPR)", "RU-PR (More Singles, 338 Seats, ListPR)",
      DesignName.ru_singles, s"rup-338-list", rup338ListDescr, FptpElectionStrategy, ListElectionStrategy),

    Params("rup-15pct-stv", "Rural-Urban PR (More Singles, More Seats)", "RU-PR (More Singles, More Seats)",
      DesignName.ru_enlargeP, s"rup-15pct-stv", rup15PctDescr, AvElectionStrategy, EkosStvElectionStrategy)

  )

  val namedSystems = featuredSystems ++ variantSystems


  val usage = s""" Usage: sbt run ${("all" :: featuredSystems.map(_.name)).mkString("[", "|", "]")} """

  def main(args: Array[String]): Unit = {

    def doOne(p: Params): Sim = {
      println(s"Simulating ${p.name} ")

      val sim = Input.getSim(p)
      Output.writeHtml(p, sim)
      sim
    }


    def doDesign(d: DesignName): Vector[Sim] = {
      val design = Input.readDesign(d)

      def singleMbrStrategies: Vector[ElectionStrategy] = {
        if (design.hasSingleMemberRidings) {
          ElectionStrategy.singleMbrStrategies
        } else {
          Vector(NotApplicableElectionStrategy)
        }
      }

      def multiMbrStrategies: Vector[ElectionStrategy] = if (design.hasMultiMemberRidings) {
        ElectionStrategy.multiMbrStrategies
      } else {
        Vector(NotApplicableElectionStrategy)
      }

      println(s"Simulating design ${d}.")

      for {
        sms <- singleMbrStrategies
        mms <- multiMbrStrategies
      } yield {

        val params = namedSystems.find(p => p.designName == d &&
          p.multiMemberElectionStrategy == mms &&
          p.singleMemberElectionStrategy == sms)
          .getOrElse {
            val name = s"${d}-${sms.name}-${mms.name}"
            Params(name, name, name, d, name, p("Description"), sms, mms)
          }

        val sim = Sim(design.transform(params), params)
        Output.writeHtml(params, sim)

        sim
      }

    }


    if (args.length != 1) {
      println(usage)
    } else if (args(0) == "all") {
      val sims = for (d ← DesignName.values) yield {
        doDesign(d)
      }
      Output.writeOverview(sims.flatten.toList)
      Output.copyLess(this.outdir)
    } else if (args(0) == "test") {
      val sims = for (d <- List(DesignName.ru_singles, DesignName.fptp, DesignName.mmp_med)) yield {
        doDesign(d)
      }
      Output.writeOverview(sims.flatten)
      Output.copyLess(this.outdir)
    } else {
      namedSystems.find(p ⇒ p.name == args(0)) match {
        case Some(p) ⇒
          doDesign(p.designName)
          Output.copyLess(this.outdir)

        case None ⇒
          println(s"Didn't find model ${args(0)}")
          println(usage)
      }

    }

  }


}


/**
  * Adjust the votes for sensitivity analysis.
  * toParty.votes += pct * fromParty.votes
  */
case class VoteAdjust(pct: Double, fromParty: Party, toParty: Party)

case class Params(name: String, // identify this set of parameters
                  title: String, // Title to put on web pages
                  shortTitle: String,
                  designName: DesignName, // how ridings are grouped, etc.
                  outDir: String,
                  description: Text.TypedTag[String],
                  singleMemberElectionStrategy: ElectionStrategy,
                  multiMemberElectionStrategy: ElectionStrategy,
                  voteAdjustment: Option[VoteAdjust] = None,
                  threshholdForTopup: Double = 0.01
                 ) {
  def matches(p: Params): Boolean = {
    this.designName == p.designName &&
      this.singleMemberElectionStrategy == p.singleMemberElectionStrategy &&
      this.multiMemberElectionStrategy == p.multiMemberElectionStrategy
  }
}

