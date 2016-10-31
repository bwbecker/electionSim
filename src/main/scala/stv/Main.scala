package stv

import scalatags.Text
import scalatags.Text.all._

import ca.bwbecker.io.CachedMkdir
import stv.electionStrategy.ElectionStrategyEnum._
import stv.electionStrategy._
import stv.io.Input.RawCandidate

import java.time.Duration

//import stv.html.OverviewHTML
import stv.io.{Input, Output}


/**
  * Created by bwbecker on 2016-05-29.
  */
object Main {

  private val numRidingsByElectionYr = Map(2015 → 338, 2011 → 308, 2008 → 308, 2006 → 308)

  def main(args: Array[String]): Unit = {
    val start = java.time.LocalTime.now()

    this.clArgParser.parse(args, CLArgs()) match {
      case Some(config) =>
        println(config)


        // Assemble the work we need to do
        val work = for {
          year ← config.years
          candidates = Input.candidates(year)
          numRidings = numRidingsByElectionYr(year)
          ridings = Input.originalRidings(numRidings)
          designName ← config.designs
          optDesign = Input.readDesign(designName, numRidings, ridings, candidates)
          if (optDesign.nonEmpty)
          electStrat ← optDesign.get.electionStrategies
        } yield {

          val params = namedSystems.find(_.matches(designName, year, electStrat.sm, electStrat.mm))
            .getOrElse {
              val name = s"${designName}-${electStrat}"
              Params(name, year, name, designName, name, None, electStrat)
            }

          (optDesign.get, params, ridings)
        }

        // Do each simulation in parallel
        val sims = work.par.map { case (design, params, ridings) ⇒
          println(s"Running election for ${params.designName}-${params.year}.")
          val sim = Sim(design, params, ridings)
          Output.writeHtml(params, sim, config.voteSwing) // side effect!
          println(s"...finished ${params.designName}-${params.year}.")
          sim
        }

        // Write the overview
        if (config.overview) {
          Output.writeOverview(sims.toList)
        }
        Output.copyLess(this.outdir)

      case None =>
      // arguments are bad, error message will have been displayed
    }

    println(Duration.between(start, java.time.LocalTime.now()))
  }


  val outdir = "html"


  case class CLArgs(all: Boolean = false,
                    years: Seq[Int] = Vector[Int](),
                    designs: Seq[DesignName] = Vector[DesignName](),
                    overview: Boolean = false,
                    voteSwing: Boolean = false
                   )

  /**
    * Parse the argument list
    */

  private implicit val weekDaysRead: scopt.Read[DesignName] = scopt.Read.reads(DesignName.withNameInsensitive(_))

  val clArgParser = new scopt.OptionParser[CLArgs]("scopt") {
    head("Election Modelling", "2.x")

    opt[Unit]("all").action((_, c) =>
      c.copy(all = true,
        years = numRidingsByElectionYr.keys.toVector.sorted,
        designs = DesignName.values,
        overview = true,
        voteSwing = true
      )).text("Produce all possible combinations")

    opt[Unit]("overview").action((_, c) =>
      c.copy(overview = true)).text("Write the overview pages")

    opt[Unit]("voteSwing").action((_, c) =>
      c.copy(voteSwing = true)).text("Perform vote swing analysis")

    opt[Seq[Int]]("years").valueName(numRidingsByElectionYr.keys.toVector.sorted.mkString("<", ">,<", ">")).action(
      (x, c) =>
        c.copy(years = x)).text("election years to base simulations on")

    opt[Seq[DesignName]]("designs").valueName(DesignName.values.mkString("<", ">,<", ">")).action((x, c) =>
      c.copy(designs = x)).text("designs for the simulations")


    opt[Unit]("erre").action((_, c) ⇒
      c.copy(years = Vector(2015),
        designs = Seq(
          DesignName.kingsley,
          DesignName.stv_huge,
          DesignName.stv_med,
          DesignName.stv_small,
          DesignName.erre_mmp5050_ProvRegions,
          DesignName.erre_mmp5050_LargeRegions,
          DesignName.erre_mmp5050_SmallRegions,
          DesignName.erre_ru3367_ProvRegions,
          DesignName.erre_ru_multiples_20pct,
          DesignName.erre_ru_multiples_15pct,
          DesignName.erre_ru_multiples_10pct,
          DesignName.erre_ru_singles,
          DesignName.fptp
        ),
        overview = true,
        voteSwing = true
      )).text("Simulations requested by ERRE.")

    opt[Unit]("test").action((_, c) ⇒
      c.copy(years = Vector(2015),
        designs = Seq(
          DesignName.erre_mmp5050_ProvRegions,
          DesignName.erre_ru_multiples_15pct,
          DesignName.stv_med,
          DesignName.fptp
        ),
        overview = true,
        voteSwing = true
      )).text("A smaller set of ERRE sims for testing purposes.")

    //    checkConfig(c =>
    //      (c.all, c.years.length, c.designs.length) match {
    //        case (true, 0, 0)                    ⇒ success
    //        case (false, x, y) if x > 0 && y > 0 ⇒ success
    //        case (_, _, _)                       ⇒
    //          failure("Required to have either --all or at least one year and at least one design.")
    //      }
    //    )
  }


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

  val avPlusDescr = div(
    p("""This design assumes that we keep our current 338 single-member ridings but
    elect the MPs using Alternative Vote (AV) instead of FPTP.  To make things more
    proportional, it adds a small top-up layer (like MMP).  It's like MMP-Lite except
    for replacing FPTP in the single-member ridings with AV.""")
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

  val ridingCentricDescr = div(
    p(strong("""Riding-Centric Rural Urban Proportional"""), """ (need a better name!) is an exciting new electoral
    systems
    model that builds on the Rural-Urban model with ideas from """,
      a(href := "http://www.parl.gc.ca/Content/HOC/Committee/421/ERRE/Brief/BR8457966/br-external/ElbertLeonid-e.pdf")
      ("Local Transferable Vote"),
      """ by Leonid A. Elbert.  An intermediate step was developed by Antony Hodgson and Byron Weber Becker with
      the last tweaks by Byron Weber Becker."""),
    p("""This model offers the following advantages:"""),
    ul(
      li(strong("""Excellent proportionality"""), """:  It has a composite Gallagher score of 2.2.  Out of the
      roughly 50 models
      currently on this web site, only 5 have a better cGallagher score -- and one of those is a variant of this
      one!  Furthermore, I'm quite sure this score can be improved with some redistricting; see below."""),
      li(strong("""An elected MP in every current riding"""), """:  This model keeps our current, 2015, ridings
      intact (exactly the same size as now) and guarantees that an MP is elected in each one.  This provides
      easy access to MPs for constituency service needs."""),
      li(strong("""Multi-member ridings"""), """:  Electing multiple MPs to serve a geographical area helps ensure that
      most voters have at least one MP who understands and can advocate for their policy positions.  The need
      for candidates to attract 2nd and 3rd choice votes keeps political life more civil and encourages
      moderating positions.  Multi-member ridings are also associated with electing more women and minority groups."""),
      li(strong("""Easy redistricting"""), """:  Many models require Elections Canada to redraw riding boundaries.
      This one
      does not.  It just requires that existing ridings be grouped into multi-member ridings of 4 to 6 and
      that those multi-member ridings be grouped into regions of about 5.  It also requires adding 42
      new seats to the House of Commons -- an increase of about 12.5%.  This would put Canada at about 92,500
      voters per MP -- well above the average of Germany (127,700), New Zealand (37,000), UK (43,800),
      and France (71,400)."""),
      li(strong("Few compensatory seats"), """: This model has only 42 compensatory seats, or about 12%.  MMP,
      in contrast, does not have a competative cGallagher index unless roughly 50% of the seats are compensatory
      and the regions are twice as large.""")
    ),

    p("How it works:"),
    ul(
      li("""Add 42 compensatory seats.  The existing seat proportions among the provinces would be almost
      identical to the 2015 allocations if we made the quotient (used in legislation to describe how
      seats are redistributed) was cut from the current value of 111,166 to 98,000 and then add 1 more seat
      to the 1985 minimum redistribution numbers.  See the """,
        a(href := "../ERRE_ModellingWithConstraints.pdf")("report to the ERRE Committee"),
        """ under "Relaxing Constraints" for more details. """),

      li("""Group our current 338 ridings into multi-member ridings of about 4-5 seats each."""),

      li("""Put multi-member ridings into 3 regions in Ontario and 2 regions in Quebec.  Everywhere else
      the region is the same as the province."""),

      li("""Conduct elections with a ballot similar to the following:"""),

      li("To be finished....")

    ),

    p("""There is one down side to Riding-Centric Rural-Urban:  the 42 additional compensatory seats
    violate the constraints stipulated in a motion passed by the ERRE Committee on October 20.  The combination
    of the constraints in that motion are extremely restrictive.  It's my hope that they were exploratory
    only and that the Committee will see fit to relax one of them to achieve a better result."""),

    hr
  )


  val featuredSystems = List(

    Params("fptp", 2015, "First-Past-The-Post",
      DesignName.fptp, s"fptp", Some(fptpDescr), MMP_FPTP),

    Params("av", 2015, "Alternative Vote",
      DesignName.fptp, s"av", Some(avDescr), MMP_AV),

    Params("stv_med", 2015, "Single Transferable Vote (Medium-sized Regions)",
      DesignName.stv_med, s"stv_med", Some(stvMedDescr), STV),

    Params("stv_small", 2015, "Single Transferable Vote (Small Regions)",
      DesignName.stv_small, s"stv_small", Some(stvSmallDescr), STV),


    Params("mmp-8-fptp", 2015, "Mixed Member Proportional (Small Regions)",
      DesignName.mmp_small, s"mmp-8-fptp", Some(mmp8fptpDescr), MMP_FPTP),


    Params("mmpLite", 2015, "Mixed Member Proportional (Lite)",
      DesignName.mmp_enlargeP, s"mmp-15pct", Some(mmpLiteDescr), MMP_FPTP),

    Params("av+", 2015, "AV+",
      DesignName.mmp_enlargeP, s"av-plus", Some(avPlusDescr), MMP_AV),

    Params("rup-338", 2015, "Rural-Urban PR (More Singles, 338 Seats)",
      DesignName.ru_singles, s"rup-338", Some(rup338Descr),
      //EkosAvRidingElectionStrategy, ListRidingElectionStrategy
      AvList
    ),

    Params("rup-15pct", 2015, "Rural-Urban PR (More Singles, 389 Seats)",
      DesignName.ru_enlargeP, s"rup-15pct", Some(rup15PctDescr),
      //FptpRidingElectionStrategy, EkosStvRidingElectionStrategy
      FptpList
    ),

    Params("rup-stv", 2015, "Rural-Urban PR (Few Singles)",
      DesignName.ru_multiples, s"rup-stv", Some(stvPlusDescr),
      STVplus
      //EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy
    ),

    Params("ru_multiples_rc2", 2015, "Riding Centric Rural-Urban PR",
      DesignName.ru_multiples_rc2, s"ru_multiples_rc2", Some(ridingCentricDescr),
      RcRUPR2
    )
  )

  val variantSystems = List(
    Params("mmp-8-av", 2015, "MMP (Small Regions, STVplus)",
      DesignName.mmp_small, s"mmp-8-av", Some(mmp8avDescr),
      STVplus
      //EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy
    ),
    Params("mmp-14-av", 2015, "MMP (Medium Regions, AV)",
      DesignName.mmp_med, s"mmp-14-av", Some(mmp14avDescr),
      STVplus
      //EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy
    ),

    Params("mmp-14-fptp", 2015, "MMP (Medium Regions, FPTP)",
      DesignName.mmp_med, s"mmp-14-fptp", Some(mmp14fptpDescr),
      MMP_FPTP
      //FptpRidingElectionStrategy, NotApplicableRidingElectionStrategy
    ),

    Params("rup-338-list", 2015, "Rural-Urban PR (More Singles, 338 Seats, ListPR)",
      DesignName.ru_singles, s"rup-338-list", Some(rup338ListDescr),
      //FptpRidingElectionStrategy, ListRidingElectionStrategy
      FptpList
    ),

    Params("rup-15pct-stv", 2015, "Rural-Urban PR (More Singles, More Seats)",
      DesignName.ru_enlargeP, s"rup-15pct-stv", Some(rup15PctDescr),
      STVplus
      //EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy
    )

  )


  val namedSystems = featuredSystems ++ variantSystems

}


/**
  * Adjust the votes for sensitivity analysis.
  * toParty.votes += pct * fromParty.votes
  */
case class VoteSwing(pct: Double, fromParty: Party, toParty: Party)

case class Params(name: String, // identify this set of parameters
                  year: Int,
                  title: String, // Title to put on web pages
                  designName: DesignName, // how ridings are grouped, etc.
                  outDir0: String,
                  description: Option[Text.TypedTag[String]],
                  electionStrat: ElectionStrategyEnum,
                  voteAdjustment: Option[VoteSwing] = None
                 ) {

  val outDir: String = if (year == 2015) this.outDir0 else s"${year}/${outDir0}"

  def matches(p: Params): Boolean = this.matches(p.designName, p.year, p.electionStrat.sm,
    p.electionStrat.mm)


  def matches(designName: DesignName, year: Int, sms: RidingElectionStrategy, mms: RidingElectionStrategy): Boolean = {
    this.designName == designName &&
      this.year == year &&
      this.electionStrat.sm == sms &&
      this.electionStrat.mm == mms

  }
}

