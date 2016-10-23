package stv

import scalatags.Text
import scalatags.Text.all._

import ca.bwbecker.io.CachedMkdir
import stv.io.Input.RawCandidate

//import stv.html.OverviewHTML
import stv.io.{Input, Output}


/**
  * Created by bwbecker on 2016-05-29.
  */
object Main {

  private val numRidingsByElectionYr = Map(2015 → 338, 2011 → 308, 2008 → 308, 2006 → 308)

  def main(args: Array[String]): Unit = {


    this.clArgParser.parse(args, CLArgs()) match {
      case Some(config) =>
        println(config)


        val sims = for {
          year ← config.years
          candidates = Input.candidates(year)
          numRidings = numRidingsByElectionYr(year)
          ridings = Input.originalRidings(numRidings)
          designName ← config.designs
          design ← Input.readDesign(designName, numRidings, ridings, candidates)
        } yield {
          println(s"Processing ${year} ${numRidings} ${designName}")


          // Seems like these ought to be included in the outer for; but gives a type error
          for {
            singleMbrStrategy ← design.singleMbrStrategies
            multiMbrStrategy ← design.multiMbrStrategies
            // if both single and multi strategies use a ranked ballot, they both use the same transfer function
            if (!singleMbrStrategy.isInstanceOf[StvRidingElectionStrategy] ||
              !multiMbrStrategy.isInstanceOf[StvRidingElectionStrategy] ||
              singleMbrStrategy.asInstanceOf[StvRidingElectionStrategy].voteXfer == multiMbrStrategy
                .asInstanceOf[StvRidingElectionStrategy].voteXfer)
          } yield {

            val params = namedSystems.find(_.matches(designName, year, singleMbrStrategy, multiMbrStrategy))
              .getOrElse {
                val name = s"${designName}-${singleMbrStrategy.name}-${multiMbrStrategy.name}"
                Params(name, year, name, designName, name, None, singleMbrStrategy, multiMbrStrategy)
              }

            val sim = Sim(design, params, ridings)
            Output.writeHtml(params, sim)
            sim
          }

        }

        if (config.overview) {
          Output.writeOverview(sims.flatten.toList)
          Output.copyLess(this.outdir)
        }

      case None =>
      // arguments are bad, error message will have been displayed
    }

  }


  val outdir = "html"


  case class CLArgs(all: Boolean = false,
                    years: Seq[Int] = Vector[Int](),
                    designs: Seq[DesignName] = Vector[DesignName](),
                    overview: Boolean = false)

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
        overview = true
      )).text("Produce all possible combinations")

    opt[Unit]("overview").action((_, c) =>
      c.copy(overview = true)).text("Write the overview pages")

    opt[Seq[Int]]("years").valueName(numRidingsByElectionYr.keys.toVector.sorted.mkString("<", ">,<", ">")).action(
      (x, c) =>
        c.copy(years = x)).text("election years to base simulations on")

    opt[Seq[DesignName]]("designs").valueName(DesignName.values.mkString("<", ">,<", ">")).action((x, c) =>
      c.copy(designs = x)).text("designs for the simulations")


    opt[Unit]("erre").action((_, c) ⇒
      c.copy(years = Vector(2015),
        designs = Seq(DesignName.erre_mmp_5050_small,
          DesignName.erre_mmp_5050_large,
//          DesignName.erre_ru,
//          DesignName.erre_ru2,
          DesignName.fptp
        ),
        overview = true
      )).text("Simulations requested by ERRE.")

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


  val featuredSystems = List(

    Params("fptp", 2015, "First-Past-The-Post",
      DesignName.fptp, s"fptp", Some(fptpDescr), FptpRidingElectionStrategy, NotApplicableRidingElectionStrategy),

    Params("av", 2015, "Alternative Vote",
      DesignName.fptp, s"av", Some(avDescr), EkosAvRidingElectionStrategy, NotApplicableRidingElectionStrategy),

    Params("stv_med", 2015, "Single Transferable Vote (Medium-sized Regions)",
      DesignName.stv_med, s"stv_med", Some(stvMedDescr), EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy),

    Params("stv_small", 2015, "Single Transferable Vote (Small Regions)",
      DesignName.stv_small, s"stv_small", Some(stvSmallDescr), EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy),


    Params("mmp-8-fptp", 2015, "Mixed Member Proportional (Small Regions)",
      DesignName.mmp_small, s"mmp-8-fptp", Some(mmp8fptpDescr), FptpRidingElectionStrategy,
      NotApplicableRidingElectionStrategy),


    Params("mmpLite", 2015, "Mixed Member Proportional (Lite)",
      DesignName.mmp_enlargeP, s"mmp-15pct", Some(mmpLiteDescr), FptpRidingElectionStrategy,
      NotApplicableRidingElectionStrategy),

    Params("av+", 2015, "AV+",
      DesignName.mmp_enlargeP, s"av-plus", Some(avPlusDescr), EkosAvRidingElectionStrategy,
      NotApplicableRidingElectionStrategy),

    Params("rup-338", 2015, "Rural-Urban PR (More Singles, 338 Seats)",
      DesignName.ru_singles, s"rup-338", Some(rup338Descr), EkosAvRidingElectionStrategy, ListRidingElectionStrategy),

    Params("rup-15pct", 2015, "Rural-Urban PR (More Singles, 389 Seats)",
      DesignName.ru_enlargeP, s"rup-15pct", Some(rup15PctDescr), FptpRidingElectionStrategy, EkosStvRidingElectionStrategy),

    Params("rup-stv", 2015, "Rural-Urban PR (Few Singles)",
      DesignName.ru_multiples, s"rup-stv", Some(stvPlusDescr), EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy)
  )

  val variantSystems = List(
    Params("mmp-8-av", 2015, "MMP (Small Regions, AV)",
      DesignName.mmp_small, s"mmp-8-av", Some(mmp8avDescr), EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy),
    Params("mmp-14-av", 2015, "MMP (Medium Regions, AV)",
      DesignName.mmp_med, s"mmp-14-av", Some(mmp14avDescr), EkosAvRidingElectionStrategy, EkosStvRidingElectionStrategy),

    Params("mmp-14-fptp", 2015, "MMP (Medium Regions, FPTP)",
      DesignName.mmp_med, s"mmp-14-fptp", Some(mmp14fptpDescr), FptpRidingElectionStrategy,
      NotApplicableRidingElectionStrategy),

    Params("rup-338-list", 2015, "Rural-Urban PR (More Singles, 338 Seats, ListPR)",
      DesignName.ru_singles, s"rup-338-list", Some(rup338ListDescr), FptpRidingElectionStrategy, ListRidingElectionStrategy),

    Params("rup-15pct-stv", 2015, "Rural-Urban PR (More Singles, More Seats)",
      DesignName.ru_enlargeP, s"rup-15pct-stv", Some(rup15PctDescr), EkosAvRidingElectionStrategy,
      EkosStvRidingElectionStrategy)

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
                  singleMemberElectionStrategy: RidingElectionStrategy,
                  multiMemberElectionStrategy: RidingElectionStrategy,
                  voteAdjustment: Option[VoteSwing] = None
                 ) {

  val outDir: String = if (year == 2015) this.outDir0 else s"${year}/${outDir0}"

  def matches(p: Params): Boolean = this.matches(p.designName, p.year, p.singleMemberElectionStrategy,
    p.multiMemberElectionStrategy)


  def matches(designName: DesignName, year: Int, sms: RidingElectionStrategy, mms: RidingElectionStrategy): Boolean = {
    this.designName == designName &&
      this.year == year &&
      this.singleMemberElectionStrategy == sms &&
      this.multiMemberElectionStrategy == mms

  }
}

