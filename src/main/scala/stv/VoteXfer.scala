package stv

import stv.Party._

import scalatags.Text.TypedTag
import scalatags.Text.all._

/**
  * Created by bwbecker on 2016-06-15.
  */

trait VoteXfer {
  val name: String
  val shortDescr: String

  def description: TypedTag[String]

  def source: Option[String]

  def xfer(p1: Party, p2: Party, dm: Int): Double


  /**
    * Generate an HTML table to display the vote transfer function.
    */
  def xferTable: TypedTag[String] = {
    val right = cls := "right"
    val left = cls := "left"

    val headers = tr(th(left)("Xfer from", raw("&darr;"), " to", raw("&rarr;")),
      for (p1 ← Party.values.sortBy(_.entryName)) yield {
        th(right)(p1.toString)
      }
    )

    table(cls := "voteTransfers")(
      headers,
      for (p1 ← Party.values.sortBy(_.entryName)) yield {
        tr(td(left)(p1.toString),
          for {
            p2 ← Party.values.sortBy(_.entryName)
            pct = (xfer(p1, p2, 2) * 100).toInt
          } yield {

            td(right)(if (pct > 0) pct.toString else "")
          }
        )
      }
    )
  }

}


object EkosXfer extends VoteXfer {
  val name = "Ekos"
  val shortDescr = "Transfers based on Ekos polling just before the 2015 election."

  //http://www.ekospolitics.com/wp-content/uploads/full_report_october_15_2015.pdf
  val map =
  Map[(Party, Party), Double](
    (Lib, Con) → 0.12,
    (Lib, NDP) → 0.45,
    (Lib, Grn) → 0.10,
    (Lib, Bloc) → 0.03,

    (Con, Lib) → 0.17,
    (Con, NDP) → 0.10,
    (Con, Grn) → 0.08,

    (NDP, Lib) → 0.53,
    (NDP, Con) → 0.06,
    (NDP, Grn) → 0.13,
    (NDP, Bloc) → 0.06,

    (Grn, Lib) → 0.16,
    (Grn, Con) → 0.08,
    (Grn, NDP) → 0.22,
    (Grn, Bloc) → 0.15,

    (Bloc, Lib) → 0.16,
    (Bloc, Con) → 0.19,
    (Bloc, NDP) → 0.29,
    (Bloc, Grn) → 0.06

  ).withDefault(t ⇒ 0.0)

  def description = div(
    p(
      """This table is based on Ekos polling performed just before the 2015 election which asked for voters' second
    choice party.  As Wilf Day has pointed out,"""),
    blockquote(
      """On Oct. 14 it had Liberals at 33.5%, Conservatives 32.6%, NDP 22.9%, Greens 5.6%, Bloc 3.4%.
    However, the E-day figures were Liberal 39.5%, Conservatives 31.9%, NDP 19.7%, Green 3.4%, and Bloc 4.7%.
    Obviously a lot of NDP and Green second-choices for Liberals had switched by E-day"""),
    p("However, it appears to be the best data we have.")
  )

  def source = Some("http://www.ekospolitics.com/wp-content/uploads/full_report_october_15_2015.pdf")

  def xfer(p1: Party, p2: Party, dm: Int): Double = map(p1, p2)
}

object ThinAirXfer extends VoteXfer {
  val name = "ThinAirXfer"
  val shortDescr = "A guess at a transfer function, to provide a different data point for ranked ballot elections."

  //http://www.ekospolitics.com/wp-content/uploads/full_report_october_15_2015.pdf
  val map =
  Map[(Party, Party), Double](
    (Lib, Con) → 0.32,
    (Lib, NDP) → 0.25,
    (Lib, Grn) → 0.10,
    (Lib, Bloc) → 0.03,

    (Con, Lib) → 0.10,
    (Con, NDP) → 0.10,
    (Con, Grn) → 0.15,

    (NDP, Lib) → 0.43,
    (NDP, Con) → 0.06,
    (NDP, Grn) → 0.23,
    (NDP, Bloc) → 0.06,

    (Grn, Lib) → 0.16,
    (Grn, Con) → 0.08,
    (Grn, NDP) → 0.22,
    (Grn, Bloc) → 0.15,

    (Bloc, Lib) → 0.11,
    (Bloc, Con) → 0.14,
    (Bloc, NDP) → 0.24,
    (Bloc, Grn) → 0.21

  ).withDefault(t ⇒ 0.0)

  def description = div(
    p(
      """Another vote transfer function, constructed out of thin air.  It's here to provide another
      data point for elections based on a ranked ballot.  With reference to the Ekos poll, it
      has more Liberals defecting to the Conservatives and fewer to the NDP.  Greens also become
      a second choice for more people.""")

  )

  def source = None

  def xfer(p1: Party, p2: Party, dm: Int): Double = map(p1, p2)
}



object XferProb2013 extends VoteXfer {
  val name = "XferProb2013"
  val shortDescr = "Transfers based on Globe & Mail polling from 2013."


  def description = div(
    p(
      """Transfers based on second choice preferences from a 2013 poll published in the Globe and Mail.  Beyond the
    issue of the data being quite old, the poll forced a complete ranking.  We know that voters don't
    generally do that.  The 3rd, 4th, etc. choices are not reflected in this model.""")
  )

  def source = Some("http://www.theglobeandmail.com/news/politics/why-a-change-to-your-ballot-would-give-the-ndp" +
    "-an-edge-next-election/article10454286/?page=all")

  def xfer(p1: Party, p2: Party, dm: Int): Double = {
    (p1, p2) match {
      case (Con, Lib) ⇒ 0.57
      case (Con, Grn) ⇒ 0.08
      case (Con, NDP) ⇒ 0.20
      case (Con, Bloc) ⇒ 0.01

      case (Lib, Con) ⇒ 0.28
      case (Lib, NDP) ⇒ 0.57
      case (Lib, Grn) ⇒ 0.12
      case (Lib, Bloc) ⇒ 0.02

      case (NDP, Lib) ⇒ 0.37
      case (NDP, Grn) ⇒ 0.33
      case (NDP, Con) ⇒ 0.15
      case (NDP, Bloc) ⇒ 0.11

      case (Bloc, Lib) ⇒ 0.02
      case (Bloc, NDP) ⇒ 0.61
      case (Bloc, Con) ⇒ 0.14
      case (Bloc, Grn) ⇒ 0.18

      case (Grn, Lib) ⇒ 0.26
      case (Grn, NDP) ⇒ 0.44
      case (Grn, Con) ⇒ 0.12
      case (Grn, Bloc) ⇒ 0.04

      case (_, _) ⇒ 0.0
    }
  }
}


object XferProbLeger2015 extends VoteXfer {
  val name = "XferProbLeger2015"
  val shortDescr = "Transfers based on Leger polling just before the 2015 election."

  /**
    * Second choice perefernces based on
    *
    */
  def description = div(
    p(
      """Transfers based on second choice preferences from 2015 polling by Leger.  As Wilf Day
    points out, this data isn't very helpful because the question was "If you were to
    change your mind, ...".  In other words, it only covers the 2nd choices of
    wavering voters, not everyone.""")
  )

  def source = Some("http://leger360.com/admin/upload/publi_pdf/soen20151017.pdf")

  def xfer(p1: Party, p2: Party, dm: Int): Double = {
    (p1, p2) match {
      case (Con, Lib) ⇒ 0.37
      case (Con, Grn) ⇒ 0.09
      case (Con, NDP) ⇒ 0.21
      case (Con, Bloc) ⇒ 0.04

      case (Lib, Con) ⇒ 0.19
      case (Lib, NDP) ⇒ 0.45
      case (Lib, Grn) ⇒ 0.14
      case (Lib, Bloc) ⇒ 0.03

      case (NDP, Lib) ⇒ 0.48
      case (NDP, Grn) ⇒ 0.15
      case (NDP, Con) ⇒ 0.06
      case (NDP, Bloc) ⇒ 0.08

      case (Bloc, Lib) ⇒ 0.17
      case (Bloc, NDP) ⇒ 0.30
      case (Bloc, Con) ⇒ 0.11
      case (Bloc, Grn) ⇒ 0.20

      case (Grn, Lib) ⇒ 0.33
      case (Grn, NDP) ⇒ 0.27
      case (Grn, Con) ⇒ 0.19
      case (Grn, Bloc) ⇒ 0.02

      case (_, _) ⇒ 0.0

    }
  }

}


object XferProbFPTP extends VoteXfer {
  val name = "XferProbFPTP"
  val shortDescr = "No transfers between parties."


  def description = div("In a FPTP election, no votes transfer.")

  def source = None

  def xfer(p1: Party, p2: Party, dm: Int): Double = 0.0

}

