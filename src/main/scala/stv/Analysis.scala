package stv

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.Analysis.StatsByParty
import ca.bwbecker.enrichments._
import stv.Party.Oth

/**
  * Created by bwbecker on 2016-06-01.
  */

object Analysis {


  case class StatsByParty(party: Party,
                          popVote: Int,
                          pctVote: Double,
                          mps: Int,
                          pctMPs: Double,
                          deservedMPs: Double,
                          numRidingMPs: Int,
                          numTopupSeats: Int) {
    override def toString = f"Stats(${party}: ${popVote}%,8d ${pctVote * 100}%5.2f  ${mps}%2d  " +
      f"${pctMPs * 100}%,5.2f  ${pctVote * 100 - pctMPs * 100}%5.2f"
  }


  def comparativeStatsByPartyAsHTML(a1: Analysis, a2: Analysis, includeNotes: Boolean = false) = {
    val stats1 = a1.statsByParty.filter(_.pctVote >= 0.01)
    val stats2 = a2.statsByParty.filter(p1 ⇒ stats1.exists(p2 ⇒ p1.party == p2.party))

    def noteRef(i: Int): Option[TypedTag[String]] = if (includeNotes) Some(sup(i)) else None

    div(cls := "analysis")(
      table()(
        thead(
          tr(
            th(colspan := 3)(),
            th(colspan := 3, cls := "compare")("Ridings Only"),
            th(colspan := 3, cls := "compare")("Ridings + Top-up")
          ),
          tr(
            th("Party"),
            th("Popular Votes", noteRef(1)),
            th("Pct Votes", noteRef(2)),
            th(cls := "subhead")("Elected MPs", noteRef(3)),
            th(cls := "subhead")("Pct Elected MPs", noteRef(4)),
            th(cls := "subhead")("Over Rep", noteRef(6)),
            th(cls := "subhead")("Elected MPs", noteRef(3)),
            th(cls := "subhead")("Pct Elected MPs", noteRef(4)),
            th(cls := "subhead")("Over Rep", noteRef(6))
          )
        ),
        tbody(
          (for {
            p ← stats1.sortBy(-_.popVote)
            topupP ← stats2.find(x ⇒ x.party == p.party)
          } yield {
            tr(td(cls := s"${p.party} party")(p.party.toString),
              td(cls := "stat")(f"${p.popVote}%,.0f"),
              td(cls := "stat")(f"${p.pctVote * 100}%3.1f%%"),
              td(cls := "stat")(p.mps),
              td(cls := "stat")(f"${p.pctMPs * 100}%3.1f%%"),
              td(cls := "stat")(f"${(p.pctMPs - p.pctVote) * 100}%3.1f%%"),
              td(cls := "stat")(topupP.mps),
              td(cls := "stat")(f"${topupP.pctMPs * 100}%3.1f%%"),
              td(cls := "stat")(f"${(topupP.pctMPs - topupP.pctVote) * 100}%3.1f%%")
            )
          }).toSeq,
          tr(td(colspan := 3, cls := "numMPs right")("Total Number of MPs:"),
            td(colspan := 3, cls := "numMPs center")(a1.elected.length),
            td(colspan := 3, cls := "numMPs center")(a2.elected.length)),
          tr(td(colspan := 3, cls := "right")("Gallagher Index:"),
            td(colspan := 3, cls := "center")(f"${a1.gallagherIndex * 100}%5.2f"),
            td(colspan := 3, cls := "center")(f"${a2.gallagherIndex * 100}%5.2f"))
        )
      ),
      includeNotes.option(partyStatsFootnotes)
    )
  }


  def partyStatsFootnotes = div(cls := "footnotes")(
    p(sup(1), "The number of votes each party received in the 2015 election."),
    p(sup(2), "The percentage of the votes each party received."),
    p(sup(3), "The number of candidates elected for each party."),
    p(sup(4), "The percentage of MPs that this party was awarded.  Ideally, this will match the percentage of " +
      "the vote."),
    p(sup(5), "The number of MPs this party would have if the results were perfectly proportional."),
    p(sup(6), "The over (or under) representation of this party in Parliment.  That is, the difference between " +
      "the percentage of MPs and the percentage of the vote.")
  )

}


/**
  * Do an analysis of the proportionality of this set of candidates.
  *
  * @param allCandidates All the candidates (both winning and losing) that competed in the area to be analyzed.
  * @param totalSeats    The total number of seats (riding + top-up) that represent the area to be analyzed.
  * @param nationWide    Should the number of MPs take into account provincial differences in pop/MP?
  *                      If false, the calc is just pctVote * totalSeats.  If true, calc under/over
  *                      rep for each province and sum
  */
class Analysis(val allCandidates: Seq[Candidate],
               val totalSeats: Int,
               val nationWide: Boolean = false
              ) {

  lazy val elected = allCandidates.filter(c ⇒ c.winner)
  lazy val unelected = allCandidates.filterNot(c ⇒ c.winner)
  lazy val electedParties = elected.map(_.party).distinct
  lazy val parties = allCandidates.map(_.party).distinct

  lazy val totalVotes = allCandidates.map(_.votes).sum

  lazy val candidatesByParty = allCandidates.groupBy(_.party).withDefault(p ⇒ List[Candidate]())

  lazy val statsByParty = this.calcStatsByParty

  /**
    * Get the percent of the vote earned by the given party.
    */
  def pctVote(party: Party): Double = statsByParty.find(_.party == party).map(stats ⇒ stats.pctVote).getOrElse(0.0)

  /**
    * Get the percent of the MPs returned for the given party.
    */
  def pctMPs(party: Party): Double = statsByParty.find(_.party == party).map(stats ⇒ stats.pctMPs).getOrElse(0.0)

  def gallagherIndex: Double = {
    val sumDifSquared = this.statsByParty.map { ps ⇒ Math.pow(ps.pctVote - ps.pctMPs, 2) }.sum

    Math.sqrt(sumDifSquared / 2.0)
  }

  private def calcStatsByParty(party: Party): StatsByParty = {
    val cand = candidatesByParty(party)
    val popVote = cand.map(_.votes).sum.toInt
    val pctVote = popVote / totalVotes.toDouble
    val mps = cand.count(_.winner)
    val pctMPs = mps / (elected.length).toDouble
    val deservedMPs = totalSeats * pctVote
    val numLocalMPs = cand.count(c => c.seatType == SeatType.RidingSeat && c.winner)
    val numTopupSeats = cand.count(c => c.seatType == SeatType.TopupSeat && c.winner)
    assert(numLocalMPs + numTopupSeats == mps, s"$numLocalMPs + $numTopupSeats != $mps")


    StatsByParty(party, popVote, pctVote, mps, pctMPs, deservedMPs, numLocalMPs, numTopupSeats)
  }

  /**
    * Stats for all parties running in the given ridings
    *
    * @return
    */
  private def calcStatsByParty: Seq[StatsByParty] = {
    if (this.nationWide) {
      /*
       * The over/under rep by party is different if you sum the individual provinces vs. calculating for
       * the nation as a whole due to differences in the ratio of MPs to population across the different
       * provinces.
       */
      //      val statsByProv:Seq[Seq[StatsByParty]] = for {
      //        prov ← ProvName.values
      //      } yield {
      //        val cand = this.allCandidates.filter(c ⇒ c.provName == prov)
      //        new Analysis(cand, 0).statsByParty
      //      }

      val stats: Seq[StatsByParty] = ProvName.values.flatMap { prov ⇒
        val cand = this.allCandidates.filter(c ⇒ c.provName == prov)
        val seats = cand.count(_.winner)
        new Analysis(cand, seats).statsByParty
      }
      val statsByParty = stats.groupBy(s ⇒ s.party)

      for {
        (party, stats) ← statsByParty.toSeq
      } yield {
        val s = calcStatsByParty(party)
        s.copy(deservedMPs = stats.foldLeft(0.0)((accum, stat) ⇒ accum + stat.deservedMPs))
      }

    } else {
      for {
        party ← parties
      } yield {
        calcStatsByParty(party)
      }
    }
  }


  def statsByPartyAsHTML(includeNotes: Boolean = false) = {
    val (majorParties, others) = this.statsByParty.partition(p ⇒ p.pctVote >= 0.01)
    val stats = if (this.nationWide) {
      val oth: StatsByParty = others.fold(StatsByParty(Oth, 0, 0.0, 0, 0.0, 0.0, 0, 0)) {
        (a, b) ⇒ StatsByParty(Oth, a.popVote + b.popVote, a.pctVote + b.pctVote, a.mps + b.mps, a.pctMPs + b.pctMPs,
          a.deservedMPs + b.deservedMPs, a.numRidingMPs + b.numRidingMPs, a.numTopupSeats + b.numTopupSeats)
      }
      majorParties :+ oth
    } else {
      majorParties
    }

    def noteRef(i: Int): Option[TypedTag[String]] = if (includeNotes) Some(sup(i)) else None

    div(cls := "analysis")(
      table()(
        thead(
          tr(
            th("Party"),
            th("Popular Votes", noteRef(1)),
            th("Pct Votes", noteRef(2)),
            th("Elected MPs", noteRef(3)),
            th("Pct Elected MPs", noteRef(4)),
            th("Proportional MPs", noteRef(5)),
            th("Over Representation", noteRef(6))
          )
        ),
        tbody(
          (for (p ← stats.sortBy(-_.popVote)) yield {
            tr(td(cls := s"${p.party} party")(p.party.toString),
              td(cls := "stat")(f"${p.popVote}%,.0f"),
              td(cls := "stat")(f"${p.pctVote * 100}%3.1f%%"),
              td(cls := "stat")(p.mps),
              td(cls := "stat")(f"${p.pctMPs * 100}%3.1f%%"),
              td(cls := "stat")(f"${p.deservedMPs}%3.1f"),
              td(cls := "stat")(f"${(p.pctMPs - p.pctVote) * 100}%3.1f%%")
            )
          }).toSeq
        )
      ),
      p(span(cls := "numMPs")(s"MPs: ${this.elected.length}"),
        span(cls := "gallagher")(f"Gallagher Index: ${this.gallagherIndex * 100}%5.2f")
      ),
      includeNotes.option(Analysis.partyStatsFootnotes)
    )
  }


  def statsAsHTML = {
    div(
      p(s"Number of MPs: ${this.elected.length}"),
      p(f"Gallagher Index of Disproportionality: ${this.gallagherIndex * 100}%5.2f"),
      this.statsByPartyAsHTML()
    )
  }

}
