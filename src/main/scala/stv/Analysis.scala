package stv

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.Analysis.StatsByParty
import ca.bwbecker.enrichments._

/**
  * Created by bwbecker on 2016-06-01.
  */

object Analysis {
  def apply(ridings: List[Riding], topups: List[Candidate] = List()): Analysis = new Analysis(
    ridings.flatMap(_.candidates) ::: topups,
    ridings.map(_.districtMagnitude).sum + topups.length
  )

  def apply(ridings: List[Riding], topups: List[Candidate], numTopups: Int): Analysis = new Analysis(
    ridings.flatMap(_.candidates) ::: topups,
    ridings.map(_.districtMagnitude).sum + numTopups
  )

  def apply(region: Region): Analysis = new Analysis(
    region.ridings.flatMap(_.candidates) ::: region.topUpCandidates,
    region.ridings.map(r ⇒ r.districtMagnitude).sum + region.topUpSeats)

  def apply(candidates: List[Candidate], seats: Int): Analysis = new Analysis(candidates, seats)

  def apply(regions: List[Region]): Analysis = new Analysis(
    regions.flatMap(r ⇒ r.ridings.flatMap(r ⇒ r.candidates) ::: r.topUpCandidates),
    regions.map(r ⇒ r.topUpSeats + r.ridings.map(_.districtMagnitude).sum).sum
  )

  /**
    * Analysis on a list of provinces, given a simulation.  Includes top-ups.
    * @param provinces
    * @param sim
    * @return
    */
  def apply(provinces: List[ProvName])(implicit sim:Sim):Analysis =
    Analysis(sim.regions.filter(r ⇒ provinces.contains(r.ridings.head.province)))


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
      includeNotes.option(
        div(cls := "footnotes")(
          p(sup(1), "The number of votes each party received in the 2015 election."),
          p(sup(2), "The percentage of the votes each party received."),
          p(sup(3), "The number of candidates elected for each party."),
          p(sup(4), "The percentage of MPs that this party was awarded.  Ideally, this will match the percentage of " +
            "the vote."),
          p(sup(5), "The number of MPs this party would have if the results were perfectly proportional at the " +
            "national level."),
          p(sup(6), "The over (or under) representation of this party in Parliment.  That is, the difference between " +
            "the percentage of MPs and the percentage of the vote.")
        )
      )
    )
  }

}


class Analysis(candidates: List[Candidate],
               val seats: Int
              ) {

  val allCandidates = candidates
  lazy val elected = allCandidates.filter(c ⇒ c.winner)
  lazy val unelected = allCandidates.filterNot(c ⇒ c.winner)
  lazy val electedParties = elected.map(_.party).distinct
  lazy val parties = allCandidates.map(_.party).distinct

  lazy val totalVotes = allCandidates.map(_.votes).sum

  lazy val candidatesByParty = allCandidates.groupBy(_.party).withDefault(p ⇒ List[Candidate]())

  lazy val statsByParty = this.calcStatsByParty


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
    val deservedMPs = seats * pctVote
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
  private def calcStatsByParty: List[StatsByParty] = {
    for {
      party ← parties
    } yield {
      calcStatsByParty(party)
    }
  }


  def statsByPartyAsHTML(includeNotes: Boolean = false) = {
    val stats = this.statsByParty.filter(_.pctVote >= 0.01)

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
              td(cls := "stat")(f"${p.pctVote * elected.length}%3.1f"),
              td(cls := "stat")(f"${(p.pctMPs - p.pctVote) * 100}%3.1f%%")
            )
          }).toSeq
        )
      ),
      p(span(cls := "numMPs")(s"MPs: ${this.elected.length}"),
        span(cls := "gallagher")(f"Gallagher Index: ${this.gallagherIndex * 100}%5.2f")
      ),
      includeNotes.option(
        div(cls := "footnotes")(
          p(sup(1), "The number of votes each party received in the 2015 election."),
          p(sup(2), "The percentage of the votes each party received."),
          p(sup(3), "The number of candidates elected for each party."),
          p(sup(4), "The percentage of MPs that this party was awarded.  Ideally, this will match the percentage of " +
            "the vote."),
          p(sup(5), "The number of MPs this party would have if the results were perfectly proportional at the " +
            "national level."),
          p(sup(6), "The over (or under) representation of this party in Parliment.  That is, the difference between " +
            "the percentage of MPs and the percentage of the vote.")
        )
      )
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