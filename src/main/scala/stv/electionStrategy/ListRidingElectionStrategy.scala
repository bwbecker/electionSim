package stv.electionStrategy

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.{Candidate, Party, SeatType}

import scala.collection.mutable

/**
  * Created by bwbecker on 2016-10-24.
  */
object ListRidingElectionStrategy extends RidingElectionStrategy {
  val name: String = "List"
  val shortName: String = "L"
  val description: TypedTag[String] = div(
    p(
      """Calculate the number of votes for each party and from that the determine the number of seats
        |won by each party using a """.stripMargin,
      a(href := "https://en.wikipedia.org/wiki/Highest_averages_method")("highest averages"),
      """ method -- specifically as described in "Quota system".  After calculating the number of
          seats for each party, make a list of all the candidates for the party, ordered by number of votes in
         the 2015 election, and choose the first n candidates as the winners.
      """.stripMargin)
  )
  val help = description

  def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {
    //assert(!candidates.exists(c => c.winner))

    //val cand = candidates.filter(c => c.party.mainStream)
    val cand = candidates
    val totalVotes = cand.map(_.votes).sum

    val candByParty = cand.groupBy(_.party).toList
    val votesByParty = candByParty.map { case (p, cLst) => (p, cLst.map {_.votes}.sum) }
    val quotientsByParty = votesByParty.map { case (p, votes) => (p, dm * votes / totalVotes) }

    implicit val priority = Ordering.by { foo: (Party, Double) => foo._2 }
    val q = mutable.PriorityQueue[(Party, Double)](quotientsByParty: _*)

    def winners(elected: Seq[Candidate], unelected: Seq[Candidate]): (Seq[Candidate], Seq[Candidate]) = {
      if (elected.length == dm) {
        (elected, unelected)
      } else {
        val (party, quot) = q.dequeue()

        try {
          val winner = unelected.filter(c => c.party == party).maxBy(c => c.votes)
          q.enqueue((party, quot - 1))
          winners(winner.copy(winner = true) +: elected, unelected.filterNot(c => c == winner))
        } catch {
          case e: UnsupportedOperationException =>
            println(s"${party} didn't run enough candidates.")
            winners(Candidate(candidates.head.ridingId,
              0,
              candidates.head.regionId,
              candidates.head.provName,
              "Party ran insufficient candidates",
              party, 0, 0, true, SeatType.RidingSeat, 0) +: elected, unelected)
        }
      }
    }

    val result = winners(List(), candidates)
    result
  }

}
