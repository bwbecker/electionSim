package stv

import scala.collection.mutable
import scalatags.Text.TypedTag
import scalatags.Text.all._


/**
  * Created by bwbecker on 2016-08-18.
  */
trait ElectionStrategy {

  val name: String
  val shortName: String
  val help: TypedTag[String]

  val description: TypedTag[String]
  val debug = false


  def runElection(candidates: List[Candidate], dm: Int): (List[Candidate], List[Candidate])

  protected def collapseCandidates(candidates: List[Candidate]): List[Candidate] = {
    val grouped = candidates.groupBy(c => c.party)
    for {
      (party, cands) <- grouped.toList
    } yield {
      if (cands.length > 1) {
        val mostVotes = cands.maxBy(_.votes)
        mostVotes.copy(votes = cands.map(_.votes).sum, effVotes = cands.map(_.effVotes).sum)
      } else {
        cands.head
      }
    }
  }

}

object ElectionStrategy {
  val values = Vector(FptpElectionStrategy,
    AvElectionStrategy,
    EkosStvElectionStrategy,
    ListElectionStrategy,
    NotApplicableElectionStrategy
  )

  val singleMbrStrategies = Vector(FptpElectionStrategy, AvElectionStrategy)

  val multiMbrStrategies = Vector(EkosStvElectionStrategy, ListElectionStrategy)
}

/*
object FptpElectionStrategy extends ElectionStrategy {
  val name:String = "FPTP"
  val description:String =
    """After collapsing all candidates running for the same party into one virtual
      |candiate, choose the virtual candidate with the most votes.
    """.stripMargin

  def runElection(candidates:List[Candidate], dm:Int):(List[Candidate], List[Candidate]) = {
    assert(dm == 1)
    val cCands = this.collapseCandidates(candidates)
    val winner = cCands.maxBy(_.votes)
    val (elected, unelected) = cCands.partition(c => c == winner)
    assert(elected.length == 1)
    assert(unelected.length == cCands.length - 1)
    (elected, unelected)
  }
}
*/

object FptpElectionStrategy extends StvElectionStrategy(XferProbFPTP) {
  override val name = "FPTP"
  override val shortName = "F"
  override val description = p("A version of FPTP that deals with merged ridings.")
  override val help = p("First-Past-The-Post -- the candidate with the most votes wins.")
}

object NotApplicableElectionStrategy extends ElectionStrategy {
  val name: String = "N/A"
  val shortName = "x"
  val help = p("No election strategy is applicable in this situation.")
  val description: TypedTag[String] = p("An election strategy for where none are applicable.  For example, for multi-member" +
    " ridings in a FPTP simulation.")

  def runElection(candidates: List[Candidate], dm: Int): (List[Candidate], List[Candidate]) = {
    throw new Exception("Election Strategy is not applicable.")
  }

}


/**
  * Just like STV but we want the AV name.
  */
object AvElectionStrategy extends StvElectionStrategy(Ekos) {
  override val name = "AV"
  override val shortName = "A"
  override val help = p("Alternative Vote: Elect one winner using a ranked ballot.  Drop the least " +
    "popular candidate and redistribute their ballots to the next choice candidate until one candidate " +
    "has at least 50%+1.")

  override def runElection(candidates: List[Candidate], dm: Int): (List[Candidate], List[Candidate]) = {
    assert(dm == 1)
    super.runElection(candidates, dm)
  }
}


object EkosStvElectionStrategy extends StvElectionStrategy(Ekos)

class StvElectionStrategy(voteXfer: VoteXfer) extends ElectionStrategy {

  val name = "STV"
  val shortName = "S"
  val help = div(p("Single Transferable Vote: Elect one or more winners using a ranked ballot. Establish a 'quota' or " +
    "maximum number of votes required to be elected.  Call that number 'Q'."),
    p("If a candidate has more than Q votes, declare the candidate elected and transfer any votes more than " +
      "Q to other candidates according to the preferences expressed on the ballots.  If no candidate has " +
      "Q votes, drop the one with the least votes and transfer their votes to other candidates according to the " +
      "preferences expressed on the ballots.")
  )

  val description = {
    div(
      p(
        """During the election in each riding, votes were transferred in two
            steps.  First, if a member of party X is eliminated and there are other
            members of party X still in the race, ALL of the votes are split equally
            between the remaining members of party X."""),
      p(
        """When the last member of a party is eliminated, the votes are transferred
            according to the following table."""),
      voteXfer.xferTable,
      voteXfer.description,
      voteXfer.source.map(s ⇒ p(s"Source: ", a(href := s)(s)))
    )
  }


  def runElection(candidates: List[Candidate], dm: Int): (List[Candidate], List[Candidate]) = {
    this.stvElection(candidates, dm, voteXfer.xfer, debug)
  }


  private case class MutCandidate(ridingId: RidingId,
                                  name: String,
                                  party: Party,
                                  votes: Double,
                                  var effVotes: Double,
                                  var winner: Boolean) {


    override def toString = s"Candidate(${ridingId}, ${name}, ${party}, ${votes}, ${effVotes})"

  }

  private def stvElection(candidates: List[Candidate], dm: Int, xferVoteProb: VoteXferFunc, debug: Boolean): (List[Candidate],
    List[Candidate]) = {
    def c2m(c: Candidate): MutCandidate = MutCandidate(c.ridingId, c.name, c.party, c.votes, c.votes, false)
    def m2c(m: MutCandidate): Candidate = Candidate(m.ridingId, m.name, m.party, m.votes, m.effVotes, m.winner, SeatType.RidingSeat)

    val (win, lose) = stvElection(
      candidates.map(c2m(_)).toSet, dm, xferVoteProb)(debug)

    (win.map(m2c(_)), lose.map(m2c(_)))
  }


  // dbug print
  private def dp(s: String)(implicit debug: Boolean): Unit = {
    if (debug) {
      println(s)
    }
  }


  /**
    * Count the votes for this election.
    */
  private def stvElection(candidates: Set[MutCandidate],
                          dm: Int,
                          xferVoteProb: VoteXferFunc)(
                           implicit debug: Boolean = false): (List[MutCandidate], List[MutCandidate]) = {


    val threshhold = this.threshhold(candidates, dm)
    dp(s"   threshhold = ${threshhold}")


    def helper(candidates: Set[MutCandidate],
               elected: List[MutCandidate],
               notElected: List[MutCandidate]
              ): (List[MutCandidate], List[MutCandidate]) = {
      val totalVotes = candidates.map(_.effVotes).sum

      dp(s"\nSTV: c=${candidates.toList.sortBy(_.effVotes).mkString("\t", "\n\t", "")}")
      dp(s"""\te=$elected\n\tne=${notElected}\ttVotes = $totalVotes, thr = $threshhold, dm = ${dm}""")
      if (dm == elected.length) {
        // We have all the winners we need -- election is over
        dp("All done")
        (elected.reverse, notElected ::: candidates.toList)
      } else if (candidates.size == 0) {
        throw new Exception("Ran out of candidates")
      } else {
        candidates.find(_.effVotes >= threshhold) match {
          case Some(hasThreshold) ⇒
            //println(s"${hasThreshold} is over the threshhold")
            // A candidate has met the threshhold.  Declare them elected and redistribute the votes
            val remainingCandidates = this.distributeVotes(
              hasThreshold.effVotes - threshhold,
              hasThreshold.party,
              candidates - hasThreshold,
              dm,
              xferVoteProb)
            hasThreshold.winner = true
            hasThreshold.effVotes = threshhold
            helper(remainingCandidates, hasThreshold :: elected, notElected)
          case None ⇒
            if (elected.length + candidates.size == dm) {
              dp(s"Exactly enough candidates left to finish the election.  ${candidates}")
              candidates.foreach(_.winner = true)
              ((candidates.toList ::: elected).reverse, notElected)
            } else {
              val loser = candidates.minBy(_.effVotes)
              dp(s"Cutting ${loser}.")
              val remainingCandidates = this.distributeVotes(loser.effVotes, loser.party, candidates - loser, dm,
                xferVoteProb)
              loser.effVotes = 0.0
              loser.winner = false
              helper(remainingCandidates, elected, loser :: notElected)
            }
        }
      }
    }

    dp(s"\nStvElection.countVotes: ")
    val (winners, losers) = helper(candidates, List(), List())
    dp(s"winners = ${winners}")
    dp(s"losers = ${losers}")
    assert(winners.length + losers.length == candidates.size, s"${winners.length} + ${losers.length} != ${candidates.size}")
    (winners, losers)
  }

  private def threshhold(candidates: Set[MutCandidate], toElect: Int): Double = {
    val votes = candidates.map {
      _.votes
    }.sum
    votes / (toElect + 1) + 1
  }


  private def distributeVotes(votes: Double, party: Party, remaining: Set[MutCandidate], dm: Int,
                              xferVoteProb: VoteXferFunc)(implicit debug: Boolean = false): Set[MutCandidate] = {
    //assert(remaining.size >= 1)
    val (sameParty, otherParties) = remaining.partition(_.party == party)
    if (sameParty.size > 0) {
      dp(s"transferring $votes votes from $party to ${
        sameParty.map {
          _.party
        }
      }.")
      // transfer votes to other members of the same party
      val xfer = votes / sameParty.size
      val r = sameParty.map { c ⇒ c.effVotes = c.effVotes + xfer; c } ++ otherParties
      assert(r.size == remaining.size)
      r
    } else {
      dp(s"transferring $votes votes from $party to ${
        otherParties.map {
          _.party
        }
      }.")
      // transfer votes to other party candidates
      val r = for {
        cand ← otherParties
        numOtherPartyCandidates = otherParties.count(c ⇒ c.party == cand.party)
        xfer = votes * xferVoteProb(party, cand.party, dm) / numOtherPartyCandidates
      } yield {
        cand.effVotes = cand.effVotes + xfer
        cand
      }
      assert(r.size == remaining.size)
      r
    }
  }

}


object ListElectionStrategy extends ElectionStrategy {
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

  def runElection(candidates: List[Candidate], dm: Int): (List[Candidate], List[Candidate]) = {
    //assert(!candidates.exists(c => c.winner))

    //val cand = candidates.filter(c => c.party.mainStream)
    val cand = candidates
    val totalVotes = cand.map(_.votes).sum

    val candByParty = cand.groupBy(_.party).toList
    val votesByParty = candByParty.map { case (p, cLst) => (p, cLst.map {
      _.votes
    }.sum)
    }
    val quotientsByParty = votesByParty.map { case (p, votes) => (p, dm * votes / totalVotes) }

    implicit val priority = Ordering.by { foo: (Party, Double) => foo._2 }
    val q = mutable.PriorityQueue[(Party, Double)](quotientsByParty: _*)

    def winners(elected: List[Candidate], unelected: List[Candidate]): (List[Candidate], List[Candidate]) = {
      if (elected.length == dm) {
        (elected, unelected)
      } else {
        val (party, quot) = q.dequeue()
        //        println(s"$party, $quot")
        //        println(s"   elected   = ${elected.mkString("\n\t")}")
        //        println(s"   unelected = ${unelected.mkString("\n\t")}")
        try {
          val winner = unelected.filter(c => c.party == party).maxBy(c => c.votes)
          q.enqueue((party, quot - 1))
          winners(winner.copy(winner = true) :: elected, unelected.filterNot(c => c == winner))
        } catch {
          case e: UnsupportedOperationException =>
            println(s"${party} didn't run enough candidates.")
            winners(Candidate(candidates.head.ridingId,
              "Party ran insufficient candidates",
              party, 0, 0, true, SeatType.RidingSeat) :: elected, unelected)
        }
      }
    }

    val result = winners(List(), candidates)
    //assert(!result._2.exists(c => c.winner))
    //    if (candidates.find(c => c.votes == 38229).isDefined) {
    //      println(result)
    //    }
    result
  }

}