package stv.electionStrategy

import scalatags.Text.all._

import stv.{Candidate, Party, ProvName, SeatType, VoteXfer, _}


/**
  * Just like STV but we want the AV name.
  */
object EkosAvRidingElectionStrategy extends StvRidingElectionStrategy(EkosXfer) {
  override val name = "AV-Ekos"
  override val shortName = "A"
  override val help = p("Alternative Vote: Elect one winner using a ranked ballot.  Drop the least " +
    "popular candidate and redistribute their ballots to the next choice candidate until one candidate " +
    "has at least 50%+1.  Use a 2nd choice function based on Ekos polling.")

  override def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {
    assert(dm == 1)
    super.runElection(candidates, dm)
  }
}

object ThinAirAvRidingElectionStrategy extends StvRidingElectionStrategy(ThinAirXfer) {
  override val name = "AV-ThinAir"
  override val shortName = "a"
  override val help = p("Alternative Vote: Elect one winner using a ranked ballot.  Drop the least " +
    "popular candidate and redistribute their ballots to the next choice candidate until one candidate " +
    "has at least 50%+1.  Use a made-up 2nd choice function.")

  override def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {
    assert(dm == 1)
    super.runElection(candidates, dm)
  }
}


object EkosStvRidingElectionStrategy extends StvRidingElectionStrategy(EkosXfer) {
  override val name = "STV-Ekos"
  override val shortName = "S"
}

object ThinAirStvRidingElectionStrategy extends StvRidingElectionStrategy(ThinAirXfer) {
  override val name = "STV-ThinAir"
  override val shortName = "T"
}


class StvRidingElectionStrategy(val voteXfer: VoteXfer) extends RidingElectionStrategy {

  val name = "STV"
  val shortName = "S"
  val help = div(p("Single Transferable Vote: Elect one or more winners using a ranked ballot. Establish a 'quota' or" +
    " " +
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


  def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {
    this.stvElection(candidates, dm, (p1: Party, p2: Party, dm: Int) => voteXfer.xfer(p1, p2), debug)
  }


  private case class MutCandidate(ridingId: RidingId,
                                  oldRidingId: Int,
                                  regionId: RegionId,
                                  provName: ProvName,
                                  name: String,
                                  party: Party,
                                  votes: Double,
                                  var effVotes: Double,
                                  var winner: Boolean,
                                  var order: Int) {


    override def toString = s"Candidate(${ridingId}, ${name}, ${party}, ${votes}, ${effVotes})"

  }

  private def stvElection(candidates: Seq[Candidate], dm: Int, xferVoteProb: VoteXferFunc, debug: Boolean):
  (Seq[Candidate],
    Seq[Candidate]) = {
    def c2m(c: Candidate): MutCandidate = MutCandidate(c.ridingId, c.oldRidingId, c.regionId, c.provName, c.name, c
      .party, c.votes,
      c.votes, false, c.order)
    def m2c(m: MutCandidate): Candidate = Candidate(m.ridingId, m.oldRidingId, m.regionId, m.provName, m.name, m
      .party, m.votes, m
      .effVotes, m.winner,
    SeatType.RidingSeat, m.order)

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

    var order = 1;

    val threshhold = this.threshhold(candidates, dm)
    dp(s"   threshhold = ${threshhold}")


    def helper(candidates: Set[MutCandidate],
               elected: List[MutCandidate],
               notElected: List[MutCandidate]
              ): (List[MutCandidate], List[MutCandidate]) = {
      val totalVotes = candidates.map(_.effVotes).sum

      dp(s"\nSTV: c=${candidates.toList.sortBy(_.effVotes).mkString("\t", "\n\t", "")}")
      dp(s"""\te=${elected.mkString("\t", "\n\t", "")}\n\tne=${notElected.mkString("\t", "\n\t", "")}\n\ttVotes =
$totalVotes, thr = $threshhold, dm = ${dm}""")
      if (dm == elected.length) {
        // We have all the winners we need -- election is over
        dp("All done")
        (elected.reverse, notElected ::: candidates.map { c ⇒ c.order = order; c }.toList)
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
            hasThreshold.order = order
            order = order + 1
            helper(remainingCandidates, hasThreshold :: elected, notElected)
          case None               ⇒
            if (elected.length + candidates.size == dm) {
              dp(s"Exactly enough candidates left to finish the election.  ${candidates}")
              candidates.foreach { c ⇒ c.winner = true; c.order = order }
              ((candidates.toList ::: elected).reverse, notElected)
            } else {
              val loser = candidates.minBy(_.effVotes)
              dp(s"Cutting ${loser}.")
              val remainingCandidates = this.distributeVotes(loser.effVotes, loser.party, candidates - loser, dm,
                xferVoteProb)
              //loser.effVotes = 0.0
              loser.winner = false
              loser.order = order
              order = order + 1
              helper(remainingCandidates, elected, loser :: notElected)
            }
        }
      }
    }

    dp(s"\nStvElection.countVotes: ")
    val (winners, losers) = helper(candidates, List(), List())
    dp(s"winners = ${winners}")
    dp(s"losers = ${losers}")
    assert(winners.length + losers.length == candidates.size, s"${winners.length} + ${losers.length} != ${
      candidates
        .size
    }")
    (winners, losers)
  }

  private def threshhold(candidates: Set[MutCandidate], toElect: Int): Double = {
    val votes = candidates.foldLeft(0.0)(_ + _.votes)
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
