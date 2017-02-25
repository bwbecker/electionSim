package stv.electionStrategy

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.{Candidate, Party, VoteXfer}

/**
  * Created by bwbecker on 2017-02-16.
  */
class LPR_ElectionStrategy(val voteXfer: VoteXfer) extends RidingElectionStrategy {

  debug = true

  case class Cand(candidate: Candidate,
                  protect: Boolean = false) {
    def effVotes = candidate.effVotes

    def ridingId = candidate.ridingId

    def oldRidingId = candidate.oldRidingId

    def party = candidate.party

    def withMoreVotes(votes: Double): Cand = this.withVotes(this.effVotes + votes)

    def withVotes(votes: Double): Cand = Cand(candidate.copy(effVotes = votes), protect)

    def withOrder(o:Int):Cand = Cand(candidate.copy(order = o), protect)

    def name = candidate.name
  }

  type CandSeq = Seq[Cand]


  private var order = 0

  def nextOrder: Int = {
    this.order = this.order + 1
    this.order
  }

  val name: String = "Local-PR"
  val shortName: String = "LPR"
  val help: TypedTag[String] = p("""An STV-like strategy that guarantees electing a candidate
  in each old riding that makes up a multi-member riding.  Transfers surplus votes as well
  as votes for losing candidates.""")

  val description: TypedTag[String] = div(
    p("""An STV-like strategy that guarantees electing a candidate in each old
      riding that makes up a multi-member riding.  It does so by prohibiting the
      elimination of the last candidate
      in the old riding.  Surplus votes are transferred.
      """)
  )

  def sumVotes(candidates: CandSeq): Double =
    candidates.foldRight(0.0)((c, sum) ⇒ sum + c.effVotes)

  def hasSurplus(candidates: CandSeq, quota: Double): Option[Cand] = {
    val max = candidates.maxBy(c ⇒ c.effVotes)
    if (max.effVotes > quota) Some(max) else None
  }


  /** Remove a candidate from a sequence */
  def minus(s: CandSeq, c: Cand): CandSeq = s.filter(x ⇒ x != c)

  /** Choose a candidate to cut -- the one with the fewest effective votes that isn't protected.
    * Return the remaining hopeful candidates.  Anyone found in need of protection is protected. */
  def candToCut(winners: CandSeq, hopeful: CandSeq): (Option[Cand], CandSeq) = {
    val (protect, other) = hopeful.partition(c ⇒ c.protect)
    if (other.length == 0) {
      (None, hopeful)
    } else {
      val toCut = other.minBy(c ⇒ c.effVotes)
      if (this.needsProtecting(toCut, winners, other)) {
        // protect toCut and look for someone else
        dp(s"Protecting ${toCut.name}")
        candToCut(winners, toCut.copy(protect = true).withOrder(this.nextOrder) +: minus(hopeful, toCut))
      } else {
        (Some(toCut), minus(hopeful, toCut))
      }
    }
  }

  /**
    * Does this candidate need to be protected?  Last one standing in his or her riding?
    */
  def needsProtecting(cand: Cand, winners: CandSeq, hopeful: CandSeq): Boolean = {
    !winners.exists(c ⇒ c.oldRidingId == cand.oldRidingId) &&
      !hopeful.exists(c ⇒ c != cand && c.oldRidingId == cand.oldRidingId)
  }

  /**
    * Transfer votes from a losing candidate to the hopeful candidates.
    * Votes are transferred at full value according to the vote transfer function.
    */
  def transferVotes(votes: Double, party: Party, hopeful: CandSeq): CandSeq = {
    val (sameParty, otherParties) = hopeful.partition(c ⇒ c.party == party)
    if (sameParty.length > 0) {
      dp(f"transferring ${votes}%10.2f votes from ${party} to ${sameParty.map {_.candidate.name}}.")

      val xferValue = votes / sameParty.length
      sameParty.map(c ⇒ c.withMoreVotes(xferValue)) ++: otherParties
    } else {
      val sb = new StringBuffer(s"transfer ${votes} from ${party} to ")
      val r = for {
        cand ← otherParties
        numOtherPartyCandidates = otherParties.count(c ⇒ c.party == cand.party)
        xfer = votes * voteXfer.xfer(party, cand.party) / numOtherPartyCandidates
      } yield {
        if (xfer > 0) {
          sb.append(s"${cand.candidate.name} : ")
          cand.withMoreVotes(xfer)
        } else {
          cand
        }
      }

      dp(sb.toString)
      assert(r.size == hopeful.size)
      r
    }
  }

  def currentSituation(winners: CandSeq, hopeful: CandSeq, losers: CandSeq): String = {
    def x(x: Cand): String = {
      val protect = if (x.protect) "P" else " "
      f"\n\t${protect} ${x.effVotes}%10.2f\t${x.party}\t${x.candidate.name}%28s ${x.candidate.oldRidingId}"
    }

    val s = new StringBuilder()
    s.append("\nWinners:")
    for (w ← winners) {
      s.append(x(w))
    }
    s.append("\nHopeful:")
    for (h ← hopeful.sortBy(c ⇒ -c.effVotes)) {
      s.append(x(h))
    }
    s.append("\nLosers:")
    for (l ← losers) {
      s.append(x(l))
    }

    s.toString()
  }

  /**
    * Calculate who wins and who loses.
    *
    * @param orig_candidates The candidates vying for the seats
    * @param dm              The number of seats available
    * @return (elected candidates, not-elected candidates)
    */
  def runElection(orig_candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {
    dp(s"""\n\nRunning Election\n""")
    val candidates = orig_candidates.sortBy(c ⇒ c.effVotes).map(c ⇒ Cand(c))

    val quota = sumVotes(candidates) / (dm + 1) + 1

    def helper(winners: CandSeq,
               hopeful: CandSeq,
               losers: CandSeq): (CandSeq, CandSeq) = {

      dp(currentSituation(winners, hopeful, losers))
      if (winners.length == dm || hopeful.count(c ⇒ !c.protect) == 0) {
        val (protect, others) = hopeful.partition(c ⇒ c.protect)
        (winners ++: protect, others ++: losers)
      } else {
        hasSurplus(hopeful, quota) match {
          case Some(newWinner) ⇒
            if (winners.exists(w ⇒ w.oldRidingId == newWinner.oldRidingId)) {
              // This one can't win because we only allow one winner per riding
              dp(s"\n*** 2nd winner in old riding ${newWinner.name} ${newWinner.ridingId}\n")
              helper(winners,
                transferVotes(newWinner.effVotes, newWinner.party, minus(hopeful, newWinner)),
                newWinner.withOrder(this.nextOrder) +: losers
              )
            } else {
              val remainingHopefuls = minus(hopeful, newWinner)
              helper(newWinner.withVotes(quota).withOrder(this.nextOrder) +: winners,
                transferVotes(newWinner.effVotes - quota, newWinner.party, remainingHopefuls),
                losers
              )
            }

          case None ⇒
            val (toCut, remainingHopefuls) = candToCut(winners, hopeful)
            toCut match {
              case Some(cut) ⇒
                helper(winners, transferVotes(cut.effVotes, cut.party, remainingHopefuls), cut.withOrder(this.nextOrder) +: losers)
              case None      ⇒
                dp("No one to cut!")
                (winners ++: remainingHopefuls, losers)
            }

        }

      }
    }


    val (elected, notElected) = helper(Seq(), candidates, Seq())
    assert(elected.length == dm, "Not enough winners!")
    assert(elected.length + notElected.length == orig_candidates.length, "Missing some candidates!")

    dp("\n\nFinal Results:")
    dp(currentSituation(elected, Seq(), notElected))
    (elected.map(c ⇒ c.candidate.copy(winner = true, protect = c.protect)), notElected.map(_.candidate))
  }

}
