package stv.electionStrategy

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.{Candidate, VoteXfer}

/**
  * Created by bwbecker on 2016-10-24.
  */
class RcStvElectionStrategy(val voteXfer: VoteXfer) extends RidingElectionStrategy {

  val name: String = "Riding-CentricSTV"
  val shortName: String = "rcSTV"
  val help: TypedTag[String] = p("""An STV-like strategy that guarantees electing a candidate in each old riding
  that makes up a multi-member riding.""")

  val description: TypedTag[String] = div(
    p("""An STV-like strategy that guarantees electing a candidate in each old
      riding that makes up a multi-member riding.  It does so by prohibiting the elimination of the last candidate
      in the old riding.
      """),
    p("""The multi-member riding typically has at least one of its seats designated as an adjustment seat.
      The topup algorithm selects a candidate from an empty old riding (ie the adjustment seat).""")
  )

  override val debug = false


  /** Find the candidate with the fewest effective votes that still has someone else from
    * their local riding that's in the race.
    */
  def candidateToCut(remaining: Seq[Candidate]): Option[Candidate] = {
    def helper(c: Seq[Candidate]): Option[Candidate] = {
      c match {
        case head +: tail if (tail.find(c ⇒ c.oldRidingId == head.oldRidingId).isDefined) ⇒ Some(head)
        case _ +: tail                                                                    ⇒ helper(tail)
        case Seq()                                                                        ⇒ None
      }
    }
    helper(remaining.sortBy(c ⇒ c.effVotes))
  }

  /**
    * Transfer votes from the cut candidate to other candidates.  For now, just distribute evenly
    * to all the remaining candidates in the given party.  Later might consider taking into account
    * local riding effects.
    */
  def transferVotes(cut: Candidate, remaining: Seq[Candidate]): Seq[Candidate] = {

    for {
      (party, candList) ← remaining.filterNot(c ⇒ c == cut).groupBy(_.party).toSeq
      num = candList.length
      cand ← candList
      xferPct = voteXfer.xfer(cut.party, party)
    } yield {
      if (cand.party == cut.party) {
        cand.copy(effVotes = cand.effVotes + cut.effVotes / num)
      } else if (xferPct > 0) {
        cand.copy(effVotes = cand.effVotes + cut.effVotes * xferPct / num)
      } else {
        cand
      }
    }
  }


  def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {


    def helper(remaining: Seq[Candidate], notElected: Seq[Candidate]): (Seq[Candidate], Seq[Candidate]) = {
      candidateToCut(remaining) match {
        case None      ⇒ (remaining.sortBy(c ⇒ -c.effVotes).take(dm).zipWithIndex.map { t ⇒
          val n = notElected.length
          t._1.copy(winner = true, order = n + dm - t._2)
        }, notElected)
        case Some(cut) ⇒ helper(transferVotes(cut, remaining), cut.copy(order = notElected.length + 1) +: notElected)
      }
    }

    helper(candidates, Vector[Candidate]())
  }


}
