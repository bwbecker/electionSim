package stv.electionStrategy

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._

//import scala.collection.mutable.{Map ⇒ MutMap, Set ⇒ MutSet}

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
        cand
        //cand.copy(effVotes = cand.effVotes + cut.effVotes * xferPct / num)
      } else {
        cand
      }
    }
  }


  def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {


    // Return (list of elected, list of unelected)
    def helper(remaining: Seq[Candidate], notElected: Seq[Candidate]): (Seq[Candidate], Seq[Candidate]) = {
      // Keep cutting candidates until we're down to one for each old riding.  Then take
      // the dm candidates with the most votes.
      candidateToCut(remaining) match {
        case None ⇒
          val n = notElected.length
          val candByPriority = remaining.sortBy(c ⇒ -c.effVotes).zipWithIndex
          val take = candByPriority.take(dm).map { t ⇒
            t._1.copy(winner = true, order = n + dm - t._2)
          }
          val leave = candByPriority.drop(dm).map { t ⇒
            t._1.copy(order = n + dm - t._2)
          }
          assert(take.length + leave.length == remaining.length)
          (take, leave ++ notElected)

        case Some(cut) ⇒ helper(transferVotes(cut, remaining), cut.copy(order = notElected.length + 1) +: notElected)
      }
    }

    val (elected, unelected) = helper(candidates, Vector[Candidate]())
    assert(elected.length + unelected.length == candidates.length)
    (elected, unelected)
  }


}


object RcStvProvAdjustment {


  def adjustmentMPs(prov: Province,
                    alreadyElected: Vector[Candidate],
                    notYetElected: Vector[Candidate]): Vector[Candidate] = {
    /*
     * prov is just here for the structure.  Digging down to the candidates isn't useful because they haven't
     * been elected yet.  That's why we have the alreadyElected list.
     */

    val mpsNeeded = prov.regions.foldLeft(0) { (a, r) ⇒ a + r.topUpSeats }


    def helper(adjustMPs: Set[Candidate],
               notElected: Set[Candidate],
               oldRidingsWithNoMP: Set[Int]): Vector[Candidate] = {

      adjustMPs.foreach { c ⇒
        assert(!oldRidingsWithNoMP.contains(c.oldRidingId))
      }
      oldRidingsWithNoMP.foreach { id ⇒
        assert(!adjustMPs.exists(c ⇒ c.oldRidingId == id) && !alreadyElected.exists(c ⇒ c.oldRidingId == id))
      }

      if (adjustMPs.size == mpsNeeded) {
        adjustMPs.toVector.map(c ⇒ c.copy(winner = true, seatType = SeatType.AdjustmentSeat))
      } else {
        val a = new Analysis(alreadyElected ++ adjustMPs ++ notElected, mpsNeeded, false)
        val mostDisadvantagedParty = if (false) {
          // Favour larger parties
          a.statsByParty
            .filter(s ⇒ s.pctVote > 0.02) // filter out small parties
            .minBy(s ⇒ s.pctMPs - s.pctVote)
            .party
        } else {
          // Favour smaller parties
          a.statsByParty
            .filter(s ⇒ s.pctVote > 0.02) // filter out small parties
            .maxBy(s ⇒ 1 - (s.numRidingMPs + s.numTopupMPs) / s.deservedMPs)
            .party
        }

        // Candidate in the right party and a riding with no MP that has the most votes.
        val candList = notElected.filter(c ⇒
          c.party == mostDisadvantagedParty &&
            oldRidingsWithNoMP.contains(c.oldRidingId))


        val cand = if (candList.nonEmpty) {
          candList.maxBy(c ⇒ c.votes)
        } else {
          println(s"\n*** Can't find a riding without an MP for with a candidate from ${mostDisadvantagedParty}.")
          // Construct an imaginary Candidate for now that is consistent in most respects.
          val oldRidingId = oldRidingsWithNoMP.head
          val modelCandidate = notYetElected.find(c ⇒ c.oldRidingId == oldRidingId).get // could bomb...
          modelCandidate
        }
        helper(adjustMPs + cand.copy(winner = true, seatType = SeatType.AdjustmentSeat),
          notElected - cand,
          oldRidingsWithNoMP - cand.oldRidingId)
      }
    }

    val allOldRidingIds = Set(prov.regions.flatMap(region ⇒
      region.ridings.flatMap(riding ⇒
        riding.candidates0.map(c ⇒ c.oldRidingId))
    ): _*)
    val oldRidingIdsWithMPs = Set(alreadyElected.map(c ⇒ c.oldRidingId): _*)

    helper(Set[Candidate](), Set(notYetElected: _*), allOldRidingIds -- oldRidingIdsWithMPs)

  }
}