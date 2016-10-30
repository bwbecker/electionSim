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


  /** Find the candidate with the fewest effective votes that still has someone else from
    * their local riding that's in the race.
    */
  def candidateToCut(remaining: Seq[Candidate], protectLast: Boolean): Option[Candidate] = {
    def helper(c: Seq[Candidate]): Option[Candidate] = {
      c match {
        case head +: tail if (!protectLast)             ⇒ Some(head)
        case head +: tail if (tail.find(c ⇒
          c.oldRidingId == head.oldRidingId).isDefined) ⇒ Some(head)
        case _ +: tail                                  ⇒ helper(tail)
        case Seq()                                      ⇒ None
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

    val (sameParty, otherParties) = remaining.partition(c ⇒ c.party == cut.party)
    if (sameParty.length > 1) {
      val n = sameParty.length - 1
      sameParty.filterNot(c ⇒ c == cut)
        .map(c ⇒ c.copy(effVotes = c.effVotes + cut.effVotes / n)) ++ otherParties

    } else {
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

  }


  /**
    * Return (a, b, c) where a is the list of elected candidates, b is the list
    * of remaining unelected candidates, and c is the id of the old riding which
    * becomes the adjustment riding.
    */
  def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {

    //debug = candidates.head.ridingId == "BC.Richmond"

    // This is an adjustment riding if dm is less than the number of old ridings.
    // If it's not an adjustment riding, we need 1 MP per old riding and thus set the protection flag
    val isAdjustmentRiding = dm < candidates.map(c ⇒ c.oldRidingId).distinct.length
    var protectLastCandidateInOldRiding = !isAdjustmentRiding



    // Return (list of elected, list of unelected)
    def helper(remaining: Seq[Candidate], notElected: Seq[Candidate]): (Seq[Candidate], Seq[Candidate]) = {

      def assembleResult: (Seq[Candidate], Seq[Candidate]) = {
        val n = notElected.length
        val rLen = remaining.length

        val candByPriority = remaining.sortBy(c ⇒ -c.effVotes).zipWithIndex
        val take = candByPriority.take(dm).map { t ⇒
          t._1.copy(winner = true, order = n + rLen - t._2)
        }
        val leave = candByPriority.drop(dm).map { t ⇒
          t._1.copy(order = n + rLen - t._2)
        }


        assert(take.length + leave.length == remaining.length)
        // no duplicate old ridings
        assert(take.map(c ⇒ c.oldRidingId).distinct.length == take.length)
        //          println(take.map(c ⇒ c.order).mkString(", ") + " | " +
        //            leave.map(c ⇒ c.order).mkString(", ") + " | " +
        //            notElected.map(c ⇒ c.order).mkString(", ")
        //          )
        (take, leave ++ notElected)

      }

      // Keep cutting candidates until we're down to one for each old riding.  Then take
      // the dm candidates with the most votes.
      if (remaining.length == dm) {
        assembleResult
      } else {
        candidateToCut(remaining, protectLastCandidateInOldRiding) match {
          case None ⇒ assembleResult

          case Some(cut) ⇒
            dp(f"Cutting ${cut.name.take(18)}%-20s with ${cut.effVotes}%7.1f votes")
            if (debug && remaining.length < dm + 3) {
              println("  Remaining = " + remaining.map(c ⇒
                f"${c.name.take(18)}%-20s ${c.effVotes}%7.1f").mkString("\n\t"))
            }
            // Was this the last one?
            protectLastCandidateInOldRiding = protectLastCandidateInOldRiding ||
              !remaining.exists(c ⇒ c != cut && c.oldRidingId == cut.oldRidingId)
            helper(transferVotes(cut, remaining), cut.copy(order = notElected.length + 1) +: notElected)
        }
      }
    }

    val (elected, unelected) = helper(candidates, Vector[Candidate]())
    assert(elected.length + unelected.length == candidates.length)
    assert(elected.length == dm)
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
        //        println("adjustMPs: returning " + adjustMPs)
        adjustMPs.toVector
      } else {
        val a = new Analysis(
          alreadyElected ++ adjustMPs ++ notElected, // status of all the MPs
          mpsNeeded + alreadyElected.length, // total MPs to elect
          false)

        val mostDisadvantagedParty =
          a.statsByParty
            .filter(s ⇒ s.party.mainStream) // filter out small parties
            .maxBy(s ⇒ s.deservedMPs - (alreadyElected.count(c ⇒ c.party == s.party) +
            adjustMPs.count(c ⇒ c.party == s.party)))
            .party

        //        println("partyAdvantages = " + a.statsByParty
        //          .filter(s ⇒ s.party.mainStream)
        //          .map(s ⇒ s"${s.party}: ${s.deservedMPs} - (${alreadyElected.count(c ⇒ c.party == s.party)} + " +
        //            s"${adjustMPs.count(c ⇒ c.party == s.party)}) = ${
        //              s.deservedMPs - (alreadyElected.count(c ⇒ c.party == s.party) +
        //                adjustMPs.count(c ⇒ c.party == s.party))
        //            }")
        //        )

        // Candidate in the right party and a riding with no MP that has the most votes.
        val candList = notElected.filter(c ⇒
          c.party == mostDisadvantagedParty &&
            oldRidingsWithNoMP.contains(c.oldRidingId))

        //println(s"   Choose MP for ${mostDisadvantagedParty} from ${candList}")

        val cand = if (candList.nonEmpty) {
          candList.maxBy(c ⇒ c.votes)
        } else {
          //println(s"\n*** Can't find a riding without an MP for with a candidate from ${mostDisadvantagedParty}.")
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