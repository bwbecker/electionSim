package stv.electionStrategy

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._

import scala.collection.mutable

trait ElectionStrategy {
  val name: String
  val shortName: String
  val help: TypedTag[String]

  val description: TypedTag[String]
  implicit var debug = false

  // dbug print
  protected def dp(s: String)(implicit debug: Boolean): Unit = {
    if (debug) {
      println(s)
    }
  }

}

/**
  * Created by bwbecker on 2016-08-18.
  */
trait RidingElectionStrategy extends ElectionStrategy {

  def sortCandidates(candidates:Seq[Candidate]):Seq[Candidate] = candidates.sortBy(c ⇒ (-c.order, -c.votes))

  def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate])

  /**
    * Return the same list but with the effective votes either the total of
    * all votes for the party (assigned to the candidate with the most votes)
    * or 0.
    *
    */
  def collapseCandidates(candidates: Seq[Candidate]): Seq[Candidate] = {
    val byParty = candidates.groupBy(_.party)
    val winnersByParty = byParty.map(
      t ⇒ t._1 → t._2.maxBy(_.votes)
    )
    for (c ← candidates) yield {
      if (c == winnersByParty(c.party)) {
        c.copy(effVotes = byParty(c.party).map(_.votes).sum)
      } else {
        c.copy(effVotes = 0)
      }
    }
  }

}

trait TopupElectionStrategy extends ElectionStrategy {

  def runElection(regionId: String,
                  allCandidates: Vector[Candidate],
                  numSeats: Int,
                  threshhold: Double): Vector[Candidate]
}

object TopupStrategy extends TopupElectionStrategy {
  val name = "Top-up"
  val shortName = "Top-up"
  val help = p("Iteratively choose the most disadvantaged party.")
  val description = p("Iteratively choose the most disadvantaged party.")

  def runElection(regionId: String, allCandidates: Vector[Candidate], numTopupSeats: Int, threshhold:
  Double): Vector[Candidate] = {

    val numRidingSeats = allCandidates.count(c ⇒ c.winner)
    var order = 0

    def helper(topups: Vector[Candidate]): Vector[Candidate] = {
      if (topups.length == numTopupSeats) {
        topups //.reverse
      } else {
        val a = new Analysis(allCandidates ++ topups, numRidingSeats + numTopupSeats)
        val stats = a.statsByParty.filter(s ⇒ s.pctVote >= threshhold)


        val disadvantaged = stats.maxBy(s ⇒ s.deservedMPs - s.mps)

        val cand = Candidate("", 0, regionId, allCandidates.head.provName, "Topup Candidate", disadvantaged.party, 0.0,
          0.0, true, SeatType.TopupSeat, order)
        order = order + 1
        helper(topups :+ cand)

      }
    }

    helper(Vector())
  }
}


object NotApplicableTopupElectionStrategy extends TopupElectionStrategy {
  val name: String = "NA"
  val shortName = "x"
  val help = p("No election strategy is applicable in this situation.")
  val description: TypedTag[String] = p("A placeholder election strategy for where no top-up strategy is applicable.")

  def runElection(regionId: String, allCandidates: Vector[Candidate],
                  numTopupSeats: Int, threshhold: Double): Vector[Candidate] = {
    throw new Exception("Election Strategy is not applicable.")
  }

}




object RidingElectionStrategy {
  val values = Vector(
    FptpRidingElectionStrategy,
    EkosAvRidingElectionStrategy,
    //ThinAirAvRidingElectionStrategy,
    EkosStvRidingElectionStrategy,
    //ThinAirStvRidingElectionStrategy,
    ListRidingElectionStrategy,
    NotApplicableRidingElectionStrategy
  )

  val singleMbrStrategies = Vector(
    FptpRidingElectionStrategy,
    //ThinAirAvRidingElectionStrategy,
    EkosAvRidingElectionStrategy
  )

  val multiMbrStrategies = Vector(
    EkosStvRidingElectionStrategy,
    //ThinAirStvRidingElectionStrategy,
    ListRidingElectionStrategy,
    new RcStvElectionStrategy(EkosXfer)
  )
}


object FptpRidingElectionStrategy extends RidingElectionStrategy {
  override val name = "FPTP"
  override val shortName = "F"
  override val description = p("""After collapsing all candidates running for the same party into one virtual
      |candiate, choose the virtual candidate with the most votes.
    """.stripMargin
  )
  override val help = p("First-Past-The-Post -- the candidate with the most votes wins.")

  override def sortCandidates(candidates: Seq[Candidate]): Seq[Candidate] = candidates.sortBy(c ⇒ (-c.votes, c.name))

  override def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {
    assert(dm == 1)
    val cCands = this.collapseCandidates(candidates)
    val winner = cCands.maxBy(_.effVotes)
    val (elected, unelected) = cCands.partition(c => c == winner)
    assert(elected.length == 1)
    assert(unelected.length == cCands.length - 1)
    (elected.map { c ⇒ c.copy(winner = true) }, unelected)
  }
}

object NotApplicableRidingElectionStrategy extends RidingElectionStrategy {
  val name: String = "NA"
  val shortName = "x"
  val help = p("No election strategy is applicable in this situation.")
  val description: TypedTag[String] = p("An election strategy for where none are applicable.  For example, for " +
    "multi-member" +
    " ridings in a FPTP simulation.")

  def runElection(candidates: Seq[Candidate], dm: Int): (Seq[Candidate], Seq[Candidate]) = {
    throw new Exception("Election Strategy is not applicable.")
  }

}

