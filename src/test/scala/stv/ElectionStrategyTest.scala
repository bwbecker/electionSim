package stv


import utest._
import utest.framework.{Result, Tree}
import stv.ProvName._
import stv.Party._
import stv.SeatType._
import stv.io.DesignReader
import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.SeatType.RidingSeat
import stv.electionStrategy.{FptpRidingElectionStrategy, StvRidingElectionStrategy}

/**
  * Created by bwbecker on 2016-10-07.
  */

object ElectionStrategyTest extends TestSuite with Common {


  val c = Seq(
    Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 0),
    Candidate("1", 1, "R1", AB, "C1", Lib, 200, 200, false, RidingSeat, 0),
    Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 0)
  )

  val c2 = c :+ Candidate("1", 1, "R1", AB, "C3", Con, 150, 150, false, RidingSeat, 0)


  val tests = this {

    "FptpRidingElectionStrategy:" - {

      "on candidates with distinct parties" - {
        val (elected, unelected) = FptpRidingElectionStrategy.runElection(c, 1)
        assert(elected == List(Candidate("1", 1, "R1", AB, "C1", Lib, 200, 200, true, RidingSeat, 0)))
        assert(unelected == List(
          Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 0),
          Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 0)
        ))
      }

      "on candidates with same party" - {
        val (elected, unelected) = FptpRidingElectionStrategy.runElection(c2, 1)
        assert(elected == List(Candidate("1", 1, "R1", AB, "C3", Con, 150, 250, true, RidingSeat, 0)))
        assert(unelected.length == 3)
        assert(unelected.map { c ⇒ c.name }.toSet == Set("C0", "C1", "C2"))
        assert(unelected(0).party == Con && unelected(0).effVotes == 0)
        assert(unelected(1).effVotes > 0)
      }
    }



    "StvRidingElectionStrategy:" - {


      val stvStrat = new StvRidingElectionStrategy(TestXfer)


      "on candidates with distinct parties" - {

        "one candidate right at quota" - {
          val candidates = c :+ Candidate("1", 1, "R1", AB, "C3", Grn, 452, 452, false, RidingSeat, 0)
          val quota = candidates.foldLeft(0.0)(_ + _.votes) / (1 + 1) + 1
          assert(quota == 452)
          val (elected, unelected) = stvStrat.runElection(candidates, 1)
          assert(elected.length == 1)
          assert(elected.head == Candidate("1", 1, "R1", AB, "C3", Grn, 452, quota, true, RidingSeat, 1))
          assert(unelected.length == 3)
          assert(unelected.foldLeft(0.0)(_ + _.effVotes) == 450)
        }
      }

      "one candidate just under quota" - {
        val candidates = c :+ Candidate("1", 1, "R1", AB, "C3", Grn, 450, 450, false, RidingSeat, 0)
        val quota = candidates.foldLeft(0.0)(_ + _.votes) / (1 + 1) + 1
        assert(quota == 451)
        val (elected, unelected) = stvStrat.runElection(candidates, 1)
        assert(elected.length == 1)
        assert(elected.head == Candidate("1", 1, "R1", AB, "C3", Grn, 450, quota, true, RidingSeat, 2))

        assert(unelected.length == 3)

        assert(unelected(0) == Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 1))
        // 25 votes to Lib; 10 to Grn; none to NDP

        assert(unelected(1) == Candidate("1", 1, "R1", AB, "C1", Lib, 200, 225, false, RidingSeat, 3))
        assert(unelected(2) == Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 3))

      }


      "winner needs multiple transfers to make quota" - {
        val candidates = c :+ Candidate("1", 1, "R1", AB, "C3", Grn, 148, 148, false, RidingSeat, 0)
        val quota = candidates.foldLeft(0.0)(_ + _.votes) / (1 + 1) + 1
        assert(quota == 300)
        val (elected, unelected) = stvStrat.runElection(candidates, 1)
        assert(elected.length == 1)
        assert(elected.head == Candidate("1", 1, "R1", AB, "C1", Lib, 200, quota, true, RidingSeat, 3))

        assert(unelected.length == 3)
        val u = unelected.sortBy(_.name)
        // Cons out.  25 to Lib; 10 to Grn.  NDP out.  75 to Lib.
        assert(u(0) == Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 1))
        assert(u(1) == Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 2))
        assert(u(2) == Candidate("1", 1, "R1", AB, "C3", Grn, 148, 158, false, RidingSeat, 4))

      }


      "winner's excess redistributed" - {
        val candidates = Seq(
          Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 0),
          Candidate("1", 1, "R1", AB, "C1", Lib, 350, 350, false, RidingSeat, 0),
          Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 0),
          Candidate("1", 1, "R1", AB, "C3", Grn, 198, 198, false, RidingSeat, 0)
        )

        val quota = candidates.foldLeft(0.0)(_ + _.votes) / (1 + 1) + 1
        assert(quota == 400)
        val (elected, unelected) = stvStrat.runElection(candidates, 1)
        assert(elected.length == 1)
        assert(elected.head == Candidate("1", 1, "R1", AB, "C1", Lib, 350, quota, true, RidingSeat, 3))

        assert(unelected.length == 3)
        val u = unelected.sortBy(_.name)
        // Cons out.  25 to Lib; 10 to Grn.  NDP out.  75 to Lib.
        // Lib now has 450 but only needs 400.  So .1*50=5 to greens
        assert(u(0) == Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 1))
        assert(u(1) == Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 2))
        assert(u(2) == Candidate("1", 1, "R1", AB, "C3", Grn, 198, 213, false, RidingSeat, 4))

      }
    }

  }
}
