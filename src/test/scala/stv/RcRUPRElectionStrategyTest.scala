package stv

import stv.Party.{Con, Lib, NDP}
import stv.ProvName.AB
import stv.SeatType._
import stv.electionStrategy.{RcStvElectionStrategy, RcStvProvAdjustment}
import stv.io.DesignReader
import utest._
import utest.framework.{Result, Tree}

/**
  * Created by bwbecker on 2016-10-29.
  */
object RcRUPRElectionStrategyTest extends TestSuite with Common {

  val rcStrat = new RcStvElectionStrategy(TestXfer)

  val candidates = Seq(
    Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 0),
    Candidate("1", 1, "R1", AB, "C1", Lib, 200, 200, false, RidingSeat, 0),
    Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 0)
  )


  val tests = this {

    'RcRUP_candidate_to_cut - {
      "lowest effective votes" - {
        val cut1 = rcStrat.candidateToCut(candidates, false)
        assert(cut1.get == candidates(0))

        val cut2 = rcStrat.candidateToCut(candidates, true)
        assert(cut2.get == candidates(0))
      }

      "protect last in old riding" - {
        val candidates = this.candidates :+
          Candidate("1", 2, "R1", AB, "C3", Lib, 10, 10, false, RidingSeat, 0)
        val cut = rcStrat.candidateToCut(candidates, true)
        assert(cut.get == candidates(0))
      }

      "cut last in old riding if not protected" - {
        val candidates = this.candidates :+
          Candidate("1", 2, "R1", AB, "C3", Lib, 10, 10, false, RidingSeat, 0)
        val cut = rcStrat.candidateToCut(candidates, false)
        assert(cut.get == candidates(3))
      }

      "no one to cut" - {
        val candidates = Seq(
          Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 0),
          Candidate("1", 2, "R1", AB, "C1", Lib, 200, 200, false, RidingSeat, 0),
          Candidate("1", 3, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 0)
        )

        val cut = rcStrat.candidateToCut(candidates, true)
        assert(cut.isEmpty)
      }
    }


    "RcRUP Election Strategy:" - {

      "Base Case" - {
        val (elected, unelected) = rcStrat.runElection(List[Candidate](), 0)
        assert(elected.isEmpty)
        assert(unelected.isEmpty)
      }

      "1 candidate w/ transfers" - {
        val (e, u) = rcStrat.runElection(candidates, 1)
        assert(e.length == 1)
        assert(u.length == 2)

        val con = candidates(0).copy(order = 1)
        val lib = candidates(1).copy(order = 3, effVotes = 200 + 25 + 75, winner = true)
        val ndp = candidates(2).copy(order = 2)

        assert(e.head == lib)

        val us = u.sortBy(c ⇒ c.order)
        assert(us(0) == con)
        assert(us(1) == ndp)
      }


      "2 candidates" - {
        /*
        dm = 2 out of five candidates.  There are three old ridings.
        (oldRiding, Name, Party, Votes, effVotes)
        (1, C0, Con, 100, 100 + 75)         // 3 cut;  175*.25/2 to each lib
        (1, C1, Lib, 200, 200 + 150*.5/2 + 175*.25/2)
        (1, C2, NDP, 150, 150)              //2 cut; 150*.5/2 to each lib
        (2, C3, Lib, 125, 125 + 150*.5/2 + 175*.25/2)  // protected
        (3, C4, Con, 75, 75)              //1 cut (not protected); 75 to C0

         */

        val candidates = this.candidates :+
          Candidate("1", 2, "R1", AB, "C3", Lib, 125, 125, false, RidingSeat, 0) :+
          Candidate("1", 3, "R1", AB, "C4", Con, 75, 75, false, RidingSeat, 0)

        val (e, u) = rcStrat.runElection(candidates, 2)
        assert(e.length == 2)
        assert(u.length == 3)

        val _c4 = candidates(4).copy(order = 1)
        val _c2 = candidates(2).copy(order = 2)
        val _c0 = candidates(0).copy(order = 3, effVotes = 100 + 75)
        val _c3 = candidates(3).copy(order = 4, effVotes = 125 + 150 * .5 / 2 + 175 * .25 / 2, winner = true)
        val _c1 = candidates(1).copy(order = 5, effVotes = 200 + 150 * .5 / 2 + 175 * .25 / 2, winner = true)
        val con1 = candidates(0).copy(order = 1)
        val lib1 = candidates(1).copy(order = 4, effVotes = 200 + 100 * .25 / 2 + 150 * .5 / 2, winner = true)
        val ndp = candidates(2).copy(order = 2, effVotes = 150)
        val lib2 = candidates(3).copy(order = 3, effVotes = 125 + 100 * .25 / 2 + 150 * .5 / 2, winner = true)

        assert(e == Seq(_c1, _c3))

        val us = u.sortBy(c ⇒ c.order)
        assert(us == Seq(_c4, _c2, _c0))
      }
    }


  }

}
