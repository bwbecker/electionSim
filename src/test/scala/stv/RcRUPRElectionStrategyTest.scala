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
        // 2nd candidate must come from a different old riding.
        val candidates = this.candidates :+
          Candidate("1", 2, "R1", AB, "C3", Lib, 125, 125, false, RidingSeat, 0)

        val (e, u) = rcStrat.runElection(candidates, 2)
        assert(e.length == 2)
        assert(u.length == 2)

        val con = candidates(0).copy(order = 1)
        val lib1 = candidates(1).copy(order = 4, effVotes = 200 + 100 * .25 / 2 + 150 * .5 / 2, winner = true)
        val ndp = candidates(2).copy(order = 2, effVotes = 150)
        val lib2 = candidates(3).copy(order = 3, effVotes = 125 + 100 * .25 / 2 + 150 * .5 / 2, winner = true)

        assert(e(0) == lib1)
        assert(e(1) == lib2)

        val us = u.sortBy(c ⇒ c.order)
        assert(us(0) == con)
        assert(us(1) == ndp)
      }
    }



    'AssignAdjustmentMPs - {
      val d = new DesignReader(TestDesigns.drc, TestDesigns.r1, TestDesigns.c1).read
      val origCand = d.candidates.map(c ⇒ (c.name, c)).toMap

      val results: Iterable[(Seq[Candidate], Seq[Candidate])] = for {
        candidates ← d.candidates.groupBy(c ⇒ c.ridingId).values
      } yield {
        rcStrat.runElection(candidates, 1) // all of the district magnitudes in drc are 1
      }

      // Collapse all of the (elected, unelected) in the list of results into just a list of elected and unelected
      val (alreadyElected, notYetElected) = results.foldLeft((Vector[Candidate](), Vector[Candidate]())) { (a, v) ⇒
        (a._1 ++ v._1, a._2 ++ v._2)
      }


      "got expected winners from STV round" - {
        val expectedElected = Set("C2-L", "C4-L", "C6-C")
        val actualElected = alreadyElected.map(c ⇒ c.name).toSet
        assert(expectedElected == actualElected)
      }

      val c2L = origCand("C2-L").copy(effVotes = 200 + 20 + (102 + 10) * .5,
        winner = true, order = 6)
      val c4L = origCand("C4-L").copy(effVotes = 34 + (22 + 10) * .5 / 2 + 68,
        winner = true, order = 6)
      val c6C = origCand("C6-C").copy(effVotes = 70 + 30, winner = true, order = 6)

      "manual first round calculations match" - {
        assert(alreadyElected.contains(c2L))
        assert(alreadyElected.contains(c4L))
        assert(alreadyElected.contains(c6C))
      }

      val c1C = origCand("C1-C").copy(effVotes = 30 + 100, order = 5)
      val c3C = origCand("C3-C").copy(effVotes = 50 + 20, order = 5)
      val c5L = origCand("C5-L").copy(effVotes = 24 + 20 + 20, order = 5)

      "runner-up candidates match manual calcs" - {
        assert(notYetElected.contains(c1C))
        assert(notYetElected.contains(c3C))
        assert(notYetElected.contains(c5L))
      }

      /*
          RawCandidate(1, "C1-C", Con, 30 + 100),   // protected
          RawCandidate(1, "C1-L", Lib, 20),   // cut 2nd; transfer to other lib
          RawCandidate(1, "C1-N", NDP, 10),   // cut first; xfer to other NPD;  ADJUST

          RawCandidate(2, "C2-C", Con, 100),    // cut 3rd; xfer to other Con
          RawCandidate(2, "C2-L", Lib, 200 + 20 + (102+10)*.5), // WINNER
          RawCandidate(2, "C2-N", NDP, 102 + 10),    // cut 4th; xfer 50% to lib


          RawCandidate(3, "C3-C", Con, 50 + 20),    // ADJUST
          RawCandidate(3, "C3-L", Lib, 60 + (22+10)*.5/2 = 68), // cut fourth
          RawCandidate(3, "C3-N", NDP, 10),   // cut first

          RawCandidate(4, "C4-C", Con, 20),   // cut second
          RawCandidate(4, "C4-L", Lib, 34 + (22+10)*.5/2 + 68),  // protected; WINNER
          RawCandidate(4, "C4-N", NDP, 22 + 10),  // cut third; 50% to libs


          RawCandidate(5, "C5-C", Con, 30),   // cut third; all to other Con
          RawCandidate(5, "C5-L", Lib, 24 + 20 + 20),  // protected;  ADJUST
          RawCandidate(5, "C5-N", NDP, 10),   // cut first; all to other NDP

          RawCandidate(6, "C6-C", Con, 70 + 30),  // WINNER
          RawCandidate(6, "C6-L", Lib, 20),   // cut second; all to other Lib
          RawCandidate(6, "C6-N", NDP, 30 + 10) // cut fourth; 1/2 to Lib

       */

      /*
        Deserved seats:
        ndp = 10 + 102 + 10 + 22 + 10 + 30 = 184
        con = 30 + 100 + 50 + 20 + 30 + 70 = 300
        lib = 20 + 200 + 60 + 34 + 24 + 20 = 358
        total = ndp + con + lib = 842
        seats = 6
        ndpSeats = ndp/total × seats = 1.3111638955
        conSeats = con/total × seats = 2.1377672209
        libSeats = lib/total × seats = 2.5510688836
        Therefore Libs get 3, cons 2, and ndp 1 by highest remainder
        NDP is most under-represented, so should get the best seat.  They are most
        under-represented in RidingA=1+2 where they got 112 seats.  Their best
        candidate (by far) got beat out, so it goes to C1-N.

        Cons are now the most under-represented.  That's in RidingA where they got 130
        votes.  But that one is all filled so we go to the next one, B=3&4 where they
        got 70 votes.  We aren't using C=5&6 because they already have a winner there.
        This one goes to C3C

        Liberals get the last seat
       */

      val adj = RcStvProvAdjustment.adjustmentMPs(d.provinces(0), alreadyElected, notYetElected)
      val allElected = alreadyElected ++ adj

      "Adjustment MPs" - {
        "length sanity tests" - {
          assert(adj.length == 3)
          assert(allElected.length == 6)
        }

        "all old ridings represented" - {
          val representedOldRidings = allElected.map(c ⇒ c.oldRidingId).sorted
          assert(representedOldRidings == 1.to(6))
        }

        "expected details" - {
          val a = origCand("C1-N").copy(effVotes = 10, winner = true,
            seatType = AdjustmentSeat, order = 1)
          assert(adj.contains(a))

          val b = origCand("C3-C").copy(effVotes = 70, winner = true,
            seatType = AdjustmentSeat, order = 5)
          assert(adj.contains(b))

          val c = origCand("C5-L").copy(effVotes = 64, winner = true,
            seatType = AdjustmentSeat, order = 5)
          assert(adj.contains(c))

        }

      }


    }

  }

}
