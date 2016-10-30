package stv

import stv.Party.{Con, Lib, NDP}
import stv.ProvName.AB
import stv.SeatType._
import stv.electionStrategy.{RcStvElectionStrategy, RcStvProvAdjustment}
import stv.io.DesignReader
import utest._

/**
  * Created by bwbecker on 2016-10-29.
  */
object RcRUPRAdjustmentStrategyTest extends TestSuite with Common {

  val rcStrat = new RcStvElectionStrategy(TestXfer)

  val candidates = Seq(
    Candidate("1", 1, "R1", AB, "C0", Con, 100, 100, false, RidingSeat, 0),
    Candidate("1", 1, "R1", AB, "C1", Lib, 200, 200, false, RidingSeat, 0),
    Candidate("1", 1, "R1", AB, "C2", NDP, 150, 150, false, RidingSeat, 0)
  )


  val tests = this {


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
        val expectedElected = Set("C2-L", "C3-L", "C6-C")
        val actualElected = alreadyElected.map(c ⇒ c.name).toSet
        assert(expectedElected == actualElected)
      }

      val c2L = origCand("C2-L").copy(effVotes = 200 + 20 + 112 * .5 + 130 * .25,
        winner = true, order = 6)
      val c4L = origCand("C3-L").copy(effVotes = 60 + 32 * .5 / 2 + 42 + 70 * .25,
        winner = true, order = 6)
      val c6C = origCand("C6-C").copy(effVotes = 70 + 30, winner = true, order = 6)

      "manual first round calculations match" - {
        assert(alreadyElected.contains(c2L))
        assert(alreadyElected.contains(c4L))
        assert(alreadyElected.contains(c6C))
      }

      val c2C = origCand("C2-C").copy(effVotes = 100 + 30, order = 5)
      val c3C = origCand("C3-C").copy(effVotes = 50 + 20, order = 5)
      val c5L = origCand("C5-L").copy(effVotes = 24 + 20 + 20, order = 5)

      "runner-up candidates match manual calcs" - {
        assert(notYetElected.contains(c2C))
        assert(notYetElected.contains(c3C))
        assert(notYetElected.contains(c5L))
      }

      /*
        RawCandidate(1, "C1-C", Con, 30),   // cut #3; xfer to Con
        RawCandidate(1, "C1-L", Lib, 20),   // cut #2; xfer to Lib
        RawCandidate(1, "C1-N", NDP, 10),   // cut #1; xfer to NDP

        RawCandidate(2, "C2-C", Con, 100 + 30), // cut #5
        RawCandidate(2, "C2-L", Lib, 200 + 20 + 112*.5 + 130*.25),  // WINNER
        RawCandidate(2, "C2-N", NDP, 102 + 10), // cut #4; 112*.5 to lib

        RawCandidate(3, "C3-C", Con, 50 + 20),    // cut #5
        RawCandidate(3, "C3-L", Lib, 60 + 32*.5/2 = 68 + 42 + 70*.25),  // WINNER
        RawCandidate(3, "C3-N", NDP, 10),   // cut #1; xfer to NDP

        RawCandidate(4, "C4-C", Con, 20),   // cut #2
        RawCandidate(4, "C4-L", Lib, 34 + 32*.5/2 = 42),  // cut #4;
        RawCandidate(4, "C4-N", NDP, 22 + 10),  // cut #3   ADJUST

        RawCandidate(5, "C5-C", Con, 30),   // cut #3   // ADJUST
        RawCandidate(5, "C5-L", Lib, 24 + 20 + 40*.5),  // cut #5
        RawCandidate(5, "C5-N", NDP, 10),   // cut #1

        RawCandidate(6, "C6-C", Con, 70 + 30),    // WINNER
        RawCandidate(6, "C6-L", Lib, 20),   // cut #2
        RawCandidate(6, "C6-N", NDP, 30 + 10)  // cut #4

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

        Taking best remaining candidate in old ridings that don't already have someone
        elected.  That's different than the riding that is most under-represented.  I'm
        not doing that because I don't know how to handle ridings that already have a
        candidate elected from that party.

        NDP is most under-represented, so should get the first seat.  NDP candidate with
        the best results is C4-N in old riding 4.

        Cons are now the most under-represented.  Their best is C5-C.

        Libs get the last one.  C1-L

       */




      "Adjustment MPs" - {
        //println("*** Calling adjustmentMPs")
        val adj = RcStvProvAdjustment.adjustmentMPs(d.provinces(0), alreadyElected, notYetElected)
        val allElected = alreadyElected ++ adj



        "length sanity tests" - {
          assert(adj.length == 3)
          assert(allElected.length == 6)
        }

        "all old ridings represented" - {
          val representedOldRidings = allElected.map(c ⇒ c.oldRidingId).sorted
          assert(representedOldRidings == 1.to(6))
        }

        "expected candidates" - {
          val actual = adj.map { c => c.name }
          actual ==> Seq("C4-N", "C5-C", "C1-L")
        }

        "expected details" - {
          val a = origCand("C4-N").copy(effVotes = 22 + 10, winner = true,
            seatType = AdjustmentSeat, order = 3)
          assert(adj.contains(a))

          val b = origCand("C5-C").copy(effVotes = 30, winner = true,
            seatType = AdjustmentSeat, order = 3)
          assert(adj.contains(b))

          val c = origCand("C1-L").copy(effVotes = 20, winner = true,
            seatType = AdjustmentSeat, order = 2)
          assert(adj.contains(c))

        }

      }


    }

  }

}
