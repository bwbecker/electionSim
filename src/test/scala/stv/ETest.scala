package stv

import utest._
import ca.bwbecker.numeric._
import utest.framework.{Tree, Result}

import scalatags.Text.all._

/**
  * Created by bwbecker on 2016-05-31.
  */
object ETest extends TestSuite {

  implicit val precision = Precision(0.0001)

  override def formatSingle(path: Seq[String], r: Result): Option[String] = None


  val tests = this {
    val d1 = io.Input.readDesign(TestDesigns.d1).transform(Params("d1", "Design1", "D1Short", DesignName.mmp_small,
      "outDir",
      p("Description"), FptpElectionStrategy, EkosStvElectionStrategy, None, 0.01))

    "Design d1 has correct structure" - {
      "1 province" - {assert(d1.provinces.length == 1)}
      "province name is AB" - {assert(d1.provinces(0).prov == ProvName.AB)}
      "AB has 2 regions" - {assert(d1.provinces(0).regions.length == 2)}

      "Region R1 has" - {
        val rg1 = d1.provinces(0).regions(0)
        "region id R1" - {assert(rg1.regionId == "R1")}
        "4 top-up seats" - {assert(rg1.topUpSeats == 2)}
        "2 ridings" - {assert(rg1.ridings.length == 2)}
        "RidingA has" - {
          val rA = rg1.ridings(0)
          "riding id of RidingA" - {assert(rA.ridingId == "RidingA")}
          "dm of 2" - {assert(rA.districtMagnitude == 2)}
          "composed of 2 old ridings" - {assert(rA.mapping.length == 2)}
          "one is 48009" - {
            assert(rA.mapping(0).ridingId == "48009")
            assert(rA.mapping(0).pct == 67)
            assert(rA.mapping(0).name == "Calgary Nose Hill")
          }
          "the other is 48010" - {
            assert(rA.mapping(1).ridingId == "48010")
            assert(rA.mapping(1).pct == 100)
            assert(rA.mapping(1).name == "Calgary Rocky Ridge")
          }
        }
      }

      "Region R2 has" - {
        val rg2 = d1.provinces(0).regions(1)

        "no top-up seats" - {assert(rg2.topUpSeats == 0)}
        "2 ridings" - {assert(rg2.ridings.length == 2)}
        "first is RidingC" - {assert(rg2.ridings(0).ridingId == "RidingC")}
      }
    }

    "Design d1's RidingA details" - {
      val ra = d1.provinces(0).regions(0).ridings(0)
      assert(ra.ridingId == "RidingA")
      "population" - {
        assert(ra.population == Math.round(109286 * 67/100.0) + 108901)
      }
      "area" - {
        assert(ra.area == Math.round(57 * 67/100.0) + 90)
      }
    }
    /*

        "An Election with 2 remaining Con candidates" - {

          val candidates = Set(
            Riding.MutCandidate("R", "A", "Con", 2, 2, false),
            Riding.MutCandidate("R", "B", "Con", 1, 1, false)
          )

          val prob = (p1: String, p2: String) ⇒ 1.0

          "should distribute whole votes to both A and B" - {
            val remain = Riding.distributeVotes(2, "Con", candidates, prob)
            remain.size ==> 2
            remain.find(_.name == "A").get.effVotes ==> 3
            remain.find(_.name == "B").get.effVotes ==> 2
          }

          "should return both when dm is two" - {
            val (winners, losers) = Riding.stvElection(candidates, 2, prob)
            winners.length ==> 2
            losers.length ==> 0
            winners.map(_.name) ==> List("A", "B")
          }

          "threshhold when dm is 1" - {
            Riding.threshhold(candidates, 1) ==> 2.5
          }

          "should return A when dm is 1" - {
            val (winners, losers) = Riding.stvElection(candidates, 1, prob)
            winners.size ==> 1
            winners.map(_.name) ==> List("A")
            winners.map(_.effVotes) ==> List(2.5)
          }
        }



        "An election with 3 candidates" - {

          val candidates = Set(
            Riding.MutCandidate("R", "L", "Lib", 3, 3, false),
            Riding.MutCandidate("R", "N", "NDP", 2, 2, false),
            Riding.MutCandidate("R", "C", "Con", 1, 1, false)
          )

          val prob = (p1: String, p2: String) ⇒ if (p2 == "Con") 0.1 else 0.2


          "transferring a Green vote should increase all three" - {
            val remaining = Riding.distributeVotes(1.0, "Grn", candidates, prob)
            remaining.size ==> 3
            remaining.find(_.name == "L").get.effVotes ==> 3.2
            remaining.find(_.name == "N").get.effVotes ==> 2.2
            remaining.find(_.name == "C").get.effVotes ==> 1.1
          }

          "should choose Lib when dm is 1" - {
            val (winners, losers) = Riding.stvElection(candidates, 1, prob)
            winners.length ==> 1
            winners.head.party ==> "Lib"
            losers.length ==> 2
            losers.map(_.name) ==> List("N", "C")
            //(winners.head.effVotes ~= 2.82) ==> true
          }

          "should choose Lib & NDP when dm is 2" - {
            val (winners, losers) = Riding.stvElection(candidates, 2, prob)
            winners.length ==> 2
            winners.map(_.name) ==> List("L", "N")
            val lib = winners.find(_.party == "Lib").get
            val ndp = winners.find(_.party == "NDP").get
            lib.effVotes ==> 3.0 // 3 is the threshhold
            ndp.effVotes ==> 2.2
          }
        }

    */

    /*

    "An election with 3 candidates, the first of which is over threshhold" - {

      val candidates = Vector(
        Candidate("R", "L", "Lib", 5),
        Candidate("R", "N", "NDP", 2),
        Candidate("R", "C", "Con", 1)
      )

      "should have a threshhold of 3.6666" - {
        val votes = candidates.map(_.votes).toSet
        assert(StvElection.threshhold(votes, 2) <= 3.66669)
        assert(StvElection.threshhold(votes, 2) >= 3.66660)
      }

      "should distribute over-threshhold votes" - {
        val toXfer = 5 - 3.6666
        val cand = StvElection.distributeVotes(toXfer, "Lib",
          Set(candidates(1), candidates(2)))

        val ndpVotes = 2 + toXfer * 0.4
        val conVotes = 1 + toXfer * 0.4
        assert(cand.map(_.effVotes).max <= ndpVotes + 0.00001)
        assert(cand.map(_.effVotes).max >= ndpVotes - 0.00001)
        assert(cand.map(_.effVotes).min <= conVotes + 0.00001)
        assert(cand.map(_.effVotes).min >= conVotes - 0.00001)
      }

      "should choose Lib & NDP when dm is 2" - {
        val (winners, losers) = StvElection.countVotes(candidates.toSet, List(), List(), 2)
        winners.length ==> 2

        val lib = winners.find(_.party == "Lib").get
        val ndp = winners.find(_.party == "NDP").get

        lib.party ==> "Lib"
        ndp.party ==> "NDP"

        assert(lib.effVotes <= 3.66669)
        assert(lib.effVotes >= 3.66660)

        assert(lib.effVotes <= 3.66669)
        assert(lib.effVotes >= 3.66660)

        val xLibVotes = (5 - 3.6666666) * 0.4
        val xConVotes = 0 // Cons don't xfer to NDP
        assert(ndp.effVotes <= 2 + xLibVotes + xConVotes + 0.00001)
        assert(ndp.effVotes >= 2 + xLibVotes + xConVotes - 0.00001)
      }

    }
*/

  }
}
