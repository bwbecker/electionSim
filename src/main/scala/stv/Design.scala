package stv

import scalatags.Text.TypedTag

import stv.electionStrategy._

/**
  * The model design that we read from a JSON file.
  *
  * @param provinces
  */

case class Design(
                   design_name: DesignName,
                   description: TypedTag[String],
                   election_strategies: List[ElectionStrategyEnum],
                   provinces: Vector[Province]
                 ) {


  val regions: Vector[Region] = this.provinces.flatMap(p => p.regions)
  val ridings: Vector[Riding] = this.regions.flatMap(r ⇒ r.ridings)

  val numSingleMemberRidings = ridings.count(r ⇒ r.districtMagnitude == 1)
  val numMultiMemberRidings = ridings.count(r ⇒ r.districtMagnitude > 1)
  val numTopupRegions = regions.count(r ⇒ r.topUpSeats > 0)

  val hasSingleMemberRidings = this.numSingleMemberRidings > 0

  val hasMultiMemberRidings = this.numMultiMemberRidings > 0

  def electionStrategies: List[ElectionStrategyEnum] = {
    this.election_strategies
  }

  def singleMbrStrategies: Vector[RidingElectionStrategy] = {
    if (this.hasSingleMemberRidings) {
      RidingElectionStrategy.singleMbrStrategies
    } else {
      Vector(NotApplicableRidingElectionStrategy)
    }
  }

  def multiMbrStrategies: Vector[RidingElectionStrategy] = {
    if (this.hasMultiMemberRidings) {
      RidingElectionStrategy.multiMbrStrategies
    } else {
      Vector(NotApplicableRidingElectionStrategy)
    }
  }


  def debug: String = {
    val sb = new StringBuilder()
    sb.append(s"Design: ${this.design_name}")
    for (prov <- provinces) {
      sb.append(s"\nProv: ${prov.prov}\n")
      for (region <- prov.regions) {
        sb.append(s"\n\t${region.regionId}, topupSeats=${region.topUpSeats}\n")
        for (riding <- region.ridings) {
          sb.append(s"\t\t${riding.name}\n")
        }
      }
    }
    return sb.toString()
  }


  /**
    * Run an election on this voting design.
    */
  def doElection(electionStrat: ElectionStrategyEnum,
                 voteSwing: Option[VoteSwing]): ElectionResults = {

    var elected = Vector[Candidate]()
    var unelected = Vector[Candidate]()
    var topup = Vector[Candidate]()

    for {
      prov ← this.provinces //.filter(p ⇒ p.prov == ProvName.PE)
    } {
      var provElected = Vector[Candidate]()
      var provUnelected = Vector[Candidate]()

      for {
        region ← prov.regions
      } {
        var regionElected = Vector[Candidate]()
        var regionUnelected = Vector[Candidate]()
        for {
          baseRiding ← region.ridings
          riding = baseRiding.swingVotes(voteSwing)
        } {
          val eStrategy = electionStrat.get(riding)
          val (e, u) = eStrategy.runElection(riding.candidates0, riding.districtMagnitude)
          regionElected = regionElected ++ e
          regionUnelected = regionUnelected ++ u
        }
        if (this.numTopupRegions > 0 && electionStrat != ElectionStrategyEnum.RcSTV) {
          // Regional-level adjustments
          topup = topup ++ electionStrat.topup.runElection(
            region.regionId,
            regionElected ++ regionUnelected, region.topUpSeats, 0.01)
        }

        provElected = provElected ++ regionElected
        provUnelected = provUnelected ++ regionUnelected
      }

      if (electionStrat == ElectionStrategyEnum.RcSTV) {
        // Provincial-level adjustments
        topup = topup ++ RcStvProvAdjustment.adjustmentMPs(prov, provElected, provUnelected)

        // topups were previously listed as unelected.  So remove them from that list.
        provUnelected = provUnelected.filterNot(c ⇒ topup.exists(tc ⇒ tc.ridingId == c.ridingId && tc.name == c.name))
      }

      elected = elected ++ provElected
      unelected = unelected ++ provUnelected
    }



    //    assert(elected.length + topup.length == 338, s"${elected.length} + ${topup.length} != 338 (338 shouldn't be
    // " +
    //      s"hardcoded")
    ElectionResults(elected, unelected, topup, this)
  }

}
