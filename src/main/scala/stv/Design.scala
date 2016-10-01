package stv

/**
  * The model design that we read from a JSON file.
  *
  * @param provinces
  */

case class Design(
                   design_name: DesignName,
                   is_proportional: Boolean,
                   provinces: Vector[Province]
                 ) {

  val regions: Vector[Region] = this.provinces.flatMap(p => p.regions)


  def hasSingleMemberRidings = {
    provinces.exists(p => p.regions.exists(region => region.ridings.exists(riding => riding.districtMagnitude == 1)))
  }

  def hasMultiMemberRidings = {
    provinces.exists(p => p.regions.exists(region => region.ridings.exists(riding => riding.districtMagnitude > 1)))
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
    * @param singleMbrStrategy The election strategy to use in single-member ridings
    * @param multiMbrStrategy The election strategy to use in multi-member ridings
    * @param topUpStrategy The election strategy to use for top-up seats
    * @param voteSwing Whether to swing the votes by a given percentage first
    * @return
    */
  def doElection(singleMbrStrategy: RidingElectionStrategy,
                 multiMbrStrategy: RidingElectionStrategy,
                 topUpStrategy: TopupElectionStrategy,
                 voteSwing: Option[VoteSwing]): ElectionResults = {

    var elected = Vector[Candidate]()
    var unelected = Vector[Candidate]()
    var topup = Vector[Candidate]()

    for {
      prov ← this.provinces
      region ← prov.regions
    } {
      var regElected = Vector[Candidate]()
      var regUnelected = Vector[Candidate]()
      for {
        baseRiding ← region.ridings
        riding = baseRiding.swingVotes(voteSwing)
      } {
        val eStrategy = if (riding.districtMagnitude == 1) {singleMbrStrategy} else {multiMbrStrategy}
        val (e, u) = eStrategy.runElection(riding.candidates0, riding.districtMagnitude)
        regElected = regElected ++ e
        regUnelected = regUnelected ++ u
      }
      topup = topup ++ topUpStrategy.runElection(
        region.regionId,
        regElected ++ regUnelected, region.topUpSeats, 0.01)

      elected = elected ++ regElected
      unelected = unelected ++ regUnelected
    }

    ElectionResults(elected, unelected, topup, this)
  }

}
