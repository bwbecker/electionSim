package stv

import stv.io.Input


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

}

case class Province(prov: ProvName,
                    regions: Vector[Region])


/**
  * A candidate that stood for election.
  */
case class Candidate(ridingId: RidingId,
                     name: String,
                     party: Party,
                     votes: Double,
                     effVotes: Double,
                     winner: Boolean,
                     seatType: SeatType) {

  override def toString = f"Candidate(${ridingId}, ${name}, ${party}, ${votes}%,4.1f, ${effVotes}%,4.1f)"

  //def partyName = stv.partyName(this.party)
}


case class Mapping(oldRidingId: RidingId,
                   newRidingId: RidingId,
                   pct: Double)

case class RegionMapping(regionId: String, prRidingId: RidingId)


case class Region(regionId: RegionId,
                  topUpSeats: Int,
                  ridings: List[Riding],
                  threshhold: Double
                 ) {

  override def toString = s"Region($regionId, $topUpSeats, ${ridings.mkString("\n")}, ${threshhold})"

  val totalCandidates = ridings.map(_.districtMagnitude).sum + topUpSeats
  lazy val topUpCandidates: List[Candidate] = this.calcTopupCandidates


  /**
    * Find the topup candidates iteratively.  At each step, find the party that is most
    * disadvantaged by the current allocation.
    *
    * @return
    */
  def calcTopupCandidates: List[Candidate] = {

    def helper(topups: List[Candidate]): List[Candidate] = {
      if (topups.length == topUpSeats) {
        topups.reverse
      } else {
        val a = Analysis(ridings, topups, topUpSeats)
        val stats = a.statsByParty.filter(s ⇒ s.pctVote >= this.threshhold)


        val disadvantaged = stats.maxBy(s ⇒ s.deservedMPs - s.mps)

        val cand = Candidate(regionId, "Topup Candidate", disadvantaged.party, 0.0, 0.0, true, SeatType.TopupSeat)
        helper(cand :: topups)

      }
    }

    helper(List())
  }


}


case class SensitivityDataPoint(shift: Double,
                                libVotes: Double,
                                libMPs: Double,
                                conVotes: Double,
                                conMPs: Double,
                                ndpVotes: Double,
                                ndpMPs: Double,
                                gallagher: Double)


case class RawFptpRiding(fptp_id: Int,
                         prov: ProvName,
                         name: String,
                         pop: Int,
                         area: Int,
                         dm: Int)
