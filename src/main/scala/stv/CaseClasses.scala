package stv

import stv.io.Input


case class Province(prov: ProvName,
                    regions: Vector[Region])


/**
  * A candidate that stood for election.
  */
case class Candidate(ridingId: RidingId,
                     regionId: RegionId,
                     provName: ProvName,
                     name: String,
                     party: Party,
                     votes: Double,
                     effVotes: Double,
                     winner: Boolean,
                     seatType: SeatType,
                     order: Int // order in which wins or loses
                    ) {

  override def toString = f"Candidate(${ridingId}, ${name}, ${party}, ${votes}%,4.1f, ${effVotes}%,4.1f, ${order})"

  val topupWinner: Boolean = this.seatType == SeatType.TopupSeat

}


case class Mapping(oldRidingId: RidingId,
                   newRidingId: RidingId,
                   pct: Double)

case class RegionMapping(regionId: String, prRidingId: RidingId)


case class Region(regionId: RegionId,
                  topUpSeats: Int,
                  ridings: Vector[Riding],
                  threshhold: Double
                 ) {

  override def toString = s"Region($regionId, $topUpSeats, ${ridings.mkString("\n")}, ${threshhold})"

  val totalCandidates = ridings.map(_.districtMagnitude).sum + topUpSeats

}


case class SensitivityDataPoint(shift: Double,
                                libVotes: Double,
                                libMPs: Double,
                                conVotes: Double,
                                conMPs: Double,
                                ndpVotes: Double,
                                ndpMPs: Double,
                                grnVotes: Double,
                                grnMPs: Double,
                                blocVotes: Double,
                                blocMPs: Double,
                                gallagher: Double
                               )


case class RawFptpRiding(fptp_id: Int,
                         prov: ProvName,
                         name: String,
                         pop: Int,
                         area: Int,
                         dm: Int)
