package stv

/**
  * Created by bwbecker on 2016-10-01.
  */
case class ElectionResults(val elected: Vector[Candidate],
                           val unelected: Vector[Candidate],
                           val topup: Vector[Candidate],
                           design: Design) {

  val allRidingCandidates = elected ++ unelected

  val allCandidates = allRidingCandidates ++ topup

  val electedByRegion: Map[RegionId, Vector[Candidate]] = (elected ++ topup).groupBy(
    cand ⇒ cand.regionId)

  val electedByRiding: Map[RidingId, Vector[Candidate]] = elected.groupBy(
    cand ⇒ cand.ridingId)

  val candidatesByRiding: Map[RidingId, Vector[Candidate]] = allCandidates.groupBy(
    cand ⇒ cand.ridingId)

  val candidatesByRegion: Map[RegionId, Vector[Candidate]] = allCandidates.groupBy(
    cand ⇒ cand.regionId)

  def topupByRegion(regionId: RegionId): Vector[Candidate] = electedByRegion(regionId).filter(c ⇒ c.seatType ==
    SeatType.TopupSeat)

  /**
    * Proportionality analysis by list of candidates.
    */
  def analysisByCandidate(candidates: Vector[Candidate], totalSeats: Int) = new Analysis(candidates, totalSeats)


  /**
    * Proportionality analysis for a set of regions.
    */
  def analysisByRegion(regions: Seq[Region]): Analysis = {
    new Analysis(regions.flatMap(region ⇒ candidatesByRegion(region.regionId)),
      regions.map(r ⇒ r.totalCandidates).sum)
  }

  /**
    * Analysis on a list of provinces, given a simulation.  Includes top-ups.
    */
  def analysisByProvince(provinces: List[ProvName]): Analysis = {
    val regions = this.design.provinces.filter(p ⇒ provinces.contains(p.prov)).flatMap(p ⇒ p.regions)
    this.analysisByRegion(regions)
  }


  /**
    * Proportionality analysis for candidates elected in a set of ridings with an optional set of
    * top-up seats.
    */
  def analysisByRiding(ridings: Seq[Riding], topups: Seq[Candidate] = Vector()): Analysis = new Analysis(
    ridings.flatMap(r ⇒ this.candidatesByRiding(r.ridingId).filterNot(c ⇒ c.topupWinner)) ++ topups,
    ridings.map(_.districtMagnitude).sum + topups.length
  )


  /**
    * The average of the Gallagher index for each province, weighted by seats
    */
  def compositeGallagher: Double = {
    import ProvName._
    implicit val sim = this

    val aLst = ProvName.values.map { p ⇒ this.analysisByProvince(List(p)) }
    val totSeats = aLst.foldLeft(0)(_ + _.totalSeats)  //aLst.map(a ⇒ a.totalSeats).sum
    val wtGallagher = aLst.foldLeft(0.0)((accum, a) ⇒ accum + a.gallagherIndex*a.totalSeats/totSeats)//aLst.map(a ⇒ a.gallagherIndex * a.totalSeats / totSeats).sum

    wtGallagher
  }

}
