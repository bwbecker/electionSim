package stv

import stv.Party.{Con, NDP, Lib}

import scala.collection.mutable


/**
  * The stuff we need to simulate an election.
  */
case class Sim(//fptpRidings: Map[RidingId, Riding],
               design: Design,
               params: Params,
               val originalRidings: Vector[RawFptpRiding]
              ) {


  val results = design.doElection(params.singleMemberElectionStrategy, params.multiMemberElectionStrategy,
    TopupStrategy, None)

  //val SENSITIVITY_STEPS =   0.02
  val SENSITIVITY_STEPS = 0.1
  val SENSITIVITY_START = -0.30
  val SENSITIVITY_END = 0.21


  val regions: Vector[Region] = design.regions

  // Sort for consistent output later.
  val newRidingsVec: Vector[Riding] = regions.flatMap { rg => rg.ridings }.toVector
  val newRidings: Map[RidingId, Riding] = newRidingsVec.sortBy(_.ridingId).map { r => (r.ridingId, r) }.toMap

  val analysis = results.analysisByRegion(regions)

  val totalArea = this.newRidings.map(_._2.area).sum

  val totalPop = this.newRidingsVec.map {_.population}.sum


  val shortName = {
    val dn = design.design_name.shortName
    val sme = params.singleMemberElectionStrategy.shortName
    val mme = params.multiMemberElectionStrategy.shortName
    s"${dn}-${sme}-${mme}"
  }

  /**
    * A List[(cumPop:Double, area:Int)] where cumPop percentage of the population
    * lives in a riding no bigger than area km2.
    */
  private lazy val cumPopArea = {
    val ridingByArea = this.newRidings.values.toVector.sortBy(_.area)
    val totalPop = ridingByArea.map {
      _.population
    }.sum.toDouble

    var cumPop = 0
    ridingByArea.map { r ⇒
      cumPop += r.population
      (cumPop / totalPop, r.area)
    }
  }

  def numRidingsLargerThan(km2: Int): Int = {
    newRidings.values.count(_.area > km2)
  }

  def avgAreaPerLocalMP: Int = {
    this.totalArea / this.numRidingMPs
  }

  /** The total number (ridings and regions) in this simulation. */
  def numMPs: Int = regions.map { region ⇒
    region.topUpSeats + region.ridings.map {
      _.districtMagnitude
    }.sum
  }.sum


  def numRidingMPs: Int = this.newRidings.map { r ⇒ r._2.districtMagnitude }.sum


  def numRegionalMPs: Int = this.regions.map {
    _.topUpSeats
  }.sum

  def avgRidingArea: Int = this.totalArea / this.newRidings.size

  def medianRidingArea: Int = {
    val sorted = this.newRidingsVec.sortBy(_.area)
    val len = this.newRidingsVec.length
    if (len % 2 == 0) {
      (sorted(len / 2).area + sorted(len / 2 - 1).area) / 2
    } else {
      sorted(len / 2).area
    }
  }

  def avgRidingRadius: Int = {
    val sum = this.newRidings.map(r ⇒ Math.sqrt(r._2.area / Math.PI)).sum
    (sum / this.newRidings.size).toInt
  }

  /**
    * Get the maximum riding area for a given percentage of the population.
    * For maxRidingAreaForPctOfPopulation(0.50), 50% of Canada's population
    * lives in a riding no larger than that size.
    */
  def maxRidingAreaForPctOfPopulation(pct: Double): Int = {
    this.cumPopArea.find(_._1 > pct).get._2
  }

  /**
    * Average number of MPs per riding.  Exclude Territories
    * and PEI.
    */
  def avgMPsPerRiding: Double = {
    //    val ridings =this.newRidings.values.filter(r ⇒ r.province match {
    //      case "PEI" | "NU" | "NT" | "YT" ⇒ false
    //      case _ ⇒ true
    //    } )
    val ridings = this.newRidings.values
    ridings.map {
      _.districtMagnitude
    }.sum / ridings.size.toDouble
  }

  def avgTopUpMPsPerRegion: Double = {
    val regions = this.regions.filter {
      _.topUpSeats > 0
    }
    if (regions.isEmpty) 0
    else regions.map {
      _.topUpSeats
    }.sum / regions.length.toDouble

  }

  def avgTotalMPsPerRegion: Double = {
    val regions = this.regions.filter {
      _.topUpSeats > 0
    }
    if (regions.isEmpty) 0
    else {
      regions.map { r ⇒
        r.topUpSeats + r.ridings.map {
          _.districtMagnitude
        }.sum
      }.sum / regions.length.toDouble
    }
  }


  def avgPopPerLocalMP: Int = {
    this.totalPop / this.numRidingMPs
  }

  def medianLocalMPRidingArea: Int = {
    val mpArea = this.newRidingsVec.flatMap(r => Vector.fill(r.districtMagnitude)(r.area)).sorted

    val len = mpArea.length
    if (len % 2 == 0) {
      (mpArea(len / 2) + mpArea(len / 2 - 1)) / 2
    } else {
      mpArea(len / 2)
    }
  }

  def pctVotersWithPreferredPartyLocally: Double = {
    var totalVoters = 0.0
    var gotPrefParty = 0.0
    for {
      candidate ← this.results.allRidingCandidates
      prefParty = candidate.party
    } {
      val candidates = this.results.electedByRiding.get(candidate.ridingId)
      if (candidates.isEmpty) {
        println(s"pctVotersWithPreferredPartyLocally: no candidates for riding ${candidate.ridingId}")
      } else if (candidates.get.exists(cand ⇒ cand.party == prefParty)) {
        gotPrefParty += candidate.votes
      }
      totalVoters += candidate.votes
    }
    gotPrefParty / totalVoters
  }

  def pctVotersWithPreferredPartyRegionally: Double = {
    var totalVoters = 0.0
    var gotPrefParty = 0.0
    for {
      candidate ← this.results.allCandidates
      prefParty = candidate.party
    } {
      val candidates = this.results.electedByRegion.get(candidate.regionId)
      if (candidates.isEmpty) {
        println(s"pctVotersWithPreferredPartyRegionally: no candidates for region ${candidate.regionId}")
      } else if (candidates.get.exists(cand ⇒ cand.party == prefParty)) {
        gotPrefParty += candidate.votes
      }
      totalVoters += candidate.votes
    }
    gotPrefParty / totalVoters
  }

  /*
  * This is used to get the average Gallagher for the model, so can't be moved to the companion
  * object.
  */
  private var sensitivityCache = mutable.Map[(Party, Party), Vector[SensitivityDataPoint]]()

  def sensitivityAnalysis(party1: Party, party2: Party): Vector[SensitivityDataPoint] = {

    //println(s"Did NOT find sensitivity for ${party1} -> ${party2} in cache.")

    val lst = (for (d ← SENSITIVITY_START until SENSITIVITY_END by SENSITIVITY_STEPS) yield {
      val p = if (d < 0) {
        params.copy(voteAdjustment = Some(VoteSwing(-d, party2, party1)))
      } else {
        params.copy(voteAdjustment = Some(VoteSwing(d, party1, party2)))
      }

      val results = this.design.doElection(p.singleMemberElectionStrategy, p.multiMemberElectionStrategy,
        TopupStrategy, p.voteAdjustment)
      val analysis = results.analysisByRegion(this.regions)

      val libVotes = analysis.pctVote(Lib)
      val libMPs = analysis.pctMPs(Lib)
      val ndpVotes = analysis.pctVote(NDP)
      val ndpMPs = analysis.pctMPs(NDP)
      val conVotes = analysis.pctVote(Con)
      val conMPs = analysis.pctMPs(Con)

      SensitivityDataPoint(d, libVotes, libMPs, conVotes, conMPs, ndpVotes, ndpMPs, analysis.gallagherIndex)
    }).toVector
    this.sensitivityCache += (party1, party2) → lst
    lst
  }


  /**
    * Given a list of SensitivityDataPOints, calculate the average Gallagher index.
    */
  def sensitivityAvgGallagher(lst: Vector[SensitivityDataPoint]): Double = {
    val sum = lst.map(_.gallagher).sum
    sum / lst.length
  }

  /**
    * Average of all the sensitivity analysis we've done for this model.
    * Works best if called late in the game :)
    */
  def sensitivityAvgGallagher: Double = {
    var sum = 0.0
    var num = 0
    for {
      oneRun ← this.sensitivityCache.values
      datapoint ← oneRun
    } {
      sum = sum + datapoint.gallagher
      num = num + 1
    }
    sum / num
  }


  /**
    * Average several Gallagher indices for this simulation:
    * -- the 2015 simulation
    * -- the average from the sensitivity analysis
    * -- the following regions:  BC, Praries, ON+QC, Maritimes
    *
    * @return
    */
  def compositeGallagher: Double = {
    import ProvName._
    implicit val sim = this

    //    val aLst = List(Analysis(List(BC)), Analysis(List(AB, SK, MB)), Analysis(List(ON)),
    //      Analysis(List(QC)), Analysis(List(NB, PE, NS, NL)), Analysis(List(YT, NT, NU)))
    //    val totSeats = aLst.map(a ⇒ a.seats).sum
    //    val wtGallagher = aLst.map(a ⇒ a.gallagherIndex * a.seats / totSeats).sum
    //
    //    wtGallagher

    val aLst = ProvName.values.map { p ⇒ this.results.analysisByProvince(List(p)) }
    val totSeats = aLst.map(a ⇒ a.totalSeats).sum
    val wtGallagher = aLst.map(a ⇒ a.gallagherIndex * a.totalSeats / totSeats).sum

    wtGallagher

  }
}



