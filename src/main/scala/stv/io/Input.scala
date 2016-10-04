package stv.io

import stv._

import stv.Pickler

import scala.collection.mutable


object Input {
  // Values from database queries to use in assertions
  private val e2015NumCandidates = 1775
  private val e2015NumVotes = 17583155


  /*
   * Stuff to read the raw JSON and then transform it into our finished data structure (Design)
   */
  case class RawCandidate(riding_id: Int,
                          candidate_name: String,
                          party_id: Party,
                          incumbent: Boolean,
                          elected: Boolean,
                          votes: Double)


  /**
    * Read the list of candidates from the 2015 election.
    *
    * @return
    */
  // ToDo:  This should be parameterized by election
  // Used to create eCandidatesByRiding for design transformation
  def candidates(year: Int): Vector[RawCandidate] = {
    val rawJson = this.fileToString(s"json/candidates/candidates-${year}.json")
    val r: Vector[RawCandidate] = Pickler.read[Vector[RawCandidate]](rawJson)

    //    val result = r.toList.map { c ⇒
    //      // Region and province will be filled in with correct values in a later step
    //      Candidate(c.riding_id.toString, "DummyRegion", ProvName.AB, c.candidate_name, c.party_id,
    //        c.votes,
    //        c.votes,
    //        false, SeatType.RidingSeat)
    //    }
    //    assert(result.length == Input.e2015NumCandidates)
    //    assert(result.map(_.votes).sum == Input.e2015NumVotes)

    //    result
    r
  }

  /**
    * Get the Candidates that ran in the 2015 election.
    */
  // Used in design transformation
  //  private val eCandidatesByRiding: Map[RidingId, List[Candidate]] = {
  //
  //    val byRiding = Input.e2015Candidates.groupBy(_.ridingId)
  //    assert(byRiding.keys.size == 338,
  //      s"eCandidatesByRiding.keys.size = ${byRiding.keys.size}")
  //
  //    byRiding
  //  }

  // Used in design transformation; SummaryHTML
  def originalRidings(numRidings: Int): Vector[RawFptpRiding] = {
    val fileName = s"json/ridings-${numRidings}/ridings.json"
    val source = scala.io.Source.fromFile(fileName)
    val rawJson = try source.getLines().mkString("\n") finally source.close()
    Pickler.read[Vector[RawFptpRiding]](rawJson)
  }

  // Used in Design transformation


  //  def readDesign(electionYear:Int, dName:DesignName): Design = {
  //    def designAsJsonString(numRidings:Int):String = {
  //      val source = scala.io.Source.fromFile(s"json/ridings-${numRidings}/${dName}.json")
  //      val rawJson = try source.getLines().mkString("\n") finally source.close()
  //      rawJson
  //    }
  //
  //
  //    val numRidings = numRidingsByElectionYr(electionYear)
  //    new DesignReader(
  //      designAsJsonString(numRidings),
  //      originalRidingsMap(numRidings),
  //      candidatesByRiding(electionYear)
  //    )
  //  }

  /**
    * Read the design from a json file.  Cached so it can be re-read without a significant performance
    * penalty.
    */
  def readDesignFromFile(fileName: String): Design = {
    this.readDesign(fileToString(fileName))
  }

  def readDesign(rawJson: String): Design = {
    new DesignReader(rawJson, originalRidings(338), candidates(2015)).read
  }

  /**
    * Read the contents of a file and return it as a string.
    */
  def fileToString(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    val rawJson = try source.getLines().mkString("\n") finally source.close()
    rawJson
  }

  def getSim(fileName: String, params: Params, ridings:Vector[RawFptpRiding]): Sim = {
    Sim(Input.readDesignFromFile(fileName), params, ridings)
  }

}

/**
  * Read a design from a json file.  Transform it into a riding Design object.
  *
  * @param rawJson
  * @param originalRidings
  * @param candidates
  */
private class DesignReader(rawJson: String,
                           originalRidings: Vector[RawFptpRiding],
                           candidates: Vector[Input.RawCandidate]
                          ) {

  private val originalRidingsMap = originalRidings.map(r => (r.fptp_id, r)).toMap
  private val candidatesByRiding: Map[Int, Vector[Input.RawCandidate]] = candidates.groupBy(_.riding_id)

  def read: Design = {
    Pickler.read[JsonDesign](rawJson).transform()
  }


  private case class JsonDesign(
                                 design_name: DesignName,
                                 is_proportional: Boolean,
                                 provinces: Vector[JsonProv]
                               ) {

    def transform(): Design = Design(
      design_name,
      is_proportional,
      provinces.map(p => p.toProvince())
    )
  }


  private case class JsonProv(prov: ProvName,
                              regions: Vector[JsonRegion]) {

    def toProvince(): Province = Province(prov,
      regions.map { r => r.toRegion(prov) }
    )
  }

  private case class JsonRegion(region_id: String,
                                top_up_seats: Int,
                                new_ridings: Vector[JsonNewRiding]) {

    def toRegion(prov: ProvName) = Region(
      region_id,
      top_up_seats,
      new_ridings.map(r => r.toRiding(region_id, prov)),
      0.01)
  }

  /*
   * The JSON input file encode the old ridings that are mapped to a new riding as a
   * comma separated string.  Split it into its constituent parts.
   */
  private case class JsonOldRiding(info: String) {
    val (riding_id, pct, name) = {
      val a = info.split(", *")
      (a(0).toInt, a(1).toInt, a(2))
    }
  }

  private case class JsonNewRiding(riding_id: String,
                                   district_mag: Int,
                                   old_ridings: Vector[String]) {

    val oldRidings = old_ridings.map(JsonOldRiding(_))


    def toRiding(regionId: RegionId, prov: ProvName): Riding = {

      // Get the list of candidates, adjusted for partial ridings.
      val candidates = oldRidings.flatMap { or =>
        val rawCandidates = candidatesByRiding(or.riding_id)
        // Change old riding ID to new riding Id.
        rawCandidates.map { cand => Candidate(riding_id, regionId, prov,
          cand.candidate_name, cand.party_id,
          cand.votes * or.pct / 100.0, cand.votes * or.pct / 100.0, false, SeatType.RidingSeat)
        }
      }


      val area = oldRidings.map { or =>
        val riding = originalRidingsMap(or.riding_id)
        riding.area * or.pct / 100.0
      }.sum

      val pop = oldRidings.map { or =>
        val riding = originalRidingsMap(or.riding_id)
        riding.pop * or.pct / 100.0
      }.sum

      new Riding(riding_id,
        prov,
        riding_id,
        Math.round(pop).toInt, // pop
        Math.round(area).toInt, // area
        district_mag,
        candidates,
        oldRidings.map(or => OldRiding(or.riding_id.toString, or.pct.toDouble, or.name))
      )
    }
  }

}

