package stv.io

import stv._

import stv.Pickler

import scala.collection.mutable


object Input {
  // Values from database queries to use in assertions
  private val e2015NumCandidates = 1775
  private val e2015NumVotes = 17583155


  class RawDesign(private val d: JsonDesign) {

    def transform(params: Params): Design = {
      d.transform(params)
    }

    def hasSingleMemberRidings = {
      d.provinces.exists(p => p.regions.exists(region => region.new_ridings.exists(riding => riding.district_mag == 1)))
    }

    def hasMultiMemberRidings = {
      d.provinces.exists(p => p.regions.exists(region => region.new_ridings.exists(riding => riding.district_mag > 1)))
    }
  }


  /*
   * Stuff to read the raw JSON and then transform it into our finished data structure (Design)
   */
  private case class RawCandidate(riding_id: Int,
                                  candidate_name: String,
                                  party_id: Party,
                                  incumbent: Boolean,
                                  elected: Boolean,
                                  votes: Double)

  private case class JsonDesign(
                                 design_name: DesignName,
                                 is_proportional: Boolean,
                                 provinces: Vector[JsonProv]
                               ) {

    def transform(params: Params): Design = Design(
      design_name,
      is_proportional,
      provinces.map(p => p.toProvince(params))
    )
  }


  private case class JsonProv(prov: ProvName,
                              regions: Vector[JsonRegion]) {

    def toProvince(params: Params): Province = Province(prov,
      regions.map { r => r.toRegion(params) }
    )
  }

  private case class JsonRegion(region_id: String,
                                top_up_seats: Int,
                                new_ridings: Vector[JsonNewRiding]) {

    def toRegion(params: Params) = Region(
      region_id,
      top_up_seats,
      new_ridings.map(r => r.toRiding(params, region_id)),
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


    def toRiding(params: Params, regionId: RegionId): Riding = {

      // Get the list of candidates, adjusted for partial ridings.
      val candidates = oldRidings.flatMap { or =>
        val r = Input.eCandidatesByRiding(or.riding_id.toString)
        // Change old riding ID to new riding Id.
        r.map { r => r.copy(ridingId = riding_id, regionId = regionId,
          votes = r.votes * or.pct / 100.0, effVotes = r.votes * or.pct / 100.0)
        }
      }


      val area = oldRidings.map { or =>
        val riding = Input.originalRidingsMap(or.riding_id)
        riding.area * or.pct / 100.0
      }.sum

      val pop = oldRidings.map { or =>
        val riding = Input.originalRidingsMap(or.riding_id)
        riding.pop * or.pct / 100.0
      }.sum

      val prov = Input.originalRidingsMap(oldRidings.head.riding_id).prov

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


  /**
    * Read the list of candidates from the 2015 election.
    *
    * @return
    */
    // ToDo:  This should be parameterized by election
  val e2015Candidates: List[Candidate] = {
    val fileName = s"json/candidates.json"
    //val fileName = s"json/candidates_ian.json"
    val source = scala.io.Source.fromFile(fileName)
    val rawJson = try source.getLines().mkString("\n") finally source.close()
    val r: Vector[RawCandidate] = Pickler.read[Vector[RawCandidate]](rawJson)

    val result = r.toList.map { c ⇒
      // Region and province will be filled in with correct values in a later step
      Candidate(c.riding_id.toString, "DummyRegion", ProvName.AB, c.candidate_name, c.party_id,
        c.votes,
        c.votes,
        false, SeatType.RidingSeat)
    }
    assert(result.length == Input.e2015NumCandidates)
    assert(result.map(_.votes).sum == Input.e2015NumVotes)

    result
  }

  /**
    * Get the Candidates that ran in the 2015 election.
    */
  val eCandidatesByRiding: Map[RidingId, List[Candidate]] = {

    val byRiding = Input.e2015Candidates.groupBy(_.ridingId)
    assert(byRiding.keys.size == 338,
      s"eCandidatesByRiding.keys.size = ${byRiding.keys.size}")

    byRiding
  }

  val originalRidings: Vector[RawFptpRiding] = {
    val fileName = s"json/ridings.json"
    val source = scala.io.Source.fromFile(fileName)
    val rawJson = try source.getLines().mkString("\n") finally source.close()
    Pickler.read[Vector[RawFptpRiding]](rawJson)
  }

  val originalRidingsMap = originalRidings.map(r => (r.fptp_id, r)).toMap

  private val designCache = mutable.Map[DesignName, RawDesign]()


  /**
    * Read the design from a json file.  Cached so it can be re-read without a significant performance
    * penalty.
    *
    * @param d
    * @return
    */
  def readDesign(d: DesignName): RawDesign = {
    this.designCache.get(d) match {
      case Some(rd) => rd
      case None     =>
        val source = scala.io.Source.fromFile(s"json/${d}.json")
        val rawJson = try source.getLines().mkString("\n") finally source.close()
        val rd = this.readDesign(rawJson)
        designCache += d -> rd
        rd
    }
  }

  def readDesign(rawJson: String): RawDesign = {
    val jd = Pickler.read[JsonDesign](rawJson)
    new RawDesign(jd)
  }


  def getSim(params: Params): Sim = {
    Sim(Input.readDesign(params.designName).transform(params),
      params)
  }

}

