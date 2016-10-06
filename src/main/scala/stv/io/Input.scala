package stv.io

import stv._
import stv.Pickler

import scala.collection.mutable

import java.io.{File, FileNotFoundException}


object Input {
  // Values from database queries to use in assertions
  private val e2015NumCandidates = 1775
  private val e2015NumVotes = 17583155


  /*
   * Read a candidate from JSON.
   */
  case class RawCandidate(riding_id: Int,
                          candidate_name: String,
                          party_id: Party,
                          incumbent: Boolean,
                          elected: Boolean,
                          votes: Double)


  /**
    * Read the list of candidates from the given election.
    */
  def candidates(year: Int): Vector[RawCandidate] = {
    this.fileToString(s"json/candidates/candidates-${year}.json").map { json ⇒
      Pickler.read[Vector[RawCandidate]](json)
    }.getOrElse(Vector())
  }

  /**
    * Read the ridings used in the election
    */
  def originalRidings(numRidings: Int): Vector[RawFptpRiding] = {
    val fileName = s"json/ridings-${numRidings}/ridings.json"
    val source = scala.io.Source.fromFile(fileName)
    val rawJson = try source.getLines().mkString("\n") finally source.close()
    Pickler.read[Vector[RawFptpRiding]](rawJson)
  }


  /**
    * Read the design from a json file.  Cached so it can be re-read without a significant performance
    * penalty.
    */
  //  def readDesignFromFile(fileName: String): Design = {
  //    this.readDesign(fileToString(fileName).get)
  //  }

  def readDesign(dName: DesignName,
                 numRidings: Int,
                 ridings: Vector[RawFptpRiding],
                 candidates: Vector[RawCandidate]): Option[Design] = {
    assert(ridings.length > 250, s"Looks like ridings didn't get read; only found ${ridings.length}.")
    assert(candidates.length > 250, s"Looks like candidates didn't get read; only found ${candidates.length}.")
    fileToString(s"json/ridings-${numRidings}/${dName}.json").map { json ⇒
      new DesignReader(json, ridings, candidates).read
    }
  }

  //  def readDesign(rawJson: String): Design = {
  //    new DesignReader(rawJson, originalRidings(338), candidates(2015)).read
  //  }

  /**
    * Read the contents of a file and return it as a string.
    */
  def fileToString(fileName: String): Option[String] = {
    val file = new File(fileName)
    if (file.exists()) {
      val source = scala.io.Source.fromFile(file)
      val rawJson = try source.getLines().mkString("\n") finally source.close()
      Some(rawJson)
    } else {
      //throw new FileNotFoundException(s"Didn't find ${fileName}.")
      None
    }
  }

  //  def getSim(fileName: String, params: Params, ridings: Vector[RawFptpRiding]): Sim = {
  //    Sim(Input.readDesignFromFile(fileName), params, ridings)
  //  }

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
    val d = Pickler.read[JsonDesign](rawJson)

    // ridingValidityChecks
    val designRidings = (for {
      p ← d.provinces
      region ← p.regions
      newRiding ← region.new_ridings
      oldRiding ← newRiding.oldRidings
    } yield {
      oldRiding.riding_id
    }).distinct

    candidates.foreach { c ⇒ if (!designRidings.contains(c.riding_id)) {
      println(s"${c.candidate_name} has riding ${c.riding_id} that isn't in list of ridings.")
    }
    }
    designRidings.sorted.foreach { r ⇒ if (!candidates.exists(c ⇒ c.riding_id == r)) {
      println(s"Riding ${r} does not have any candidates.")
    }
    }

    d.transform()
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

