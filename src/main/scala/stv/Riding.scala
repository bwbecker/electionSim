package stv

/**
  * Created by bwbecker on 2016-06-06.
  */

/**
  * A riding under current FPTP scenario.
  */
class Riding(val ridingId: RidingId,
             val province: ProvName,
             val name: String,
             val population: Int,
             val area: Int,
             val districtMagnitude: Int,
             val candidates0: Seq[Candidate],
             // old ridings mapped to this one: riding id, pct, riding name
             val mapping: Seq[OldRiding]
            ) {

  override def toString = s"""Riding($ridingId, $province, $name, $population, $area, ${districtMagnitude},
    ${candidates0.sortBy(c => -c.votes).take(3).mkString("\t", "\n\t", "")})
    ${mapping.take(3).mkString("\n")})"""


  /**
    * Swing this ridings votes between parties for sensitivity analysis.
    */
  def swingVotes(voteSwing: Option[VoteSwing]): Riding = {
    if (voteSwing.isDefined) {

      val VoteSwing(pct, fromParty, toParty) = voteSwing.get

      assert(pct >= 0.0, s"pct = $pct")

      val fromCand = candidates0.filter(_.party == fromParty)
      val toCand = candidates0.filter(_.party == toParty)
      val fromVotes = fromCand.map(_.votes).sum * pct
      val toVotes = fromVotes / toCand.length

      val cand = candidates0.map { c ⇒
        val cv = if (c.party == fromParty) {
          c.votes - c.votes * pct
        } else if (c.party == toParty) {
          c.votes + toVotes
        } else {
          c.votes
        }

        c.copy(votes = cv, effVotes = cv)
      }

      // New riding that's exactly the same except for the candidates.
      new Riding(this.ridingId, this.province, this.name, this.population, this.area, this.districtMagnitude, cand,
        this.mapping)
    } else {
      this
    }
  }
}

case class OldRiding(ridingId: RidingId, pct: Double, name: String)