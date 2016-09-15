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
             candidates0: List[Candidate],
             // old ridings mapped to this one: riding id, pct, riding name
             val mapping: List[OldRiding],
             val electionStrategy: ElectionStrategy
            ) {

  override def toString = s"""Riding($ridingId, $province, $name, $population, $area, ${districtMagnitude},
    ${candidates0.sortBy(c => -c.votes).take(3).mkString("\t", "\n\t", "")})
    ${mapping.take(3).mkString("\n")}
    ${electionStrategy.name})"""

  //println(s"Riding: ${ridingId}")
  val (elected, unelected) = electionStrategy.runElection(this.candidates0, this.districtMagnitude)
  //assert(elected.length + unelected.length == candidates0.length)
  assert(elected.length == districtMagnitude, s"${elected.length} != ${districtMagnitude} for ${ridingId}")
  val candidates = elected ::: unelected

}

case class OldRiding(ridingId: RidingId, pct: Double, name: String)