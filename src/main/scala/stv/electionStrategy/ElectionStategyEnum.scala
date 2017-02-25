package stv.electionStrategy

import enumeratum.EnumEntry
import stv.{EkosXfer, EnumP, Riding}

/**
  * Created by bwbecker on 2016-10-24.
  */
sealed abstract class ElectionStrategyEnum(val sm: RidingElectionStrategy,
                                           val mm: RidingElectionStrategy,
                                           val topup: TopupElectionStrategy
                                          ) extends EnumEntry {
  def get(riding: Riding): RidingElectionStrategy = {
    if (riding.districtMagnitude == 1) {sm} else {mm}
  }
}

object ElectionStrategyEnum extends EnumP[ElectionStrategyEnum] {
  val values = findValues


  //-----------------------------------------------
  // Majoritarian:  Single member ridings only, no top-up
  //-----------------------------------------------
  case object AV extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    NotApplicableRidingElectionStrategy,
    NotApplicableTopupElectionStrategy
  )

  case object FPTP extends ElectionStrategyEnum(
    FptpRidingElectionStrategy,
    NotApplicableRidingElectionStrategy,
    NotApplicableTopupElectionStrategy
  )


  //-----------------------------------------------
  // Pure MMP:  Single member ridings w/ top-up
  //-----------------------------------------------
  case object MMP_FPTP extends ElectionStrategyEnum(
    FptpRidingElectionStrategy,
    NotApplicableRidingElectionStrategy,
    TopupStrategy
  )

  case object MMP_AV extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    NotApplicableRidingElectionStrategy,
    TopupStrategy
  )


  //-----------------------------------------------
  // MMP with a few multi-member ridings
  //-----------------------------------------------
  case object MMP_FPTP_mm extends ElectionStrategyEnum(
    FptpRidingElectionStrategy,
    ListRidingElectionStrategy, // mmp_505_* has a very few very small multi-member ridings
    TopupStrategy
  )

  //-----------------------------------------------
  // Multi-member, probably with at least a few single-member, no top-up
  //-----------------------------------------------
  case object STV extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    EkosStvRidingElectionStrategy,
    NotApplicableTopupElectionStrategy
  )

  case object MMbrList extends ElectionStrategyEnum(
    FptpRidingElectionStrategy,
    ListRidingElectionStrategy,
    NotApplicableTopupElectionStrategy
  )


  //-----------------------------------------------
  // RU-PR: single- and multi-member, top-ups
  //-----------------------------------------------
  case object RcRUPR extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    new RcStvElectionStrategy(EkosXfer),
    NotApplicableTopupElectionStrategy
  )

  case object RcRUPR2 extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    new RcStvElectionStrategy(EkosXfer),
    TopupStrategy
  )

  /**
   * Local Proportional Representation
   */
  case object LPR extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    new LPR_ElectionStrategy(EkosXfer),
    NotApplicableTopupElectionStrategy
  )

  /**
   * Local Proportional Representation
   */
  case object ToppedUpLPR extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    new LPR_ElectionStrategy(EkosXfer),
    TopupStrategy
  )

  case object STVplus extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    EkosStvRidingElectionStrategy,
    TopupStrategy
  )

  case object FptpList extends ElectionStrategyEnum(
    FptpRidingElectionStrategy,
    ListRidingElectionStrategy,
    TopupStrategy
  )

  /* This combination was used on the public web site's featured
   * models list.  Probably shouldn't have been. Continue using
   * it to avoid question of why it changed.  Don't use it for
   * anything but ru-singles.
   */
  case object AvList extends ElectionStrategyEnum(
    EkosAvRidingElectionStrategy,
    ListRidingElectionStrategy,
    TopupStrategy
  )



}