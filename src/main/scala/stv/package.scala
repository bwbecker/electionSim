import stv.Party._

/**
 * Created by bwbecker on 2016-05-31.
 */
package object stv {

  type PrId = String
  type FptpId = String

  type RidingId = String
  type RegionId = String


  /**
    * Transfer from party
    * Transfer to party
    * District magnitude
   */
  type VoteXferFunc = (Party, Party, Int) => Double
}
