package stv

import enumeratum._
import upickle.Js


/**
  * Created by bwbecker on 2016-08-09.
  */

abstract class EnumP[T <: EnumEntry] extends Enum[T] {

  implicit val writer: Pickler.Writer[T] = Pickler.Writer[T] {
    case t => Js.Str(t.toString)
  }

  implicit val reader = Pickler.Reader[T] {
    case Js.Str(str) => this.withName(str)
    case x           ⇒ throw upickle.Invalid.Data(x,
      s"$x is not one of {${
        EnumP.this.values.map {
          _.entryName
        }.mkString(", ")
      }}.")
  }

}

sealed abstract class DesignName(val shortName: String, val descr: String) extends EnumEntry

object DesignName extends EnumP[DesignName] {

  val values = findValues

  // Try to keep these names to 6 characters or less to fit in Overview table.
  case object fptp extends DesignName("FP", "338 single-member ridings")

  case object stv_huge extends DesignName("", "")
  case object stv_med extends DesignName("SM", "STV with medium-sized multi-member ridings")

  case object stv_small extends DesignName("SS", "STV with small multi-member ridings")

  case object mmp_small extends DesignName("MS", "MMP with small regions")

  case object mmp_med extends DesignName("MM", "MMP with medium-sized regions")

  case object mmp_enlargeP extends DesignName("ME", "MMP with 338 local ridings (enlarged Parliament)")

  case object mmp_lite_prov extends DesignName("ML", "MMP with 338 local ridings (enlarged Parliament; provincial " +
    "regions)")

  case object ru_singles extends DesignName("RS", "Rural-Urban with more single-member ridings")

  case object ru_enlargeP extends DesignName("RE", "Rural-Urban with more single-member ridings and an enlarged " +
    "Parliament")

  case object ru_lite_prov extends DesignName("RE", "Rural-Urban with more single-member ridings and 10% top-up; " +
    "provincial regions")

  case object ru_multiples extends DesignName("RM", "Rural-Urban with more multi-member ridings and fewer " +
    "single-member ridings")

  case object kingsley extends DesignName("Ki", "Kingsley's model (similar to Rural-Urban, but with no top-up seats)")

  //case object markbc_best extends DesignName("MBC", "Mark BC's STV+ Best Regions")

  case object erre_ru extends DesignName("ER", "ERRE RU that gets top-ups from large multi-member ridings")

  case object erre_ru_singles extends DesignName("ER", "ERRE RU that gets top-ups from large multi-member ridings; region = " +
    "province")

  case object erre_ru_multiples extends DesignName("", "")

  case object erre_mmp_5050_small extends DesignName("ER", "ERRE MMP that gets top-ups from twinned 2015 ridings; " +
    "smaller regions")

  case object erre_mmp_5050_large extends DesignName("ER", "ERRE MMP that gets top-ups from twinned 2015 ridings; " +
    "larger regions")

  case object erre_mmp5050_ProvRegions extends DesignName("XX", "DesignName parameter not filled in.")
  case object erre_mmp5050_LargeRegions extends DesignName("XX", "DesignName parameter not filled in.")
  case object erre_ru3367_ProvRegions extends DesignName("XX", "DesignName parameter not filled in.")
}


sealed abstract class SeatType extends EnumEntry

object SeatType extends EnumP[SeatType] {
  val values = findValues

  case object RidingSeat extends SeatType

  case object TopupSeat extends SeatType

  case object AdjustmentSeat extends SeatType

}


sealed abstract class Party(val longName: String, val mainStream: Boolean) extends EnumEntry

object Party extends EnumP[Party] {
  val values = findValues

  case object Lib extends Party("Liberal", true)

  case object Con extends Party("Conservative", true)

  case object NDP extends Party("NDP", true)

  case object Grn extends Party("Green", true)

  case object Bloc extends Party("Bloc", true)

  case object Ind extends Party("Independent", false)

  case object CHP extends Party("Christian Heritage", false)

  case object Com extends Party("Communist", false)

  case object Lbt extends Party("Liberatarian", false)

  case object `M-L` extends Party("Maxist-Leninist", false)

  case object Oth extends Party("Other", false)

}

sealed abstract class ProvName(val longName: String) extends EnumEntry

object ProvName extends EnumP[ProvName] {
  val values = findValues

  case object AB extends ProvName("Alberta")

  case object BC extends ProvName("British Columbia")

  case object MB extends ProvName("Manitoba")

  case object NB extends ProvName("New Brunswick")

  case object NL extends ProvName("Newfoundland & Labrador")

  case object NS extends ProvName("Nova Scotia")

  case object NT extends ProvName("Northwest Territories")

  case object NU extends ProvName("Nunavut")

  case object ON extends ProvName("Ontario")

  case object PE extends ProvName("Prince Edward Island")

  case object QC extends ProvName("Quebec")

  case object SK extends ProvName("Saskatchewan")

  case object YT extends ProvName("Yukon Territories")

}