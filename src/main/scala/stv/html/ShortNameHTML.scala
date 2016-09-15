package stv.html


import scalatags.Text.TypedTag
import scalatags.Text.all._
import stv._

/**
  * Created by bwbecker on 2016-07-04.
  */
object ShortNameHTML extends Page {

  val outDir = "overview"
  val outFile = "shortName.html"
  val pgTitle = "Short Name Abbreviations"
  override protected val includeThisModelMenu = false


  protected def content: TypedTag[String] = {
    div(
      p("Short names are a consise summary of the key parameters for a simulation."),
      p("The first two letters are the riding design with a possible variant.  They are:"),
      ul(
        for (dn <- DesignName.values) yield {
          li(s"${dn.shortName}: ${dn.descr}")
        }
      ),
      p("The third letter is the election strategy used in single-member ridings.  They are:"),
      ul(
        for (sms <- ElectionStrategy.singleMbrStrategies) yield {
          li(s"${sms.shortName}: ", sms.help)
        }
      ),
      p("The fourth letter is the election strategy used in multi-member ridings.  They are:"),
      ul(
        for (mms <- ElectionStrategy.multiMbrStrategies :+ NotApplicableElectionStrategy) yield {
          li(s"${mms.shortName}: ", mms.help)
        }
      )
    )
  }
}

