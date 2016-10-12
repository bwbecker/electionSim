package stv.html

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv.{Sim, Params}

/**
  * Created by bwbecker on 2016-07-04.
  */
case class AboutHTML(sim: Option[Sim]) extends Page {

  val outDir = sim.map(_.params.outDir).getOrElse("overview")
  val outFile = "about.html"
  val pgTitle = "About Elections Modelling"
  override protected val includeThisModelMenu = sim.isDefined


  protected def content: TypedTag[String] = {
    div(
      h2("What it is"),
      p("""These pages are the output of a program that simulates a electoral systems.
          It can accomodate models that have ridings with 1 or more MPs and regions with
          0 or more "top-up" seats assigned to clusters of ridings.  That is, it can model:
        """,
        ul(
          li("First-Past-The-Post: 1 MP in each riding, no top-up seats"),
          li("Alternative Vote: 1 MP in each riding, no top-up seats"),
          li("Mixed Member Proportional: 1 MP in each riding with top-up seats"),
          li("Single Transferable Vote: multiple MPs in each riding, no top-up seats"),
          li("Hybrid models: 1 or more MPs in each riding with top-up seats.")
        )

      ),
      h2("How it works"),
      p("""The overall model is:""",
        ol(li("""Group existing ridings into new ridings, each with an
          assigned district magnitude (which may be as low as 1).  An existing riding may be split
          between new ridings."""),
          li("""Group the new ridings into compensation regions, giving each a
            number of compensatory or "top-up" seats (which may be as low as 0)."""),
          li("""Run an election in each of the ridings to determine the riding MPs.  Several different
          election algorithms are implemented."""),
          li("""For each region, determine which parties should be assigned the top-up seats."""))),

      
      h2(id := "contact")("Contact"),
      p(a(href := "mailto:bwbecker@election-modelling.ca")("Byron Weber Becker"), "(bwbecker@election-modelling.ca)")
    )
  }

}
