package stv

import scalatags.Text.all._

import stv.Party.{Con, Grn, Lib, NDP}

/**
  * Created by bwbecker on 2016-10-29.
  */
trait Common {


  object TestXfer extends VoteXfer {
    val name = "TextXfer"
    val shortDescr = "testXfer"
    val description = p("TestXfer")
    val source = None

    def xfer(p1: Party, p2: Party): Double = {
      (p1, p2) match {
        // Great idea, but the production code doesn't do this.
        //case (x, y) if (x == y) ⇒ 1.0
        case (Con, Lib) ⇒ 0.25
        case (Con, Grn) ⇒ 0.10
        case (NDP, Lib) ⇒ 0.5
        case (Lib, Grn) ⇒ 0.10
        case (_, _)     ⇒ 0.0
      }
    }
  }

}
