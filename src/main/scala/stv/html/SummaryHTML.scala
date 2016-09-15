package stv.html

import stv.ProvName._
import stv.Party._
import stv.io.Input

import scalatags.Text.TypedTag
import scalatags.Text.all._

import stv._

/**
  * Created by bwbecker on 2016-06-15.
  */
case class SummaryHTML(params: Params, sim: Sim) extends Page {


  /**
    * Summarize the simulation with stats for FPTP, stats for STV only (no top-up seats)
    * and stats for STV in the ridings plus top-up seats.  Throw in a few others, as well.
    */
  val ridingAnalysis = Analysis(sim.newRidings.values.toList)
  val hybridAnalysis = Analysis(sim.regions)

  protected val outDir: String = params.outDir
  protected val outFile: String = "index.html"
  protected val pgTitle: String = s"Summary of ${params.title}"

  protected def content: TypedTag[String] = {
    div(

      params.description,

      h2("Summary Statistics"),
      div(cls := "blockIndent")(
        p("""Statistics concerning all of the MPs elected -- both in ridings and as top-ups."""),
        hybridAnalysis.statsByPartyAsHTML(true)
      ),

      sensitivity(List((Con, Lib), (NDP, Lib))),

      subsets,

      h2("Population vs. Riding Area"),
      ridingPopVsAreaGraph("popVsAreaGraph"),

      districtMagnitudes,

      h2("Methodology"),
      this.methodology
    )
  }


  private def methodology: TypedTag[String] = {

    val smes = params.singleMemberElectionStrategy
    val mmes = params.multiMemberElectionStrategy

    if (smes == mmes) {
      div(cls := "blockIndent")(
        h3(s"Ridings: ${smes.name}"),
        smes.description
      )
    } else {
      div(cls := "blockIndent")(
        h3(s"Single-Member Ridings: ${smes.name}"),
        smes.description,
        h3(s"Multi-Member Ridings: ${mmes.name}"),
        mmes.description,
        h3("Top-up Seats"),
        p("write a description")
      )
    }
  }



  def districtMagnitudes = {

    def mkTable(counts: Seq[Int]) = {
      val groups = counts.groupBy(n ⇒ n)
      val max = groups.keys.max
      val min = groups.keys.min
      var sum = 0.0
      var n = 0

      div(
        table(
          tr(th("# of MPs"), th("# of Districts")),
          for (dm ← groups.keys.toVector.sorted) yield {
            val cnt = groups.get(dm).map(_.length).getOrElse(0)
            sum = sum + dm * cnt
            n = n + cnt

            tr(td(cls := "right")(dm), td(cls := "right")(if (cnt == 0) "" else cnt.toString))
          },
          tr(td(cls := "right")("Average:"), td(cls := "right")(f"${sum / n}%4.1f"))
        )
      )
    }

    def combined: Seq[Int] = {
      val r = for {
        region ← sim.regions
        prov = region.ridings.head.province
        if prov != "YT" && prov != "NT" && prov != "NU"
      } yield {
        region.ridings.map(_.districtMagnitude).sum + region.topUpSeats
      }
      //println(r)
      r
    }

    div(
      h2("District Magnitudes"),

      div(cls := "districtMag blockIndent")(
        p(
          s"""The district magnitude is the number of MPs that represent as specific area.  With FPTP, all
        ridings are represented by a single MP, so the district magnitude is 1 for every riding.  In
        other systems, the number of MPs may vary.  These tables show the number of districts (riding or region)
        that have a given number of MPs representing it for the ${params.shortTitle} electoral model."""),

        h3("Riding-Level District Magnitudes"),
        p(
          """When we considers the local riding, how many MPs are there?
          How many ridings have that same number?"""),
        mkTable(sim.newRidings.values.map(r ⇒ r.districtMagnitude).toSeq),

        h3("Region-Level District Magnitudes"),
        p(
          """When we considers only the top-up MPs in a region, how many MPs are there?
          How many regions have that same number?"""),
        mkTable(sim.regions.map(_.topUpSeats)),

        h3("Combined District Magnitude"),
        p(
          """When we consider the total number of MPs in a region (all of the local riding plus the top-up MPs),
          how many MPs are there?  How many ridings have that same number?"""),
        p(
          """In electoral models that don't have the concept of a region with top-up MPs (like STV, FPTP, and AV),
        the "region" is the province.  Territories are always excluded from this table."""),

        mkTable(combined)

      )
    )
  }


  /**
    * Statistics about various subsets of ridings.
    */
  def subsets = {

    def ridingsOnlyStats: TypedTag[String] = {
      div(
        h3("Local-Ridings Only"),
        div(cls := "blockIndent")(
          p(
            """Statistics concerning only the MPs elected in ridings, without the top-up MPs.
          This is useful for understanding how much the top-up MPs help create proportionality."""),
          ridingAnalysis.statsByPartyAsHTML(true)
        )
      )

    }


    def singleMemberRidingStats: TypedTag[String] = {

      val singles = Analysis(sim.newRidings.values.filter(r ⇒ r.districtMagnitude == 1).toList)

      div(
        h3("Single-Member Riding Stats"),
        p(
          """Statistics on all of the single-member ridings as a group.  In a FPTP simulation, this
        will be the same as the above.  In an MMP simulation it will be similar to a FPTP because
        the top-up MPs are not included.  A Hybrid model is where it's the most interesting.  How
        out of whack are the single-member ridings?"""),
        singles.statsByPartyAsHTML()
      )
    }

    def multiMemberRidingStats: Option[TypedTag[String]] = {

      val ridings = sim.newRidings.values.filter(r ⇒ r.districtMagnitude > 1).toList
      if (ridings.isEmpty) {
        None
      } else {

        val multi = Analysis(ridings)
        Some(div(
          h3("Multi-Member Riding Stats"),
          p("""Statistics on all of the multi-member ridings as a group."""),
          multi.statsByPartyAsHTML()
        )
        )
      }
    }


    def provinceStats(title: String, descr: String, provinces: List[ProvName]): TypedTag[String] = {
      val ridings = Analysis(sim.newRidings.values.filter(r ⇒ provinces.contains(r.province)).toList)
      val regions = Analysis(sim.regions.filter(r ⇒ provinces.contains(r.ridings.head.province)))

      div(id := "provStats")(
        h3(title),
        p(descr),
        Analysis.comparativeStatsByPartyAsHTML(ridings, regions)
      )
    }

    def eastRidingStats = {
      val provinces = List(ON, QC, NB, NL, NS)
      provinceStats(
        "Eastern Provinces",
        s"""Statistics on all of the "eastern" provinces: ${provinces.mkString(", ")}.""",
        provinces
      )
    }

    def maritimesRidingStats = {
      val provinces = List(NB, NL, NS, PE)
      provinceStats(
        "Maritime Provinces",
        s"""Statistics on all of the "Maritime" provinces: ${provinces.mkString(", ")}.""",
        provinces
      )
    }

    def bcRidingStats = {
      val provinces = List(BC)
      provinceStats(
        "BC",
        s"""Statistics on British Columbia.""",
        provinces)
    }

    def prairieRidingStats = {
      val provinces = List(AB, MB, SK)
      provinceStats(
        "Prairie Provinces",
        s"""Statistics on all of the "prairie" provinces: ${provinces.mkString(", ")}.""",
        provinces)
    }

    def ontarioRidingStats = {
      val provinces = List(ON)

      provinceStats(
        "Ontario",
        s"""Statistics on Ontario.""",
        provinces
      )
    }

    def quebecRidingStats = {
      val provinces = List(QC)

      provinceStats(
        "Quebec",
        s"""Statistics on Quebec.""",
        provinces
      )
    }

    def albertaRidingStats = {
      val provinces = List(AB)

      provinceStats(
        "Alberta",
        s"""Statistics on Alberta.""",
        provinces
      )
    }


    div(id := "subsets")(
      h2("Statistics for various subsets of ridings"),
      div(cls := "blockIndent")(
        ridingsOnlyStats,
        singleMemberRidingStats,
        multiMemberRidingStats,
        bcRidingStats,
        prairieRidingStats,
        eastRidingStats,
        maritimesRidingStats,
        ontarioRidingStats,
        quebecRidingStats,
        albertaRidingStats
      ))
  }


  def ridingPopVsAreaGraph(cssId: String): TypedTag[String] = {

    def mkData(ridingByArea: Vector[(Int, Int)], totalPop: Double): (Vector[String], Vector[(Int, Double)]) = {
      var cumPop = 0
      val cumStats = ridingByArea.map { case (area, population) ⇒
        cumPop += population
        (area, cumPop / totalPop)
      }
      val data = cumStats.map { case (area, pctPop) ⇒
        s"{x:${area}, y:${(pctPop * 100).toInt}}"
      }
      (data, cumStats)
    }

    val ridingByArea = sim.newRidings.values.toVector.sortBy(_.area)
    val maxArea = ridingByArea.last.area

    val totalPop = ridingByArea.map {
      _.population
    }.sum.toDouble

    val (modelData, cumStats) = mkData(ridingByArea.map{r => (r.area, r.population)}, totalPop)
    def popArea(pct: Double): Int = {
      cumStats.find(_._2 > pct).get._1
    }

    val (fptpData, _) = mkData(Input.originalRidings.sortBy(_.area).map{r => (r.area, r.pop) }, totalPop)

    div(
      p(
        """One concern in developing an electoral system for Canada is the diversity in riding
        geographical sizes.  They
        currently range from as small as 6km""", sup("2"), "to almost 2.1 million km",
        sup("2"),
        """.  This graph gives the means to compare how
        different electoral systems deal with riding sizes.  It answers the question "What percentage of
        Canada's population lives in ridings smaller than """, i("x"), "km", sup("2"),
        """?". """),

      p(
        f"""This graph shows that with this model 50%% of our population would live in ridings
      smaller than
      ${popArea(0.5)}%,1d km""", sup("2"),
        f""" and 90%% of our population live in ridings
      smaller than
      ${popArea(0.9)}%,1d km""", sup("2"),
        """."""),

      div(cls := "chart")(
        canvas(id := cssId, width := 400.px, height := 400.px),
        fieldset(
          legend("X Scale: "),
          input(`type` := "radio", name := "scale", id := "log", value := "log")("Logarithmic"),
          input(`type` := "radio", name := "scale", id := "linear", value := "linear", checked)("Linear")
        )),

      script(raw(
        s"""
        window.onload = function() {
           document.getElementById("log").addEventListener("click", function(){
              chartOptions.options.scales.xAxes[0].type = "logarithmic";
              myChart.destroy();
              myChart = new Chart(ctx, chartOptions);
           });

           document.getElementById("linear").addEventListener("click", function(){
              chartOptions.options.scales.xAxes[0].type = "linear";
              myChart.destroy();
              myChart = new Chart(ctx, chartOptions);
           });
        };


    var chartType = "logarithmic"
    var ctx = document.getElementById("${cssId}");
    var chartOptions = {
    type: 'line',
    data: {
        datasets: [
        {   label: 'New ridings',
            data: ${modelData.mkString("[", ",", "]")},
            backgroundColor: 'rgba(5, 5, 5, 0)',
            borderColor: 'rgba(50, 50, 50, 1)',
            borderWidth: 1,
            pointRadius: 1
        },
        {   label: 'Current ridings',
            data: ${fptpData.mkString("[", ",", "]")},
            backgroundColor: 'rgba(15, 15, 15, 0)',
            borderColor: 'rgba(100, 0, 0, 1)',
            borderWidth: 1,
            pointRadius: 1
        }

      ]
    },
    options: {
        scales: {
            yAxes: [{
                ticks: {
                    beginAtZero:true,
                    callback: function(value, index, values) {
                        return value + '%';
                    }
                },
                scaleLabel: {
                  display: true,
                  labelString: "Percent of Population"
                }
            }],
            xAxes: [{
              type: "logarithmic",
              position: 'bottom',
              ticks: {
                min: 10,
                max: 4000000, //${maxArea},
                callback: function(value, index, values) {
                  return value.toLocaleString();
                }
              },
              scaleLabel: {
                display: true,
                labelString: "Area (km^2)"
              }
            }]
        }
    }
  };
  var myChart = new Chart(ctx, chartOptions);
"""))
    )
  }

  /**
    * How sensitive is this model to voter mood swings?
    */
  def sensitivity(pairs: List[(Party, Party)]): TypedTag[String] = {

    val fromName = pairs.head._1.longName //stv.partyName(pairs.head._1)
    val toName = pairs.head._2.longName // stv.partyName(pairs.head._2)

    val explanation = div(cls := "blockIndent")(
      p(
        s"""What happens if public sentiment swings towards one party and away from another?
      This graph tries to answer that question.  Using the riding-by-riding results from
      2015, it systematically moves an increasing number of votes from one party to
      another."""),

      p(
        s"""If the lines representing the ${fromName}'s votes tracks the line for
      the ${fromName}'s MPs (and similar for the other parties), then the electoral
      system is proportional across a wide range of electoral scenarios."""),

      p(
        s"""On the other hand, if the lines for the votes earned and the MPs elected
      are farther apart -- as is the case for FPTP and AV -- then the electoral system is not
      proportional."""),

      if (pairs.head._1 == Con && pairs.head._2 == Lib) {
        p("Examples: ",
          ul(
            li(
              """At -20% on the bottom axis, 20% of the Liberal's vote in 2015 is
          given to the Conservatives to simulate an election where the Liberals earned
          31% of the vote and the Conservatives earned almost 40%.  The lighter
          red and blue lines show how many MPs would have been elected for each
          party by this voting system."""),
            li(
              """At +6% on the bottom axis, 6% of the Conservative's vote in 2015 is
          given to the Liberals to simulate an even more lop-sided win (41.4% to 30%).
          Again, the
          light red and blue lines show how many MPs would have been elected for each
          party by this voting system.""")
          )
        )
      } else {
        p()
      },

      p(
        """The black line, hopefully along the bottom of the graph, shows the
      Gallagher Index, an index of voting proportionality.  Smaller numbers
      are better."""),

      if (sim.design.is_proportional) {
        p(
          """The Liberals are consistenly over-represented in the graph, below.  Why?""",
          ul(
            li("Liberals win in the Yukon, Northwest Terretories, and Nunavut where there are single-member ridings" +
              "with no compensation mechanism."),
            li("Liberals win in New Brunswick and Newfoundland & Labrador by a large enough margin that " +
              "compensation mechanisms can't adequately compensate."),
            li(
              """When Green and NDP voters
                |fail to elect a candidate, their votes tend to transfer to the Liberals.  So the Liberals
                |get the seats but the graph still compares against first-choice preferences.""")
          )
        )
      } else {
        p()
      }

    )

    def sensitivityGraph(canvasId: String,
                         fromPartyName: String,
                         toPartyName: String,
                         data: Vector[SensitivityDataPoint]): TypedTag[String] = {

      def toData(lst: Seq[Double]): String = {
        //data: ${lst.map(v ⇒ f"${v.libVotes*100}%4.1f").mkString("[", ", ", "]")},
        lst.map { d ⇒ f"${d * 100}%4.1f" }.mkString("[", ", ", "]")
      }

      script(raw(
        s"""
    var ctx = document.getElementById('$canvasId');
    var myChart = new Chart(ctx, {
    type: 'line',
    data: {
        labels: ${data.map(s ⇒ f"'${s.shift * 100}%4.0f%%'").mkString("[", ", ", "]")},
        datasets: [
        {
            label: 'Lib Votes',
            data: ${toData(data.map(_.libVotes))},
            backgroundColor: 'rgba(5, 5, 5, 0)',
            borderColor: 'red',
            borderWidth: 3,
            pointStyle: 'circle',
            pointRadius: 4
        },
        {
            label: 'Lib MPs',
            data: ${toData(data.map(_.libMPs))},
            backgroundColor: 'rgba(10, 10, 10, 0)',
            borderColor: 'rgba(200, 10, 10, 1)',
            borderWidth: 1,
            pointStyle: 'circle',
            pointRadius: 5
        },
        {
            label: 'Con Votes',
            data: ${toData(data.map(_.conVotes))},
            backgroundColor: 'rgba(15, 15, 15, 0)',
            borderColor: 'rgba(0, 0, 255, 1)',
            borderWidth: 3,
            pointStyle: 'triangle',
            pointRadius: 4
        },
        {
            label: 'Con MPs',
            data: ${toData(data.map(_.conMPs))},
            backgroundColor: 'rgba(20, 20, 20, 0)',
            borderColor: 'rgba(10, 10, 200, 1)',
            borderWidth: 1,
            pointStyle: 'triangle',
            pointRadius: 5
        },
        {
            label: 'NDP Votes',
            data: ${toData(data.map(_.ndpVotes))},
            backgroundColor: 'rgba(25, 25, 25, 0)',
            borderColor: 'orange',
            borderWidth: 3,
            pointStyle: 'rect',
            pointRadius: 4
        },
        {
            label: 'NDP MPs',
            data: ${toData(data.map(_.ndpMPs))},
            backgroundColor: 'rgba(35, 35, 35, 0)',
            borderColor: 'rgba(200, 100, 100, 1)',
            borderWidth: 1,
            pointStyle: 'rect',
            pointRadius: 5
        },
        {
            label: 'Gallagher',
            data: ${toData(data.map(_.gallagher))},
            backgroundColor: 'rgba(30, 30, 30, 0)',
            borderColor: 'rgba(5, 5, 5, 1)',
            borderWidth: 2,
            borderDash: [2,2],
            pointStyle: 'cross'
        }
      ]
    },
    options: {
        legend: {
            display: true,
            labels: {
                usePointStyle: true,
                boxWidth: 10
            }
        },
        scales: {
            yAxes: [{
                ticks: {
                    beginAtZero:true,
                    max: 80,
                    callback: function(value, index, values) {
                        return value + '%';
                    }
                },
                scaleLabel: {
                  labelString: "Percent votes or MPs",
                  display: true
                }
            }],
            xAxes: [{
                scaleLabel: {
                  labelString: "%Votes Shifted from ${toPartyName} to ${fromPartyName}" +
                  "                %Votes shifted from ${fromPartyName} to ${toPartyName}",
                  display: true
                  }
               }
            ]
        }
    }
});
"""))
    }


    div(id := "sensitivity", cls := "sensitivity")(
      h2("Vote Swing Analysis"),
      explanation,
      for {(fromParty, toParty) ← pairs
           fromPartyName = fromParty.longName
           toPartyName = toParty.longName
      } yield {
        val data = sim.sensitivityAnalysis(fromParty, toParty)
        val canvasId = s"c$fromParty$toParty"
        div(
          h3(s"Voters shift between ${fromPartyName} and ${toPartyName}"),
          canvas(id := canvasId, width := 400.px, height := 400.px),

          sensitivityGraph(canvasId, fromPartyName, toPartyName, data),
          p(f"Average Gallagher Index: ${sim.sensitivityAvgGallagher(data) * 100}%4.1f%%")
        )
      }
    )
  }

}
