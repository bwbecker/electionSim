package stv.html

import scalatags.Text.TypedTag
import scalatags.Text.all._

import ca.bwbecker.io._
import ca.bwbecker.enrichments._
import stv.Main


import java.io.File


trait Page {

  protected val outDir: String
  protected val outFile: String
  protected val pgTitle: String
  protected val includeThisModelMenu = true

  protected def content: TypedTag[String]

  // if outDir has a year, we need to go up one more directory level for css and overviews.
  private lazy val prefix = if ("\\d{4}/.*".r.matches(outDir)) {"../.."} else {".."}

  private def renderPage: TypedTag[String] = {
    val css = s"${prefix}/css/main.css"
    scalatags.Text.all.html(
      head(
        meta(charset := "UTF-8"),
        link(rel := "stylesheet", href := css, `type` := "text/css"),
        //link(rel := "stylesheet", href := "https://yui.yahooapis.com/pure/0.6.0/base-min.css"),
        link(rel := "stylesheet", href := "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.6.3/css/font-awesome.css"),
        script(src := "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.2.2/Chart.min.js"),
        scalatags.Text.tags2.title(pgTitle)),
      body(
        div(cls := "centreColumn")(
          menus,
          //p(cls := "group"),
          div(cls := "content")(
            //p("stuff"),
            h1(pgTitle),
            content,
            tracking
          )
        )
      )
    )

  }


  /** Print this page to a file. */
  def print: Unit = {
    printToFile(new File(s"${Main.outdir}/${outDir}/${outFile}")) { p ⇒ p.println(renderPage) }
  }

  def menus = {
    scalatags.Text.tags2.nav(cls := "group")(
      ul(
        li(
          a(href := "#")("Overview"),
          ul(
            li(a(href := s"${prefix}/overview/index.html")("Featured Systems")),
            li(a(href := s"${prefix}/overview/allSimulations.html")("All Systems")),
            li(a(href := s"${prefix}/ModellingElections_en.pdf")("ERRE Submission"))
          )
        ),
        li(
          a(href := "#")("Featured Models"),
          ul(
            for (p ← stv.Main.featuredSystems.sortBy(_.name)) yield {
              val file = if (this.includeThisModelMenu) this.outFile else "index.html"
              li(a(href := s"${prefix}/${p.outDir}/${file}")(p.title))
            }
          )
        ),
        if (this.includeThisModelMenu) {
          li(
            a(href := "#")("This Model"),
            ul(
              li(a(href := "index.html")("Summary")),
              li(a(href := "ridingResults.html")("Riding Results")),
              li(a(href := "ridingStats.html")("Riding Stats")),
              li(a(href := "regionResults.html")("Region Results")),
              li(a(href := "regionStats.html")("Region Stats")),
              li(a(href := "provResults.html")("Provincial Results")),
              li(a(href := "params.html")("Parameters"))
            )
          )
        } else {
          li(a(raw("&nbsp;")), ul())
        },
        li(
          a(href := "about.html")("About"),
          ul(
            li(a(href := "about.html")("About")),
            li(a(href := "about.html#contact")("Contact"))
          )
        )
      )

    )
  }


  def tracking = raw(
    """
    <script>
        (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
        })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

        ga('create', 'UA-69741814-2', 'auto');
        ga('send', 'pageview');

  </script>
    """
  )

}
