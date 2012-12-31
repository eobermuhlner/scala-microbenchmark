package ch.obermuhlner.scala.microbenchmark

import scala.collection.mutable.ListBuffer

import scala.xml.PrettyPrinter
import scala.xml.Node

trait HtmlReport extends ImageReport {
  val toc = new ListBuffer[Node]
  val main = new ListBuffer[Node]

  var reportDetails = false

  var doubleFormat = "%.4f"

  def add[T, V](title: String, description: String, datasetLabel: String, datasets: Seq[T], suite: Suite[T, V]) {
    add(title, description, datasetLabel, Seq[T](), datasets, suite)
  }

  def add[T, V](title: String, description: String, datasetLabel: String, warmupDatasets: Seq[T], datasets: Seq[T], suite: Suite[T, V]) {
    val mainFile = "suite_" + toIdentifier(title) + ".html"
    val detailsFile = "details_" + toIdentifier(title) + ".html"

    val startMillis = System.currentTimeMillis()
    val suiteResults = runner.execute(warmupDatasets, datasets, suite.benchmarks)

    toc += <li><a href={ "#suite_" + toIdentifier(title) }>Suite { title }</a></li>

    main += <hr></hr>
    main += <a name={ "suite_" + toIdentifier(title) }></a>
    main += <h1>Suite { title }</h1>
    main += <p>{ description }</p>

    chartWidth = 800
    chartHeight = 600
    val lineChart95 = lineChart(title, datasetLabel, 0.95, suiteResults)
    main += <img src={ lineChart95 } alt={ title + " (Average of lowest 95%)" }/>

    main += <table border="0">
              {
                for (benchmark <- suite.benchmarks) yield {
                  <tr><td><b>{ benchmark.name }</b></td><td>{ benchmark.description }</td></tr>
                }
              }
            </table>

    main += <p>Input ranges from { datasets.first } to { datasets.last } in { datasets.size - 1 } steps.</p>
    
    main += <p>Additional information and statistics about measurements in the <a href={ detailsFile }>details report</a>.</p>

    val details = new ListBuffer[Node]

    details += <h1>Details - { title }</h1>
    details += <img src={ lineChart95 } alt={ title + " (Average of lowest 95%)" }/>
    details += <img src={ statisticalLineChart(title + " (Statistics)", datasetLabel, suiteResults) } alt={ title + " (Average and standard deviation)" }/>

    details += <a name={ "measurements_" + toIdentifier(title) }/>
    details += <h2>Measurements Suite { title }</h2>

    val referenceResults = suiteResults(0)

    for (results <- suiteResults) {
      details += <h3>{ title } - { results(0).name } ({ results.length } datasets)</h3>
      details += <p>{ results(0).description }</p>
      val chartTitle = title + " " + results(0).name + " (all results)"
      val chartName = "all_measurements_" + title + "_" + results(0).name
      chartWidth = 800
      chartHeight = 600
      details += <img src={ allMeasurementsChart(chartName, chartTitle, false, results) } alt={ chartTitle }/>

      details += <table border="1">
                   <tr>
                     <td>Dataset</td>
                     <td>Length</td>
                     <td>Min</td>
                     <td>Max</td>
                     <td>Average</td>
                     <td>Median</td>
                     <td>Standard Deviation</td>
                     <td>Factor (Average)</td>
                     <td>Factor (Median)</td>
                   </tr>
                   {
                     for ((result, referenceResult) <- results zip referenceResults) yield {
                       val statistics = new Statistics(result.elapsedTimes)
                       val referenceStatistics = new Statistics(referenceResult.elapsedTimes)
                       <tr>
                         <td>{ result.dataset }</td>
                         <td>{ statistics.length }</td>
                         <td>{ statistics.min.formatted(doubleFormat) }</td>
                         <td>{ statistics.max.formatted(doubleFormat) }</td>
                         <td>{ statistics.average.formatted(doubleFormat) }</td>
                         <td>{ statistics.median.formatted(doubleFormat) }</td>
                         <td>{ statistics.standardDeviation.formatted(doubleFormat) }</td>
                         <td>{ (statistics.average / referenceStatistics.average).formatted(doubleFormat) }</td>
                         <td>{ (statistics.median / referenceStatistics.median).formatted(doubleFormat) }</td>
                       </tr>
                     }
                   }
                 </table>

      if (reportDetails) {
        for (result <- results) {
          details += <h4>{ title } - { result.name } { result.dataset }</h4>

          val chartName = title + "_" + result.name + "_" + result.dataset
          val chartTitle = title + " - " + result.name + " " + result.dataset
          chartWidth = 800
          chartHeight = 300
          val bins = (result.elapsedTimes.length / 10) min 100
          details += <img src={ allMeasurementsChart("all_measurements_" + chartName, chartTitle, true, result) } alt={ chartTitle }/>
          details += <img src={ histogramChart("histogram_" + chartName, chartTitle, bins, result) } alt={ chartTitle }/>
          details += <p>Input:</p>
          details += <pre>{ result.dataset }</pre>
          details += <p>Output:</p>
          details += <pre>{ result.result }</pre>
        }
      } else {
        if (results.size >= 1) {
          val r = results map (_.result)
        }
      }

      details += <p>The overview over the suite is in chapter <a href={ "index.html#suite_" + toIdentifier(title) }>Suite { title }</a>.</p>
    }

    val elapsedSeconds = (System.currentTimeMillis() - startMillis) / 1000.0
    details += <p>The report for this suite was generated in { elapsedSeconds } seconds.</p>

    scala.xml.XML.save(detailsFile, singlePage("Appendix " + title, details))
    scala.xml.XML.save("index.html", mainPage)
  }

  private def toIdentifier(string: String) = {
    string
  }

  private def singlePage(title: String, content: Seq[Node]) = {
    <html>
      <head>
        <title>{ title }</title>
      </head>
      <body>
        { content }
      </body>
    </html>
  }

  private def tocPage = {
    <html>
      <head>
        <title>Benchmark Report</title>
      </head>
      <body>
        <h1>Table Of Contents</h1>
        <ol>
          { toc }
        </ol>
      </body>
    </html>
  }

  private def mainPage = {
    <html>
      <head>
        <title>Benchmark Report</title>
      </head>
      <body>
        <h1>Table Of Contents</h1>
        <ol>
          { toc }
        </ol>
        { main }
      </body>
    </html>
  }

  def report() {
    scala.xml.XML.save("index.html", mainPage)
  }
}
