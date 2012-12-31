package ch.obermuhlner.scala.microbenchmark

import scala.collection.mutable.ListBuffer

import scala.xml.PrettyPrinter
import scala.xml.Node

trait SingleHtmlReport extends ImageReport {
  val toc = new ListBuffer[Node]
  val main = new ListBuffer[Node]
  val appendix = new ListBuffer[Node]

  var reportDetails = false

  var doubleFormat = "%.4f"

  def add[T, V](title: String, description: String, datasetLabel: String, datasets: Seq[T], suite: Suite[T, V]) {
    add(title, description, datasetLabel, Seq[T](), datasets, suite)
  }

  def add[T, V](title: String, description: String, datasetLabel: String, warmupDatasets: Seq[T], datasets: Seq[T], suite: Suite[T, V]) {
    val startMillis = System.currentTimeMillis()
    val suiteResults = runner.execute(warmupDatasets, datasets, suite.benchmarks)

    toc += <li><a href={ "#suite_" + toIdentifier(title) }>Suite { title }</a></li>

    main += <a name={ "suite_" + toIdentifier(title) }/>
    main += <h1>Suite { title }</h1>
    main += <p>{ description }</p>

    chartWidth = 800
    chartHeight = 600
    main += <img src={ statisticalLineChart("Statistics " + title, datasetLabel, suiteResults) } alt={ "Statistics " + title }/>
    main += <img src={ lineChart(title, datasetLabel, 0.95, suiteResults) } alt={ "Best 95% " + title }/>

    main += <table border="0">
              {
                for (benchmark <- suite.benchmarks) yield {
                  <tr><td><b>{ benchmark.name }</b></td><td>{ benchmark.description }</td></tr>
                }
              }
            </table>

    main += <p>Details about measurements in the <a href={ "#measurements_" + toIdentifier(title) }>appendix</a>.</p>

    appendix += <a name={ "measurements_" + toIdentifier(title) }/>
    appendix += <h2>Measurements Suite { title }</h2>

    val referenceResults = suiteResults(0)

    for (results <- suiteResults) {
      appendix += <h3>{ title } - { results(0).name } ({ results.length } datasets)</h3>
      appendix += <p>{ results(0).description }</p>
      val chartName = "all_measurements_" + title + "_" + results(0).name
      val chartTitle = title + " " + results(0).name
      chartWidth = 800
      chartHeight = 600
      appendix += <img src={ allMeasurementsChart(chartName, false, results) } alt={ chartTitle }/>

      appendix += <table border="1">
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
          appendix += <h4>{ title } - { result.name } { result.dataset }</h4>

          val chartName = title + "_" + result.name + "_" + result.dataset
          val chartTitle = title + " - " + result.name + " " + result.dataset
          chartWidth = 800
          chartHeight = 300
          val bins = (result.elapsedTimes.length / 10) min 100
          appendix += <img src={ allMeasurementsChart("all_measurements_" + chartName, chartTitle, true, result) } alt={ chartTitle }/>
          appendix += <img src={ histogramChart("histogram_" + chartName, chartTitle, bins, result) } alt={ chartTitle }/>
          appendix += <p>Input:</p>
          appendix += <pre>{ result.dataset }</pre>
          appendix += <p>Output:</p>
          appendix += <pre>{ result.result }</pre>
        }
      } else {
        if (results.size >= 1) {
          val r = results map (_.result)
        }
      }

      appendix += <p>The overview over the suite is in chapter <a href={ "#suite_" + toIdentifier(title) }>Suite { title }</a>.</p>
    }

    val elapsedSeconds = (System.currentTimeMillis() - startMillis) / 1000.0
    appendix += <p>The report for this suite was generated in { elapsedSeconds } seconds.</p>
  }

  private def toIdentifier(string: String) = {
    string
  }

  private def singlePage = {
    <html>
      <head>
        <title>Benchmark Report</title>
      </head>
      <body>
        <h1>Table Of Contents</h1>
        <ol>
          { toc }
          <li><a href="#appendix">Appendix</a></li>
        </ol>
        { main }
        <a name="appendix"/>
        <h1>Appendix</h1>
        { appendix }
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
          <li><a href="#appendix">Appendix</a></li>
        </ol>
      </body>
    </html>
  }

  def report() {
  }

  def report(filename: String) {
    scala.xml.XML.save(filename, singlePage)
  }
}
