
package com.rockthejvm

import com.rockthejvm.DataCollector.detectAndHandleIssues

import java.io.{BufferedWriter, FileWriter}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDate, LocalDateTime, YearMonth}
import scala.io.Source
import scala.io.StdIn.readLine

object GetData {
  import java.io.BufferedReader
  import java.io.InputStreamReader
  import java.net.HttpURLConnection
  import java.net.URL

  def fetchData(dataID: String): Unit = {
    try {
      val urlString = s"https://data.fingrid.fi/api/datasets/$dataID/data?format=json&locale=en&sortBy=startTime&sortOrder=desc"
      val url = new URL(urlString)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]

      // Request headers
      connection.setRequestProperty("Cache-Control", "no-cache")
      connection.setRequestProperty("x-api-key", "dde765149c744ba3bc873d756b9ed406")
      connection.setRequestMethod("GET")

      val status = connection.getResponseCode
      println(s"HTTP Status: $status")

      val in = new BufferedReader(new InputStreamReader(connection.getInputStream))
      val content = new StringBuilder
      var inputLine = in.readLine()
      while (inputLine != null) {
        content.append(inputLine)
        inputLine = in.readLine()
      }
      in.close()

      println(content.toString)  // Print the JSON response

      connection.disconnect()
    } catch {
      case ex: Exception => println(s"Exception: ${ex.getMessage}")
    }
  }
}

object DataCollector {def detectAndHandleIssues(data: List[RenewableEnergyData]): Unit = {
  val lowEnergyThreshold = 50.0 // Assumed low energy threshold
  val issues = data.filter(d => d.value < lowEnergyThreshold)

  if (issues.nonEmpty) {
    println("Warning: Low energy output detected in the following records:")
    issues.foreach { issue =>
      println(s"Dataset ID: ${issue.datasetId}, Start: ${issue.startTime}, End: ${issue.endTime}, Value: ${issue.value}")
    }
  } else {
    println("No low energy outputs detected.")
  }

  // Assuming fault detection, the actual logic may need to be more complex
  val potentialMalfunctions = data.sliding(3).filter { window =>
    window.map(_.value).forall(_ < lowEnergyThreshold)
  }.toList

  if (potentialMalfunctions.nonEmpty) {
    println("Potential equipment malfunction detected due to consistent low energy output over multiple intervals.")
    potentialMalfunctions.foreach { malfunction =>
      malfunction.foreach { entry =>
        println(s"Potential malfunction -> Dataset ID: ${entry.datasetId}, Time: ${entry.startTime} to ${entry.endTime}, Value: ${entry.value}")
      }
    }
  } else {
    println("No equipment malfunctions detected.")
  }
}
  def monitorWindData(): Unit = {
    val windTurbinesId = "181"
    println("Monitoring wind turbine data:")
    GetData.fetchData(windTurbinesId)
  }

  def monitorSolarData(): Unit = {
    val solarPanelsId = "248"
    println("Monitoring solar panel data:")
    GetData.fetchData(solarPanelsId)
  }

  def monitorHydroData(): Unit = {
    val hydroPowerId = "191"
    println("Monitorhing hydropower data:")
    GetData.fetchData(hydroPowerId)
  }

  def run(): Unit = {
    var continue = true
    while (continue) {
      println("\nEnter the type of data to monitor:")
      println("1: Wind Turbine Data")
      println("2: Solar Panel Data")
      println("3: Hydropower Data")
      println("0: Exit")
      val choice = readLine("Please enter your choice (0-3): ")

      choice match {
        case "1" => monitorWindData()
        case "2" => monitorSolarData()
        case "3" => monitorHydroData()
        case "0" =>
          println("Exiting the monitor")
          continue = false
        case _ => println("Invalid choice, please enter a number between 0 and 3.")
      }
    }
  }
}

//First defining a case class to represent renewable energy data
case class RenewableEnergyData(datasetId: Int, startTime: LocalDateTime, endTime: LocalDateTime, value: Double)
object REPS_2024 extends App{
  // Creating a function to parse a line of data from the file into RenewableEnergyData
  def parseLine(line: String): RenewableEnergyData = {
    val fields = line.trim.split(",")
    val datasetId = fields(0).split(":")(1).trim.toInt
    val startTime = LocalDateTime.parse(fields(1).split(":")(1).trim + ":" + fields(1).split(":")(2).trim, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm"))
    val endTime = LocalDateTime.parse(fields(2).split(":")(1).trim + ":" + fields(2).split(":")(2).trim, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm"))
    val value = fields(3).split(":")(1).trim.toDouble
    RenewableEnergyData(datasetId, startTime, endTime, value)
  }

  // Creating a fununction to read data from file (for demo purpose: we use "SolarEnergy.txt" and return as a list of RenewableEnergyData
  def readData(filePath: String): List[RenewableEnergyData] = {
    val bufferedSource = Source.fromFile(filePath)
    val dataList = bufferedSource.getLines().map(parseLine).toList
    bufferedSource.close
    dataList
  }
  def menu(): Unit = {
    var exit = false
    while (!exit) {
      println("Select a use case from 1 to 5.Type 6 to end the program!")
      print("Enter your choice: ")
      val choice = scala.io.StdIn.readLine()
      choice match {
        case "1" =>
          // Write code for this use case goes here
          println("Activating Data Collector...")
          DataCollector.run()
        case "2" =>
          def collectEnergyValue(source: String):Unit = {
            val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")

            println("Enter datasetId:")
            val datasetId = scala.io.StdIn.readLine()

            //Asking user for date and time and implementing error HANDLING.
            println("Enter start time (YYYY-MM-DDTHH:MM):")
            var startTime: LocalDateTime = null
            var correctStartTime = false
            while (!correctStartTime) {
              try {
                startTime = LocalDateTime.parse(scala.io.StdIn.readLine(), formatter)
                correctStartTime = true
              } catch {
                case e: DateTimeParseException =>
                  println("Invalid format. Please enter in YYYY-MM-DDTHH:MM format.")
              }
            }

            println("Enter end time (YYYY-MM-DDTHH:MM):")
            var endTime: LocalDateTime = null
            var correctEndTime = false
            while (!correctEndTime) {
              try {
                endTime = LocalDateTime.parse(scala.io.StdIn.readLine(), formatter)
                correctEndTime = true
              } catch {
                case e: DateTimeParseException =>
                  println("Invalid format. Please enter in YYYY-MM-DDTHH:MM format.")
              }
            }
            println("Enter the value:")
            val value = scala.io.StdIn.readDouble()

            // Creating the energy data in text format
            val data = s"datasetId: $datasetId,startTime: $startTime,endTime: $endTime,value: $value"

            // Writing the data to the corresponding files
            val writer = new BufferedWriter(new FileWriter(s"${source}Energy.txt",true))
            writer.write(data)
            writer.newLine()
            writer.close()

            println(s"Energy information collected and stored in ${source}Energy.txt")
          }

          //Asking user for energy related information
          var repeat = true
          while (repeat) {
            println("Do you want to add wind energy, solar energy, or hydro energy? (Type s for solar, w for wind, h for hydro, or e to exit):")
            val sourceChoice = scala.io.StdIn.readLine()

            // Process user's choice and call collectEnergy function accordingly
            sourceChoice match {
              case "s" => collectEnergyValue("Solar")
              case "w" => collectEnergyValue("Wind")
              case "h" => collectEnergyValue("Hydro")
              case "e" => repeat = false // Exit the loop if 'e' is entered
              case _ => println("Invalid choice.")
            }
          }

        case "3" =>
          val file = "PowerPlantInfo.txt"
          val contents = Source.fromFile(file).getLines().toList

          contents.foreach(println)

        case "4" =>


          // Creating a function to filter data based on user-specified criteria(data filtering)
          def filterData(data: List[RenewableEnergyData], filter: String, value: Option[Any] = None): List[RenewableEnergyData] = {
            filter match {
              case "hourly" =>
                val hourStart = LocalDateTime.parse(value.get.toString, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm"))
                val hourEnd = hourStart.plusHours(1)
                data.filter(d => (d.startTime.isEqual(hourStart) || d.startTime.isAfter(hourStart)) && d.startTime.isBefore(hourEnd))
              case "daily" =>
                val date = LocalDate.parse(value.get.toString, DateTimeFormatter.ofPattern("yyyy-MM-dd"))
                data.filter(d => d.startTime.toLocalDate == date)
              case "weekly" =>
                val weekStart = LocalDate.parse(value.get.toString, DateTimeFormatter.ofPattern("yyyy-MM-dd"))
                val weekEnd = weekStart.plusDays(7)
                data.filter(d => d.startTime.toLocalDate.isEqual(weekStart) || d.startTime.toLocalDate.isBefore(weekEnd))
              case "monthly" =>
                val month = YearMonth.parse(value.get.toString, DateTimeFormatter.ofPattern("yyyy-MM"))
                data.filter(d => d.startTime.getMonthValue == month.getMonthValue)
              case _ => throw new IllegalArgumentException("Invalid filter provided")
            }
          }

          //Reading data from "SolarEnergy.txt"
          val sampleData = readData("SolarEnergy.txt")

          var continue = true
          while (continue) {
            // Asking the user for filter criteria
            println("Enter filter criteria: hourly, daily, weekly, monthly ? Type e to exit")
            val filter = scala.io.StdIn.readLine()

            if (filter == "e") {
              continue = false
            }
            else {
              // Printing the value based on the selected filter
              val value = filter match {
                case "hourly" =>
                  println("Enter hour (format: yyyy-MM-dd'T'HH:mm):")
                  Some(scala.io.StdIn.readLine())
                case "daily" =>
                  println("Enter date (format: yyyy-MM-dd):")
                  Some(scala.io.StdIn.readLine())
                case "weekly" =>
                  println("Enter start date of the week (format: yyyy-MM-dd):")
                  Some(scala.io.StdIn.readLine())
                case "monthly" =>
                  println("Enter month (format: yyyy-MM):")
                  Some(scala.io.StdIn.readLine())
                case e => //Exit the loop
                  continue = false
                  None
                case _ => None
              }

              // Printing filtered data based on user input
              val filteredData = filterData(sampleData, filter, value)
              println("Filtered Data:")
              filteredData.foreach(println)
            }

            //Data Analysis
            def meanCalculation(values: List[Double]): Double = {
              values.sum / values.length
            }

            def medianCalculation(values: List[Double]): Double = {
              val sortedValues = values.sorted
              val a = sortedValues.length
              if (a % 2 == 0) {
                val midIndex1 = (a / 2) - 1
                val midIndex2 = a / 2
                (sortedValues(midIndex1) + sortedValues(midIndex2)) / 2
              } else {
                sortedValues(a / 2)
              }
            }
            def modeCalculation(values: List[Double]): Any= {
              val allValues = values.groupBy(identity)
              val maxFrequency = allValues.map(_._2.size).max
              val modes = allValues.filter(_._2.size == maxFrequency).keys.toList
              if (modes.size == 1) modes.head else modes
            }

            def rangeCalculation(values: List[Double]): Double = {
              values.max - values.min
            }

            def midrangeCalculation(values: List[Double]): Double = {
              (values.min + values.max) / 2
            }

            def dataAnalysis(values: List[Double]): Unit = {
              println(s"Mean: ${meanCalculation(values)}")
              println(s"Median: ${medianCalculation(values)}")
              println(s"Mode: ${modeCalculation(values)}")
              println(s"Range: ${rangeCalculation(values)}")
              println(s"Midrange: ${midrangeCalculation(values)}")
            }

            val values = sampleData.map(_.value)

            dataAnalysis(values)
          }

        case "5" =>
          println("Checking for issues with energy outputs and equipment...")
          val sampleData = readData("SolarEnergy.txt")
          detectAndHandleIssues(sampleData)
        case "6" =>
          println("Program Ends!")
          exit = true
        case _ =>
          println("Invalid choice. Please enter a number from 1 to 6.")
      }
    }
  }

  // Starting the menu
  menu()

}
