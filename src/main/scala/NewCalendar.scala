import java.io.{File, FileOutputStream}

import scala.io.Source

/**
  * 새로운 달력.
  *
  * @author Ha Neul, Kim
  * @since 0.1
  */
object NewCalendar extends App {

  val fileSize = "small"

  val lines = Source.fromInputStream(getClass.getResourceAsStream("NewCalendar_" + fileSize + ".in")).getLines()
  val outputFile = new File("src/main/resources/NewCalendar_" + fileSize + ".out")
  if (!outputFile.exists()) outputFile.createNewFile()
  val fos = new FileOutputStream(outputFile)

  for (i <- 1 to lines.next().toInt) {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = lines.next().split(" ").map(_.toLong)
    lineOut(s"Case #$i: ${solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt)}")
  }

  def lineOut(out: String): Unit = {
    print(out + "\n")
    //fos.write((out + "\n").getBytes())
  }

  def solve(totalMonths: Long, dayPerMonth: Int, dayPerWeek: Int): Long = {
    if (dayPerMonth > dayPerWeek) {
      val plusLine = dayPerMonth % dayPerWeek
      totalMonths * dayPerMonth / dayPerWeek + plusLine
    } else if (dayPerMonth == dayPerWeek) {
      totalMonths
    } else {
      val plusLine = if (dayPerWeek % dayPerMonth == 0) 0 else totalMonths * dayPerMonth / dayPerWeek
      totalMonths + plusLine
    }
  }

  fos.close()
}

