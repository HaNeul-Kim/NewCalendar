import org.scalatest.{FunSuite, Matchers}

/**
  * 새로운 달력 테스트.
  *
  * @author Ha Neul, Kim
  * @since 0.1
  */
class NewCalendarTest extends FunSuite with Matchers {

  /*test("sample #1") {
    assert(solve(3, 11, 4) === 11)
  }

  test("sample #2") {
    assert(solve(12, 28, 7) === 48)
  }

  test("sample #3") {
    assert(solve(10, 35, 10) === 40)
  }

  test("sample #4") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "12 28 7".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 48)
  }

  test("sample #5") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "17 7 67".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 18)
  }

  test("sample #6") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "1 1 73".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 1)
  }

  test("sample #7") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "11 1 1".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 11)
  }

  test("sample #8") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "1 8 4".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 2)
  }

  test("sample #9") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "6 5 45".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 6)
  }

  test("sample #10") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "19 1 69".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 19)
  }

  test("sample #11") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "1 6 25".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 1)
  }

  test("sample #12") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "13 1 80".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 13)
  }

  test("sample #13") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "11 7 15".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 16)
  }

  test("sample #14") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "5 7 15".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 7)
  }

  test("sample #15") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "8 5 35".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 8)
  }

  test("sample #16") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "17 9 45".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 17)
  }

  test("sample #17") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "1000000000000 1000000 1".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === (1000000000000L * 1000000))
  }*/

  test("sample #18") {
    val Array(totalMonths, dayPerMonth, dayPerWeek) = "786674420701 308049 31".split(" ").map(_.toLong)
    assert(solve(totalMonths, dayPerMonth.toInt, dayPerWeek.toInt) === 7817995769520754L)
    //                                                                 7817234471704208
    //                                                                 7817234471694271
  }

  def solve(totalMonths: Long, dayPerMonth: Int, dayPerWeek: Int): Long = {
    /*
    // small
    if (dayPerMonth > dayPerWeek) {
      val plusLine = dayPerMonth % dayPerWeek
      totalMonths * dayPerMonth / dayPerWeek + plusLine
    } else if (dayPerMonth == dayPerWeek) {
      totalMonths
    } else {
      val plusLine = if (dayPerWeek % dayPerMonth == 0) 0 else totalMonths * dayPerMonth / dayPerWeek
      totalMonths + plusLine
    }
    */

    // large
    //0
    def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

    val nset = dayPerWeek / gcd(dayPerMonth, dayPerWeek)

    val lines = for (i <- 0L until nset) yield {
      val n = ((dayPerMonth % dayPerWeek) * i) % dayPerWeek + dayPerMonth

      if (n % dayPerWeek == 0) n / dayPerWeek else n / dayPerWeek + 1
    }
    (totalMonths / nset) * lines.sum + lines.take((totalMonths % nset).toInt).sum
  }
}
