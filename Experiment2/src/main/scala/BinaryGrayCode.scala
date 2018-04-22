import scala.io.Source
import scala.Array._

class BinaryGrayCode {
  var result: Array[Array[Byte]] = null;
  def init(num: Int): Unit = {
    result = new Array[Array[Byte]](Math.pow(2, num).toInt)
    for (i <- 0 until result.length) {
      result(i) = new Array[Byte](num)
    }
  }

  def generateGrayCode(arr: Array[Array[Byte]], index: Int): Unit = {
    var beforeValue = 0
    var laterValue = 1
    var oneNum = 0
    if (index > 0) {
      for (i <- 0 until index) {
        if (arr(0)(i) == 1) {
          oneNum += 1
        }
      }
    }
    if (oneNum % 2 == 1) {

      beforeValue = 1
      laterValue = 0
    }
    for (i <- 0 until arr.length) {
      if (i < arr.length/2) {
        arr(i)(index) = beforeValue.toByte
      } else {
        arr(i)(index) = laterValue.toByte
      }
    }
    if (arr.length == 2) {
      return
    } else {
      val half = new Array[Array[Byte]](arr.length/2)
      val otherHalf = new Array[Array[Byte]](arr.length/2)
      copy(arr, 0, half, 0, arr.length/2)
      copy(arr, arr.length/2, otherHalf, 0, arr.length/2)
      generateGrayCode(half, index + 1)
      generateGrayCode(otherHalf, index + 1)
    }
  }
}

object BinaryGrayCode {
  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("/home/cat/Files/Algorithm/Experiment2/resources/input.txt")
    val b = new BinaryGrayCode
    var flag = false
    var time = 0
    for (line <- file.getLines()) {
      if (flag == false) {
        time = line.toInt
        flag = true
      } else {
          b.init(line.toInt)
          b.generateGrayCode(b.result, 0)
          println("=========================================")
          for (i <- 0 until b.result.length) {
            for (j <- 0 until b.result(i).length) {
              print(b.result(i)(j) + " ")
            }
            println()
          }
      }
    }
  }
}
