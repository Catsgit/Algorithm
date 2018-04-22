import scala.util.Random

class KthSmallElement {
  var data: Array[Int] = null

  def init(n: Int): Unit = {
    data = new Array[Int](n)

    for (i <- 0 until n) {
      data(i) = Random.nextInt(10*n)
    }
  }

  def findElement(k: Int, start: Int, end: Int, data: Array[Int]): Int = {
    if (k == 1) {
      if ((end-start+1)==1) {
        return data(start)
      }
    }
    val value = data(start)
    val temp = new Array[Int](end - start + 1)
    var index = 0
    var smallerIndex = 0
    var equalIndex = 0
    for (i <- start to end) {
      if (data(i) < value) {
        temp(index) = data(i)
        index += 1
      }
    }
    smallerIndex = index - 1
    for (i <- start to end) {
      if (data(i) == value) {
        temp(index) = data(i)
        index += 1
      }
    }
    equalIndex = index - 1
    for (i <- start to end) {
      if (data(i) > value) {
        temp(index) = data(i)
        index += 1
      }
    }

    if (smallerIndex + 1 >= k) {
      findElement(k, 0, smallerIndex, temp)
    } else if (equalIndex + 1 >= k) {
//      findElement(k-(smallerIndex+1), smallerIndex+1, equalIndex, temp)
      return temp(equalIndex)
    } else {
      findElement(k-(equalIndex+1), equalIndex+1, temp.length-1, temp)
    }
  }
}

object KthSmallElement {
  def main(args: Array[String]): Unit = {
    val k = new KthSmallElement
    k.init(10)
    for (i <- 0 until 10) {
      print(k.data(i) + " ")
    }
    println()
    for (i <- 1 to 10) {
      val temp = k.data.clone()
      println(i + "小元素 " + k.findElement(i, 0, 9, temp))
//        k.findElement(i, 0, 9, temp)
    }
  }
}
