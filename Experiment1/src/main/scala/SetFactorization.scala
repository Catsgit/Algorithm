import java.io.{File, PrintWriter}

import breeze.linalg.roll

import scala.util.{Random, Sorting}
import scala.Array._

class SetFactorization {
  val data: Array[Array[Int]] = new Array[Array[Int]](11);
  val time = new Array[Double](11);
  val size = Array[Int](100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000);

  def generateData(): Unit = {
    for (i <- 0 until size.length) {
      data(i) = Seq.fill(size(i))(Random.nextInt(10000000)).toArray
    }
  }

  def method1(): Unit = {
    for (i <- 0 until data.length) {
      val start = System.nanoTime()
      val smaller = new Array[Int](data(i).size/2)
      val bigger = new Array[Int](data(i).size/2)

      Sorting.quickSort(data(i))
      copy(data(i), 0, smaller, 0, data(i).size/2)
      copy(data(i), data(i).size/2, bigger, 0, data(i).size/2)
      val end = System.nanoTime()
      println(i)
      time(i) = (end - start) / (1000000000*1.0)
    }
  }

  def method3(): Unit = {
    for (i <- 0 until data.length - 2) {
      val start = System.nanoTime()
      val middleValue = findKthNumber(data(i).clone(), data(i).size/2, 0, data(i).length-1)
      val smaller = new Array[Int](data(i).size/2)
      var smallerNum = 0
      val bigger = new Array[Int](data(i).size/2)
      var biggerNum = 0
      for (j <- 0 until data(i).length) {
        if (data(i)(j) < middleValue) {
          smaller(smallerNum) = data(i)(j)
          smallerNum += 1
        } else if (data(i)(j) > middleValue) {
          bigger(biggerNum) = data(i)(j)
          biggerNum += 1
        } else {
          if (smallerNum < data(i).size/2) {
            smaller(smallerNum) = data(i)(j)
            smallerNum += 1
          } else if (biggerNum < data(i).size/2) {
            bigger(biggerNum) = data(i)(j)
            biggerNum += 1
          }
        }
      }
      val end = System.nanoTime()
      println(i)
      time(i) = (end - start) / (1000000000*1.0)
    }
  }

  def printlnArr(arr: Array[Int]): Unit = {
    for (i <- 0 until arr.length) {
      print(arr(i) + " ")
    }
    println()
  }
  def findKthNumber(arr: Array[Int], kth: Int, s: Int, e: Int): Int = {
    var start = s
    var value = arr(start)
    var end = e
    var k = kth
    var j = start
    var be = 0
    var flag = false
    //将比value小的值都移到value的左边
    do {
      if (flag == true) {
        if (be < k-1) {
          start = j+1
          k = k-be-1
          value = arr(start)
          j = start
          be = 0
        } else if (be > k-1) {
          end = j-1
          value = arr(start)
          be = 0
        }
      }

      for (i <- start+1 to end) {
        if (arr(i) < value) {
          j += 1

          //交换元素
          val temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp

          be += 1
        }
      }
      val temp = arr(start)
      arr(start) = arr(j)
      arr(j) = temp

      flag = true
    } while (be != k-1)
    return value
  }

  def method2(): Unit = {
    for (i <- 0 until data.length) {
      val start = System.nanoTime()
      val smaller = new Array[Int](data(i).size/2)
      val bigger = new Array[Int](data(i).size/2)

      copy(data(i), 0, smaller, 0, data(i).size/2)
      copy(data(i), data(i).size/2, bigger, 0, data(i).size/2)

      Sorting.quickSort(smaller)
      Sorting.quickSort(bigger)

      if (smaller(smaller.length-1) > bigger(bigger.length-1)) {
        val temp = new Array[Int](data(i).size/2)
        copy(smaller, 0, temp, 0, smaller.length)
        copy(bigger, 0, smaller, 0, smaller.length)
        copy(temp, 0, bigger, 0, smaller.length)
      }

      var smallerBigest = smaller.length-1
      var biggerSmallest = 0

      while (smaller(smallerBigest) > bigger(biggerSmallest)) {
        val temp = smaller(smallerBigest)
        smaller(smallerBigest) = bigger(biggerSmallest)
        bigger(biggerSmallest) = temp
        smallerBigest -= 1
        biggerSmallest += 1
      }

      val end = System.nanoTime()
      println(i)
      time(i) = (end - start) / (1000000000*1.0)
    }
  }
}

object  SetFactorization {
  def main(args: Array[String]): Unit = {
    val s = new SetFactorization
    val writer = new PrintWriter(new File("/home/cat/Files/Algorithm/result/Experiment1/time.dat"))
    s.generateData()
    s.method1()
    for (i <- 0 until s.time.length) {
      writer.println(s.size(i) + " " + s.time(i))
    }
    writer.println("================================================================")
    s.method2()
    for (i <- 0 until s.time.length) {
      writer.println(s.size(i) + " " + s.time(i))
    }
    writer.println("================================================================")
    s.method3()
    for (i <- 0 until s.time.length - 2) {
      writer.println(s.size(i) + " " + s.time(i))
    }
    writer.println("================================================================")
    writer.close()
  }
}
