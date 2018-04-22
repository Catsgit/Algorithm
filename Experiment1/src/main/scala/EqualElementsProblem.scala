import java.io.{File, PrintWriter}
import javafx.print.Printer

import breeze.linalg._
import breeze.plot._

import scala.io.Source
import scala.util.Random
import scala.util.control.Breaks._

class EqualElementsProblem {

  val data: Array[Array[Int]] = new Array[Array[Int]](11);
  val result = new Array[Int](11);
  val time = new Array[Double](11);
  val size = Array[Int](100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000, 10000000);
  def generateData(): Unit = {
    for (i <- 0 until size.length) {
      data(i) = Seq.fill(size(i))(Random.nextInt(10000000)).toArray
    }

  }

  def method1(): Unit = {
    for (i <- 0 until result.length) {
      val start = System.nanoTime()
      val m = data(i).max
      val validateArray = new Array[Int](m+1)
      result(i) = 0
      breakable(
        for(j <- 0 until data(i).length) {
          if(validateArray(data(i)(j)) != 0) {
            result(i) = 1
            break()
          } else {
            validateArray(data(i)(j)) = 1
          }
        }
      )
      val end = System.nanoTime()
      time(i) = (end-start)/(1000000000*1.0)
    }
  }

  def method2(): Unit = {
    for (i <- 0 until result.length) {
      val start = System.nanoTime()
      breakable(
        for (j <- 0 until data(i).length) {
          val temp = data(i)(j)
          for (h <- j+1 until data(i).length) {
            if (temp == data(i)(h)) {
              result(i) = 1
              break()
            }
          }
        }
      )
      val end = System.nanoTime()
      time(i) = (end-start)/(1000000000*1.0)
    }
  }

  def method3(): Unit = {
    for (i <- 0 until result.length) {
      val start = System.nanoTime()
      scala.util.Sorting.quickSort(data(i))

      breakable(
        for (j <- 0 until data(i).length-1) {
          if (data(i)(j) == data(i)(j+1)) {
            result(i) = 1
            break()
          }
        }
      )
      val end = System.nanoTime()
      time(i) = (end-start)/(1000000000*1.0)
    }
  }
}

object EqualElementsProblem {
  //此外 还有逐个元素确认的方法 以及排序后检查的方法
  def main(args: Array[String]): Unit = {

    val e = new EqualElementsProblem
    val f = Figure()
    val p = f.subplot(0)
    val x = new DenseVector[Double](11)
    val y = new DenseVector[Double](11)

    e.generateData()
    e.method1()
    println("方法一结果: ")
    for (i <- 0 until e.result.length) {
      if (e.result(i) == 0) {
        println("No")
      } else {
        println("Yes")
      }
    }

    val writer = new PrintWriter(new File("/home/cat/Files/Algorithm/result/Experiment1/time.dat"))
    for (i <- 0 until e.time.length) {
      writer.println(e.size(i) + " " + e.time(i))
    }
    writer.println("================================================================")

    for (i <- 0 until 11) {
      x(i) = e.size(i)
    }
    for (i <- 0 until 11) {
      y(i) = e.time(i)
    }
    println(y)
    p += plot(x, y)
//
    e.method2()
    println("方法二结果: ")
    for (i <- 0 until e.result.length) {
      if (e.result(i) == 0) {
        println("No")
      } else {
        println("Yes")
      }
    }

    for (i <- 0 until e.time.length) {
      writer.println(e.size(i) + " " + e.time(i))
    }
    writer.println("================================================================")

    for(i <- 0 until 11) {
      y(i) = e.time(i)
    }
    println(y)
    p += plot(x, y, '.')

    e.method3()
    println("方法三结果: ")
    for (i <- 0 until e.result.length) {
      if (e.result(i) == 0) {
        println("No")
      } else {
        println("Yes")
      }
    }
    for (i <- 0 until e.time.length) {
      writer.println(e.size(i) + " " + e.time(i))
    }
    writer.println("================================================================")
    writer.close()

    for(i <- 0 until 11) {
      y(i) = e.time(i)
    }
    println(y)
    p += plot(x, y)
    p.xlabel = "data-size"
    p.ylabel = "time"
    f.saveas("/home/cat/Files/Algorithm/result/result.png")
  }
}
