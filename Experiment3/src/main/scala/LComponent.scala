import breeze.linalg.DenseMatrix

class LComponent {
  var data: DenseMatrix[Int] = null
  var index = -1

  def init(n: Int): Unit = {
    data = DenseMatrix.ones[Int](n, n)
    data *= -1
  }

  def fill(rowStart: Int, rowEnd: Int, colStart: Int, colEnd: Int, isEmpty: Boolean, quadrant: Int): Unit = {
    index += 1
    val width = rowEnd - rowStart + 1
    if (width == 2) {
      if (isEmpty == true) {
        if (quadrant == 1) {
          if (data(rowStart, colStart) == -1) {
            data(rowStart, colStart) = -2
          }
        } else if (quadrant == 2) {
          if (data(rowStart, colEnd) == -1) {
            data(rowStart, colEnd) = -2
          }
        } else if (quadrant == 3) {
          if (data(rowEnd, colStart) == -1) {
            data(rowEnd, colStart) = -2
          }
        } else {
          if (data(rowEnd, colEnd) == -1) {
            data(rowEnd, colEnd) = -2
          }
        }
      }
      for (i <- rowStart to rowEnd) {
        for (j <- colStart to colEnd) {
          if (data(i, j) == -1) {
            data(i, j) = index
          }
        }
      }
      return
    } else {
      val row = width/2 + rowStart - 1
      val col = width/2 + colStart - 1
      // 若该子棋盘的边角被填充
      if (!isEmpty) {
        if (quadrant == 1) {
          //第一象限继续分割, 并且分割的子棋盘的1,2,3象限填充数字, 而第四象限为空
          data(row, col) = index
          data(row, col+1) = index
          data(row+1, col) = index
          fill(rowStart, row, colStart, col, false, 1)
          fill(rowStart, row, col+1, colEnd, false, 2)
          fill(row+1, rowEnd, colStart, col, false, 3)
          fill(row+1, rowEnd, col+1, colEnd, true, 4)
        } else if (quadrant == 2) {
          data(row, col) = index
          data(row, col+1) = index
          data(row+1, col+1) = index
          fill(rowStart, row, colStart, col, false, 1)
          fill(rowStart, row, col+1, colEnd, false, 2)
          fill(row+1, rowEnd, colStart, col, true, 3)
          fill(row+1, rowEnd, col+1, colEnd, false, 4)
        } else if (quadrant == 3) {
          data(row, col) = index
          data(row+1, col) = index
          data(row+1, col+1) = index
          fill(rowStart, row, colStart, col, false, 1)
          fill(rowStart, row, col+1, colEnd, true, 2)
          fill(row+1, rowEnd, colStart, col, false, 3)
          fill(row+1, rowEnd, col+1, colEnd, false, 4)
        } else {
          data(row, col+1) = index
          data(row+1, col) = index
          data(row+1, col+1) = index
          fill(rowStart, row, colStart, col, true, 1)
          fill(rowStart, row, col+1, colEnd, false, 2)
          fill(row+1, rowEnd, colStart, col, false, 3)
          fill(row+1, rowEnd, col+1, colEnd, false, 4)
        }
      } else {
        //若该子棋盘的边角没有被填充
        if (quadrant == 1) {
          //第一象限继续分割, 并且分割的子棋盘的2,3,4象限填充数字, 而第一象限为空
          data(row, col+1) = index
          data(row+1, col) = index
          data(row+1, col+1) = index
          fill(rowStart, row, colStart, col, true, 1)
          fill(rowStart, row, col+1, colEnd, false, 2)
          fill(row+1, rowEnd, colStart, col, false, 3)
          fill(row+1, rowEnd, col+1, colEnd, false, 4)
        } else if (quadrant == 2) {
          data(row, col) = index
          data(row+1, col) = index
          data(row+1, col+1) = index
          fill(rowStart, row, colStart, col, false, 1)
          fill(rowStart, row, col+1, colEnd, true, 2)
          fill(row+1, rowEnd, colStart, col, false, 3)
          fill(row+1, rowEnd, col+1, colEnd, false, 4)
        } else if (quadrant == 3) {
          data(row, col) = index
          data(row, col+1) = index
          data(row+1, col+1) = index
          fill(rowStart, row, colStart, col, false, 1)
          fill(rowStart, row, col+1, colEnd, false, 2)
          fill(row+1, rowEnd, colStart, col, true, 3)
          fill(row+1, rowEnd, col+1, colEnd, false, 4)
        } else {
          data(row, col) = index
          data(row, col+1) = index
          data(row+1, col) = index
          fill(rowStart, row, colStart, col, false, 1)
          fill(rowStart, row, col+1, colEnd, false, 2)
          fill(row+1, rowEnd, colStart, col, false, 3)
          fill(row+1, rowEnd, col+1, colEnd, true, 4)
        }
      }
    }
  }
}

object LComponent {
  def main(args: Array[String]): Unit = {
    val l = new LComponent
    val width = 8
    l.init(width)
    l.fill(0, width-1, 0, width-1, true, 1)
    for (i <- 0 until width) {
      for (j <- 0 until width) {
        if (l.data(i, j) == -2) {
          print("A\t")
        } else {
          print(l.data(i, j) + "\t")
        }
      }
      println()
    }
  }
}

