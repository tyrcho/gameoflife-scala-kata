package gameoflife

import java.io.InputStream

case class GameOfLife(rows: Vector[String]) {
  val width = rows.head.size
  val height = rows.size

  def neighbours(i: Int, max: Int): Seq[Int] = {
    if (i == 0) Seq(max - 1, 0, 1)
    else if (i == max - 1) Seq(max - 2, max - 1, 0)
    else i - 1 to i + 1

  }

  def liveNeighbours(row: Int, col: Int): Int = {
    for {
      r <- neighbours(row, height) // 0 until height
      line = rows(r)
      w <- neighbours(col, width) //0 until width
      if r != row || w != col
    } yield if (line(w) == 'X') 1 else 0
  }.sum

  def nextStep: GameOfLife = GameOfLife {
    for {
      i <- (0 until height).toVector
    } yield {
      for {
        j <- 0 until width
      } yield {
        if ((liveNeighbours(i, j) == 2 && rows(i)(j) == 'X') || liveNeighbours(i, j) == 3) 'X'
        else '.'
      }

    }.mkString
  }

  def print() = {
    rows.foreach(println)
    println()
  }
}

object GameOfLife {
  def load(resource: String): GameOfLife = {
    val stream = GameOfLife.getClass.getResourceAsStream(resource)
    assert(stream != null, "resource not found")
    load(stream)
  }
  def load(stream: InputStream): GameOfLife = {
    val lines = io.Source.fromInputStream(stream).getLines.toVector
    GameOfLife(lines)
  }
}