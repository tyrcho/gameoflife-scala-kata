package gameoflife

import java.io.InputStream

case class GameOfLife(width: Int, height: Int, data: Set[(Int, Int)]) {

  def liveNeighbours(row: Int, col: Int): Int = {
    for {
      r <- neighbours(row, height)
      w <- neighbours(col, width)
      if r != row || w != col
    } yield if (isLive(w, r)) 1 else 0
  }.sum

  def nextStep: GameOfLife = copy(data = {
    for {
      i <- 0 until height
      j <- 0 until width
      ln = liveNeighbours(i, j)
      if (ln == 2 && isLive(j, i)) || ln == 3
    } yield (j, i)
  }.toSet)

  def rows: Vector[String] = for {
    i <- (0 until height).toVector
  } yield {
    for {
      j <- 0 until width
    } yield if (isLive(j, i)) 'X' else '.'
  }.mkString

  def print() = {
    rows.foreach(println)
    println()
  }

  private def isLive(x: Int, y: Int) = data.contains((x, y))

  private def neighbours(i: Int, max: Int): Seq[Int] = {
    if (i == 0) Seq(max - 1, 0, 1)
    else if (i == max - 1) Seq(max - 2, max - 1, 0)
    else i - 1 to i + 1
  }
}

object GameOfLife {
  def apply(rows: Vector[String]): GameOfLife = {
    val width = rows.head.size
    val height = rows.size
    val data = for {
      x <- 0 until width
      y <- 0 until height
      if rows(y)(x) == 'X'
    } yield (x, y)
    GameOfLife(width, height, data.toSet)
  }

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