
package gameoflife

import org.junit.runner.RunWith
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GameOfLifeSuite extends FlatSpec with Matchers {

  "a cell" should "count its live neighbours" in {
    val game = GameOfLife(Vector(
      "...",
      ".XX",
      "..."))

    game.liveNeighbours(1, 1) shouldBe 1
    game.liveNeighbours(0, 1) shouldBe 2

  }

  it should "count neighbours in a larger grid" in {
    val game = GameOfLife(Vector(
      "....",
      ".XX.",
      ".XX.",
      ".XXX"))

    game.liveNeighbours(1, 1) shouldBe 3

  }

  "Game" should "produce next step when all die" in {
    val game = GameOfLife(Vector(
      "...",
      ".XX",
      "...")).nextStep

    game.rows shouldBe Vector.fill(3)("...")

  }

  it should "produce next step when some live" in {
    val game = GameOfLife(Vector(
      ".....",
      ".....",
      ".XXX.",
      ".....",
      ".....")).nextStep

    game.rows shouldBe Vector(
      ".....",
      "..X..",
      "..X..",
      "..X..",
      ".....")

  }

  "GameOfLife" should "load an inputstream" in {
    GameOfLife.load("/resource.csv").rows shouldBe Vector(
      ".....",
      "..X..",
      "..X..",
      "..X..",
      ".....")
  }

}
