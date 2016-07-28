package gameoflife

object Main extends App {
  var initial = GameOfLife.load("/acorn.txt").copy(width = 100, height = 50)
  var round = 1
  while (true) {
    initial.print()
    initial = initial.nextStep
    println(round)
    round += 1
    Thread.sleep(100)
  }
}