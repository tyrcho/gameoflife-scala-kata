package gameoflife

object Main extends App {
  var initial = GameOfLife.load("/acorn.txt")
  while (true) {
    initial.print()
    initial = initial.nextStep
    readLine()
  }
}