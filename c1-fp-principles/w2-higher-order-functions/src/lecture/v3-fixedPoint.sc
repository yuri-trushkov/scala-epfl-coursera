import math.abs


object exercise {
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x)  < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println("guess = " + guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
//  does not converge
//  fixedPoint(x => 1 + x / 2)(1)
//  def sqrt(x: Double) = fixedPoint(y => x / y)(1)
//  sqrt(2)


  fixedPoint(x => 1 + x / 2)(1)

  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)
  sqrt(2)

  def averageDump(f: Double => Double)(x: Double) =
    (x + f(x)) / 2
  def sqrt2(x: Double) =
    fixedPoint(averageDump(y => x / y))(1)
  sqrt2(2)

}