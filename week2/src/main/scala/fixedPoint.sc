import math.abs
def tolerance = 0.001


def isCloseEnough(x: Double, y: Double) =
  abs(x - y)/x < tolerance*x


def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def loop(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else loop(next)
  }
  loop(firstGuess)
}

fixedPoint(x => 1+ x/2)(1)

//def sqrt (x: Double) = fixedPoint(y => (y + x/y)/2)(1)

def averageDamp (f: Double => Double) (x: Double) = (x + f(x))/2


def sqrt(x: Double): Double = fixedPoint(averageDamp(y => x/y))(1)

sqrt(2)