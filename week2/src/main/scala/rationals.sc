class Rational (m: Int, n: Int) {
  def this(x: Int) = this(x, 1)

  require (n != 0, "denominator must be non-zero")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
  private val g = gcd (m, n)

  def numer = m / g
  def denom = n / g

  def + (other: Rational): Rational = {
    new Rational (
      numer * other.denom + other.numer * denom,
      denom * other.denom
    )
  }

  def unary_- : Rational = new Rational(-numer, denom)

  def - (other: Rational): Rational = this + (-other)

  def < (other: Rational): Boolean =
    this.numer * other.denom < other.numer * this.denom

  def max(other: Rational): Rational =
    if (this < other) other else this

  override def toString: String = numer + "/" + denom
}

val x = new Rational(1,2) + new Rational(1,4)
-x

x - new Rational(1, 4)

//val y = new Rational(1, 0)

new Rational(3) < x