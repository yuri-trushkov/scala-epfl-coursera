import com.sun.imageio.spi.RAFImageOutputStreamSpi

object rationals {

  class Rational(x: Int, y: Int) {
    require(y != 0, "denom must be non zero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)
    def numer = x / g
    def denom = y / g

    def < (that: Rational) =
      numer * that.denom < that.numer * denom

    def max(that: Rational) =
      if (this < that) that
      else this

    def + (that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    override def toString: String = numer + "/" + denom

    def unary_- : Rational = new Rational(-numer, denom)
    def - (that: Rational): Rational = this + -that

  }


  val x1 = new Rational(1, 2)
  x1.numer
  x1.denom
  val y1 = new Rational(2, 3)
  println(x1 + y1)

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x - y - z
  y + y
  x < y
  x.max(y)

//  val strange = new Rational(1, 0)
//  strange.add(strange)

  new Rational(2)
}

