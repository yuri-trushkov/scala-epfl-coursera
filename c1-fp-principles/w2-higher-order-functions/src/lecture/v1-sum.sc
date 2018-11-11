object exercise {

  def sum0(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum0(f, a + 1, b)
  }
  sum0(x => x, 3, 5)
  sum0(x => x * x * x, 3, 5)



  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }
  sum(x => x)(3, 5)
  sum(x => x * x * x)(3, 5)

  def sumInts(a: Int, b: Int) =
    sum(x => x)(a, b)
  def sumCubes(a: Int, b: Int) =
    sum(x => x * x * x)(a, b)

  sumInts(3, 5)
  sumCubes(3, 5)
