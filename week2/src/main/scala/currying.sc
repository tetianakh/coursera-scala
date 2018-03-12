def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)


product(x => x * x)(1, 3)



def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * sum(f)(a + 1, b)


def gen(f: Int => Int, combine: (Int, Int) => Int, base: Int)(a: Int, b: Int): Int =
  if (a > b) base
  else combine(f(a), gen(f, combine, base)(a + 1, b))

def prod(f: Int => Int)(a: Int, b: Int) =
  gen(f, (x, y) => x * y, 1)(a, b)


def fact(n: Int) = prod(x => x)(1, n)

fact(5)


