type Set = Int => Boolean
def contains(s: Set, elem: Int): Boolean = s(elem)

def singletonSet(elem: Int): Set = x => x == elem


def s = singletonSet(1)
contains(s, 1)
contains(s, 2)

val bound = 1000

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (!p(a) && contains(s, a)) false
    else iter(a + 1)
  }

  iter(-bound)
}
/**
  * Returns whether there exists a bounded integer within `s`
  * that satisfies `p`.
  */
def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, elem => !p(elem))

def union(s: Set, t: Set): Set = elem => contains(s, elem) || contains(t, elem)

val s1 = union(s , singletonSet(2))

/** write a function map which transforms a given set into another one
  * by applying to each of its elements the given function.
  */
def map(s: Set, f: Int => Int): Set = elem =>
  exists( s, x => contains(s, x) && f(x) == elem )

contains(map(s1, x => x*x), 4)
def s2 = map(s1, x => x*x)

contains(s2, 4)
