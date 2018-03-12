def countChange(money: Int, coins: List[Int]): Int = {
  if (money < 0) 0
  else if (money == 0) 1
  else if (coins.isEmpty) 0
  else countChange(money - coins.head, coins) +
       countChange(money, coins.tail)

}


countChange(4, List(1,2)) == 3

countChange(25, List(5)) == 1

countChange(15, List(5, 10)) == 2


countChange(5, List(1,2,3))



def fact(n: Int): Int = {
  def loop(acc:Int, n: Int): Int =
    if (n == 0) acc
    else loop(n*acc, n-1)

  loop(1, n)
}

fact(5)