package recfun
import scala.collection.mutable.Stack

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("----------------------------- Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    println("----------------------------- balance 1")
    balance("(just an) example".toList)
    println("----------------------------- balance 2")
    balance("())(".toList)
    println("----------------------------- countChange")
    println(countChange(4, List(1, 2)))

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    //        1                00
    //       1 1             01  11
    //      1 2 1          02  12  22
    //     1 3 3 1       03  13  23  33
    //    1 4 6 4 1    04  14  24  34  44
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val par = Stack[Char]()
    def parentheses(chars: List[Char], par: Stack[Char]): Boolean = {
      println("par", par, "          chars", chars)
      if (chars.isEmpty) true
      else {
        if (chars.head == '(')
          parentheses(chars.tail, par.push('('))
        else if (chars.head == ')') {
          if (par.length > 0 && par.last == '(') {
            par.pop()
            parentheses(chars.tail, par)
          }
          else
            false
        }
        else
          parentheses(chars.tail, par)
      }
    }
    parentheses(chars, par)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
//                          4,(1,2)
//          4,(2)              +                      3,(1,2)
//    4,()   +     2,(2)               3,(2)              +              2,(1,2)
//     0       2,() + 0,(2)         3,() +    1,(2)            2,(2)       +             1,(1,2)
//     0        0      1             0    1,() + -1,(2)     2,() + 0,(2)         1,(2)      +   0,(1,2)
//     0        0      1             0     0       0         0      1        1,(), + -1,(2)        1
//     0        0      1             0     0       0         0      1         0        0           1
    println("money", money, "          coins", coins)
    if (money == 0) {
      println(" -> 1")
      1
    }
    else if (money < 0) {
      println(" -> 0")
      0
    }
    else if (money > 0 && coins.isEmpty) {
      println(" -> 0")
      0
    }
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
