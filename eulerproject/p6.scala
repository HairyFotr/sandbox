/*The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.*/
/*object p6 {
    def main(args:Array[String]) =
        println(math.pow((1 to 100).reduceLeft(_+_),2).toInt-(1 to 100).reduceLeft(_ + math.pow(_, 2).toInt))
}*/

object p6 extends App {
    println(math.pow((1 to 100).reduce(_+_),2).toInt-(1 to 100).reduce(_ + math.pow(_, 2).toInt))
}
