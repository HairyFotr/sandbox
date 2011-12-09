/*object p1 {
   def main(args:Array[String]) = println((1 until 1000).filter(x => x%3==0||x%5==0).reduceLeft(_+_));
}*/

object p1 extends App {
    println((1 until 1000).filter(x => x%3==0||x%5==0).reduce(_+_))
    println((1 until 1000).filter(x => List(3,5).exists(x%_==0)).reduce(_+_))
}
