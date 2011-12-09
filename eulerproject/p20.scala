object p20 {
    def main(args:Array[String]) = 
        println((1 to 100).map(BigInt.apply(_)).reduceLeft(_*_).toString.toCharArray.map(_.toInt).reduceLeft(_+_));
}
