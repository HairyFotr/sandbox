object p48 {
    def main(args:Array[String]) = 
        println((1 to 1000).map(BigInt.apply(_)).map((a:BigInt)=>a.modPow(a,10000000000L)).reduceLeft(_+_)%10000000000L);
}
