object p25 extends App {
    val cache = new scala.collection.mutable.HashMap[Int,BigInt] ++ Map(1->BigInt(1), 2->BigInt(1))
    def fib(n:Int):BigInt = cache.getOrElseUpdate(n, fib(n-1)+fib(n-2))
    var n = 1; while(fib({n+=1; n}).toString.length < 1000){};
    println(n)
}
