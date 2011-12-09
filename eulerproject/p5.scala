/*2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?*/
/*object p5 {
    def main(args:Array[String]) {
        var best=0
        var n=20L
        while(best<20) {
            var nb=2
            var last=0;
            for(i <- 19 until 1 by -1) if(n%i==0) nb+=1 else last=i
            
            if(nb>best){
                best = nb
                println(n+" "+best);
            }
            n*=last;
        }
        println(n)
    }
    
}//*/
/*
object p5 extends App {
    var best=0
    var n=20L
    while(best<20) {
        val d = (1 to 20).flatMap(if(n%_==0) List(1) else Nil ).length
        
        if(d > best) {
            best = d
            println(n+" "+best);
        }
        n *= d
    }
    println((n/2))
    //232792560
    //698377680
}//*/