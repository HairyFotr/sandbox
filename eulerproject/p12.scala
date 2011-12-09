/*object p12 {
    
    def main(args:Array[String]) {
		var (i, best) = (1, 0);
		do {
            val n = (i+1)*(i/2);
            if(n%2==n%3) {
                val kk = 2 + 3 + (5 to n/2).foldLeft(0)((a:Int, b:Int) => a + (if(n%b==0) 1 else 0));
                
                if(kk>best) {
                    best=kk
                    println(best+" "+n)
                    if(best>500) {
                        i=0;
                    }
                }
            }
            i+=1;
        } while(i>0)
    }
    
}*/
object p12 extends App {
    var best = 0
    var i = 1;
    val p = new scala.collection.mutable.ListBuffer[Int]
    var pmax = 0
    var maxp = 0;
    def nextPrimes() = {
        (pmax until pmax+10000).par.foreach(a=>if(java.math.BigInteger.valueOf(a).isProbablePrime(5)) p += a)
        maxp = p.max
        pmax+=10000
    }
    def getPrime(n:Int) = {
        while(n>=p.size) nextPrimes()
        p(n)
    }
    def isPrime(n:Int):Boolean = {
        while(n>=maxp) nextPrimes()
        p.contains(n)
    }
    val ps = new scala.collection.mutable.ListBuffer[Int]
    while(true) {
        var n = (i+1)*(i/2)
        val nn=n;
        
        var ps = 1
        while(n>=2) {
            for(a <- p.takeWhile(_<=math.sqrt(n)).reverse)
                var c=0
                while(n%a==0) { c+=1; n/=a }
                ps *= c+1
            }
        }
        
        if(ps>best) {
            best=ps
            println(best+" "+nn+" "+i)
            if(best>500) { println(nn); sys.exit }
        }
        i+=1
    }
}