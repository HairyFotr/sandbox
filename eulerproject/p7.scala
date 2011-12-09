/*object p7 {
    
    var cache = new collection.mutable.HashSet[Long];
    def isPrime(p:Long):Boolean = {
        if((p==2)||(p==3)||(p==5)||(p==7)) return true;
        if((p%2==0)||(p%3==0)||(p%5==0)||(p%7==0)) return false;
        
        for(i <- 3 to math.sqrt(p).toInt + 1 by 2) if(p%i == 0) return false;

        cache.addEntry(p);
        return true;
    }
    
    
    def main(args:Array[String]) = println(
        (1 to 10001).foldLeft(1)((a:Int, b:Int)=>{
            var n = a+1;
            while(!isPrime(n)) n+=1;
            n;
        }));
    
    /*def main(args:Array[String]) = println({
        var n=1;
        for(i <- 1 to 10001) {
            n+=1; while(!isPrime(n)) n+=1;
        }
    n})*/
    
}
*/
object p7 extends App {
    println((1 to 10001).reduce((p,np)=>{ java.math.BigInteger.valueOf(p+1).nextProbablePrime.intValue }));
}