object p10 {
    
    var cache = new collection.mutable.HashSet[Long];
    def isPrime(p:Long):Boolean = {
        if((p==2)||(p==3)||(p==5)||(p==7)) return true;
        if((p%2==0)||(p%3==0)||(p%5==0)||(p%7==0)) return false;
        if(cache.contains(p)) return true;
        
        for(i <- 3 to Math.sqrt(p).toInt + 1 by 2) if(p%i == 0) return false;

        cache.addEntry(p);
        return true;
    }

    def main(args:Array[String]) {
        var max=2000000;
        var sum=2L;
        for(i <- 3 until max by 2; if(isPrime(i))) sum+=i;
 
        println(sum);
    }
    
}
