object p27 extends App {
    var longest=(0,0,0)
    for(a <- (-999 to 999); b <- (1 to 999).filter(BigInt(_).isProbablePrime(5))) {
        var num = (0 to 79).takeWhile(n=>BigInt(n*n + a*n + b).isProbablePrime(5)).length
        if(num>longest._3) { longest = (a,b,num); println(longest) }
    }
    
    println(longest._1*longest._2)
}
