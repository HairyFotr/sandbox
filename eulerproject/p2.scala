/*object p2 {

    def main(args:Array[String]) = println({
        var xx=0
        var x=1
        var sum=0
        var c=0
        while(x<4000000) {
            c=xx+x; xx=x; x=c;
            if(c%2==0) sum+=c;
        }
        sum})
}*/

/*object p2 extends App {
    var xx=0
    var x=1
    var sum=0
    var c=0
    while(x<4000000) {
        c=xx+x; xx=x; x=c;
        if(c%2==0) sum+=c;
    }
    
    println(sum)
}*/

object p2 extends App {
    def nextFib(p:(Int,Int)):(Int,Int) = (p._2,p._1+p._2)
    var fib = (1,2)
    var sum = 0
    do {
        if(fib._2%2==0) sum += fib._2
        fib = nextFib(fib)
    } while(fib._2 < 4000000)
    
    println(sum)
}