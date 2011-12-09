/*object p3 {
    def main(args:Array[String]) = println({
        var n:Long=600851475143L;
        var c=2;
        while(c<n){while(n%c==0)n/=c;c+=1}

        //while(++c<n)while(n%c==0)n/=c;//scala,y you no ++ haben?
 
        println(c);
    }
    
}*/

object p3 extends App {
    var (n,c)=(600851475143L,1);
    while({c+=1;c}<n)while(n%c==0)n/=c;

    println(c);
}
