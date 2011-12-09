/*object p4 {
    def main(args:Array[String]) = println({
        var l=0;
        for(i <- 999 until 1 by -1; if(i*i > l); 
            j <- i until 1 by -1; if(i*j) > l;
            if({var s = (i*j).toString();
                var p = true;
                for(k <- 0 to s.length()/2)
                    p &= (s.charAt(k) == s.charAt(s.length()-1-k)); 
                p}))
                l=i*j;
        l})

}*/

object p4 extends App {
    var max=0
    for(i <- (100 until 999 reverse); j <- (100 until i reverse); val p = i*j) {
        if(p>max && (p.toString equals p.toString.reverse)) max = p;
        if(i*i < max) { println(max); sys.exit }
    }
}
