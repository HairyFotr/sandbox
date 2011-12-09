object p9 {
    
    def main(args:Array[String]) {
		val sumcond = 1000;		
        for(a <- 1 until sumcond; b <- a+1 until sumcond; val c = Math.sqrt(a*a + b*b) if(c==c.toInt && a*a+b*b==c*c && a+b+c==sumcond)) {
            println(a*b*c)
            return
        }
    }
    
}
