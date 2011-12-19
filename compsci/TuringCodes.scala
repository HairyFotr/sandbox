// Generator of Turing codes (just translated from an early python version by @zidarsk8)

object TuringCodes extends App {
    import scala.collection.mutable._
    val arr = new ListBuffer[BigInt]

    def ins(p:BigInt,n:Int,d:Int) {
        if(d==0) {
            if(p.&(15) > 0) arr += p
        } else {
            val p2 = (p*2+1)<<n
            for(i <- 0 until n) ins(p2>>i, i+1, d-1)
        }
    }
    
    ins(1,30,5)

    arr.sorted.foreach(i=>println(i+" "+i.toString(2)))
}
