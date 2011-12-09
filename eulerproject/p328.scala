import scala.collection.mutable.HashMap;

object p328 extends App {
    val mem = new HashMap[(Int,Int), Int];
    
    def splits(from:Int,to:Int):Int = {
        val len = to-from+1;
        if(len <= 1) return 0
        if(len <= 3) return to-1
        return mem.getOrElseUpdate((from, to), 
            (from+2 to to-1).fold(Int.MaxValue)(
                (min,i)=>{
                    //print("("+from+","+(i)+","+to+")");
                    val (l,r) = (splits(from,i-1),splits(i+1, to))
                    val newMin = if(l>r) l+i else r+i;
                    if(newMin<min) newMin else min;
                })
        )
    }
    
    val res = for(i <- 1 to 500) yield splits(1,i);
    val ressum = res.reduce(_+_);
    
    println(res);
    println(ressum);
}
