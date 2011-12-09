object p30 extends App {
    def lol(n:Int):Int = n.toString.toCharArray.foldLeft(0)((sum,i)=>sum+math.pow(i.toInt-'0',5).toInt)

    println((2 to 295245*5).foldLeft(0)((sum,n)=> if(n==lol(n)) { println(n); sum+n } else sum))
}
