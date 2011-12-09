object p29 extends App {
    val set = new scala.collection.mutable.HashSet[BigInt]
    for(a <- 2 to 100; b <- 2 to 100) set += BigDecimal(math.pow(a,b)).toBigInt
    println(set.size)
}
