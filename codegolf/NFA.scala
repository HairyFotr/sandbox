//usage: NFA aaba baaab abaa
object NFA extends App {
    import scala.collection.mutable._
    val gates = HashMap[(String,Char), ListBuffer[String]]()
    val lines = io.Source.fromFile("NFA.txt").getLines.toSeq
    val (init,finals) = (lines(0).split(" ")(1),lines(1).split(" ").tail)
    lines.tail.tail.map(_.split(" ")).foreach 
        {s=> gates.getOrElseUpdate((s(0),s(1)(0)), ListBuffer()) += s(3)}
 
    def NFA(state:String, input:String):Boolean = 
        if(input!="") (false /: gates.getOrElse((state,input(0)), return false))
            {_ || NFA(_, input.tail)} else finals.contains(state)
 
    args.foreach(in=> println(in + (if(NFA(init,in)) " YES" else " NO")))
}