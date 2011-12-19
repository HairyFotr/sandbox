//usage: StackMachine aaba baaab abaa
object StackMachine extends App {
    import scala.collection.mutable._
    val gates = HashMap[(String,Char,Char), HashSet[(String,String)]]()
    val lines = io.Source.fromFile("StackMachine.txt").getLines.toSeq
    val (init,finals) = (lines(0).split(" ")(1),lines(1).split(" ").tail)
    def epsy(s:String):String = (if(s=="eps") " " else s)
    lines.tail.tail.map(_.split(" ")).foreach {s => 
        gates.getOrElseUpdate((s(0),epsy(s(1))(0),epsy(s(2))(0)), HashSet()) += ((s(4),epsy(s(5))))}

    val visited = HashSet[(String,String,String)]();
    def SM(state:String, input:String, stack:String="Z"):Boolean = 
        if(visited.contains(state,input,stack)) false else { visited += ((state,input,stack)); 
            if(input.trim=="" && (finals.contains(state) || (stack.trim==""))) true else 
            (false /: List((input(0),stack(0)),(' ',stack(0)),(input(0),' '),(' ',' ')))
            {(r,s)=> {r || (false /: gates.getOrElse((state,s._1,s._2), HashSet[(String,String)]()))
                {(rr,ss)=> rr || SM(ss._1, (if(s._1==' ') input else input.tail.trim+" "), 
                                   (ss._2+(if(s._2==' ') stack else stack.tail)).trim+" ")}}}}

    args.foreach(in=> {visited.clear; println(in + (if(SM(init,in)) " YES" else " NO"))})
}
