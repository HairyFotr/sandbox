// a graph/forest implementation... allows self-connection, will maybe allow multigraphs (if I can make it not too confusing)
// would like it if the graph magically knew some of its properties (full, connected, where the cycles are, ...)
// and if the graph could act valued or unvalued, directed or undirected at any time and stuff like that... again, if I can make it not too confusing :)
// tl;dr graphlib that does all, but doesn't yet
import scala.collection.mutable._
import scala.util.Random

object Globals {
    type NodeSet[T] = LinkedHashSet[Node[T]]
    type ConnSet[T] = HashSet[Conn[T]]
    type TwoNode[T] = (Node[T], Node[T])
    type NodeList[T] = List[Node[T]]
    implicit def TwoNode2NodeList[T](n:TwoNode[T]):NodeList[T] = List(n._1, n._2)
    implicit def Someting2Node[A](v:A) = new Node[A](v);

    def nextId():Int = Random.nextInt
}
import Globals._

class Node[V] (val conns:ConnSet[V] = new ConnSet[V], var value:Option[V] = None, val id:Int=nextId(), var tag:Int=0) {
    var valued = value match {
        case Some(_) => true
        case None => false
    }

    def this(n:Node[V]) = this(value=n.value,id=n.id,tag=n.tag)
    def this(v:V) = this(value = if(v==null) None else Some(v))
    def this(v:Option[V]) = this(value = v)
    
    
    def graphWhile(condition:Node[V]=>Boolean, tag:Int=1, bounded:Boolean=true)(f:Node[V]=>Unit) {
        if(condition(this) && (!bounded || (bounded && this.tag<tag))) {
            f(this)
            this.tag+=1;
            for(conn <- conns) 
                (if(conn.nodes._1==this) conn.nodes._2 else conn.nodes._1).graphWhile(condition, tag, bounded)(f)
        }
    }
    override def hashCode:Int = id
    override def equals(v:Any) = v.hashCode==this.hashCode
    override def toString:String = id+(if(valued) "("+value+")" else "")
}
object Node {
    def apply[A](a:A) = new Node[A](a);
}
class Conn[V] (val nodes:TwoNode[V], var value:Option[V] = None, val id:Int = nextId()) {// would this work better as a pimped Tuple
    var valued = value match {
        case Some(_) => true
        case None => false
    }
    var directed = false;
    def this(n1:Node[V], n2:Node[V]) { this((n1,n2)) }
    
    override def hashCode:Int = if(directed) (nodes).hashCode else nodes.foldLeft(0)((code, node)=>code+node.hashCode)
    override def equals(v:Any) = v.hashCode==this.hashCode
    override def toString:String = nodes._1.id+"--"+id+"-"+(if(directed) ">" else "-")+nodes._2.id
}

class Graph[V] {//TODO: Forest actually, detect connectedness
    def this(g:Graph[V]) {// Doesn't copy connections
        this()
        g.allNodes.foreach(node=> addNode(new Node[V](node)))
    }
    class Properties(var valuedNodes:Boolean=false, var valuedConns:Boolean=false)
    var properties = new Properties()
    //TODO: make sure it's all in all...
    val allNodes = new NodeSet[V]
    val allConns = new ConnSet[V]
    //TODO: keep this pre-built
    def freeNodes:NodeSet[V] = allNodes.filter(_.conns.size==0)
    def findFreeNode:Option[Node[V]] = allNodes.find(_.conns.size==0) 
    def findAnotherFreeNode(node:Node[V]):Option[Node[V]] = allNodes.find(n=> n.conns.size==0 && n!=node) 
    def connNodes:NodeSet[V] = allNodes.filter(_.conns.size>0)

    private var _root:Option[Node[V]] = None
    def root_=(r:Option[Node[V]]) = _root = r
    def root:Option[Node[V]] = _root match {
        case Some(r) => Some(r)
        case None => if(allNodes.size>0) Some(allNodes.head) else None
    }
    
    def addNode(value:Option[V]=None):Node[V] = {
        val n = new Node[V](value);
        allNodes += n
        n
    }
    def addNode(node:Node[V]):Node[V] = {
        val n = new Node[V](node);
        allNodes += n
        n
    }
    def addNodes(nodes:Node[V]*) {
        nodes.foreach(addNode(_));
    }
    def addConn(c:Conn[V]) {
        c.nodes.foreach(n=> n.conns += c)
        allConns += c
        allNodes ++= c.nodes
    }
    def addConn(n1:Node[V], n2:Node[V], value:Option[V]=None) { addConn(new Conn[V]((n1,n2),value)) }
    
    /*def graphWhile(condition:Node[V]=>Boolean)(f:Node[V]=>Unit) {
        if(allNodes.size > 0) root.graphWhile(condition)(f);
    }
    def graphWalk(f:Node[V]=>Unit, tag:Int=1) {
        if(allNodes.size > 0) root.graphWhile((n)=>true, tag)(f);
    }*/
    
    //
    def fullGraph():Graph[V] = {
        val graph = new Graph[V](this)
        val nodes = graph.allNodes.toSeq
        for(n1 <- nodes; n2 <- nodes) graph.addConn(n1, n2)
        graph
    }
    def CircularGraph():Graph[V] = {
        val graph = new Graph[V](this)
        val nodes = graph.allNodes.toSeq
        for(i <- 0 until nodes.size; val (n1,n2) = (nodes(i), nodes((i+1)%nodes.size))) graph.addConn(n1, n2)
        graph
    }
    def spanningTree():Graph[V] = {
        val graph = new Graph[V](this)
        var curr = graph.root
        while(graph.freeNodes.size > 0 && graph.allNodes.size > 1)
            graph.addConn(curr.get, {curr = graph.findAnotherFreeNode(curr.get); curr.get})
            
        graph
    }
    def randomGraph():Graph[V] = {
        val graph = new Graph[V](this)
        val nodes = graph.allNodes.toSeq
        for(n1 <- nodes; i <- 0 until Random.nextInt(nodes.size); val n2 = nodes(Random.nextInt(nodes.size))) graph.addConn(n1, n2)
        graph
    }
    def emptyGraph():Graph[V] = new Graph[V](this)
    
    
    override def toString:String = {
        var str = "";
        println(allNodes.size+" nodes, "+allConns.size+" conns")
        if(allNodes.size > 0) allNodes.foreach((n)=>{
            val skip = " "*(n.toString.length-1)+"\\-"
            str += n+"-" +
                (for(c <- n.conns) yield "-("+c.value+")--> "+(if(c.nodes._1==n) c.nodes._2 else c.nodes._1)).mkString("\n"+skip)
            str += "\n"
        })
        if(str.length==0) str else str.substring(0,str.length-1)
    }
}
object Graph {
    import scala.collection.Iterable
    
    abstract class GraphGenerator
    case class Random() extends GraphGenerator
    case class Full() extends GraphGenerator
    case class Circular() extends GraphGenerator
    case class Spanning() extends GraphGenerator
    case class Empty() extends GraphGenerator

    def fromNodes[A](nodes:Iterable[Node[A]], graphType:GraphGenerator = Empty()):Graph[A] = {
        var graph = new Graph[A]
        graph.addNodes(nodes.toSeq:_*)
        graphType match {//TODO: keep graph properties
            case Random() => graph.randomGraph
            case Full() => graph.fullGraph
            case Circular() => graph.CircularGraph
            case Spanning() => graph.spanningTree
            case _ => graph.emptyGraph
        }
    }
    /*
    //confused about how to copy in a nice way, and if that's necesarry
    def fromConns[A](conns:Iterable[Conn[A]]):Graph[A] = {
        var nodes = conns.flatMap(c=> List(c._1,c._2))
        var graph = new Graph[A]
        graph.addNodes(nodes:_*)
        graph.find
        
    }*/

    def main(args:Array[String]) {
        //testing and debugging stuff by hand
        //*
        var graph = Graph.fromNodes[Int](List(11,Node(22)))

        //*/
        //scalacheck testing
        //*
        import org.scalacheck._
        import org.scalacheck.Prop._
        import scala.collection.immutable.{Set=>immutableSet}
        //implicit def arbitraryNode:Arbitrary[Node[Int]] = Arbitrary(new Node[Int](Random.nextInt))
        implicit def arbitraryNodeSet:Arbitrary[immutableSet[Node[Int]]] = 
            Arbitrary(Gen.sized(size => immutableSet((0 to size).map(i=>new Node[Int](i)):_*)))
            
        forAll((nodes:immutableSet[Node[Int]])=> {
            var g = Graph.fromNodes[Int](nodes)
            ("init" |: {
                g.allNodes.foldLeft(true)((empty, node)=> empty && node.conns.size==0) &&
                g.allNodes.size == nodes.size
            }) &&
            ("fullGraph" |: {
                g = g.fullGraph
                g.allConns.size == (1 to nodes.size).sum &&
                g.allNodes.foldLeft(true)((full, node)=> full && node.conns.size == nodes.size)
            }) &&
            ("randomGraph" |: {
                g = g.randomGraph
                g.allConns.size <= (1 to nodes.size).sum
            }) &&
            ("CircularGraph" |: {
                g = g.CircularGraph
                g.allNodes.size<=2 || 
                (g.allConns.size == nodes.size &&
                    g.allNodes.foldLeft(true)((full, node)=> full && node.conns.size == 2))
            }) &&
            ("spanningTree" |: {
                g = g.spanningTree
                (nodes.size==0 && g.allConns.size==0) || 
                (g.allConns.size == nodes.size-1 &&
                    g.allNodes.foldLeft(0)((full, node)=> full + node.conns.size) == nodes.size*2-2)
            }) &&
            ("emptyGraph" |: {
                g = g.emptyGraph
                g.allNodes.foldLeft(true)((empty, node)=> empty && node.conns.size==0)
            })
        }).check
        //*/
    }
}
