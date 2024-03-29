// a graph/forest implementation... allows self-connection, will maybe allow multigraphs (if I can make it not too confusing)
// would like it if the graph magically knew some of its properties (full, connected, where the cycles are, ...)
// and if the graph could act valued or unvalued, directed or undirected at any time and stuff like that... again, if I can make it not too confusing :)
// tl;dr graphlib that does all, but doesn't yet

//should everything be copied inside graph, or should we just wing it
////aim for immutability -- copy or reuse if immutable below
////.seal() makes it immutable (also then you can precompute a bunch of stuff easier)
//should most methods return graph for chaining?
////yes.
//are you reading this code as a seasoned functional developer and screaming in terror
////yes.

import scala.collection.mutable._
import scala.util.Random

object Globals {
    type NodeSet[T] = LinkedHashSet[Node[T]]
    type ConnSet[T] = HashSet[Conn[T]]
    type NodePair[T] = (Node[T], Node[T])
    type NodeList[T] = List[Node[T]]
    implicit def NodePair2NodeList[T](n:NodePair[T]):NodeList[T] = List(n._1, n._2)
    implicit def Someting2Node[A](v:A) = new Node[A](v);

    def nextId():Int = Random.nextInt
}
import Globals._

//TODO notsure if doing it for added convenience, or because I don't really get Option or default values.
class Tag(var tag:Option[Int]=None) {
    def this(i:Int) = this(Some(i))
    
    def tagged = tag.isDefined
    def untag = tag = None
    def apply():Int = tag.getOrElse(0)
    def apply(i:Int):Tag = { tag = Some(i); this }
    def apply(t:Tag):Tag = { if(t.tagged) tag = Some(t.tag.get) else tag = None; this }
}
object Tag {
    def apply() = new Tag()
    def apply(i:Int) = new Tag(i)
    def apply(i:Option[Int]) = new Tag(i)
}

class Node[V] (val conns:ConnSet[V] = new ConnSet[V], var value:Option[V] = None, val id:Int=nextId(), val tag:Tag=Tag()) {
    var valued = value.isDefined
    def this(n:Node[V]) = this(value=n.value,id=n.id,tag=n.tag)
    def this(v:V) = this(value = (if(v==null) None else Some(v)))
    def this(v:Option[V]) = this(value = v)
    
    def graphWhile(condition:Node[V]=>Boolean, tag:Tag=Tag(), noReturn:Option[Node[V]]=None)(f:Node[V]=>Unit) {
        if(condition(this) && ((!tag.tagged || !this.tag.tagged || this.tag() != tag()))) {
            f(this)
            if(tag.tagged) this.tag(tag());
            for(conn <- conns) {
                val node = conn.other(this)
                if(noReturn == None || node != noReturn.get) node.graphWhile(condition, tag, Some(this))(f)
            }
        }
    }
    override def hashCode:Int = id + (if(valued) value.get.hashCode else 0)
    override def equals(v:Any) = v.hashCode==this.hashCode
    override def toString:String = id+(if(valued) "("+value.get+")" else "")
}
object Node {
    def apply[A]() = new Node[A]();
    def apply[A](a:A) = new Node[A](a);
}
class Conn[V] (val nodes:NodePair[V], var value:Option[V] = None, val id:Int = nextId()) {// would this work better as a pimped Tuple
    var valued = value.isDefined
    var directed = false;
    def this(n1:Node[V], n2:Node[V]) { this((n1,n2)) }
    def other(n:Node[V]):Node[V] = (if(nodes._1==n) nodes._2 else nodes._1)
    
    override def hashCode:Int = if(directed) nodes.hashCode else nodes.foldLeft(0)((code, node)=>code+node.hashCode)
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

    //TODO: check if these nodes still exist, also which scala.collection trait defines this stuff
    // root or just the fist thing we can get out hands onto
    private var _head:Option[Node[V]] = None
    def head:Option[Node[V]] = _head.orElse(if(allNodes.size>0) Some(allNodes.head) else None)
    
    // last thing inserted into allNodes - why as var? Consider reinsertion g.addNode(alreadyInserted); g.last
    private var _last:Option[Node[V]] = None
    def last:Option[Node[V]] = _last.orElse(if(allNodes.size>0) Some(allNodes.last) else None)
    
    def addNode(value:Option[V]=None):Graph[V] = {
        addNode(new Node[V](value));
        this
    }
    def addNode(node:Node[V]):Graph[V] = {
        val n = new Node[V](node);
        allNodes += n
        _last = Some(n)
        this
    }
    def addNodes(nodes:Node[V]*):Graph[V] = {
        nodes.foreach(addNode(_));
        this
    }
    def addConn(c:Conn[V]):Graph[V] = {
        c.nodes.foreach(n=> n.conns += c)
        allConns += c
        allNodes ++= c.nodes
        this
    }
    //def addConn(n1:Node[V], n2:Node[V], value:Option[V]=None):Graph[V] = addConn(new Conn[V]((n1,n2),value))
    def addConn(n:NodePair[V], value:Option[V]=None):Graph[V] = addConn(new Conn[V](n,value))
    
    def removeConn(c:Conn[V]):Graph[V] = {
        c.nodes.foreach(n=> n.conns -= c)
        allConns -= c
        this
    }
    
    def graphWalk(tag:Tag=Tag())(f:Node[V]=>Unit) = graphWhile((n)=>true, tag)(f);
    def graphWhile(condition:Node[V]=>Boolean, tag:Tag=Tag())(f:Node[V]=>Unit) {
        if(head != None) head.get.graphWhile(condition, tag)(f);
    }

    def randomStep(n:Node[V]):Option[Node[V]] = 
        if(n.conns.size == 0) 
            None
        else 
            Some(n.conns.toSeq(Random.nextInt(n.conns.size)).other(n))
        
    def markovChainStep(n:Node[V]):Option[Node[V]] = { //TODO: detect types
        var sum = 0d
        var sums = new LinkedHashMap[Double, Node[V]]
        n.conns.foreach(c=> c.value.foreach(_ match {
            case v:Double => if(v>0) { sum+=v; sums+=sum->c.other(n) }
        }))
        if(sum > 0) {
            var r = Random.nextDouble * sum
            Some(sums.find(m => r <= m._1).get._2)
        } else {
            None
        }
    }
    /*
        if(!n.valued)
            None
        else 
            n.value.get match {
                case v:Double =>
                case _ => None
            }*/
            
    var curr:Option[Node[V]] = None
    def graphStep(func:(Node[V]=>Option[Node[V]])=randomStep):Option[Node[V]] = {
        if(curr==None) curr = head
        if(curr!=None) curr = func(curr.get)
        curr
    }
    
    //
    def fullGraph():Graph[V] = {
        val graph = new Graph[V](this)
        val nodes = graph.allNodes
        for(n1 <- nodes; n2 <- nodes) graph.addConn(n1->n2)
        graph
    }
    def CircularGraph():Graph[V] = {
        val graph = new Graph[V](this)
        val nodes = graph.allNodes.toSeq
        for(i <- 0 until nodes.size; val (n1,n2) = (nodes(i), nodes((i+1)%nodes.size))) graph.addConn(n1->n2)
        graph
    }
    def spanningTree():Graph[V] = {
        val graph = new Graph[V](this)
        var curr = graph.head
        while(graph.freeNodes.size > 0 && graph.allNodes.size > 1)
            graph.addConn(curr.get -> {curr = graph.findAnotherFreeNode(curr.get); curr.get})
            
        graph
    }
    def randomGraph():Graph[V] = {
        val graph = new Graph[V](this)
        val nodes = graph.allNodes.toSeq
        for(n1 <- nodes; i <- 0 until Random.nextInt(nodes.size); val n2 = nodes(Random.nextInt(nodes.size))) graph.addConn(n1->n2)
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
    
    //node->None->node
    def fromPairs[A](pairs:NodePair[A]*):Graph[A] = {
        var graph = new Graph[A]
        pairs.foreach(p=> graph.addConn(p))
        graph
    }
        
    //curse you type erasure
    /*def fromTriplets[A](triplets:((Node[A],A),Node[A])*):Graph[A] =
        fromTriplets(triplets.map(t=> t._1._1->Some(t._1._2)->t._2):_*);*/
    
    def fromTriplets[A](triplets:((Node[A],A),Node[A])*):Graph[A] = {//node->conn->node
        var graph = new Graph[A]
        triplets.foreach(t=> graph.addConn(t._1._1 -> t._2, Some(t._1._2)))
        graph
    }
    def fromProbMatrix[A](m:scala.collection.Seq[scala.collection.Seq[A]]):Graph[A] = {//node->conn->node
        var graph = new Graph[A]
        var nodes = for(i <- 0 until m.size) yield Node[A]()
        for(i <- 0 until m.size; j <- 0 until m(i).size) graph.addConn(nodes(i) -> nodes(j), Some(m(i)(j)))
        graph
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
        val matrix = List(
            List(0.3, 0.1, 0.6),
            List(0.1, 0.6, 0.3),
            List(0.4, 0.4, 0.2)
        )
        var graph = Graph.fromProbMatrix[Double](matrix)
        /*graph.curr = {//beautify
            var r=util.Random.nextDouble
            if(r<=1/3d) 
                Some(nodes(0)) 
            else if(r<=2/3d) 
                Some(nodes(1)) 
            else
                Some(nodes(2)) 
        }*/
        val runs=75000
        var probs = new HashMap[Node[Double], Int]
        for(i <- 1 to runs) {
            graph.graphStep(graph.markovChainStep)
            probs += graph.curr.get -> (probs.getOrElseUpdate(graph.curr.get, 0)+1)
        }

        println(probs.map(m=> m._1->(m._2/(runs+0d))))
        //println(graph)
        
        //scalacheck testing
        /*
        import org.scalacheck._
        import org.scalacheck.Prop._
        import scala.collection.immutable.{Set=>immutableSet}
        
        //implicit def arbitraryNode:Arbitrary[Node[Int]] = Arbitrary(new Node[Int](Random.nextInt))
        implicit def arbitraryNodeSet:Arbitrary[immutableSet[Node[Int]]] = 
            Arbitrary(Gen.sized(size => immutableSet((0 to size).map(i=>new Node[Int](i)):_*)))
            
        forAll((nodes:immutableSet[Node[Int]])=> {
            {
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
            } &&
            ("addNode" |: {
                var g = Graph.fromNodes[Int](nodes)
                val before = g.allNodes.size
                g.addNode(new Node[Int])
                val after = g.allNodes.size
                after == before+1
            }) &&
            ("addNodes" |: {
                var g = Graph.fromNodes[Int](nodes)
                val before = g.allNodes.size
                g.addNodes(new Node[Int],new Node[Int],new Node[Int])
                val after = g.allNodes.size
                after == before+3
            }) &&
            ("chaining" |: {
                var g = Graph.fromNodes[Int](nodes)
                val before = g.allNodes.size
                g.addNode(new Node[Int]).addNodes(new Node[Int],new Node[Int]).addNode(new Node[Int])
                val after = g.allNodes.size
                after == before+4
            }) &&
            ("graphWalkTag" |: { //should work even for circular
                var g = Graph.fromNodes[Int](nodes)
                g = g.fullGraph
                var allNodes = new NodeSet[Int]
                var counter = 0
                g.graphWalk(Tag(1))((n) => {
                    allNodes += n
                    counter += 1
                })
                counter==g.allNodes.size && allNodes.size == g.allNodes.size
            }) &&
            ("graphWalkNotag" |: { //should work for non-circular
                var g = Graph.fromNodes[Int](nodes)
                g = g.spanningTree
                var allNodes = new NodeSet[Int]
                var counter = 0
                g.graphWalk()((n) => {
                    allNodes += n
                    counter += 1
                })
                counter==g.allNodes.size && allNodes.size == g.allNodes.size
            }) &&
            ("fromPairs" |: {
                var nds = nodes.toSeq
                var pairs = for(i <- 0 until nds.size) yield nds(i)->nds((i+1)%nds.size)
                var g = Graph.fromPairs[Int](pairs:_*)
                var g2 = Graph.fromNodes[Int](nodes, Circular())
                
                g.allNodes.size == g2.allNodes.size &&
                g.allConns.size == g2.allConns.size
            }) //*&& 
        }).check(org.scalacheck.Test.Params(minSuccessfulTests=10))
        //*/
    }
}
