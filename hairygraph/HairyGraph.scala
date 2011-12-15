//just a basic graph implementation

//TODO: graphviz support

import scala.collection.mutable._

object HairyTypeAliases {
    type HairyNodeSet[T] = LinkedHashSet[HairyNode[T]]
    type HairyConnSet[T] = HashSet[HairyConn[T]]
    type HairyTwoNode[T] = (HairyNode[T], HairyNode[T])
    type HairyNodeList[T] = List[HairyNode[T]]
    implicit def nodes2list[T](n:HairyTwoNode[T]):HairyNodeList[T] = List(n._1, n._2)
} 
import HairyTypeAliases._

import scala.util.Random.{nextInt=>randomInt}
object HairyGlobals {
    implicit def val2node[A](v:A) = new HairyNode[A](v);
    //implicit def vallist2nodelist[A](v:List[A]) = List[HairyNode[A]]() ++ v;
    var idCounter=0
    def nextId() = { randomInt }//{ idCounter+=1; idCounter }
}
import HairyGlobals._

class HairyNode[V] (var value:V, val id:Int=nextId(), val conns:HairyConnSet[V] = new HairyConnSet[V]) {
    def this(n:HairyNode[V]) {
        this(n.value, n.id)
        tag = n.tag
    }
    var tag=0;
    def graphWhile(condition:HairyNode[V]=>Boolean, tag:Int=1, bounded:Boolean=true)(f:HairyNode[V]=>Unit) {
        if(condition(this) && (!bounded || (bounded && this.tag<tag))) {
            f(this)
            this.tag+=1;
            for(conn <- conns) 
                (if(conn.nodes._1==this) conn.nodes._2 else conn.nodes._1).graphWhile(condition, tag, bounded)(f)
        }
    }
    override def hashCode:Int = hashCode2 //+ conns.foldLeft(0)((code, conn)=>code+conn.hashCode2)
    def hashCode2:Int = id + value.hashCode
    override def equals(v:Any) = v.hashCode==this.hashCode
    override def toString:String = id+"("+value+")"
}
object HairyNode {
    def apply[A](a:A) = new HairyNode[A](a);
}
class HairyConn[V] (var value:V, val nodes:HairyTwoNode[V]){
    val id = nextId()
    def this(nodes:HairyTwoNode[V]) { this(null.asInstanceOf[V], nodes) }
    def this(n1:HairyNode[V], n2:HairyNode[V]) { this((n1,n2)) }
    
    override def hashCode:Int = hashCode2 + nodes.foldLeft(0)((code, node)=>code+node.hashCode2)
    def hashCode2:Int = (if(value!=null) value.hashCode else 0)
    override def toString:String = if(value!=null) value.toString else ""
}

class HairyGraph[V] {//TODO: Forest actually, detect connectedness
    def this(g:HairyGraph[V]) {//copy constructor TODO: copy connections
        this()
        allNodes ++= (for(n <- g.allNodes) yield new HairyNode[V](n))
    }
    //TODO: make sure it's all in all...
    val allNodes = new HairyNodeSet[V]
    val allConns = new HairyConnSet[V]
    //TODO: keep this pre-built
    def freeNodes:HairyNodeSet[V] = allNodes.filter(_.conns.size==0)
    def findFreeNode:Option[HairyNode[V]] = allNodes.find(_.conns.size==0) 
    def findAnotherFreeNode(node:HairyNode[V]):Option[HairyNode[V]] = allNodes.find(n=> n.conns.size==0 && n!=node) 
    def connNodes:HairyNodeSet[V] = allNodes.filter(_.conns.size>0)

    var _root:HairyNode[V] = null;//TODO: check the getter setter thing already...
    def root:HairyNode[V] = if(_root==null) allNodes.head else _root;
    
    def addNode(value:V, conns:HairyConn[V]*) {
        val n = new HairyNode[V](value);
        n.conns ++= conns
        allNodes += n
        allConns ++= conns
    }
    def addNodes(nodes:HairyNode[V]*) = { allNodes ++= nodes }
    def addConn(c:HairyConn[V]) {
        c.nodes.foreach(n=>n.conns += c)
        allConns += c
        allNodes ++= c.nodes
    }
    def addConn(value:V, n1:HairyNode[V], n2:HairyNode[V]) { addConn(new HairyConn[V](value, (n1,n2))) }
    def addConn(n1:HairyNode[V], n2:HairyNode[V]) { addConn(new HairyConn[V](n1,n2)) }
    //def addConn(value:V, n1:V, n2:V) { addConn(value, new HairyNode(n1), new HairyNode(n2)) }
    
    def graphWhile(condition:HairyNode[V]=>Boolean)(f:HairyNode[V]=>Unit) {
        if(allNodes.size > 0) root.graphWhile(condition)(f);
    }
    def graphWalk(f:HairyNode[V]=>Unit, tag:Int=1) {
        if(allNodes.size > 0) root.graphWhile((n)=>true, tag)(f);
    }
    
    //
    def fullGraph():HairyGraph[V] = {
        val graph = new HairyGraph[V](this)
        val nodes = graph.allNodes.toSeq
        for(n1 <- nodes; n2 <- nodes) graph.addConn(n1, n2)
        graph
    }
    def circleGraph():HairyGraph[V] = {
        val graph = new HairyGraph[V](this)
        val nodes = graph.allNodes.toSeq
        for(i <- 0 until nodes.size; val (n1,n2) = (nodes(i), nodes((i+1)%nodes.size))) graph.addConn(n1, n2)
        graph
    }
    def spanningTree():HairyGraph[V] = {
        val graph = new HairyGraph[V](this)
        var curr = graph.root
        while(graph.freeNodes.size > 0) graph.addConn(curr, {curr = graph.findAnotherFreeNode(curr).get; curr})
        graph
    }
    def randomGraph():HairyGraph[V] = {
        val graph = new HairyGraph[V](this)
        val nodes = graph.allNodes.toSeq
        for(n1 <- nodes; i <- 0 until randomInt(nodes.size); val n2 = nodes(randomInt(nodes.size))) graph.addConn(n1, n2)
        graph
    }
    def emptyGraph():HairyGraph[V] = new HairyGraph[V](this)
    
    
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
object HairyGraph {
    abstract class HairyGraphType
    case class Random() extends HairyGraphType
    case class Full() extends HairyGraphType
    case class Circle() extends HairyGraphType
    case class Spanning() extends HairyGraphType
    case class Empty() extends HairyGraphType

    def generate[A](nodes:List[HairyNode[A]], graphType:HairyGraphType = Empty()):HairyGraph[A] = {
        var graph = new HairyGraph[A]
        graph.addNodes(nodes:_*);
        graphType match {//TODO: keep graph properties
            case Random() => graph = graph.randomGraph
            case Full() => graph = graph.fullGraph
            case Circle() => graph = graph.circleGraph
            case Spanning() => graph = graph.spanningTree
            case Empty() => graph = graph.emptyGraph
        }
        graph
    }

    def main(args:Array[String]) {
        //testing and debugging stuff by hand
        var graph = HairyGraph.generate[Int](List(11,HairyNode(22)))
        graph = HairyGraph.generate[Int](List(0,0))
        println(graph)
        graph = graph.fullGraph
        println
        println(graph)
        graph = graph.emptyGraph
        println
        println(graph)
        println(graph.allNodes.foldLeft(true)((empty, node)=> empty && node.conns.size==0))

        println
        (println)

        //scalacheck testing
        /*
        import org.scalacheck._
        Prop.forAll((n1:Int, n2:Int)=> {
            var nodes = List(n1,n2)
            println(nodes)
            var g = HairyGraph.generate[Int](nodes.map(n=>new HairyNode(n)))
            var r = g.allNodes.foldLeft(true)((empty, node)=> empty && node.conns.size==0)
            //println(r)
            println(g)
            g = g.fullGraph
            r &= g.allNodes.foldLeft(true)((full, node)=> full && node.conns.size==2*(g.allNodes.size-1)+g.allNodes.size)
            println(g.allNodes.foldLeft(0)((sum,n)=>sum + n.conns.size))
            //println(r)
            println(g)
            g = g.emptyGraph
            r &= g.allNodes.foldLeft(true)((empty, node)=> empty && node.conns.size==0)
            //println(r)
            println(g)
            r
        }).check//*/
    }

//TODO: scalacheck
/*
import org.scalacheck._
    val hg = new HairyGraph[Double]
    implicit val connGen: Arbitrary[HairyConn[Double]] = Arbitrary(new HairyConn[Double](...))
    
    test("addNode") = Prop.forAll((conn:HairyConn[Double]) => {
        val len = hg.allNodes.size
        hg.addNode(value, conn)
        hg.addNode(value, conn)
        if(hg.allNodes.size-len > 1) false else true
    })*/
}
