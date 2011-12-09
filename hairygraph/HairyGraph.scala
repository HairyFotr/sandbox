//just a basic graph implementation

import scala.collection.mutable._

object HairyTypeAliases {
    type HairyNodeSet[T] = HashSet[HairyNode[T]]
    type HairyConnSet[T] = HashSet[HairyConn[T]]
    type HairyTwoNode[T] = (HairyNode[T], HairyNode[T])
} 
import HairyTypeAliases._

class HairyNode[V] (var value:V, val conns:HairyConnSet[V] = new HairyConnSet[V]) {
    def graphWhile(condition:HairyNode[V]=>Boolean)(f:HairyNode[V]=>Unit) {
        if(condition(this)) {
            f(this)
            for(conn <- conns) (if(conn.nodes._1==this) conn.nodes._2 else conn.nodes._1).graphWhile(condition)(f)
        }
    }    
}
class HairyConn[V] (var value:V, val nodes:HairyTwoNode[V])//TODO: Override hashcode

class HairyGraph[V] {//Forest actually
    val allNodes = new HairyNodeSet[V]
    val allConns = new HairyConnSet[V]
    
    //TODO make sure it's all in all*...
    def addNode(value:V, conns:HairyConn[V]*) {
        val c = (new HairyConnSet[V]) ++ conns
        allNodes += new HairyNode[V](value, c);
        allConns ++= c
    }
    def addConn(value:V, nodes:HairyTwoNode[V]) {
        allConns += new HairyConn[V](value, nodes)
        allNodes += nodes._1
        allNodes += nodes._2
    }
    
}

object HairyGraph {
    def main(args:Array[String]) {
        println("hi")
        var f = -543
        for(i <- 0 to 343432432) f+=24;
        println(f)
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
