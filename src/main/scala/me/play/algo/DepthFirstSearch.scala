package me.play.algo

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

/**
  * Created by mangeeteden on 4/24/17.
  */
object DepthFirstSearch {

  def main(args: Array[String]): Unit = {

    // 1 -> 2 -> 3 -> 7
    // 1 -> 4 -> 6 -> 7
    // 2 -> 5 -> 6
    // 8 -> 9
    val graph = new GraphBuilder[Int]().addEdge(1, 2).addEdge(2, 3).addEdge(3, 7).addEdge(1, 4).addEdge(4, 6).
      addEdge(6, 7).addEdge(2, 5).addEdge(5, 6).addEdge(8, 9).build

    val dfs = DepthFirstSearch[Int](graph, 2)

    println(dfs)
    println(s"Connected[2 -> 6] ?: ${dfs.connected(6)}")
    println(s"Connected[2 -> 8] ?: ${dfs.connected(8)}")
  }
}

case class DepthFirstSearch[T](graph: Graph[T], sourceVertex: T) {

  private[algo] var marked: Map[T, Boolean] = Map()
  private[algo] var edgeTo: ListBuffer[(T, T)] = ListBuffer()

  {
    dfs(graph, sourceVertex)
  }

  /* performs the dfs from source vertex 'v' */
  def dfs(graph: Graph[T], v: T): Unit = {
    marked += v -> true
    for(w <- graph.adjTo(v)) {
      if (!isMarked(w)) {
        dfs(graph, w)
      }
      edgeTo += ((v, w))
    }
  }

  private def isMarked(v: T): Boolean = {
    marked.get(v) match {
      case None => false
      case _ => marked.get(v).get
    }
  }

  def connected(v: T) = {
    isMarked(v)
  }

  override def toString(): String = {
    edgeTo.mkString(", ")
  }
}
