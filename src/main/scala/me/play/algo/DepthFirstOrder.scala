package me.play.algo

/**
  * Topological sorting of graph
  * Created by mangeeteden on 4/28/17.
  */
object DepthFirstOrder {

  def main(args: Array[String]): Unit = {

    // 1 -> 3 -> 5 -> 7 -> 9 -> 11
    // 1 -> 2 -> 4 -> 6 -> 8 -> 12
    // 11 -> 13
    // 12 -> 13
    val graph = new GraphBuilder[Int]().addEdge(1, 3).addEdge(3, 5).addEdge(5, 7).addEdge(7, 9).addEdge(9, 11).
      addEdge(1, 2).addEdge(2, 4).addEdge(4, 6).addEdge(6, 8).addEdge(8, 12).
      addEdge(11, 13).
      addEdge(12, 13).build

    println(graph.vertices)
    val dfOrder = DepthFirstOrder[Int](graph)
    println(dfOrder.order.toList)
  }
}

case class DepthFirstOrder[T](graph: Graph[T]) {

  private[algo] var marked: Map[T, Boolean] = Map()
  private[algo] var reversePost = new Stack[T]

  {
    for (v <- graph.vertices) {
      if (!isMarked(v)) {
        dfs(graph, v)
      }
    }
  }

  /* performs the dfs from source vertex 'v' */
  def dfs(graph: Graph[T], sourceVertex: T): Unit = {

    marked += sourceVertex -> true

    for (w <- graph.adjTo(sourceVertex)) {
      if (!isMarked(w)) {
        dfs(graph, w)
      }
    }
    reversePost.push(sourceVertex)
  }

  private def isMarked(v: T): Boolean = {
    marked.get(v) match {
      case None => false
      case _ => marked.get(v).get
    }
  }

  def connected(v: T): Boolean = {
    isMarked(v)
  }

  def order(): Iterator[T] = {
    reversePost
  }
}