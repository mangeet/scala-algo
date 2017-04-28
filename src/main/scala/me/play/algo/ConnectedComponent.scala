package me.play.algo

/**
  * To find the connected components.
  *
  * Created by mangeeteden on 4/26/17.
  */
object ConnectedComponent {

  def main(args: Array[String]): Unit = {

    // 1 -> 2 -> 3 -> 4 -> 5 -> 8
    // 2 -> 6 -> 7 -> 8
    // 8 -> 9 -> 10
    // 11 -> 12 -> 13
    val graph = new GraphBuilder[Int]().addEdge(1, 2).addEdge(2, 3).addEdge(3, 4).addEdge(4, 5).addEdge(5, 8).
      addEdge(2, 6).addEdge(6, 7).addEdge(7, 8).
      addEdge(8, 9).addEdge(9, 10).
      addEdge(11, 12).addEdge(12, 13).build

    println(graph.vertices)
    val cc = ConnectedComponent[Int](graph)
    println(cc.cc)
  }
}

case class ConnectedComponent[T](graph: Graph[T]) {

  private[algo] var marked: Map[T, Boolean] = Map()
  private[algo] var cc: Map[T, Int] = Map()

  var count: Int = 0

  {
    for (v <- graph.vertices) {
      if (!isMarked(v)) {
        dfs(graph, v)
        count += 1
      }
    }
  }

  /* performs the dfs from source vertex 'v' */
  def dfs(graph: Graph[T], sourceVertex: T): Unit = {

    marked += sourceVertex -> true
    cc += sourceVertex -> count

    for (w <- graph.adjTo(sourceVertex)) {
      if (!isMarked(w)) {
        dfs(graph, w)
      }
    }
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

  def numOfConnectedCompoments: Int = {
    count
  }
}