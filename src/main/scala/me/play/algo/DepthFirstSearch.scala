package me.play.algo

/**
  * Created by mangeeteden on 4/24/17.
  */
object DepthFirstSearch {

  def main(args: Array[String]): Unit = {

    // 1 -> 2 -> 3 -> 7
    // 1 -> 4 -> 6 -> 7
    // 2 -> 5 -> 6
    val graph = new GraphBuilder[Int]().addEdge(1, 2).addEdge(2, 3).addEdge(3, 7).addEdge(1, 4).addEdge(4, 6).
      addEdge(6, 7).addEdge(2, 5).addEdge(5, 6).build

    val dfs = DepthFirstSearch[Int](graph, 1)

    println(s"Connected[1 -> 7] ?: $dfs.connected(7)")
    println(s"Connected[1 -> 6] ?: $dfs.connected(6)")
  }
}

case class DepthFirstSearch[T](graph: Graph[T], sourceVertex: T) {

  private[algo] var marked: Map[T, Boolean] = Map()
  private[algo] var edgeTo: Map[T, T] = Map()

  {
    dfs(graph, sourceVertex)
  }

  /* performs the dfs from source vertex 'v' */
  def dfs(graph: Graph[T], v: T): Unit = {
    marked += v -> true
    for (w <- graph.adjTo(v)) {
      if (!marked(w)) {
        dfs(graph, w)
      }
      edgeTo += v -> w
    }
  }

  def connected(v: T) = {
    marked(v)
  }

  override def toString(): String = {
    edgeTo.mkString(", ")
  }
}
