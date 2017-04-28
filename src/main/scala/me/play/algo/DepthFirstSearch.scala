package me.play.algo

/**
  * Created by mangeeteden on 4/24/17.
  */
object DepthFirstSearch {

  def main(args: Array[String]): Unit = {

    testGraph2
  }

  def testGraph1: Unit = {

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
    println(s"Path to [2 -> 7] ?: ${dfs.pathTo(7).toList}")
  }

  def testGraph2: Unit = {

    // 1 -> 2 -> 3 -> 4 -> 5 -> 8
    // 2 -> 6 -> 7 -> 8
    // 8 -> 9 -> 10
    // 11 -> 12 -> 13
    // 10 -> 14
    // 13 -> 14
    // 14 -> 15
    // 16 -> 17
    val graph = new GraphBuilder[Int]().addEdge(1, 2).addEdge(2, 3).addEdge(3, 4).addEdge(4, 5).addEdge(5, 8).
      addEdge(2, 6).addEdge(6, 7).addEdge(7, 8).
      addEdge(8, 9).addEdge(9, 10).
      addEdge(11, 12).addEdge(12, 13).
      addEdge(10, 14).
      addEdge(13, 14).
      addEdge(14, 15).
      addEdge(16, 17).build

    val dfs = DepthFirstSearch[Int](graph, 1)

    println(dfs)
    println(s"Connected[1 -> 8] ?: ${dfs.connected(8)}")
    println(s"Connected[1 -> 17] ?: ${dfs.connected(17)}")
    println(s"Path to [1 -> 15] ?: ${dfs.pathTo(15).toList}")
  }
}

case class DepthFirstSearch[T](graph: Graph[T], sourceVertex: T) {

  private[algo] var marked: Map[T, Boolean] = Map()
  private[algo] var edges: Map[T, T] = Map()
  private[algo] var paths: Map[T, Queue[T]] = Map()

  {
    dfs(graph, sourceVertex)
  }

  /* performs the dfs from source vertex 'v' */
  def dfs(graph: Graph[T], v: T): Unit = {
    marked += v -> true
    for (w <- graph.adjTo(v)) {
      if (!isMarked(w)) {
        dfs(graph, w)
      }
      edges += v -> w
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

  def pathTo(targetVertex: T): Iterator[T] = {

    if (connected(targetVertex) && targetVertex != sourceVertex) {

      val path = paths.get(sourceVertex) match {
        case None =>

          val newPath = new Queue[T]
          newPath.enqueue(sourceVertex)
          var tempVertex = sourceVertex

          while (tempVertex != targetVertex) {
            val edge: T = edgeTo(tempVertex)
            newPath.enqueue(edge)
            tempVertex = edge
          }

          paths += targetVertex -> newPath
          newPath
        case _ => paths.get(sourceVertex).get
      }
      path
    } else {
      List.empty[T].toIterator
    }
  }

  /**
    * Returns the vertex(source) which makes edge to vertex 'targetVertex'.
    */
  def edgeTo(targetVertex: T): T = {
    edges.get(targetVertex).get
  }

  override def toString(): String = {
    edges.mkString(", ")
  }
}
