package me.play.algo

/**
  * Created by mangeeteden on 4/24/17.
  */

object Graph {

  def main(args: Array[String]): Unit = {

    val graph = new GraphBuilder().addEdge(1, 2).addEdge(2, 3).addEdge(1, 4).build
    val itr = graph.adjTo(1)
    while(itr.hasNext) {
      println(itr.next)
    }
  }
}

/* Graph definition */
class Graph(numOfVertices: Int, adjVertices: Map[Int, Stack[Int]]) {

  def adjTo(v: Int): Iterator[Int] = {
    adjVertices.get(v) match {
      case None => new Stack[Int]
      case _ => adjVertices.get(v).get
    }
  }
}

/* Builds the Graph */
class GraphBuilder {

  var numOfVertices: Int = 0
  var adjVertices: Map[Int, Stack[Int]] = Map()

  def addEdge(v: Int, w: Int): GraphBuilder = {

    val adjVerticesOf = adjVertices.get(v) match {
      case None => new Stack[Int]
      case _ => adjVertices.get(v).get
    }
    adjVerticesOf.push(w)
    adjVertices += v -> adjVerticesOf
    numOfVertices = if (v != w) numOfVertices + 1 else numOfVertices + 2
    this
  }

  def build: Graph = new Graph(numOfVertices, adjVertices)
}
