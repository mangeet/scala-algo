package me.play.algo

import scala.collection.mutable

/**
  * Created by mangeeteden on 4/24/17.
  */

object Graph {

  def main(args: Array[String]): Unit = {

    val graph = new GraphBuilder().addEdge(1, 2).addEdge(2, 3).addEdge(1, 4).build

    println(s"Vertices: ${graph.vertices}")
    val itr = graph.adjTo(1)
    while(itr.hasNext) {
      println(itr.next)
    }
  }
}

/* Graph definition */
class Graph[T](val vertices: mutable.Set[T], adjVertices: Map[T, Stack[T]]) {

  val numOfVertices = vertices.size

  def adjTo(v: T): Iterator[T] = {
    adjVertices.get(v) match {
      case None => new Stack[T]
      case _ => adjVertices.get(v).get
    }
  }
}

/* Builds the Graph */
class GraphBuilder[T] {

  var numOfVertices: Int = 0
  var vertices: mutable.Set[T] = mutable.LinkedHashSet()
  var adjVertices: Map[T, Stack[T]] = Map()

  def addEdge(v: T, w: T): GraphBuilder[T] = {

    val adjVerticesOf = adjVertices.get(v) match {
      case None => new Stack[T]
      case _ => adjVertices.get(v).get
    }
    adjVerticesOf.push(w)
    adjVertices += v -> adjVerticesOf
    vertices += (v, w)
    this
  }

  def build: Graph[T] = new Graph[T](vertices, adjVertices)
}
