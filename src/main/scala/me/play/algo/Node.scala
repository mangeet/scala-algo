package me.play.algo

/**
  * Created by mangeeteden on 4/24/17.
  */
class Node[T] {

  var item: T = _
  var next: Node[T] = _

  override def toString = {
    s"$item"
  }
}
