package me.play.algo

/**
  * Created by mangeeteden on 4/24/17.
  */
object Queue {
  def main(args: Array[String]) {

    val queue = new Queue[String]
    queue.enqueue("A")
    queue.enqueue("B")
    queue.enqueue("C")
    System.out.println(queue.isEmpty)
    System.out.println(queue.dequeue)
  }
}

class Queue[T] {
  private var first: Node[T] = null
  private var last: Node[T] = null

  def isEmpty: Boolean = {
    return first == null
  }

  def enqueue(item: T) {
    val oldLast: Node[T] = last
    last = new Node[T]
    last.item = item
    last.next = null
    if (isEmpty) {
      first = last
    }
    else {
      oldLast.next = last
    }
  }

  def dequeue: T = {
    val item: T = first.item
    first = first.next.asInstanceOf[Node[T]]
    if (isEmpty) {
      last = null
    }
    return item
  }
}
