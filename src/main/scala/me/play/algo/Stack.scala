package me.play.algo

/**
  * Created by mangeeteden on 4/24/17.
  */
object Stack {

  def main(args: Array[String]) {

    val stack = new Stack[String]
    stack.push("A")
    stack.push("B")
    stack.push("C")
    System.out.println(stack.isEmpty)
    System.out.println(stack.pop)
  }
}

class Stack[T] extends Iterator[T] {

  private[play] var first: Node[T] = null

  override def isEmpty: Boolean = {
    return first == null
  }

  def push(item: T) {

    val oldFirst: Node[T] = first
    first = new Node[T]
    first.item = item
    first.next = oldFirst
  }

  def pop: T = {
    if (!isEmpty) {
      val item: T = first.item
      first = first.next
      return item
    }
    return ???
  }

  override def next(): T = {
    pop
  }

  override def hasNext = {
    !isEmpty
  }
}

