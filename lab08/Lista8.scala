// Hanna Grodzicka

import scala.reflect.ClassTag

class FullException(msg: String) extends Exception(msg)

/*
  1. Abstract class for generic mutable queues
 */
abstract class MyQueue[E] {

  @throws[FullException]
  def enqueue(x: E): Unit

  def dequeue: Unit

  @throws[NoSuchElementException]
  def first: E

  def isEmpty: Boolean

  def isFull: Boolean
}

/*
  1a. Queue implemented by cyclic array
*/
class QueueMut[E: ClassTag](val capacity: Int = 1000) extends MyQueue[E] {

  private[this] val array = new Array[E](capacity + 1)

  private[this] var f, r = 0

  override def enqueue(x: E): Unit =
    if (this.isFull) throw new FullException("Queue is full!")
    else {
      array(r) = x
      r = (r + 1) % array.length
    }

  override def dequeue: Unit =
    if (this.isEmpty) ()
    else f = (f + 1) % array.length

  override def first: E =
    if (this.isEmpty) throw new NoSuchElementException("Queue is empty!")
    else array(f)

  override def isEmpty: Boolean = f == r

  override def isFull: Boolean = (r + 1) % array.length == f

  override def toString: String = s"${array.mkString(", ")}\tf = $f\tr = $r"
}


/*
  1b. Companion object with apply and empty methods
 */
object QueueMut {

  def apply[E: ClassTag](xs: E*): QueueMut[E] = {
    val queue = QueueMut.empty(xs.size)
    xs.foreach(queue.enqueue)
    queue
  }

  def empty[E: ClassTag](capacity: Int = 1000): QueueMut[E] = new QueueMut[E](capacity)
}


object Lista8 {

  def main(args: Array[String]): Unit = {

    val e1 = 1
    val e2 = 2

    /* ways to define the queue */
    val q0 = new QueueMut
    val q1 = new QueueMut[Int]()
    val q2 = new QueueMut[Int](3)
    val q3 = QueueMut.empty()
    var q4 = QueueMut.empty[Int](3)
    val q5 = QueueMut('a', 'b', 'c')

    /* isEmpty (empty) == true */
    println(q4.isEmpty)

    /* isEmpty (enqueue (e1,q)) == false */
    q4.enqueue(e1)
    println(!q4.isEmpty)

    /* dequeue (enqueue(e1,empty)) == empty */
    q4.dequeue
    println(q4.isEmpty)

    /* dequeue (empty) == empty */
    q4.dequeue
    println(q4.isEmpty)

    /* dequeue (enqueue(e1,enqueue(e2,q))) == enqueue(e1,dequeue(enqueue(e2,q))) */
    q4 = QueueMut.empty[Int](3)
    q4.enqueue(e2); q4.enqueue(e1); q4.dequeue
    q2.enqueue(e2); q2.dequeue; q2.enqueue(e1)
    println(q4)
    println(q2)

    /* first (enqueue(e1,enqueue(e2,q))) == first(enqueue(e2,q)) */
    q4.enqueue(e2); q4.enqueue(e1)
    q2.enqueue(e2)
    println(s"${q4.first} == ${q2.first}")

    /* first (enqueue(e1,empty)) == e1 */
    q1.enqueue(e1)
    println(s"${q1.first} == $e1")

    /* first (empty) == ERROR */
    try {
      q0.first
    } catch {
      case _: NoSuchElementException => println(true)
      case _: Exception => println(false)
    }

    /* FullException */
    println(q5)
    q5.dequeue
    q5.enqueue('d')
    println(q5)
    println(q5.first)
    try {
      q5.enqueue('e')
    } catch {
      case _: FullException => println(true)
      case _: Exception => println(false)
    }
  }
}
