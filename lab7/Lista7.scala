// Hanna Grodzicka

/** 1. Generic class for a non-modifiable covariant queue, represented by a pair of lists. */
class MyQueue[+A] private(private val in: List[A], private val out: List[A]) {

  def this() = this(Nil, Nil)

  /** empty: -> Queue */
  def empty = new MyQueue

  /** enqueue: Elem * Queue -> Queue */
  def enqueue[B >: A](elem: B): MyQueue[B] =
    if (in.nonEmpty)
      new MyQueue(in, elem :: out)
    else
      new MyQueue(List(elem), out)

  /** isEmpty: Queue -> bool */
  def isEmpty: Boolean = in.isEmpty

  /** first: Queue -> Elem */
  def first(): A = in.head

  /** dequeue: Queue -> Queue */
  def dequeue(): MyQueue[A] = in match {
    case _ :: Nil => new MyQueue(out.reverse, Nil)
    case _ :: t => new MyQueue(t, out)
    case _ => this
  }

  /** firstOption: Queue -> Option[Elem] */
  def firstOption(): Option[A] = in match {
    case h :: _ => Some(h)
    case _ => None
  }

  override def toString: String = if (in == Nil && out == Nil) "empty" else (in ++ out.reverse) mkString ", "

  override def equals(obj: Any): Boolean = obj match {
    case obj: MyQueue[A] => obj.isInstanceOf[MyQueue[A]] && this.hashCode == obj.hashCode
    case _ => false
  }

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (in == null) 0 else in.hashCode)
    result += prime * result + (if (out == null) 0 else out.hashCode)
    result
  }
}



object MyQueue {
  /* empty: -> Queue */
  def empty = new MyQueue

  /* apply: Elem* -> Queue[Elem] */
  def apply[A](xs: A*): MyQueue[A] = new MyQueue(xs.toList, Nil)
}



/** 2 */
sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]



object Lista7 {

  def main(args: Array[String]): Unit = {

    /** 1 */

    /** 4 ways to define new queue */
    val q1 = new MyQueue
    val q2 = MyQueue()
    val q3 = MyQueue.empty
    val q4 = MyQueue('a', 'b', 'c')

    val q = MyQueue()
    val e1 = 1
    val e2 = 2

    println("isEmpty (empty) == true")
    println(q.isEmpty)

    println("isEmpty (enqueue (e1,q)) == false")
    println(!q.enqueue(e1).isEmpty)

    println("dequeue (enqueue(e1,empty)) == empty")
    println(q.enqueue(e1).dequeue() == MyQueue.empty)

    println("dequeue (empty) == empty\t")
    println(q.dequeue() == MyQueue.empty)

    println("dequeue (enqueue(e1,enqueue(e2,q))) == enqueue(e1,dequeue(enqueue(e2,q)))")
    println(q.enqueue(e2).enqueue(e1).dequeue() == q.enqueue(e2).dequeue().enqueue(e1))

    println("first (enqueue(e1,enqueue(e2,q))) == first(enqueue(e2,q))")
    println(q.enqueue(e2).enqueue(e1).first() == q.enqueue(e2).first())

    println("first (enqueue(e1,empty)) == e1")
    println(q.enqueue(e1).first() == e1)

    println("first (empty) == ERROR")
    try {
      q.first()
    } catch {
      case e: NoSuchElementException => println(true)
      case e: Exception => println(false)
    }

    println("firstOption(enqueue(e1,enqueue(e2,q))) == firstOption(enqueue(e2,q))")
    println(q.enqueue(e2).enqueue(e1).firstOption() == q.enqueue(e2).firstOption())

    println("firstOption(enqueue(e1,empty)) == Some(e1)")
    println(q.enqueue(e1).firstOption() == Some(e1))

    println("firstOption(empty) == None")
    println(q.firstOption() == None)


    /** 2 */
    val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
    println(breadthBT(t) == List(1, 2, 3))

    val tt = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))
    println(breadthBT(tt) == List(1, 2, 3, 4, 5, 6))
  }

  /** 2. Applies breadth search to the tree and returns a list of values from its nodes */
  def breadthBT[A](tree: BT[A]): List[A] = {
    def breadth(queue: MyQueue[BT[A]]): List[A] =
      queue.firstOption() match {
        case Some(Node(e, l, r)) => e :: breadth(queue.dequeue().enqueue(l).enqueue(r))
        case Some(Empty) => breadth(queue.dequeue())
        case _ => Nil
      }

    breadth(MyQueue(tree))
  }
}
