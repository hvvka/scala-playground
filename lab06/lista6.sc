// Hanna Grodzicka


var count = 0
while (count < 5) {
  println(count)
  count += 1
}

/* 1. Custom definition of classic while loop (same as above) */
def whileLoop(condition: => Boolean)(statement: => Unit): Unit =
  if (condition) {
    statement
    whileLoop(condition)(statement)
  }

var c = 0
whileLoop(c < 5) {
  println(c)
  c += 1
}



/* 2. Returns a stream in which each element is repeated k times */
def lrepeat[A](k: Int)(stream: Stream[A]): Stream[A] = {
  stream.flatMap(x => {
    1 to k
  }.map(_ => x))
}

def lrepeat1[A](k: Int)(stream: Stream[A]): Stream[A] = {
  stream.flatMap(x => Stream.fill(k)(x))
}

// fully custom
def lrepeat2[A](k: Int)(stream: Stream[A]): Stream[A] = {
  def llrepeat2(n: Int)(s: Stream[A]): Stream[A] =
    s match {
      case h #:: t => if (n > 0) h #:: llrepeat2(n - 1)(s) else lrepeat2(k)(t)
      case Stream.Empty => s
    }

  llrepeat2(k)(stream)
}

(lrepeat(3)(Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
(lrepeat1(3)(Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
(lrepeat2(3)(Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
lrepeat2(2)(Stream("a", "b", "c")).toList == List("a", "a", "b", "b", "c", "c")
lrepeat2(2)(Stream.empty).toList == List()



/* 3. Polymorphic lazy binary trees */
sealed trait lBT[+A]

case object LEmpty extends lBT[Nothing]

case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]


/* 
  3a. Returns a stream with all the node values of a lazy binary tree.
  Applies breadth search. The queue is represented as a regular list.
 */
def lBreadth[A](ltree: lBT[A]): Stream[A] = {
  def llBreadth(queue: List[lBT[A]]): Stream[A] = queue match {
    case LNode(e, l, r) :: t => e #:: llBreadth(t ::: List(l(), r()))
    case LEmpty :: t => llBreadth(t)
    case Nil => Stream.Empty
  }

  llBreadth(List(ltree))
}

val tree = LNode(1,
  () => LNode(2,
    () => LNode(4,
      () => LEmpty, () => LEmpty),
    () => LEmpty),
  () => LNode(3,
    () => LNode(5,
      () => LEmpty,
      () => LNode(6,
        () => LEmpty, () => LEmpty)),
    () => LEmpty))

lBreadth(tree).toList == List(1, 2, 3, 4, 5, 6)
lBreadth(LEmpty) == Stream.empty


/* 3b. Constructs an infinite lazy binary tree with a root value of n and with two subtrees */
def lTree(n: Int): lBT[Int] = {
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}

lBreadth(lTree(1)).take(10).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
lBreadth(lTree(2)).take(7).toList == List(2, 4, 5, 8, 9, 10, 11)
