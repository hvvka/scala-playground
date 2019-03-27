// Hanna Grodzicka

sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)


/* 1. Sums integers stored in tree nodes. */
def sumBT[A](bt: BT[Int]): Int = bt match {
  case Node(v, l, r) => v + sumBT(l) + sumBT(r)
  case Empty => 0
}

sumBT(t) == 6
sumBT(Node(1, Empty, Node(-1, Empty, Empty))) == 0
sumBT(Empty) == 0


/*
  2. Generalizes the function of adding values from nodes of a binary tree
  just as the function foldRight generalizes the function of summing list elements.
 */
def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B = {
  bt match {
    case Node(v, l, r) => f(v)(foldBT(f)(acc)(l), foldBT(f)(acc)(r))
    case Empty => acc
  }
}


/* 3a. Uses foldBT to sum integer in tree. */
def sumBTfold[A](bt: BT[Int]): Int = foldBT((v: Int) => (r: Int, l: Int) => v + r + l)(0)(bt)

sumBTfold(t) == 6
sumBTfold(Node(1, Node(2, Node(4, Empty, Empty), Node(3, Empty, Empty)), Empty)) == 10
sumBTfold(Node(1, Empty, Empty)) == 1
sumBTfold(Empty) == 0

/* 3b. Uses foldBT; returns a list of values stored in tree nodes (in an infix bypass). */
def inorderBTfold[A](bt: BT[A]): List[A] =
  foldBT((v: A) => (l: List[A], r: List[A]) => l ::: v :: r)(Nil)(bt)

inorderBTfold(t) == List(2, 3, 1)
inorderBTfold(Empty) == Nil
inorderBTfold(Node(1, Empty, Empty)) == List(1)


/* 4. Uses foldBT; applies the given function to values in all tree nodes. */
def mapBT[A, B](f: A => B)(tree: BT[A]): BT[B] =
  foldBT[A, BT[B]](v => Node(f(v), _, _): BT[B])(Empty)(tree)

mapBT((v: Int) => 2 * v)(t: BT[Int]) == Node(2, Node(4, Empty, Node(6, Empty, Empty)), Empty)
mapBT(3 * (_: Int))(Empty) == Empty
mapBT(4 + (_: Int))(Node(1, Node(-2, Empty, Empty), Node(-6, Empty, Empty))) ==
  Node(5, Node(2, Empty, Empty), Node(-2, Empty, Empty))



sealed trait Graphs[A]

case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) => i match {
  case 0 => List(3)
  case 1 => List(0, 2, 4)
  case 2 => List(1)
  case 3 => Nil
  case 4 => List(0, 2)
  case n => throw
    new NoSuchElementException("Graph g: node" + n + " doesn't exist")
})


/* 5. Checks whether there is a path between the given vertices of the graph. */
def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  def search(visited: List[A])(toVisit: List[A]): Boolean =
    toVisit match {
      case h :: t => if (visited contains h) search(visited)(t)
      else h == to || search(h :: visited)(t ::: (g succ h))
      case Nil => false
    }

  search(Nil)(List(from))
}

pathExists(g)(4, 1)
!pathExists(g)(0, 4)
val gg = Graph((i: Int) =>
  i match {
    case 0 => List(99)
    case n => throw new NoSuchElementException("Graph gg: node" + n + " doesn't exist")
  })
pathExists(gg)(0, 99)
pathExists(gg)(0, 0)
