// Hanna Grodzicka


// 1a
def existsA[A](xs: List[A])(p: A => Boolean): Boolean = xs match {
  case h :: t => p(h) || existsA(t)(p)
  case Nil => false
}

existsA(List(5, 1, 2, 3))(_ == 2)
existsA(List(1))(_ == 1)
!existsA(List(1, 2, 3))(_ == 4)
!existsA(List())(_ == 11)

// 1b
def existsB[A](xs: List[A])(p: A => Boolean): Boolean = {
  xs.foldLeft(false)(_ || p(_))
}

existsB(List(5, 1, 2, 3))(_ == 2)
existsB(List(1))(_ == 1)
!existsB(List(1, 2, 3))(_ == 4)
!existsB(List())(_ == 11)

// 1c
def existsC[A](xs: List[A])(p: A => Boolean): Boolean = {
  xs.foldRight(false)(p(_) || _)
}

existsC(List(5, 1, 2, 3))(_ == 2)
existsC(List(1))(_ == 1)
!existsC(List(1, 2, 3))(_ == 4)
!existsC(List())(_ == 11)



// 2
def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  xs.foldRight(List[A]())((h, acc) => if (p(h)) h :: acc else acc)


filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ > 3) == List(7, 7, 8, 4, 6, 9)
filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ < 4) == List(2, 1, 3, 1)
filter(List(1))(_ == 1) == List(1)
filter(List())(_ != 0) == List()



// 3a
def remove1[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
  case h :: t => if (p(h)) t else h :: remove1(t)(p)
  case Nil => List()
}

remove1(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove1(List(1, 3, 5))(_ == 2) == List(1, 3, 5)
remove1(List())(_ == 2) == List()

// 3b
def remove2[A](xs: List[A])(p: A => Boolean): List[A] = {
  def remove2Iter(acc: List[A], xs: List[A]): List[A] = {
    xs match {
      case h :: t => if (p(h)) t.reverse_:::(acc) else remove2Iter(h :: acc, t)
      case Nil => acc.reverse
    }
  }

  remove2Iter(List(), xs)
}

remove2(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove2(List(1, 3, 5))(_ == 2) == List(1, 3, 5)
remove2(List())(_ == 2) == List()



// 4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) = {
  def splitAtIter(x1: List[A], x2: List[A])(n: Int): (List[A], List[A]) = x2 match {
    case h :: t => if (n >= 1) splitAtIter(h :: x1, t)(n - 1) else (x1.reverse, x2)
    case Nil => (xs, Nil)
  }

  splitAtIter(Nil, xs)(n)
}

splitAt(List('a', 'b', 'c', 'd', 'e'))(2) == (List('a', 'b'), List('c', 'd', 'e'))
splitAt(List('a', 'b', 'c', 'd', 'e'))(0) == (List(), List('a', 'b', 'c', 'd', 'e'))
splitAt(List('a', 'b', 'c', 'd', 'e'))(-2) == (List(), List('a', 'b', 'c', 'd', 'e'))
splitAt(List('a', 'b', 'c', 'd', 'e'))(3) == (List('a', 'b', 'c'), List('d', 'e'))
splitAt(List('a', 'b', 'c', 'd', 'e'))(5) == (List('a', 'b', 'c', 'd', 'e'), List())
splitAt(List('a', 'b', 'c', 'd', 'e'))(7) == (List('a', 'b', 'c', 'd', 'e'), List())

