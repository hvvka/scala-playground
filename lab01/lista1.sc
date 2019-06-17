// Hanna Grodzicka


/** 1. sums up list elements */
def suma: List[Double] => Double = (xs: List[Double]) => {
  if (xs == Nil) 0
  else xs.head + suma(xs.tail)
}

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6


/** 2. Returns a list of first and last elements on the list */
def ends[A](xs: List[A]): (A, A) = {
  if (xs == Nil) throw new NoSuchElementException("Empty list!")
  else if (xs.tail == Nil) (xs.head, xs.head)
  else (xs.head, ends(xs.tail)._2)
}

ends(List(1, 2, 3, 5)) == (1, 5)
ends(List("aaa", "bbb")) == ("aaa", "bbb")
ends(List(1)) == (1, 1)
try {
  ends(List())
} catch {
  case e: NoSuchElementException => true
  case e: Exception => false
}


/** 3. Checks if the list is sorted */
def posortowana: List[Int] => Boolean = (xs: List[Int]) => {
  (xs == Nil || xs.tail == Nil) ||
    (xs.head <= xs.tail.head && posortowana(xs.tail))
}

posortowana(List(1, 3, 3, 5, 6, 7)) == true
posortowana(List())
posortowana(List(1, 2, 3))
posortowana(List(1))
!posortowana(List(3, 2, 1))
!posortowana(List(3, 2))


/** 4. Concatenates list elements with provided separator */
def glue: (List[String], String) => String = (xs: List[String], sep: String) => {
  if (xs == Nil) ""
  else if (xs.tail == Nil) xs.head
  else xs.head + sep + glue(xs.tail, sep)
}

glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(Nil, "-") == ""
glue(List("A", "B", "C"), "--") == "A--B--C"
glue(List("a"), "11111") == "a"
glue(List(), "0000") == ""
