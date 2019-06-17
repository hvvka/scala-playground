// Hanna Grodzicka

/** Gets last list element */
def last[A](xs: List[A]): A = {
  if (xs == Nil) throw new Exception("Empty list!")
  else if (xs.tail == Nil) xs.head
  else last(xs.tail)
}

last(List(1, 2, 3, 5))
last(List("a", 123, Nil, "last"))
last(List("One element"))
last(List())
