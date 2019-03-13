// Hanna Grodzicka


/* 1. Returns first n elements from the xs list – ugly recursion */
def takeWrong[A](n: Int, xs: List[A]): List[A] = n match {
  case x if x <= 0 => Nil
  case _ => if (xs == Nil) xs else xs.head :: take(n - 1, xs.tail)
}

/* 1. Returns first n elements from the xs list – better recursion */
def take[A](n: Int, xs: List[A]): List[A] = xs match {
  case h :: t => if (n > 0) h :: take(n - 1, t) else Nil
  case Nil => Nil
}

take(2, List(1, 2, 3, 5, 6)) == List(1, 2)
take(-2, List(1, 2, 3, 5, 6)) == Nil
take(8, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
take(3, List(1, 2, 3)) == List(1, 2, 3)


/* 2. Drops first n elements from the xs list – ugly recursion */
def dropWrong[A](n: Int, xs: List[A]): List[A] = n match {
  case x if x <= 0 => xs
  case _ => if (xs == Nil) Nil else drop(n - 1, xs.tail)
}

/* 2. Drops first n elements from the xs list – tail recursion */
def drop[A](n: Int, xs: List[A]): List[A] = xs match {
  case h :: t => if (n > 0) drop(n - 1, t) else xs
  case Nil => Nil
}

drop(2, List(1, 2, 3, 5, 6)) == List(3, 5, 6)
drop(-2, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
drop(8, List(1, 2, 3, 5, 6)) == Nil
drop(0, List(1, 2, 3)) == List(1, 2, 3)
drop(1, List(1, 2, 3)) == List(2, 3)
drop(2, List(1, 2, 3)) == List(3)
drop(3, List(1, 2, 3)) == Nil


/* 3. Returns reversed xs list – ugly recursion version */
def reverseWrong[A](xs: List[A]): List[A] = xs match {
  case h :: t => reverseWrong(t) ::: h :: Nil
  case Nil => Nil
}

/* 3. Returns reversed xs list – optimized tail recursion */
def reverse[A](xs: List[A]): List[A] = {
  def reverseIter[B](xs: List[B], accum: List[B]): List[B] =
    xs match {
      case h :: t => reverseIter(t, h :: accum)
      case Nil => accum
    }

  reverseIter(xs, Nil)
}

reverse(List(1, 2, 3)) == List(3, 2, 1)
reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(List()) == List()


/* 4. Multiplies list elements accordingly to their value – rather ugly solution, but acceptable */
val replicate: List[Int] => List[Int] = {
  case x :: xs => multiply(x) ::: replicate(xs)
  case Nil => Nil
}

def multiply(m: Int): List[Int] = {
  def multiplyx(n: Int, times: Int): List[Int] = times match {
    case x if x >= 1 => n :: multiplyx(n, times - 1)
    case x if x <= 0 => Nil
  }

  multiplyx(m, m)
}

replicate(List(1, 2, 3)) == List(1, 2, 2, 3, 3, 3)
replicate(List(1, 0, 4, -2, 3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(List()) == List()


/* 5. Calculates 3rd root of "a" estimation – tail recursion */
def root3(a: Double): Double = {
  val epsilon = 10e-15

  def loop(x: Double): Double = {
    if (math.abs(x * x * x - a) <= epsilon * math.abs(a)) x
    else loop(x + ((a / (x * x)) - x) / 3)
  }

  loop(if (a > 1) a / 3 else a)
}

root3(-8.0) == -2.0
root3(0) == 0
root3(1) == 1
root3(8) == 2
root3(17)
root3(27) == 3
root3(64) == 4
root3(125) == 5
