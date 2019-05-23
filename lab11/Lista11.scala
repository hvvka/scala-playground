// Hanna Grodzicka

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

/** Returns a pair of futures (uses zip) */
object Zad1A extends App {

  def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] = fut1 zip fut2

  val result1 = pairFut(Future {"This is fut1!"}, Future {1 + 1})
  println(result1.isCompleted)
  println(result1.value)

  val result2 = pairFut(Future {Thread.sleep(100); "This is fut" + 1}, Future {Thread.sleep(200); "Fut" + 1 + 1})
  println(result2.isCompleted)
  Thread.sleep(300)
  println(result2.isCompleted)
  println(result2.value)
}

/** Returns a pair of futures (uses for) */
object Zad1B extends App {

  def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for {
      x <- fut1
      y <- fut2
    } yield (x, y)

  val result1 = pairFut(Future {"This is fut1!"}, Future {1 + 1})
  println(result1.isCompleted)
  println(result1.value)

  val result2 = pairFut(Future {Thread.sleep(100); "This is fut" + 1}, Future {Thread.sleep(200); "Fut" + 1 + 1})
  println(result2.isCompleted)
  Thread.sleep(300)
  println(result2.isCompleted)
  println(result2.value)
}


/** Method exists on type Future[T] (uses promise) */
object Zad2A extends App {

  implicit class FutureOps[T](val self: Future[T]) {

    def exists(p: T => Boolean): Future[Boolean] = {
      val promise = Promise[Boolean]
      self onComplete {
        case Success(value) => promise.success(p(value))
        case Failure(_) => promise.success(false)
      }
      promise.future
    }
  }

  val success = Future {Thread.sleep(1000); 1}.exists(_ > 0)
  val failure = Future {-1}.exists(_ > 0)
  val exception = Future {1/0}.exists(_ > 0)

  println(Await.result(success, 1.second))
  println(failure.value)
  println(exception.value)
}

/** Method exists on type Future[T] (uses map) */
object Zad2B extends App {

  implicit class FutureOps[T](val self: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] = self map p recover { case _ => false }
  }

  val success = Future {Thread.sleep(1000); 1}.exists(_ > 0)
  val failure = Future {-1}.exists(_ > 0)
  val exception = Future {1 / 0}.exists(_ > 0)

  println(Await.result(success, 1.second))
  println(failure.value)
  println(exception.value)
}


/** Asynchronously counts words in text files from given directory */
object WordCount {

  def main(args: Array[String]) {
    val path = "resources/"
    val promiseOfFinalResult = Promise[Seq[(String, Int)]]

    // Tu oblicz promiseOfFinalResult
    scanFiles(path) onComplete {
      case Success(fileNames) => processFiles(fileNames) onComplete {
        case Success(wordCountSeq) => promiseOfFinalResult.success(wordCountSeq.sortBy(_._2))
        case Failure(countingError) => promiseOfFinalResult.failure(countingError)
      }
      case Failure(scanningError) => promiseOfFinalResult.failure(scanningError)
    }

    promiseOfFinalResult.future onComplete {
      case Success(result) => result foreach println
      case Failure(t) => t.printStackTrace()
    }

    Thread.sleep(5000)
  }

  /** Calculates the number of words in each file from the input sequence */
  private def processFiles(fileNames: Seq[String]): Future[Seq[(String, Int)]] =
    Future.sequence(fileNames.map(name => processFile(name)))

  /** Calculates the number of words in the given file */
  private def processFile(fileName: String): Future[(String, Int)] = {
    val wordCount = scala.io.Source.fromFile(fileName)
      .getLines()
      .flatMap(_.split(" "))
      .size
    Future { (fileName, wordCount) }
  }

  /** Returns the sequence of file names from the specified directory */
  private def scanFiles(docRoot: String): Future[Seq[String]] = Future {
    new java.io.File(docRoot).list.map(docRoot + _)
  }
}