// Hanna Grodzicka

import java.util.concurrent.Semaphore

/** Two threads increase 200 000 times shared counter by 1 (non-synchronized) */
object Zad1 extends App {
  var counter = 0 // counter variable

  def readWriteCounter(): Unit = {
    //    val incrementedCounter = counter + 1 // reading counter
    //    counter = incrementedCounter // writing to counter
    counter += 1 // shorter code
  }

  val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter)
  val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter)
  val startTime = System.nanoTime
  p.start
  q.start
  p.join
  q.join
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}


/** Two threads increase 200 000 times shared counter by 1 (synchronized) */
object Zad1b extends App {
  var counter = 0 // counter variable

  def readWriteCounter(): Unit = this.synchronized {
    counter += 1 // shorter code
  }

  val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start()
  q.start()
  p.join()
  q.join()
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}


/** Two threads increase 200 000 times shared counter by 1 (semaphore) */
object Zad1c extends App {
  var counter = 0 // counter variable
  val semaphore = new Semaphore(1)

  def readWriteCounter(): Unit = {
    semaphore.acquire()
    counter += 1 // shorter code
    semaphore.release()
  }

  val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start()
  q.start()
  p.join()
  q.join()
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}


/** Takes two blocks of code, executes them simultaneously in separate threads and returns the results as pair */
object Zad2 extends App {
  def parallelUgly[A, B](block1: => A, block2: => B): (A, B) = {
    var a: A = null.asInstanceOf[A]
    var b: B = null.asInstanceOf[B]
    val p = new Thread(() => b = block2)
    val q = new Thread(() => a = block1)
    p.start()
    q.start()
    p.join()
    q.join()
    (a, b)
  }

  def parallel[A, B](block1: => A, block2: => B): (A, B) = {
    var a: Option[A] = None
    var b: Option[B] = None
    val p = new Thread(() => a = Some(block1))
    val q = new Thread(() => b = Some(block2))
    p.start()
    q.start()
    p.join()
    q.join()
    (a.get, b.get)
  }

  println(parallel("a" + 1, "b" + 2))
  println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
  val x = 2
  println(parallel(x * 2, x - 50))
}


/** Creates a daemon thread that executes given block of code with pauses (duration in ms), up to times */
object Zad3 extends App {
  def periodically(duration: Long, times: Int)(block: => Unit): Unit = {
    val daemon = new Thread(() =>
      for (_ <- 0 until times) {
        block
        Thread.sleep(duration)
      }
    )

    daemon.setDaemon(true)
    daemon.start()
  }

  periodically(1000, 5)(print("y "))
  periodically(1000, 25)(print("x "))
  Thread.sleep(10000)
  println("Done sleeping")
}
