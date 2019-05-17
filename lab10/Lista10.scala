// Hanna Grodzicka

import java.util.Random
import java.util.concurrent.{ArrayBlockingQueue, Semaphore}

import scala.concurrent.ExecutionContext


/*
  1. Producer / consumer program with limited cyclic buffer (from lecture)
 */
object Zad1 extends App {

  class BoundedBuffer(N: Int) {
    private var in, out, n: Int = 0
    private val elems = new Array[Int](N)

    def put(x: Int) = this.synchronized {
      while (n >= N) {
        println(s"${Thread.currentThread.getName} waiting")
        wait
      }
      elems(in) = x
      in = (in + 1) % N
      n += 1
      println(s"${Thread.currentThread.getName} putting $x")
      elems.foreach(e => print(s"$e "))
      println
      if (n == 1) notifyAll
    }

    def take: Int = this.synchronized {
      while (n == 0) {
        println(s"${Thread.currentThread.getName} waiting")
        wait
      }
      val x = elems(out)
      elems(out) = 0
      out = (out + 1) % N
      n -= 1
      elems.foreach(e => print(s"$e "))
      println
      if (n == N - 1) notifyAll
      x
    }
  }

  class Producer(name: String, buf: BoundedBuffer) extends Thread(name) {
    override def run: Unit =
      for (i <- 1 to 10) {
        println(s"$getName producing $i"); buf.put(i)
      }
  }

  class Consumer(name: String, buf: BoundedBuffer) extends Thread(name) {
    override def run =
      for (i <- 1 to 10) println(s"$getName consumed ${buf.take}")
  }

  val buf: BoundedBuffer = new BoundedBuffer(5)
  new Producer("Producer", buf).start
  new Consumer("Consumer", buf).start
}

/*
  1a. Instead of the BoundedBuffer class, the library class java.util.concurrent.ArrayBlockingQueue was used.
 */
object Zad1a extends App {

  class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (i <- 1 to 10) {
        println(s"$getName producing $i"); buf.put(i); println(buf)
      }
  }

  class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (_ <- 1 to 10) println(s"$getName consumed ${buf.take}")
  }

  val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue(5)

  new Producer("Producer", buf).start()
  new Consumer("Consumer", buf).start()
}


/*
  1b. Program from 1a with 2 producers and 3 consumers. It doesn't end.
 */
object Zad1b extends App {

  class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (i <- 1 to 10) {
        println(s"$getName producing $i"); buf.put(i); println(buf)
      }
  }

  class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
    override def run(): Unit =
      for (_ <- 1 to 10) println(s"$getName consumed ${buf.take}")
  }

  val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue(5)

  val producerCount = 2
  val consumerCount = 3

  for (i <- 1 to producerCount) {
    new Producer(s"Producer$i", buf).start()
  }

  for (i <- 1 to consumerCount) {
    new Consumer(s"Consumer$i", buf).start()
  }
}


/*
  1c. Program from 1b using ExecutionContext instead of Producer and Consumer definitions. The program ends.
 */
object Zad1c extends App {

  val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue(5)
  val ctx = ExecutionContext.global

  val producerCount = 2
  val consumerCount = 3

  for (i <- 1 to producerCount) {
    ctx.execute(() =>
      for (j <- 1 to 10) {
        println(s"Producer$i producing $j"); buf.put(j); println(buf)
      }
    )
  }

  for (i <- 1 to consumerCount) {
    ctx.execute(() => for (_ <- 1 to 10) println(s"Consumer$i consumed ${buf.take}"))
  }

  Thread.sleep(500)
}


/*
  2. Program solving the dining philosophers problem using semaphores.
 */
object Zad2 extends App {

  class Table(private val seats: Int) {

    private[this] val sticks: List[Semaphore] = List.fill(seats)(new Semaphore(1))

    private[this] val doorman = new Semaphore(seats - 1)

    private[this] val random = new Random()

    def eat(id: Int): Unit = {
      println(s"Philosopher $id entered dinning room")

      val stick1 = id
      val stick2 = (id + 1) % seats

      sticks(stick1).acquire()
      sticks(stick2).acquire()

      println(s"Philosopher $id starts eating")
      val time = countTime({
        Thread.sleep(getRandomBetween)
        sticks(stick1).release()
        sticks(stick2).release()
      })
      println(s"Philosopher $id eating time: $time ms")

      doorman.release()
      println(s"Philosopher $id left dinning room")
    }

    def meditate(id: Int): Unit = {
      val time = countTime({
        Thread.sleep(getRandomBetween)
        doorman.acquire()
      })
      println(s"Philosopher $id meditation time: $time ms")
    }

    private def countTime(task: => Unit): Long = {
      val startTime = System.nanoTime
      task
      (System.nanoTime - startTime) / 1000000
    }

    private def getRandomBetween: Int = {
      val start = 1000
      val end = 4000
      start + random.nextInt((end - start) + 1)
    }
  }

  val philosophers = 5
  val rounds = 10
  val table = new Table(philosophers)

  for (i <- 0 until philosophers) {
    println(s"Summoned philosopher $i")
    new Thread(() =>
      for (_ <- 1 to rounds) {
        table.meditate(i)
        table.eat(i)
      }
    ).start()
  }
}
