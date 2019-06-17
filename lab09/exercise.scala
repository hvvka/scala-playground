object Zad1 extends App {
  var counter = 0 // counter variable
  
  def readWriteCounter(): Unit = {
    val incrementedCounter = counter + 1 // reading counter
    counter = incrementedCounter // writing to counter
  // counter += 1 // shorter code
  }
  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter)
  val startTime = System.nanoTime
  p.start; q.start
  p.join; q.join
  val estimatedTime = (System.nanoTime - startTime)/1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}