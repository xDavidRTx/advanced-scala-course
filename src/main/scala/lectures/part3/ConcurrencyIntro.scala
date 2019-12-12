package lectures.part3

import java.util.concurrent.Executors

object ConcurrencyIntro extends App {

  // JVM threads
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  aThread.start() // gives the signal to the JVM to start a JVM thread
  // creates a JVM thread => OS thread

  aThread.join() // blocks until aThread finishes running

  val threadHello = new Thread(() => (1 to 5).foreach(_=>println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_=>println("goodbye")))
  threadHello.start()
  threadGoodbye.start()
  // they run in random order =/

  //executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("Something in tha pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println("1 second nap")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost awake!")
    Thread.sleep(1000)
    println("2 second nap")
  })
  pool.shutdown()

  // This will blowww it not accept any more requests
  //pool.execute(() => {
    //println("almost awake!")
  //})

 // pool.shutdownNow() //this kills everything
  println(pool.isShutdown)

  def runInParallel = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)
  }

  for (_ <- 1 to 10000) runInParallel
  // race condition



}

object ConcurrencyIntro2 extends App {
  class BankAccount(var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
//    println("I''ve bought "+ thing)
//    println("my account is now " + account)
  }

  for(_ <- 1 to 1000000) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => buy(account, "shoes", 3000))
    val thread2 = new Thread(() => buy(account, "iPhone", 4000))

    thread1.start()
    thread2.start()
    Thread.sleep(10)
    if (account.amount != 43000) println("AHA: " + account.amount) // At Random time e buy without paying =O
  }
}
