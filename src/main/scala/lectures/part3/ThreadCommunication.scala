package lectures.part3

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {

  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0
    def set(newValue: Int): Unit = value = newValue
    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[Consumer] waiting...")
      while (container.isEmpty) { // Computing waste
        println("[Container] actively waiting....")
      }
      println("[Consumer] I hve consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[Producer] computing....")
      Thread.sleep(500)
      val value = 42
      println("[Producer] I have produced the value " + value)
      container.set(value)
    })
    consumer.start()
    producer.start()
  }
 // naiveProdCons()

  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[Consumer] waiting...")
      container.synchronized({
        container.wait()
      })
      println("[Consumer] I have consumed" + container.get)
    })

    def producer = new Thread(() => {
      println("[producer] Computing")
      Thread.sleep(2000)
      val value = 42

      container.synchronized{
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }})

    consumer.start()
    producer.start()
  }

 smartProdCons()

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()
      while(true){
        if(buffer.isEmpty){
          println("[Consumer] buffer empty, waiting...")
          buffer.wait()
        }

        val x = buffer.dequeue()
        println("[Consumer] consumed " + x)
      }
      Thread.sleep(random.nextInt(500))
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while(true){
        buffer.synchronized{
          if (buffer.size == capacity) {
            println("[Producer] buffer is full, waiting")
            buffer.wait()
          }
        }
      }
    })
  }


}
