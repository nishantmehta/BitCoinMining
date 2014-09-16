package com.bitcoin

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.routing.RoundRobinRouter
import scala.util.Random
import scala.collection
import scala.util.control.Breaks._
import java.security.MessageDigest
import java.util.HashSet
import java.util.Collections
import scala.collection.SortedMap

import com.bitcoin.BitCoinMining._

class BitCoinMaster(numberOfWorkers: Int, prefix: String, timeInMinutes: Int) extends Actor {
  val workerRouter = context.actorOf(Props[BitCoinMiner].withRouter(RoundRobinRouter(numberOfWorkers)), name = "workerRouter")
  var bitCoins: SortedMap[String, String] = SortedMap[String, String]()(implicitly[Ordering[String]].reverse)
  var finishedCounter = 0
  def receive = {
    case ScheduleMining =>
      for (i <- 0 until numberOfWorkers) {
        val postfix = "Worker" + i
        workerRouter ! BitCoinWork("nishantmehta.n", postfix, timeInMinutes)
      }
    case BitCoinResult(bitCoinString, hashValue) => {
      bitCoins += (hashValue -> bitCoinString)
      println(bitCoinString + "\t" + hashValue)
    }
    case FinishedMining => {
      finishedCounter = finishedCounter + 1
      if (finishedCounter == numberOfWorkers - 1) {
        printBitCoins(bitCoins)
        println("stopping master" + "with " + bitCoins.size)
        context.system.shutdown()
      }
    }
  }

}