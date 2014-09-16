package com.bitcoin;

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

/*
 * This file has the master
 */
object BitCoinMining extends App {

  sealed trait BitCoinMessage
  case class ScheduleMining() extends BitCoinMessage
  case class BitCoinWork(prefix: String, postfix: String, timeInMinutes: Int) extends BitCoinMessage
  case class BitCoinResult(bitCoinString: String, hashValue: String) extends BitCoinMessage
  case class FinishedMining() extends BitCoinMessage

  val randomStringLenght = 7
  val minimumZeros = 1
  var numberOfMinutesToRun: Int = 1
  var numberOfWorkers = 3

  startMining()

  def startMining() {
    if (args.size > 2) {
      numberOfMinutesToRun = Integer.parseInt(args(0))
      numberOfWorkers = Integer.parseInt(args(1))
    }
    val system = ActorSystem("BitCoinMining")
    val master = system.actorOf(Props(new BitCoinMaster(numberOfWorkers, "nishantmehta.n", numberOfMinutesToRun)), name = "master")
    master ! ScheduleMining
  }

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

  def hex_digest(s: String): String = {
    val sha = MessageDigest.getInstance("SHA-256")
    sha.digest(s.getBytes)
      .foldLeft("")((s: String, b: Byte) => s +
        Character.forDigit((b & 0xf0) >> 4, 16) +
        Character.forDigit(b & 0x0f, 16))
  }
  def checkIfValidBitCoin(bitCoinString: String, minumumZeros: Int): Boolean = {
    var lead = "0"
    for (i <- 1 until minumumZeros) {
      lead = lead + "0";
    }
    return bitCoinString.startsWith(lead);
  }

  def printBitCoins(bitCoins: SortedMap[String, String]) {
    bitCoins foreach {
      case (key, value) => {
        println(value + "\t" + key)
      }
    }
  }
} 