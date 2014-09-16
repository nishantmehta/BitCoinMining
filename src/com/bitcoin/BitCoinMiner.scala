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

class BitCoinMiner extends Actor {
  val random = new scala.util.Random
  def receive = {
    case BitCoinWork(prefix, postfix, timeInMinutes) => {
      val startTime = System.currentTimeMillis();
      val timeDuration = timeInMinutes * 60 * 1000;
      while (true) {
        if (System.currentTimeMillis() - startTime > timeDuration) {
          println("breaking")
          sender ! FinishedMining
          break
        }
        val variableString = randomString("abcdefghijklmnopqrstuvwxyz0123456789")(randomStringLenght)
        val bitCoinString = prefix + variableString + postfix
        val hashCode = hex_digest(bitCoinString)
        if (checkIfValidBitCoin(hashCode, minimumZeros)) {
          sender ! BitCoinResult(bitCoinString, hashCode)
        }
      }

    }
  }
  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
}