package com.typesafe.atmos.sample

import akka.actor._
import scala.concurrent.duration._

object Sample extends App {
  val system = ActorSystem("SimpleApp")
  val pingActor = system.actorOf(Props[PingActor], "pingActor")
  implicit val exec = system.dispatcher
  system.scheduler.schedule(0 seconds, 1 seconds, pingActor, Ping)
}

case object Ping

class PingActor extends Actor {
  def receive = {
    case Ping => println("Pinged at: " + System.currentTimeMillis)
  }
}