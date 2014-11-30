package com.example

import akka.actor.ActorSystem

object GameServerMain extends App {
  val system = ActorSystem("GameServerSystem")
  // we need a referee actor
  // we also need some players who will join. In the future they will join through another actor
  system.awaitTermination()
}