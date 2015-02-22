package com.github.jlprat.gameserver.actor

import akka.actor.{Props, ActorSystem}
import akka.testkit.{EventFilter, TestProbe, ImplicitSender, TestKit}
import com.github.jlprat.gameserver.actors.Player
import com.github.jlprat.gameserver.model.Hand
import com.github.jlprat.gameserver.protocol.Protocol._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
 * Specification on how the player should behave
 * Created by josep on 2/22/15.
 */
class PlayerActorSpec (_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("GameServerSpec"))

  val tableActorProbe = TestProbe()
  val playerActor = system.actorOf(Props(classOf[Player], 1, tableActorProbe.ref))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A player starts with inactive state," when {
    "receives any message except a TakenCards" must {
      "log the message received" in {
        val message = NextTurn(2)
        playerActor ! message
        EventFilter.error(message = s"Unknown message $message", occurrences = 1)
      }
    }
    "receives a TakenCards message for other player" must{
      "log the message received" in {
        val message = TakenCards(Hand(),1)
        playerActor ! message
        EventFilter.info(message = s"Informative message is received $message", occurrences = 1)
      }
    }
  }

}
