package com.github.jlprat.gameserver.actor

import akka.actor.{ActorRef, Props, ActorSystem}
import akka.testkit.{EventFilter, TestProbe, ImplicitSender, TestKit}
import com.github.jlprat.gameserver.actors.Player
import com.github.jlprat.gameserver.model.{Card, Hand}
import com.github.jlprat.gameserver.protocol.ClientProtocol._
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
  val clientActorProbe = TestProbe()
  val playerActor = system.actorOf(Props(classOf[Player], 1, tableActorProbe.ref, clientActorProbe.ref))
  val otherPlayerActor = system.actorOf(Props(classOf[Player], 2, tableActorProbe.ref, clientActorProbe.ref))
  val playerHand = Hand(List.tabulate(5)(elem => Card(elem, elem, "blue")))

  override def afterAll() {
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
    "receives a TakenCards message" when {
      val red1 = Card(1, 1, "red")
      "is for other player" must {
        "log the message received" in {
          val message = TakenCards(Hand(red1), playerId = 2)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCardOpponent(numberCards = 1, playerId = 2))
          EventFilter.info(message = s"Player ${message.playerId} receives ${message.hand.size} cards", occurrences = 1)
        }
      }
      "is for same player" must {
        "communicate it back to client" in {
          val message = TakenCards(Hand(red1), playerId = 1)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCard(Hand(red1), playerId = 1))
        }
        "receive other TakenCards for other players" in {
          val message = TakenCards(Hand(Card(2,2,"red")), playerId = 2)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCardOpponent(numberCards = 1, playerId = 2))
        }
        "ignore play cards requests" in {
          val message = In.PlayCardRequest(card = red1)
          playerActor ! message
          clientActorProbe.expectMsg(Out.NotInTurn)
        }
        "except LastCard calls" in {
          playerActor ! In.AnnounceLastCard
          tableActorProbe.expectMsg(AnnounceLastCard(playerId = 1))
        }
      }
    }
  }

  "After dealt, a player" can {
    otherPlayerActor ! TakenCards(playerHand, playerId = 2)
    clientActorProbe.expectMsg(Out.ReceiveCard(playerHand, playerId = 2))
    "be in turn" when {
      "receives NextTurn message" in {
        otherPlayerActor ! NextTurn(playerId = 2)
        clientActorProbe.expectMsg(Out.PlayerInTurn(playerId = 2))
      }
    }
  }
}
