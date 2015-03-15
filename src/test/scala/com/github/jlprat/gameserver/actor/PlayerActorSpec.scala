package com.github.jlprat.gameserver.actor

import java.util.concurrent.TimeUnit

import akka.actor.{Props, ActorSystem}
import akka.testkit._
import com.github.jlprat.gameserver.actors.Player
import com.github.jlprat.gameserver.model.{Card, Hand}
import com.github.jlprat.gameserver.protocol.ClientProtocol._
import com.github.jlprat.gameserver.protocol.Protocol._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration.FiniteDuration

/**
 * Specification on how the player should behave
 * Created by josep on 2/22/15.
 */
class PlayerActorSpec (_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("GameServerSpec"))

  val tableActorProbe = TestProbe()
  val clientActorProbe = TestProbe()
  val playerHand = Hand(List.tabulate(5)(elem => Card(elem, elem, "blue")))

  def giveMeAPlayerActor(id: Int = 1) : TestActorRef[Player] = {
    TestActorRef(Props(classOf[Player], id, tableActorProbe.ref, clientActorProbe.ref))
  }

  def giveMeAPlayerInTurn(id: Int = 1) : TestActorRef[Player] = {
    val playerActor = giveMeAPlayerActor(id)
    playerActor ! TakenCards(playerHand, playerId = id)
    clientActorProbe.expectMsg(Out.ReceiveCard(playerHand, playerId = id))
    playerActor ! NextTurn(playerId = id)
    clientActorProbe.expectMsg(Out.PlayerInTurn(playerId = id))
    assert(playerActor.underlyingActor.playersHand === playerHand)
    playerActor
  }

  "A player starts without cards," when {
    "receives any message except a TakenCards" must {
      val playerActor = giveMeAPlayerActor()
      "log the message received" in {
        val message = NextTurn(2)
        playerActor ! message
        EventFilter.error(message = s"Unknown message $message", occurrences = 1)
        assert(playerActor.underlyingActor.playersHand === Hand())
      }
      "send a wrong message to the client if the message comes from the them" in {
        playerActor ! In.PlayCardRequest(Card(1,1,"blue"))
        clientActorProbe.expectMsg(Out.WrongAction)
        assert(playerActor.underlyingActor.playersHand === Hand())
      }
    }
    "receives a TakenCards message" when {
      val playerActor = giveMeAPlayerActor()
      val red1 = Card(1, 1, "red")
      "is for other player" must {
        "log the message received" in {
          val message = TakenCards(Hand(red1), playerId = 2)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCardOpponent(numberCards = 1, playerId = 2))
          EventFilter.info(message = s"Player ${message.playerId} receives ${message.hand.size} cards", occurrences = 1)
          assert(playerActor.underlyingActor.playersHand === Hand())
        }
      }
      "is for same player" must {
        "communicate it back to client" in {
          val message = TakenCards(Hand(red1), playerId = 1)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCard(Hand(red1), playerId = 1))
          assert(playerActor.underlyingActor.playersHand === Hand(red1))
        }
        "receive other TakenCards for other players" in {
          val message = TakenCards(Hand(Card(2,2,"red")), playerId = 2)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCardOpponent(numberCards = 1, playerId = 2))
          assert(playerActor.underlyingActor.playersHand === Hand(red1))
        }
        "ignore Incoming requests" in {
          val message = In.PlayCardRequest(card = red1)
          playerActor ! message
          clientActorProbe.expectMsg(Out.NotInTurn)
        }
        "accept LastCard calls" in {
          playerActor ! In.AnnounceLastCard
          tableActorProbe.expectMsg(AnnounceLastCard(playerId = 1))
        }
      }
    }
  }

  "After dealt, a not in turn player" can {
    val playerActor = giveMeAPlayerActor(id = 2)
    playerActor ! TakenCards(playerHand, playerId = 2)
    assert(playerActor.underlyingActor.playersHand === playerHand)
    clientActorProbe.expectMsg(Out.ReceiveCard(playerHand, playerId = 2))
    "receive Next turn messages for other players, several times" in {
      playerActor ! NextTurn(playerId = 1)
      playerActor ! NextTurn(playerId = 3)
      clientActorProbe.expectMsg(Out.PlayerInTurn(playerId = 1))
      clientActorProbe.expectMsg(Out.PlayerInTurn(playerId = 3))
    }
    val cardInHand: Card = Card(0, 0, "blue")
    "not play any card" when {
      "not in turn" must {
        "client get a notification" in {
          playerActor ! In.PlayCardRequest(cardInHand)
          clientActorProbe.expectMsg(Out.NotInTurn)
          assert(playerActor.underlyingActor.playersHand === playerHand)
        }
        "table get no message" in {
          tableActorProbe.expectNoMsg(FiniteDuration(100, TimeUnit.MILLISECONDS))
        }
      }
    }
  }
  "A player is in turn" when {
    "receives NextTurn message for thyself" in {
      val playerActor = giveMeAPlayerInTurn(2)
      assert(playerActor.underlyingActor.playersHand === playerHand)
    }
    "After this," can {
      "play a card" when {
        val playerActor = giveMeAPlayerInTurn(2)
        "not in hand, client must be notified" in {
          playerActor ! In.PlayCardRequest(Card(2, 2, "red"))
          clientActorProbe.expectMsg(Out.WrongAction)
          assert(playerActor.underlyingActor.playersHand === playerHand)
        }
        "in hand, table must get this request" in {
          val cardInHand: Card = Card(0, 0, "blue")
          playerActor ! In.PlayCardRequest(cardInHand)
          assert(playerActor.underlyingActor.playersHand === playerHand.play(cardInHand)._2)
          tableActorProbe.expectMsg(PlayCard(cardInHand, playerId = 2))
        }
      }
      "ask for more cards" in {
        val playerActor = giveMeAPlayerInTurn(2)
        playerActor ! In.TakeCardsRequest
        tableActorProbe.expectMsg(TakeCard(playerId = 2))
        assert(playerActor.underlyingActor.playersHand === playerHand)
      }
      "not announce last card" when {
        "it's not their last card" in {
          val playerActor = giveMeAPlayerInTurn(2)
          playerActor ! In.AnnounceLastCard
          tableActorProbe.expectMsg(TakeCard(playerId = 2))
          clientActorProbe.expectMsg(Out.WrongAction)
          assert(playerActor.underlyingActor.playersHand === playerHand)
        }
      }
    }
  }

  "Once a player requested to play a card" when {
    val cardPlayed = Card(1, 1, "blue")
    val handAfterPlay: Hand = playerHand.play(cardPlayed)._2
    "correctly played" must {
      val playerActor = giveMeAPlayerInTurn(3)
      playerActor ! In.PlayCardRequest(cardPlayed)
      tableActorProbe.expectMsg(PlayCard(cardPlayed, playerId = 3))
      assert(playerActor.underlyingActor.playersHand === handAfterPlay)
      "receive confirmation that the card was played" in {
        playerActor ! PlayedCard(Card(1, 1, "blue"), playerId = 3)
        clientActorProbe.expectMsg(Out.PlayedCardSuccessfully(cardPlayed, playerId = 3))
        assert(playerActor.underlyingActor.playersHand === handAfterPlay)
      }
    }
    "incorrectly played" must {
      val playerActor = giveMeAPlayerInTurn(3)
      playerActor ! In.PlayCardRequest(cardPlayed)
      tableActorProbe.expectMsg(PlayCard(cardPlayed, playerId = 3))
      assert(playerActor.underlyingActor.playersHand === handAfterPlay)
      "receive confirmation of wrong move" in {
        playerActor ! PlayedCardIllegal(Card(1, 1, "blue"), playerId = 3)
        clientActorProbe.expectMsg(Out.PlayedCardIrregularly(cardPlayed, playerId = 3))
        assert(playerActor.underlyingActor.playersHand.sort === playerHand.sort)
      }
    }
  }

  "Once a player requested some cards" when {
    "table decides to send only 1 card" must {
      val playerActor = giveMeAPlayerInTurn(3)
      playerActor ! In.TakeCardsRequest
      tableActorProbe.expectMsg(TakeCard(playerId = 3))
      assert(playerActor.underlyingActor.playersHand === playerHand)
      "send cards to player" in {
        val incomingCard = Card(10, 1, "red")
        playerActor ! TakenCards(Hand(incomingCard), playerId = 3)
        clientActorProbe.expectMsg(Out.ReceiveCard(Hand(incomingCard), playerId = 3))
        assert(playerActor.underlyingActor.playersHand === (playerHand ::: Hand(incomingCard)))
      }
    }
    "table decides to send only multiple cards" must {
      val playerActor = giveMeAPlayerInTurn(3)
      playerActor ! In.TakeCardsRequest
      tableActorProbe.expectMsg(TakeCard(playerId = 3))
      assert(playerActor.underlyingActor.playersHand === playerHand)
      "send cards to player" in {
        val incomingHand = Hand(List(Card(10, 1, "red"), Card(20, 2, "yellow")))
        playerActor ! TakenCards(incomingHand, playerId = 3)
        clientActorProbe.expectMsg(Out.ReceiveCard(incomingHand, playerId = 3))
        assert(playerActor.underlyingActor.playersHand === (playerHand ::: incomingHand))
      }
    }
  }

  "A player can chose the suit" when {
    "plays any 8" should {
      val playerActor = giveMeAPlayerInTurn(4)
      val eight: Card = Card(8, 8, "blue")
      playerActor.underlyingActor.playersHand = eight :: playerActor.underlyingActor.playersHand
      playerActor ! In.PlayCardRequest(eight)
      tableActorProbe.expectMsg(PlayCard(eight, playerId = 4))
      playerActor ! ChangeSuitRequest(playerId = 4)
      clientActorProbe.expectMsg(Out.SelectSuitRequest(playerId = 4))
      "select the suit" in {
        playerActor ! In.PlayCardRequest(Card(0, 0, "blue"))
        tableActorProbe.expectNoMsg(FiniteDuration(100, TimeUnit.MILLISECONDS))
        assert(playerActor.underlyingActor.playersHand === playerHand)
        playerActor ! In.SelectSuitRequest(suit = "yellow")
        tableActorProbe.expectMsg(ChangeSuit(suit = "yellow", playerId = 4))

        playerActor ! ChangedSuit("yellow", playerId = 4)
        clientActorProbe.expectMsg(Out.NewSuitSelected("yellow", playerId = 4))
      }

    }
  }

}
