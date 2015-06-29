package com.github.jlprat.gameserver.become.actor

import akka.actor.{Props, ActorSystem}
import akka.testkit._
import com.github.jlprat.gameserver.become.actors.Player
import com.github.jlprat.gameserver.become.model.{Card, Hand}
import com.github.jlprat.gameserver.become.protocol.ClientProtocol._
import com.github.jlprat.gameserver.become.protocol.Protocol._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

/**
 * Specification on how the player should behave
 * Created by josep on 2/22/15.
 */
class PlayerActorSpec (_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("GameServerSpec"))

  val turnDuration = 400 milliseconds
  val eight: Card = Card(8, 8, "blue")
  val playerHand = Hand(List.tabulate(5)(elem => Card(elem, elem, "blue")))

  def giveMeAPlayerActor(id: Int = 1) : (TestActorRef[Player], TestProbe, TestProbe) = {
    val tableActorProbe = TestProbe()
    val clientActorProbe = TestProbe()
    (TestActorRef(Props(classOf[Player], id, tableActorProbe.ref, clientActorProbe.ref, turnDuration)), tableActorProbe, clientActorProbe)
  }

  def giveMeAPlayerInTurn(id: Int = 1) : (TestActorRef[Player], TestProbe, TestProbe) = {
    val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerActor(id)
    playerActor ! TakenCards(playerHand, playerId = id)
    clientActorProbe.expectMsg(Out.ReceiveCard(playerHand, playerId = id))
    playerActor ! NextTurn(playerId = id)
    clientActorProbe.expectMsg(Out.PlayerInTurn(playerId = id))
    assert(playerActor.underlyingActor.playersHand === playerHand)
    (playerActor, tableActorProbe, clientActorProbe)
  }

  "A player starts without cards," when {
    "receives any message except a TakenCards" must {
      val (playerActor, _, clientActorProbe) = giveMeAPlayerActor()
      "log the message received" in {
        val message = NextTurn(2)
        playerActor ! message
        EventFilter.error(message = s"Unknown message $message", occurrences = 1)
        assert(playerActor.underlyingActor.playersHand === Hand())
        assert(playerActor.underlyingActor.turnTimer === None)
      }
      "send a wrong message to the client if the message comes from the them" in {
        playerActor ! In.PlayCardRequest(Card(1,1,"blue"))
        clientActorProbe.expectMsg(Out.WrongAction)
        assert(playerActor.underlyingActor.playersHand === Hand())
        assert(playerActor.underlyingActor.turnTimer === None)
      }
    }
    "receives a TakenCards message" when {
      val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerActor()
      val red1 = Card(1, 1, "red")
      "is for other player" must {
        "log the message received" in {
          val message = TakenCards(Hand(red1), playerId = 2)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCardOpponent(numberCards = 1, playerId = 2))
          EventFilter.info(message = s"Player ${message.playerId} receives ${message.hand.size} cards", occurrences = 1)
          assert(playerActor.underlyingActor.playersHand === Hand())
          assert(playerActor.underlyingActor.turnTimer === None)
        }
      }
      "is for same player" must {
        "communicate it back to client" in {
          val message = TakenCards(Hand(red1), playerId = 1)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCard(Hand(red1), playerId = 1))
          assert(playerActor.underlyingActor.playersHand === Hand(red1))
          assert(playerActor.underlyingActor.turnTimer === None)
        }
        "receive other TakenCards for other players" in {
          val message = TakenCards(Hand(Card(2,2,"red")), playerId = 2)
          playerActor ! message
          clientActorProbe.expectMsg(Out.ReceiveCardOpponent(numberCards = 1, playerId = 2))
          assert(playerActor.underlyingActor.playersHand === Hand(red1))
          assert(playerActor.underlyingActor.turnTimer === None)
        }
        "ignore Incoming requests" in {
          val message = In.PlayCardRequest(card = red1)
          playerActor ! message
          clientActorProbe.expectMsg(Out.NotInTurn)
          assert(playerActor.underlyingActor.turnTimer === None)
        }
        "accept LastCard calls" in {
          playerActor ! In.AnnounceLastCard
          tableActorProbe.expectMsg(AnnounceLastCard(playerId = 1))
          assert(playerActor.underlyingActor.turnTimer === None)
        }
      }
    }
  }
  
  "A player" must {
    "be informed about the top card" when {
      "is in the inactive state" in {
        val (playerActor, _, clientActorProbe) = giveMeAPlayerActor(id = 20)
        playerActor ! TakenCards(playerHand, playerId = 20)
        assert(playerActor.underlyingActor.playersHand === playerHand)
        clientActorProbe.expectMsg(Out.ReceiveCard(playerHand, playerId = 20))
        val aCard = Card(30, 1, "yellow")
        playerActor ! TopCard(aCard)
        clientActorProbe.expectMsg(Out.TopCard(aCard))
      }
    }
  }

  "After dealt, a not in turn player" can {
    val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerActor(id = 2)
    playerActor ! TakenCards(playerHand, playerId = 2)
    assert(playerActor.underlyingActor.playersHand === playerHand)
    clientActorProbe.expectMsg(Out.ReceiveCard(playerHand, playerId = 2))
    "receive Next turn messages for other players, several times" in {
      playerActor ! NextTurn(playerId = 1)
      playerActor ! NextTurn(playerId = 3)
      clientActorProbe.expectMsg(Out.PlayerInTurn(playerId = 1))
      clientActorProbe.expectMsg(Out.PlayerInTurn(playerId = 3))
      assert(playerActor.underlyingActor.turnTimer === None)
    }
    val cardInHand: Card = Card(0, 0, "blue")
    "not play any card" when {
      "not in turn" must {
        "client get a notification" in {
          playerActor ! In.PlayCardRequest(cardInHand)
          clientActorProbe.expectMsg(Out.NotInTurn)
          assert(playerActor.underlyingActor.playersHand === playerHand)
          assert(playerActor.underlyingActor.turnTimer === None)
        }
        "table get no message" in {
          tableActorProbe.expectNoMsg(50 milliseconds)
        }
      }
    }
  }
  "A player is in turn" when {
    "receives NextTurn message for thyself" in {
      val (playerActor, _, _) = giveMeAPlayerInTurn(2)
      assert(playerActor.underlyingActor.playersHand === playerHand)
      assert(playerActor.underlyingActor.turnTimer.isDefined)
    }
    "After this," can {
      "play a card" when {
        val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(2)
        "not in hand, client must be notified" in {
          playerActor ! In.PlayCardRequest(Card(2, 2, "red"))
          clientActorProbe.expectMsg(Out.WrongAction)
          assert(playerActor.underlyingActor.playersHand === playerHand)
          assert(playerActor.underlyingActor.turnTimer.isDefined)
        }
        "in hand, table must get this request" in {
          val cardInHand: Card = Card(0, 0, "blue")
          playerActor ! In.PlayCardRequest(cardInHand)
          assert(playerActor.underlyingActor.playersHand === playerHand.play(cardInHand)._2)
          tableActorProbe.expectMsg(PlayCard(cardInHand, playerId = 2))
          assert(playerActor.underlyingActor.turnTimer.isDefined)
        }
      }
      "ask for more cards" in {
        val (playerActor, tableActorProbe, _) = giveMeAPlayerInTurn(2)
        playerActor ! In.TakeCardsRequest
        tableActorProbe.expectMsg(TakeCard(playerId = 2))
        assert(playerActor.underlyingActor.playersHand === playerHand)
        assert(playerActor.underlyingActor.turnTimer.isDefined)
      }
      "not announce last card" when {
        "it's not their last card" in {
          val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(2)
          playerActor ! In.AnnounceLastCard
          tableActorProbe.expectMsg(TakeCard(playerId = 2))
          clientActorProbe.expectMsg(Out.WrongAction)
          assert(playerActor.underlyingActor.playersHand === playerHand)
          assert(playerActor.underlyingActor.turnTimer.isDefined)
        }
      }
    }
  }

  "Once a player requested to play a card" when {
    val cardPlayed = Card(1, 1, "blue")
    val handAfterPlay: Hand = playerHand.play(cardPlayed)._2
    "correctly played" must {
      val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(3)
      playerActor ! In.PlayCardRequest(cardPlayed)
      tableActorProbe.expectMsg(PlayCard(cardPlayed, playerId = 3))
      assert(playerActor.underlyingActor.playersHand === handAfterPlay)
      "receive confirmation that the card was played" in {
        playerActor ! PlayedCard(Card(1, 1, "blue"), playerId = 3)
        clientActorProbe.expectMsg(Out.PlayedCardSuccessfully(cardPlayed, playerId = 3))
        assert(playerActor.underlyingActor.playersHand === handAfterPlay)
        assert(playerActor.underlyingActor.turnTimer.isDefined)
      }
    }
    "incorrectly played" must {
      val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(3)
      playerActor ! In.PlayCardRequest(cardPlayed)
      tableActorProbe.expectMsg(PlayCard(cardPlayed, playerId = 3))
      assert(playerActor.underlyingActor.playersHand === handAfterPlay)
      assert(playerActor.underlyingActor.turnTimer.isDefined)
      "receive confirmation of wrong move" in {
        playerActor ! PlayedCardIllegal(Card(1, 1, "blue"), playerId = 3)
        clientActorProbe.expectMsg(Out.PlayedCardIrregularly(cardPlayed, playerId = 3))
        assert(playerActor.underlyingActor.playersHand.sort === playerHand.sort)
        assert(playerActor.underlyingActor.turnTimer.isDefined)
      }
    }
  }

  "Once a player requested some cards" when {
    "table decides to send only 1 card" must {
      val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(3)
      playerActor ! In.TakeCardsRequest
      tableActorProbe.expectMsg(TakeCard(playerId = 3))
      assert(playerActor.underlyingActor.playersHand === playerHand)
      "send cards to player" in {
        val incomingCard = Card(10, 1, "red")
        playerActor ! TakenCards(Hand(incomingCard), playerId = 3)
        clientActorProbe.expectMsg(Out.ReceiveCard(Hand(incomingCard), playerId = 3))
        assert(playerActor.underlyingActor.playersHand === (playerHand ::: Hand(incomingCard)))
        assert(playerActor.underlyingActor.turnTimer.isDefined)
      }
    }
    "table decides to send only multiple cards" must {
      val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(3)
      playerActor ! In.TakeCardsRequest
      tableActorProbe.expectMsg(TakeCard(playerId = 3))
      assert(playerActor.underlyingActor.playersHand === playerHand)
      "send cards to player" in {
        val incomingHand = Hand(List(Card(10, 1, "red"), Card(20, 2, "yellow")))
        playerActor ! TakenCards(incomingHand, playerId = 3)
        clientActorProbe.expectMsg(Out.ReceiveCard(incomingHand, playerId = 3))
        assert(playerActor.underlyingActor.playersHand === (playerHand ::: incomingHand))
        assert(playerActor.underlyingActor.turnTimer.isDefined)
      }
    }
  }

  "A player can chose the suit" when {
    "plays any 8" should {
      val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(4)
      playerActor.underlyingActor.playersHand = eight :: playerActor.underlyingActor.playersHand
      playerActor ! In.PlayCardRequest(eight)
      assert(playerActor.underlyingActor.turnTimer.isDefined)
      tableActorProbe.expectMsg(PlayCard(eight, playerId = 4))
      playerActor ! ChangeSuitRequest(playerId = 4)
      clientActorProbe.expectMsg(Out.SelectSuitRequest(playerId = 4))
      "select the suit" in {
        playerActor ! In.PlayCardRequest(Card(0, 0, "blue"))
        tableActorProbe.expectNoMsg(50 milliseconds)
        assert(playerActor.underlyingActor.playersHand === playerHand)
        playerActor ! In.SelectSuitRequest(suit = "yellow")
        tableActorProbe.expectMsg(ChangeSuit(suit = "yellow", playerId = 4))

        playerActor ! ChangedSuit("yellow", playerId = 4)
        clientActorProbe.expectMsg(Out.NewSuitSelected("yellow", playerId = 4))
        assert(playerActor.underlyingActor.turnTimer.isDefined)
      }

    }
  }

  "A player" must {
    "be not in turn after getting a nextTurn for some other player" in {
      val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(5)
      assert(playerActor.underlyingActor.turnTimer.isDefined)
      playerActor ! In.TakeCardsRequest
      tableActorProbe.expectMsg(TakeCard(playerId = 5))
      val incomingCard = Card(10, 1, "red")
      playerActor ! TakenCards(Hand(incomingCard), playerId = 5)
      clientActorProbe.expectMsg(Out.ReceiveCard(Hand(incomingCard), playerId = 5))
      assert(playerActor.underlyingActor.playersHand === (playerHand ::: Hand(incomingCard)))
      val turnTimer = playerActor.underlyingActor.turnTimer
      assert(turnTimer.isDefined)
      playerActor ! NextTurn(1)
      assert(playerActor.underlyingActor.turnTimer === None)
      assert(turnTimer.forall(_.isCancelled))
      clientActorProbe.expectMsg(Out.PlayerInTurn(1))
    }
  }

  "A player times out" when {
    "didn't perform any action" must {
      "Take Cards be sent" in {
        val (playerActor, tableActorProbe, _) = giveMeAPlayerInTurn(6)
        assert(playerActor.underlyingActor.turnTimer.isDefined)
        within(450 milliseconds) {
          tableActorProbe.expectMsg(TakeCard(playerId = 6))
        }
      }
    }
    "Played an 8" must {
      "automatically pick up a color" in {
        val (playerActor, tableActorProbe, clientActorProbe) = giveMeAPlayerInTurn(6)
        assert(playerActor.underlyingActor.turnTimer.isDefined)
        playerActor.underlyingActor.playersHand = eight :: playerActor.underlyingActor.playersHand
        playerActor ! In.PlayCardRequest(eight)
        assert(playerActor.underlyingActor.turnTimer.isDefined)
        tableActorProbe.expectMsg(PlayCard(eight, playerId = 6))
        playerActor ! ChangeSuitRequest(playerId = 6)
        clientActorProbe.expectMsg(Out.SelectSuitRequest(playerId = 6))
        within(450 milliseconds) {
          tableActorProbe.expectMsg(ChangeSuit(suit = "blue", playerId = 6))
        }
      }
    }
  }

}
