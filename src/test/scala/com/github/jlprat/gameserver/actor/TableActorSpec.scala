package com.github.jlprat.gameserver.actor

import akka.actor.{Props, ActorSystem}
import akka.testkit._
import com.github.jlprat.gameserver.become.actors.Table
import com.github.jlprat.gameserver.become.model._
import com.github.jlprat.gameserver.become.protocol.ClientProtocol.Out.WrongAction
import com.github.jlprat.gameserver.become.protocol.Protocol._
import scala.concurrent.duration._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
 * Specification on how the table actor should work
 * Created by josep on 3/21/15.
 */
class TableActorSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("TableGameServerSpec"))

  def giveMeATable(numberPlayers: Int, seed: Long): (TestActorRef[Table], List[(TestProbe, Int)]) = {
    val players = List.tabulate(numberPlayers)(x => (TestProbe(), x))
    (TestActorRef(Props(classOf[Table], players.map(x => (x._1.ref, x._2)), seed)), players)
  }

  def giveMeAnInitTable(numberPlayers: Int, seed: Long): (TestActorRef[Table], List[(TestProbe, Int)]) = {
    val (table, playersTuple) = giveMeATable(numberPlayers, seed)
    table ! Table.Initialize
    playersTuple.foreach {
      case (probe, _) =>
        List.range(0, numberPlayers).foreach(_ => probe.expectMsgClass(classOf[TakenCards]))
        probe.expectMsgClass(classOf[TopCard])
        probe.expectMsgClass(classOf[NextTurn])
    }
    (table, playersTuple)
  }

  val initialTopCard = Card(50, 9, "green")

  "After creation, a table" should {
    val (table, playerProbes) = giveMeATable(3, 1)
    table ! Table.Initialize
    "deal cards to players" in {
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(TakenCards(Hand(List(Card(32, 4, "red"), Card(37, 9, "red"), Card(6, 4, "blue"),
            Card(19, 4, "yellow"), Card(41, 13, "red"), Card(33, 5, "red"))), playerId = 0))
          probe.expectMsg(TakenCards(Hand(List(Card(13, 11, "blue"), Card(1, 0, "joker"), Card(35, 7, "red"),
            Card(53, 12, "green"), Card(23, 8, "yellow"), Card(36, 8, "red"))), playerId = 1))
          probe.expectMsg(TakenCards(Hand(List(Card(16, 1, "yellow"), Card(44, 3, "green"), Card(22, 7, "yellow"),
            Card(2, 0, "joker"), Card(46, 5, "green"), Card(10, 8, "blue"))), playerId = 2))
      }
    }
    "put a card in the top of the discard pile" in {
      table.underlyingActor.discardPile.topCard.foreach(topCard => {
        assert(topCard === initialTopCard)
      })
      playerProbes.foreach {
        case (probe, _) => probe.expectMsg(TopCard(initialTopCard))
      }
    }
    "decide who is the first player in turn" in {
      playerProbes.foreach {
        case (probe, _) => probe.expectMsg(NextTurn(0))
      }
    }
  }

  "Several cards must be drawn" when {
    "the top card is a special card" in {
      val (table, playerProbes) = giveMeATable(2, 1231)
      table ! Table.Initialize
      //ignore Taken Cards messages
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsgClass(classOf[TakenCards])
          probe.expectMsgClass(classOf[TakenCards])
      }
      assert(table.underlyingActor.discardPile.cards.size == 2)
      assert(table.underlyingActor.deck.size === 40)
      playerProbes.foreach {
        case (probe, _) => probe.expectMsg(TopCard(Card(21, 6, "yellow")))
      }
    }
  }

  "Player receives Wrong Action" when {
    "table is initialized and player is not in turn" when {
      "asks for cards" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        table ! TakeCard(playerId = 1)
        playerProbes.foreach {
          case (probe, id) if id == 1 => probe.expectMsg(WrongAction)
          case (probe, _) => probe.expectNoMsg(50 milliseconds)
        }
      }
      "plays any card" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        table ! PlayCard(Card(32, 4, "red"), playerId = 1)
        playerProbes.foreach {
          case (probe, id) if id == 1 => probe.expectMsg(WrongAction)
          case (probe, _) => probe.expectNoMsg(50 milliseconds)
        }
      }
    }
  }

  "Player receives only 1 card" when {
    "player Takes a card and they are in turn" in {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table ! TakeCard(playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(TakenCards(Hand(Card(4, 2, "blue")), playerId = 0))
          probe.expectMsg(NextTurn(1))
      }
      table.underlyingActor.discardPile.topCard.foreach(topCard => {
        assert(topCard === initialTopCard)
      })
      assert(table.underlyingActor.deck.size === 34)
    }
  }

  "Given a 9 Green, a Player" can {
    "play a normal card successfully if in turn" when {
      "card is green" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        //Top card is 9 "green"
        val playedCard = Card(-49, 4, "green")
        table ! PlayCard(playedCard, playerId = 0)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
            probe.expectMsg(NextTurn(1))
        }
        table.underlyingActor.discardPile.topCard.foreach(topCard => {
          assert(topCard === playedCard)
        })
        assert(table.underlyingActor.deck.size === 35)
      }
      "card is 9" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        //Top card is 9 "green"
        val playedCard = Card(-50, 9, "blue")
        table ! PlayCard(playedCard, playerId = 0)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
            probe.expectMsg(NextTurn(1))
        }
        table.underlyingActor.discardPile.topCard.foreach(topCard => {
          assert(topCard === playedCard)
        })
        assert(table.underlyingActor.deck.size === 35)
      }
      "card is a joker" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        //Top card is 9 "green"
        val playedCard = Card(-51, 0, "joker")
        table ! PlayCard(playedCard, playerId = 0)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
            probe.expectMsg(NextTurn(1))
        }
        table.underlyingActor.discardPile.topCard.foreach(topCard => {
          assert(topCard === playedCard)
        })
        assert(table.underlyingActor.deck.size === 35)
      }
    }
  }

  "Given a 9 Green, a Player" can {
    "play special cards" when {
      "is an 8 card" which {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        //Top card is 9 "green"
        val playedCard = Card(-52, 8, "blue")
        table ! PlayCard(playedCard, playerId = 0)
        playerProbes.foreach {
          case (probe, 0) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
            probe.expectMsg(ChangeSuitRequest(0))
          case (probe, _) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
        }
        table.underlyingActor.discardPile.topCard.foreach(topCard => {
          assert(topCard === playedCard)
        })
        assert(table.underlyingActor.deck.size === 35)
      }
      "is an Ace" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        //Top card is 9 "green"
        val playedCard = Card(-53, 1, "green")
        table ! PlayCard(playedCard, playerId = 0)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
            probe.expectMsg(SkipPlayer(playerId = 1))
            probe.expectMsg(NextTurn(playerId = 2))
        }
        table.underlyingActor.discardPile.topCard.foreach(topCard => {
          assert(topCard === playedCard)
        })
        assert(table.underlyingActor.activePlayerIndex === 2)
      }
      "is a 10" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        //Top card is 9 "green"
        val playedCard = Card(-54, 10, "green")
        table ! PlayCard(playedCard, playerId = 0)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
            probe.expectMsg(PlayAgain(playerId = 0))
            probe.expectMsg(NextTurn(playerId = 0))
        }
        table.underlyingActor.discardPile.topCard.foreach(topCard => {
          assert(topCard === playedCard)
        })
        assert(table.underlyingActor.activePlayerIndex === 0)
      }
      "is a Jack" in {
        val (table, playerProbes) = giveMeAnInitTable(3, 1)
        //Top card is 9 "green"
        val playedCard = Card(-55, 11, "green")
        table ! PlayCard(playedCard, playerId = 0)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCard(playedCard, playerId = 0))
            probe.expectMsg(ChangeDirection(clockwise = false))
            probe.expectMsg(NextTurn(playerId = 2))
        }
        table.underlyingActor.discardPile.topCard.foreach(topCard => {
          assert(topCard === playedCard)
        })
        assert(table.underlyingActor.activePlayerIndex === 2)
      }
    }
  }

  //Change Suit consequences
  "After playing an 8, a Player" must {
    "select a suit" which {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table.underlyingActor.context.become(table.underlyingActor.waitingForSuit)
      table ! ChangeSuit("red", playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(ChangedSuit("red", playerId = 0))
          probe.expectMsg(NextTurn(playerId = 1))
      }
      "will determine which card can be played afterwards" in {
        val wrongCard = Card(-55, 3, "blue")
        table ! PlayCard(wrongCard, playerId = 1)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCardIllegal(wrongCard, playerId = 1))
        }
        val rightCard = Card(-56, 3, "red")
        table ! PlayCard(rightCard, playerId = 1)
        playerProbes.foreach {
          case (probe, _) =>
            probe.expectMsg(PlayedCard(rightCard, playerId = 1))
            probe.expectMsg(NextTurn(playerId = 2))
        }
      }
    }
  }

  "An 8" can {
    "not be played on top of another 8" in {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table.underlyingActor.context.become(table.underlyingActor.overriddenSuit("blue"))
      val eight = Card(-57, 8, "blue")
      table ! PlayCard(eight, playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(PlayedCardIllegal(eight, playerId = 0))
      }
    }
  }

  //Take 2 consequences
  "After playing a 2, the next player" can {
    "not play any regular card" in {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table.underlyingActor.context.become(table.underlyingActor.withPenaltyCards)
      table.underlyingActor.penaltyCards = 2
      table.underlyingActor.discardPile = Card(-58, 2, "blue") :: table.underlyingActor.discardPile
      val regularCard = Card(-59, 3, "blue")
      table ! PlayCard(regularCard, playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(PlayedCardIllegal(regularCard, playerId = 0))
      }
    }
    "play another 2" in {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table.underlyingActor.context.become(table.underlyingActor.withPenaltyCards)
      table.underlyingActor.penaltyCards = 2
      table.underlyingActor.discardPile = Card(-58, 2, "blue") :: table.underlyingActor.discardPile
      val another2 = Card(-60, 2, "red")
      table ! PlayCard(another2, playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(NextTurn(playerId = 1))
      }
      assert(table.underlyingActor.penaltyCards == 4)
    }
    "take cards" in {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table.underlyingActor.context.become(table.underlyingActor.withPenaltyCards)
      table.underlyingActor.penaltyCards = 2
      table.underlyingActor.discardPile = Card(-58, 2, "blue") :: table.underlyingActor.discardPile
      table ! TakeCard(playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(TakenCards(Hand(List(Card(4, 2, "blue"), Card(11, 9, "blue"))), playerId = 0))
          probe.expectMsg(NextTurn(playerId = 0))
      }
    }
  }

  "After taking penalty cards, a player" can {
    "play any regular card" in {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table.underlyingActor.context.become(table.underlyingActor.withPenaltyCards)
      table.underlyingActor.penaltyCards = 2
      table.underlyingActor.discardPile = Card(-61, 2, "blue") :: table.underlyingActor.discardPile
      table ! TakeCard(playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(TakenCards(Hand(List(Card(4, 2, "blue"), Card(11, 9, "blue"))), playerId = 0))
          probe.expectMsg(NextTurn(playerId = 0))
      }
      val card = Card(-63, 5, "blue")
      table ! PlayCard(card, playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(PlayedCard(card, playerId = 0))
          probe.expectMsg(NextTurn(playerId = 1))
      }
    }
    "take yet another card" in {
      val (table, playerProbes) = giveMeAnInitTable(3, 1)
      table.underlyingActor.context.become(table.underlyingActor.withPenaltyCards)
      table.underlyingActor.penaltyCards = 2
      table.underlyingActor.discardPile = Card(-61, 2, "blue") :: table.underlyingActor.discardPile
      table ! TakeCard(playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(TakenCards(Hand(List(Card(4, 2, "blue"), Card(11, 9, "blue"))), playerId = 0))
          probe.expectMsg(NextTurn(playerId = 0))
      }
      table ! TakeCard(playerId = 0)
      playerProbes.foreach {
        case (probe, _) =>
          probe.expectMsg(TakenCards(Hand(List(Card(38, 10, "red"))), playerId = 0))
          probe.expectMsg(NextTurn(playerId = 1))
      }
    }
  }

}
