package com.github.jlprat.gameserver.actor

import akka.actor.{Props, ActorSystem}
import akka.testkit._
import com.github.jlprat.gameserver.actors.Table
import com.github.jlprat.gameserver.model._
import com.github.jlprat.gameserver.protocol.Protocol.{NextTurn, TakenCards, TopCard}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
 * Specification on how the table actor should work
 * Created by josep on 3/21/15.
 */
class TableActorSpec (_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("TableGameServerSpec"))

  def giveMeATable(numberPlayers: Int, seed: Long): (TestActorRef[Table], List[(TestProbe, Int)]) = {
    val players = List.tabulate(numberPlayers)(x => (TestProbe(), x))
    (TestActorRef(Props(classOf[Table], players.map(x => (x._1.ref, x._2)), seed)), players)
  }

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
      val topCard = Card(50, 9, "green")
      table.underlyingActor.discardPile.topCard.foreach(topCard => {
        assert(topCard === topCard)
      })
      playerProbes.foreach {
        case (probe, _) => probe.expectMsg(TopCard(topCard))
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
        case (probe, _) => probe.expectMsg(TopCard(Card(21, 6,"yellow")))
      }
    }
  }
}
