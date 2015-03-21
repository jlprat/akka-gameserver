package com.github.jlprat.gameserver.actor

import akka.actor.{Props, ActorSystem}
import akka.testkit._
import com.github.jlprat.gameserver.actors.Table
import com.github.jlprat.gameserver.model._
import com.github.jlprat.gameserver.protocol.Protocol.{TakenCards, TopCard}
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
    //val generatedDeck = table.underlyingActor.generateDeck(1)
    "deal cards to players" in {
      playerProbes.foreach {
        case (probe, _) => {
          probe.expectMsg(TakenCards(Hand(List(Card(45, 4, "green"), Card(27, 12, "yellow"), Card(25, 10, "yellow"),
            Card(34, 6, "red"), Card(12, 10, "blue"), Card(36, 8, "red"))), playerId = 0))
          probe.expectMsg(TakenCards(Hand(List(Card(17, 2, "yellow"), Card(33, 5, "red"), Card(26, 11, "yellow"),
            Card(41, 13, "red"), Card(42, 1, "green"), Card(7, 5, "blue"))), playerId = 1))
          probe.expectMsg(TakenCards(Hand(List(Card(29, 1, "red"), Card(30, 2, "red"), Card(10, 8, "blue"),
            Card(40, 12, "red"), Card(46, 5, "green"), Card(6, 4, "blue"))), playerId = 2))
        }
      }
    }
    "put a card in the top of the discard pile" in {
      table.underlyingActor.topCard.foreach(topCard => assert(topCard === Card(16, 1, "yellow")))
    }
  }
}
