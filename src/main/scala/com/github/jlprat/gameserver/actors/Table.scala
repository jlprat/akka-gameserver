package com.github.jlprat.gameserver.actors

import akka.actor.{ActorRef, Actor}
import com.github.jlprat.gameserver.model.Hand
import com.github.jlprat.gameserver.protocol.Protocol.{TopCard, TakenCards}

/**
 * Actor that models a Table. In a more realistic world, this could also be a crupier, or the referee, that takes care
 * that nobody cheats, and that the plays are legal.
 * The table object is responsible of:
 * <ul>
 *   <li>Maintain how many players are active</li>
 *   <li>Which player is in turn</li>
 *   <li>Manage the stock pile</li>
 *   <li>Manage the discard pile</li>
 * </ul>
 * Created by josep on 3/21/15.
 * @param players the players who will play on this table
 * @param seed the seed to randomize the deck
 */
class Table(players: List[(ActorRef, Int)], seed: Long) extends Actor{

  import com.github.jlprat.gameserver.model.{Card, Deck}

  def generateDeck(seed: Long): Deck = {
    val cards = (0,"joker") :: (0,"joker") :: (for {
      suit <- List("blue", "yellow", "red", "green")
      number <- 1 to 13
    } yield (number, suit))

    Deck(cards.zip(1 to 52).map{
      case (card, id) => Card(id, card._1, card._2)
    }).shuffle(seed)
  }

  var deck = generateDeck(seed)
  var topCard: Option[Card]= None


  override def preStart():Unit = {
    val drawResult = deck.draw(players.size, 6, 2)
    drawResult.foreach{
      case (hands, rest) =>
        deck = rest
        hands.zip(players.map(_._2)).foreach {
          case (hand, playerId) =>
            players.map(_._1).foreach( _ ! TakenCards(hand, playerId))
        }
    }
    deck.take(1).foreach{
      case (hand, rest) =>
        deck = rest
        topCard = hand.cards.headOption.map(card => {
          players.map(_._1).foreach(_ ! TopCard(card))
          card
        })
    }
  }

  /**
   * should not call preStart at least at the moment
   * @param reason the reason of the restart
   */
  override def postRestart(reason: Throwable): Unit = ()

  override def receive: Receive = {
    case _ =>
  }
}
