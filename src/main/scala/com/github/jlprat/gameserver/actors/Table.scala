package com.github.jlprat.gameserver.actors

import akka.actor.{ActorRef, Actor}
import com.github.jlprat.gameserver.model.{DiscardPile, Hand}

object Table {

  /**
   * This message is not meant to be spoken between table and players
   */
  case object Initialize
}

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
  import com.github.jlprat.gameserver.protocol.Protocol._
  import Table._

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
  var discardPile = DiscardPile.empty
  var activePlayers = players.toVector
  var activePlayerId = 0
  var chosenSuit: Option[String] = None

  def drawCards: Option[(Deck, Iterable[(Hand, Int)])] = {
    val drawResult = deck.draw(activePlayers.size, 6, 2)
    drawResult.map {
      case (hands, rest) =>
        (rest, hands.zip(activePlayers.map(_._2)))
    }
  }

  def placeTopCard: Option[(Deck, DiscardPile)] = {
    //TODO draw another card, if the top one is "take 2", "Change color" "skip" "again" "change direction"?
    deck.take(1).map{
      case (hand, rest) => (deck, DiscardPile(hand.cards))
    }
  }

  def validPlay(card: Card): Boolean = {
    true
  }

  val playing: Receive = {
    case PlayCard(card, playerId) if playerId == activePlayerId && validPlay(card) =>

  }

  val initialState: Receive = {
    case Initialize =>
      drawCards.foreach {
        case (remainingDeck, cards) =>
          deck = remainingDeck
          cards.foreach{
            case (hand, playerId) => activePlayers.map(_._1).foreach(_ ! TakenCards(hand, playerId))
          }
      }
      placeTopCard.foreach {
        case(remainingDeck, cardsToPutOnTop) =>
          deck = remainingDeck
          discardPile = cardsToPutOnTop
      }
      discardPile.topCard.foreach(card => activePlayers.map(_._1).foreach(_ ! TopCard(card)))
      val playerIdInTurn = activePlayers(activePlayerId)._2
      activePlayers.foreach{
        case (player,_) => player ! NextTurn(playerIdInTurn)
      }
      context.become(playing, discardOld = true)
  }

  override def receive: Receive = initialState
}
