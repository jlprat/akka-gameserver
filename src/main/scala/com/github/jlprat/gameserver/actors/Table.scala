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

  /**
   * generates a shuffled deck
   * @param seed given seed
   * @return the Deck of 54 cards (13 cards plus 2 jokers) shuffled
   */
  def generateDeck(seed: Long): Deck = {
    val cards = (0,"joker") :: (0,"joker") :: (for {
      suit <- List("blue", "yellow", "red", "green")
      number <- 1 to 13
    } yield (number, suit))

    Deck(cards.zip(1 to 54).map{
      case (card, id) => Card(id, card._1, card._2)
    }).shuffle(seed)
  }

  var deck = generateDeck(seed)
  var discardPile = DiscardPile.empty
  var activePlayers = players.toVector
  var activePlayerId = 0
  var chosenSuit: Option[String] = None

  /**
   * Draws cards to players, without sending any message
   * @return If there are enough cards, The remaining deck after drawing and an Iterable of pairs of Hand and its owner
   */
  def drawCards: Option[(Deck, Iterable[(Hand, Int)])] = {
    val drawResult = deck.draw(activePlayers.size, 6, 2)
    drawResult.map {
      case (hands, rest) =>
        (rest, hands.zip(activePlayers.map(_._2)))
    }
  }

  /**
   * Determines if the card is not a special card
   * Special cards are:
   * <ul>
   *   <li>Ace - Skip Turn</li>
   *   <li>2 - Take 2</li>
   *   <li>8 - Change Suit</li>
   *   <li>10 - Play Again</li>
   *   <li>11 - Change direction</li>
   *   <li>Joker - Any card can be played</li>
   * </ul>
   * @param card the card to be tested
   * @return if the card is not special
   */
  def isNotSpecialCard(card: Card): Boolean = {
    card match {
      case Card(_, 1, _) => false //skip turn
      case Card(_, 2, _) => false //take 2
      case Card(_, 8, _) => false //change suit
      case Card(_, 10, _) => false //play again
      case Card(_, 11, _) => false //change direction
      case Card(_, _, "joker") => false //joker
      case _ => true
    }
  }

  def placeTopCard: Option[(Deck, DiscardPile)] = {
    deck.takeUntil(isNotSpecialCard).map{
      case (hand, rest) => (rest, DiscardPile(hand.cards.reverse))
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
