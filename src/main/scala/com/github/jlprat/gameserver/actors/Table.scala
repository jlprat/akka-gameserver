package com.github.jlprat.gameserver.actors

import akka.actor.{ActorRef, Actor}
import com.github.jlprat.gameserver.model.{DiscardPile, Hand}
import com.github.jlprat.gameserver.protocol.ClientProtocol.Out.WrongAction

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

  var deck = generateDeck(seed)
  var discardPile = DiscardPile.empty

  var activePlayers = players.toVector
  var activePlayerIndex = 0
  def activePlayerId = activePlayers(activePlayerIndex)._2

  var chosenSuit: Option[String] = None
  var penaltyCards = 1
  //direction flag, 1 clockwise, -1 anti-clockwise
  var direction = 1

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

  /**
   * broadcasts a message to all active players
   * @param msg the message to be broadcasted
   */
  def broadcast(msg: AnyRef): Unit = {
    activePlayers.foreach{
      case (player,_) => player ! msg
    }
  }

  /**
   * Places the top card, it must be a non special card
   * @return 'None' if there are not suitable cards. Otherwise, the remaining deck and the cards to be placed
   *         in the discard pile
   */
  def placeTopCard: Option[(Deck, DiscardPile)] = {
    deck.takeUntil(isNotSpecialCard).map{
      case (hand, rest) => (rest, DiscardPile(hand.cards.reverse))
    }
  }

  /**
   * Determines if the card is a valid play. Meaning it can be placed on top of the current top card
   * @param card the card that must be evaluated
   * @return ´true´ if the card can be played, ´false´ otherwise.
   */
  def validPlay(card: Card): Boolean = {
    discardPile.topCard.forall(_ match {
      case Card(_, _, suit) if suit == card.suit => true
      case Card(_, rank, _) if rank == card.rank => true
      case Card(_, _, "joker") => true
      case _ if card.suit == "joker" => true
      case _ if card.rank == 8 => true
      case _ => false
    })
  }

  /**
   * Mathematical modulo
   * @param i the dividend
   * @param n the divisor
   * @return the mathematical output of ´i mod n´
   */
  def mod(i: Int, n: Int): Int = i % n match {
    case x if x < 0 => n - x
    case x => x
  }

  /**
   * Calculates the next player in turn
   * @param numberOfPlayersToSkip indicates how many players must be skipped
   * @return the ID of the next player
   */
  def nextPlayerInTurn(numberOfPlayersToSkip: Int): Int = {
    activePlayerIndex = mod(activePlayerIndex + (numberOfPlayersToSkip * direction), activePlayers.size)
    activePlayerId
  }

  /**
   * flips the direction of the table
   * @return the new direction
   */
  def changeDirection: Int = {
    direction = direction * -1
    direction
  }

  def findPlayerById(id: Int): Option[ActorRef] = {
    activePlayers.find(_._2 == id).map(_._1)
  }

  /**
   * This Receive partial function models the state of the table where no special actions need to be taken
   */
  val normalState: Receive = {
    case TakeCard(playerId) if playerId == activePlayerId =>
      deck.take(penaltyCards).foreach{
        case (hand, deckAfter) =>
          deck = deckAfter
          broadcast(TakenCards(hand, activePlayerId))
          val playerIdInTurn = nextPlayerInTurn(1)
          broadcast(NextTurn(playerIdInTurn))
      }
    case TakeCard(playerId) =>
      findPlayerById(playerId).foreach(_ ! WrongAction)
    case PlayCard(card, playerId) if playerId == activePlayerId && validPlay(card) =>
      discardPile = card :: discardPile
      broadcast(PlayedCard(card, playerId))
      val playerIdInTurn = nextPlayerInTurn(1)
      broadcast(NextTurn(playerIdInTurn))
    case PlayCard(card, playerId) if playerId == activePlayerId =>
      broadcast(PlayedCardIllegal(card, playerId))
    case PlayCard(_, playerId) =>
      findPlayerById(playerId).foreach(_ ! WrongAction)
  }

  /**
   * this Receive partial function takes care of the initialization of the table.
   * Its duties are:
   * <ul>
   *   <li>Draw cards to players</li>
   *   <li>Communicate hands to players</li>
   *   <li>Place top discarded card</li>
   *   <li>Determine who is the first user in turn</li>
   * </ul>
   */
  val initialState: Receive = {
    case Initialize =>
      drawCards.foreach {
        case (remainingDeck, cards) =>
          deck = remainingDeck
          cards.foreach{
            case (hand, playerId) => broadcast(TakenCards(hand, playerId))
          }
      }
      placeTopCard.foreach {
        case(remainingDeck, cardsToPutOnTop) =>
          deck = remainingDeck
          discardPile = cardsToPutOnTop
      }
      discardPile.topCard.foreach(card => broadcast(TopCard(card)))
      val playerIdInTurn = nextPlayerInTurn(0)
      broadcast(NextTurn(playerIdInTurn))
      context.become(normalState, discardOld = true)
  }

  override def receive: Receive = initialState
}
