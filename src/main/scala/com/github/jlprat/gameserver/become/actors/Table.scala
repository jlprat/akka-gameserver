package com.github.jlprat.gameserver.become.actors

import akka.actor.{ActorLogging, ActorRef, Actor}
import com.github.jlprat.gameserver.become.model.{DiscardPile, Hand}
import com.github.jlprat.gameserver.become.protocol.ClientProtocol.Out.WrongAction

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
class Table(players: List[(ActorRef, Int)], seed: Long) extends Actor with ActorLogging{

  import com.github.jlprat.gameserver.become.model.{Card, Deck}
  import com.github.jlprat.gameserver.become.protocol.Protocol._
  import Table._

  var deck = generateDeck(seed)
  var discardPile = DiscardPile.empty

  var activePlayers = players.toVector
  var activePlayerIndex = 0
  def activePlayerId = activePlayers(activePlayerIndex)._2

  var penaltyCards = 0
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
    discardPile.topCard.forall {
      case Card(_, _, suit) if suit == card.suit => true
      case Card(_, rank, _) if rank == card.rank => true
      case Card(_, _, "joker") => true
      case _ if card.suit == "joker" => true
      case _ if card.rank == 8 => true
      case _ => false
    }
  }

  /**
   * Mathematical modulo
   * @param i the dividend
   * @param n the divisor
   * @return the mathematical output of ´i mod n´
   */
  def mod(i: Int, n: Int): Int = i % n match {
    case x if x < 0 => n + x
    case x => x
  }

  /**
   * Calculates the next player index in turn
   * @param numberOfPlayersToSkip indicates how many players must be skipped. A zero means same player is in turn
   * @return the index of the next player
   */
  def nextPlayerIndex(numberOfPlayersToSkip: Int): Int = {
    mod(activePlayerIndex + (numberOfPlayersToSkip * direction), activePlayers.size)
  }

  /**
   * flips the direction of the table
   * @return the new direction
   */
  def changeDirection: Int = {
    direction * -1
  }

  def findPlayerById(id: Int): Option[ActorRef] = {
    activePlayers.find(_._2 == id).map(_._1)
  }

  def handleImpactOfCard(card: Card): Unit = card match {
    case Card(_, 2, _) =>
      penaltyCards = penaltyCards + 2
      activePlayerIndex = nextPlayerIndex(1)
      broadcast(NextTurn(activePlayerId))
      context.become(withPenaltyCards, discardOld = true)
    case Card(_, 8, _) =>
      findPlayerById(activePlayerId).foreach(_ ! ChangeSuitRequest(activePlayerId))
      context.become(waitingForSuit,discardOld = true)
    case Card(_, 1, _) => //skip
      broadcast(SkipPlayer(activePlayers(nextPlayerIndex(1))._2))
      activePlayerIndex = nextPlayerIndex(2)
      broadcast(NextTurn(activePlayerId))
    case Card(_ , 10, _) => // Again
      broadcast(PlayAgain(activePlayerId))
      broadcast(NextTurn(activePlayerId))
    case Card(_, 11, _) => //Change direction
      log.info(s"direction $direction")
      direction = changeDirection
      log.info(s"direction $direction")
      broadcast(ChangeDirection(if (direction == 1) true else false))
      activePlayerIndex = nextPlayerIndex(1)
      broadcast(NextTurn(activePlayerId))
    case _ =>
      activePlayerIndex = nextPlayerIndex(1)
      broadcast(NextTurn(activePlayerId))
  }

  /**
   * This Receive partial function takes care of the state when the table is accumulating penalty cards
   */
  val withPenaltyCards: Receive = {
    case PlayCard(card @ Card(_, 2, _), playerId) if activePlayerId == playerId =>
      penaltyCards = penaltyCards + 2
      activePlayerIndex = nextPlayerIndex(1)
      broadcast(NextTurn(activePlayerId))
    case PlayCard(card, playerId) if activePlayerId == playerId =>
      broadcast(PlayedCardIllegal(card, playerId))
    case TakeCard(playerId) if activePlayerId == playerId =>
      deck.take(penaltyCards).foreach{
        case (hand, deckAfter) =>
          deck = deckAfter
          broadcast(TakenCards(hand, activePlayerId))
          broadcast(NextTurn(activePlayerId))
      }
      penaltyCards = 0
      context.become(normalState, discardOld = true)
  }

  /**
   * This Receive PF takes care of the state where the table is waiting for the player to pick a suit
   */
  val waitingForSuit: Receive = {
    case ChangeSuit(suit, playerId) if activePlayerId == playerId =>
      broadcast(ChangedSuit(suit, playerId))
      activePlayerIndex = nextPlayerIndex(1)
      broadcast(NextTurn(activePlayerId))
      context.become(overriddenSuit(suit), discardOld = true)
    case msg => log.error(s"Didn't expect this message now: $msg")
  }

  /**
   * This models the state when the top card is an 8 and the new suit is picked by the user
   * @param suit the suit of the top card
   * @return the Receive PF
   */
  def overriddenSuit(suit: String): Receive = {
    case TakeCard(playerId) if playerId == activePlayerId =>
      deck.take(1).foreach{
        case (hand, deckAfter) =>
          deck = deckAfter
          broadcast(TakenCards(hand, activePlayerId))
          activePlayerIndex = nextPlayerIndex(1)
          broadcast(NextTurn(activePlayerId))
      }
    case TakeCard(playerId) =>
      findPlayerById(playerId).foreach(_ ! WrongAction)
    case PlayCard(card, playerId) if playerId == activePlayerId && card.suit == suit && card.rank != 8 =>
      discardPile = card :: discardPile
      broadcast(PlayedCard(card, playerId))
      context.become(normalState, discardOld = true)
      handleImpactOfCard(card)
    case PlayCard(card, playerId) if playerId == activePlayerId =>
      broadcast(PlayedCardIllegal(card, playerId))
    case PlayCard(_, playerId) =>
      findPlayerById(playerId).foreach(_ ! WrongAction)
  }

  /**
   * This Receive partial function models the state of the table where no special card is on top
   */
  val normalState: Receive = {
    case TakeCard(playerId) if playerId == activePlayerId =>
      deck.take(1).foreach{
        case (hand, deckAfter) =>
          deck = deckAfter
          broadcast(TakenCards(hand, activePlayerId))
          activePlayerIndex = nextPlayerIndex(1)
          broadcast(NextTurn(activePlayerId))
      }
    case TakeCard(playerId) =>
      findPlayerById(playerId).foreach(_ ! WrongAction)
    case PlayCard(card, playerId) if playerId == activePlayerId && validPlay(card) =>
      discardPile = card :: discardPile
      broadcast(PlayedCard(card, playerId))
      handleImpactOfCard(card)
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
      activePlayerIndex = nextPlayerIndex(0)
      broadcast(NextTurn(activePlayerId))
      context.become(normalState, discardOld = true)
  }

  override def receive: Receive = initialState
}
