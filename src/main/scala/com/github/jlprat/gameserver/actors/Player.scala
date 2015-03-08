package com.github.jlprat.gameserver.actors

import akka.actor.{Actor, ActorLogging, ActorRef}

/**
 * An actor that models a player
 * @param id the id of the player (given by your father)
 * @param tableActor the actor that models the table the player is playing
 */
class Player (val id: Int, val tableActor: ActorRef, val client: ActorRef) extends Actor with ActorLogging {

  import com.github.jlprat.gameserver.protocol.ClientProtocol._
  import com.github.jlprat.gameserver.protocol.Protocol._
  import com.github.jlprat.gameserver.model._
  import context._

  /**
   * state where the player waits for getting the cards dealt
   * @return
   */
  def waitingForCards: Receive = {
    case TakenCards(hand, playerId) if playerId == id =>
      log.info(s"I receive a hand $hand")
      client ! Out.ReceiveCard(hand, playerId)
      become(inactivePlayer(hand), discardOld = true)
    case TakenCards(hand, playerId) =>
      log.info(s"Player $playerId receives ${hand.size} cards")
      client ! Out.ReceiveCardOpponent(hand.size, playerId)
    case _: In.Incoming => client ! Out.WrongAction
    case message =>
      log.error(s"Unknown message $message")
  }

  /**
   * State where the player is not in turn and waits for getting active
   * @param hand the hand of the player
   * @return
   */
  def inactivePlayer(hand: Hand): Receive = {
    case TakenCards(hand, playerId) if playerId != id =>
      log.info(s"Player $playerId receives ${hand.size} cards")
      client ! Out.ReceiveCardOpponent(hand.size, playerId)
    case NextTurn(playerId) if playerId == id =>
      log.info(s"I receive a Next Turn message for me")
      client ! Out.PlayerInTurn(playerId)
      become(activePlayer(hand), discardOld = true)
    case NextTurn(playerId) =>
      log.info(s"I receive a Next Turn message for $playerId")
      client ! Out.PlayerInTurn(playerId)
    case In.Leave => tableActor ! Leave(id)
    case In.AnnounceLastCard if hand.size == 1 =>
      tableActor ! AnnounceLastCard(id)
    case In.AnnounceLastCard if hand.size > 1 =>
      tableActor ! TakeCard(id)
      client ! Out.WrongAction
      become(playerMadeAction(hand), discardOld = true)
    case _ : In.Incoming =>
      log.error("Not your turn")
      client ! Out.NotInTurn
    case message => log.info(s"Informative message is received $message")
  }

  /**
   * State where the player is in turn and can perform actions
   * TODO install a timer for the turn timeout
   * @param hand the hand of the player
   * @return
   */
  def activePlayer(hand: Hand): Receive = {
    case msg @ In.PlayCardRequest(card) if hand.exists( _ == card) =>
      log.info(s"Player tries to play a card we have $msg")
      val (cardOption, newHand) = hand.play(card)
      cardOption.foreach(playedCard => tableActor ! PlayCard(playedCard, id))
      become(playerMadeAction(newHand), discardOld = true)
    case In.PlayCardRequest(_) =>
      log.error("Client tries to play a card we don't have!")
      client ! Out.WrongAction
    case In.TakeCardsRequest =>
      tableActor ! TakeCard(id)
      become(playerMadeAction(hand), discardOld = true)
    case In.Leave =>
      tableActor ! Leave(id)
      become(leftPlayer, discardOld = true)
    case In.AnnounceLastCard if hand.size == 1 =>
      tableActor ! AnnounceLastCard(id)
    case In.AnnounceLastCard if hand.size > 1 =>
      tableActor ! TakeCard(id)
      client ! Out.WrongAction
      become(playerMadeAction(hand), discardOld = true)
    case NextTurn(playerId) if playerId != id =>
      //TODO this should be a clear corner case once the timers are installed
      log.error("I am out of sync, or somebody impersonated me!")
    case message => log.debug(s"received $message")
  }

  /**
   * State where the player took an action and is waiting for a proper answer
   * @param hand the hand of the player
   * @return
   */
  def playerMadeAction(hand: Hand): Receive = {
    case ChangeSuitRequest =>
      client ! Out.SelectSuitRequest
      become(changeSuit(hand), discardOld = true)
    case PlayedCard(card, playerId) =>
      client ! Out.PlayedCardSuccessfully(card, playerId)
    case PlayedCardIllegal(card, playerId) =>
      client ! Out.PlayedCardIrregularly(card, playerId)
      become(activePlayer(card :: hand), discardOld = true)
    case TakenCards(receivedCards, playerId) if playerId == id => become(playerWaitingForNextTurn(hand:::receivedCards), discardOld = true)
    case message => log.debug(s"received $message")
  }

  def playerWaitingForNextTurn(hand: Hand): Receive = {
    case NextTurn(playerId) if playerId == id => become(activePlayer(hand), discardOld = true)
    case NextTurn => become(inactivePlayer(hand), discardOld = true)
    case message => log.debug(s"received $message")
  }

  def changeSuit(hand: Hand): Receive = {
    case In.SelectSuitRequest(suit) =>
      tableActor ! ChangeSuit(suit, id)
      become(playerWaitingForNextTurn(hand), discardOld = true)
    case message => log.debug(s"received $message")
  }

  /**
   * The user left, TODO this function should take care of the timeout just in case
   * @return
   */
  def leftPlayer: Receive = {
    case _  => //do nothing! The user is gone
  }

  override def receive: Receive = waitingForCards
}
