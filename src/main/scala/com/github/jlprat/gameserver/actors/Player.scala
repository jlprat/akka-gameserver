package com.github.jlprat.gameserver.actors

import akka.actor.{Actor, ActorLogging, ActorRef}

import scala.concurrent.duration.FiniteDuration

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

  var playersHand = Hand()

  /**
   * state where the player waits for getting the cards dealt
   * @return
   */
  def waitingForCards: Receive = {
    case TakenCards(hand, playerId) if playerId == id =>
      log.info(s"I receive a hand $hand")
      client ! Out.ReceiveCard(hand, playerId)
      playersHand = hand
      become(inactivePlayer(), discardOld = true)
    case TakenCards(hand, playerId) =>
      log.info(s"Player $playerId receives ${hand.size} cards")
      client ! Out.ReceiveCardOpponent(hand.size, playerId)
    case _: In.Incoming => client ! Out.WrongAction
    case message =>
      log.error(s"Unknown message $message")
  }

  /**
   * State where the player is not in turn and waits for getting active
   * @return
   */
  def inactivePlayer(): Receive = {
    case TakenCards(hand, playerId) if playerId != id =>
      log.info(s"Player $playerId receives ${hand.size} cards")
      client ! Out.ReceiveCardOpponent(hand.size, playerId)
    case NextTurn(playerId) if playerId == id =>
      log.info(s"I receive a Next Turn message for me")
      client ! Out.PlayerInTurn(playerId)
      //system.scheduler.scheduleOnce(duration, self, PlayerTimeOut)
      become(activePlayer(), discardOld = true)
    case NextTurn(playerId) =>
      log.info(s"I receive a Next Turn message for $playerId")
      client ! Out.PlayerInTurn(playerId)
    case In.AnnounceLastCard if playersHand.size == 1 =>
      tableActor ! AnnounceLastCard(id)
    case In.AnnounceLastCard if playersHand.size > 1 =>
      tableActor ! TakeCard(id)
      client ! Out.WrongAction
      become(playerMadeAction(), discardOld = true)
    case ChangedSuit(suit, playerId) => client ! Out.NewSuitSelected(suit, playerId)
    case _ : In.Incoming =>
      log.error("Not your turn")
      client ! Out.NotInTurn
    case message => log.info(s"Informative message is received $message")
  }

  /**
   * State where the player is in turn and can perform actions
   * TODO install a timer for the turn timeout
   * @return
   */
  def activePlayer(): Receive = {
    case msg @ In.PlayCardRequest(card) if playersHand.exists( _ == card) =>
      log.info(s"Player tries to play a card we have $msg")
      val (cardOption, newHand) = playersHand.play(card)
      cardOption.foreach(playedCard => tableActor ! PlayCard(playedCard, id))
      playersHand = newHand
      become(playerMadeAction(), discardOld = true)
    case In.PlayCardRequest(_) =>
      log.error("Client tries to play a card we don't have!")
      client ! Out.WrongAction
    case In.TakeCardsRequest =>
      tableActor ! TakeCard(id)
      become(playerMadeAction(), discardOld = true)
    case In.Leave =>
      tableActor ! Leave(id)
      become(leftPlayer, discardOld = true)
    case In.AnnounceLastCard if playersHand.size == 1 =>
      tableActor ! AnnounceLastCard(id)
    case In.AnnounceLastCard if playersHand.size > 1 =>
      tableActor ! TakeCard(id)
      client ! Out.WrongAction
      become(playerMadeAction(), discardOld = true)
    case NextTurn(playerId) if playerId != id =>
      //TODO this should be a clear corner case once the timers are installed
      log.error("I am out of sync, or somebody impersonated me!")
    case message => log.debug(s"received $message")
  }

  /**
   * State where the player took an action and is waiting for a proper answer
   * @return
   */
  def playerMadeAction(): Receive = {
    case ChangeSuitRequest(playerId) =>
      client ! Out.SelectSuitRequest(playerId)
      become(changeSuit(), discardOld = true)
    case PlayedCard(card, playerId) =>
      client ! Out.PlayedCardSuccessfully(card, playerId)
    case PlayedCardIllegal(card, playerId) =>
      client ! Out.PlayedCardIrregularly(card, playerId)
      playersHand = card :: playersHand
      become(activePlayer(), discardOld = true)
    case TakenCards(receivedCards, playerId) if playerId == id =>
      playersHand = playersHand ::: receivedCards
      client ! Out.ReceiveCard(receivedCards, playerId)
      become(playerWaitingForNextTurn(), discardOld = true)
    case message => log.debug(s"received $message")
  }

  def playerWaitingForNextTurn(): Receive = {
    case NextTurn(playerId) if playerId == id => become(activePlayer(), discardOld = true)
    case NextTurn => become(inactivePlayer(), discardOld = true)
    case ChangedSuit(suit, playerId) => client ! Out.NewSuitSelected(suit, playerId)
    case message => log.debug(s"received $message")
  }

  def changeSuit(): Receive = {
    case In.SelectSuitRequest(suit) =>
      tableActor ! ChangeSuit(suit, id)
      become(playerWaitingForNextTurn(), discardOld = true)
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
