package com.github.jlprat.gameserver.become.actors

import akka.actor.{Cancellable, Actor, ActorLogging, ActorRef}

import scala.concurrent.duration.FiniteDuration

/**
 * An actor that models a player, this actor is responsible for anything that relates to a player and its turn.
 * It doesn't know though, most of the specific rules of the game.
 * The PlayerActor is responsible of:
 * <ul>
 *   <li>Maintaining their state machine</li>
 *   <li>Keep their hand up to date</li>
 *   <li>Managing their turn time outs</li>
 * </ul>
 * This implementation models a state machine using become. As it can be seen, it contains pretty much redundancy
 * as all actions that can occur at any point in time must be handled in any Receive block
 * @param id the id of the player (given by your father)
 * @param tableActor the actor that models the table the player is playing
 * @param clientActor the actor that is in charge of communicating and dealing with the user
 * @param turnDuration the duration of each turn
 */
class Player (val id: Int, val tableActor: ActorRef, val clientActor: ActorRef, val turnDuration: FiniteDuration) extends Actor with ActorLogging {

  //TODO last card is incomplete (no handling of not calling last card)

  import com.github.jlprat.gameserver.become.protocol.ClientProtocol._
  import com.github.jlprat.gameserver.become.protocol.Protocol._
  import com.github.jlprat.gameserver.become.model._
  import context._

  var playersHand = Hand()
  var turnTimer: Option[Cancellable] = None

  /**
   * state where the player waits for getting the cards dealt
   * @return
   */
  def waitingForCards: Receive = {
    case TakenCards(hand, playerId) if playerId == id =>
      log.info(s"I receive a hand $hand")
      clientActor ! Out.ReceiveCard(hand, playerId)
      playersHand = hand
      become(inactivePlayer(), discardOld = true)
    case TakenCards(hand, playerId) =>
      log.info(s"Player $playerId receives ${hand.size} cards")
      clientActor ! Out.ReceiveCardOpponent(hand.size, playerId)
    case _: In.Incoming => clientActor ! Out.WrongAction
    case message =>
      log.error(s"Unknown message $message")
  }

  /**
   * State where the player is not in turn and waits for getting active
   * @return
   */
  def inactivePlayer(): Receive = {
    case TopCard(card) => clientActor ! Out.TopCard(card)
    case TakenCards(hand, playerId) if playerId != id =>
      log.info(s"Player $playerId receives ${hand.size} cards")
      clientActor ! Out.ReceiveCardOpponent(hand.size, playerId)
    case NextTurn(playerId) if playerId == id =>
      log.info(s"I receive a Next Turn message for me")
      clientActor ! Out.PlayerInTurn(playerId)
      turnTimer = Some(system.scheduler.scheduleOnce(turnDuration, self, PlayerTimeOut(playerId = id)))
      become(activePlayer(), discardOld = true)
    case NextTurn(playerId) =>
      log.info(s"I receive a Next Turn message for $playerId")
      clientActor ! Out.PlayerInTurn(playerId)
    case In.AnnounceLastCard if playersHand.size == 1 =>
      tableActor ! AnnounceLastCard(id)
    case In.AnnounceLastCard if playersHand.size > 1 =>
      tableActor ! TakeCard(id)
      clientActor ! Out.WrongAction
      become(playerMadeAction(), discardOld = true)
    case ChangedSuit(suit, playerId) => clientActor ! Out.NewSuitSelected(suit, playerId)
    case In.Leave =>
      tableActor ! Leave(id)
      become(leftPlayer, discardOld = true)
    case _ : In.Incoming =>
      log.error("Not your turn")
      clientActor ! Out.NotInTurn
    case message => log.info(s"Informative message is received $message")
  }

  /**
   * State where the player is in turn and can perform actions
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
      clientActor ! Out.WrongAction
    case In.TakeCardsRequest =>
      tableActor ! TakeCard(playerId = id)
      become(playerMadeAction(), discardOld = true)
    case In.Leave =>
      tableActor ! Leave(id)
      become(leftPlayer, discardOld = true)
    case In.AnnounceLastCard if playersHand.size == 1 =>
      tableActor ! AnnounceLastCard(playerId = id)
    case In.AnnounceLastCard if playersHand.size > 1 =>
      tableActor ! TakeCard(playerId = id)
      clientActor ! Out.WrongAction
      become(playerMadeAction(), discardOld = true)
    case NextTurn(playerId) if playerId != id =>
      log.error("I am out of sync, or somebody impersonated me!")
    case PlayerTimeOut(_) =>
      tableActor ! TakeCard(playerId = id)
      become(playerMadeAction(), discardOld = true)
    case message => log.debug(s"received $message")
  }

  /**
   * State where the player took an action and is waiting for a proper answer
   * @return
   */
  def playerMadeAction(): Receive = {
    case ChangeSuitRequest(playerId) =>
      clientActor ! Out.SelectSuitRequest(playerId)
      become(changeSuit(), discardOld = true)
    case PlayedCard(card, playerId) =>
      clientActor ! Out.PlayedCardSuccessfully(card, playerId)
      become(playerWaitingForNextTurn(), discardOld = true)
    case PlayedCardIllegal(card, playerId) =>
      clientActor ! Out.PlayedCardIrregularly(card, playerId)
      playersHand = card :: playersHand
      become(activePlayer(), discardOld = true)
    case TakenCards(receivedCards, playerId) if playerId == id =>
      playersHand = playersHand ::: receivedCards
      clientActor ! Out.ReceiveCard(receivedCards, playerId)
      become(playerWaitingForNextTurn(), discardOld = true)
    case In.Leave =>
      tableActor ! Leave(id)
      become(leftPlayer, discardOld = true)
    case PlayerTimeOut(_) =>
      log.info("I'm waiting for table to answer me!")
      //TODO table answer might come in a moment or we are out of sync!
    case message => log.debug(s"received $message")
  }

  /**
   * State where the player already did all the actions and is just waiting for the next turn message,
   * or a ChangedSuit message
   * @return
   */
  def playerWaitingForNextTurn(): Receive = {
    case NextTurn(playerId) if playerId == id =>
      turnTimer.map(_.cancel())
      turnTimer = Some(system.scheduler.scheduleOnce(turnDuration, self, PlayerTimeOut(playerId = id)))
      clientActor ! Out.PlayerInTurn(playerId)
      become(activePlayer(), discardOld = true)
    case NextTurn(playerId) =>
      turnTimer.map(_.cancel())
      turnTimer= None
      clientActor ! Out.PlayerInTurn(playerId)
      become(inactivePlayer(), discardOld = true)
    case ChangedSuit(suit, playerId) => clientActor ! Out.NewSuitSelected(suit, playerId)
    case In.Leave =>
      tableActor ! Leave(id)
      become(leftPlayer, discardOld = true)
    case PlayerTimeOut(_) =>
      log.info("I'm waiting for table to answer me!")
    //TODO table answer might come in a moment or we are out of sync!
    case message => log.debug(s"received $message")
  }

  /**
   * State where the player must pick a suit to play
   * @return
   */
  def changeSuit(): Receive = {
    case In.SelectSuitRequest(suit) =>
      tableActor ! ChangeSuit(suit, id)
      become(playerWaitingForNextTurn(), discardOld = true)
    case In.Leave =>
      tableActor ! ChangeSuit("blue", id)
      tableActor ! Leave(id)
      become(leftPlayer, discardOld = true)
    case PlayerTimeOut(_) =>
      tableActor ! ChangeSuit("blue", id)
      become(playerWaitingForNextTurn(), discardOld = true)
    case message => log.debug(s"received $message")
  }

  /**
   * The user left
   * @return
   */
  def leftPlayer: Receive = {
    case _  => //do nothing! The user is gone
  }

  /**
   * default state
   * @return
   */
  override def receive: Receive = waitingForCards
}
