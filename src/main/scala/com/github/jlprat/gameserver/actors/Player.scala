package com.github.jlprat.gameserver.actors

import akka.actor.{Actor, ActorLogging, ActorRef}

/**
 * An actor that models a player
 * @param id the id of the player (given by your father)
 * @param tableActor the actor that models the table the player is playing
 */
class Player (val id: Int, val tableActor: ActorRef) extends Actor with ActorLogging {

  import com.github.jlprat.gameserver.protocol.Protocol._
  import com.github.jlprat.gameserver.model._

  /**
   * state where the player waits for getting the cards dealt
   * @return
   */
  def waitingForCards: Receive = {
    case TakenCards(hand, playerId) if playerId == id => inactivePlayer(hand)
    case TakenCards(hand, playerId) => log.info(s"Player $playerId receives ${hand.size} cards")
    case message => log.error(s"Unknown message $message")
  }

  def inactivePlayer(hand: Hand): Receive = {
    case NextTurn(playerId) if playerId == id => ???
    case message => log.info(s"Informative message is received $message")
  }

  override def receive: Receive = waitingForCards
}
