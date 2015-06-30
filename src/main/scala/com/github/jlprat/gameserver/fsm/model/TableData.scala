package com.github.jlprat.gameserver.fsm.model

import akka.actor.ActorRef
import Card._

/**
 * Helper class that holds an actor and its internal business ID
 * @param actorRef the actorRef
 * @param id the business Id of the Player
 * TODO: refactor this id to its own object
 */
case class PlayerInfo(actorRef: ActorRef, id: Int)

/**
 * Created by josep on 6/30/15.
 */
sealed trait TableData

case class Initial(activePlayers: Vector[PlayerInfo]) extends TableData

case class DefaultData(activePlayer: PlayerInfo,
                       remainingPlayers: Vector[PlayerInfo],
                       deck: Deck,
                       discardPile: DiscardPile,
                       clockwise: Boolean) extends TableData

case class WithPenaltyData(activePlayer: PlayerInfo,
                           remainingPlayers: Vector[PlayerInfo],
                           deck: Deck,
                           discardPile: DiscardPile,
                           clockwise: Boolean,
                           penaltyCards: Int) extends TableData

case class OverridenSuitData(activePlayer: PlayerInfo,
                             remainingPlayers: Vector[PlayerInfo],
                             deck: Deck,
                             discardPile: DiscardPile,
                             clockwise: Boolean,
                             suit: Suit) extends TableData
