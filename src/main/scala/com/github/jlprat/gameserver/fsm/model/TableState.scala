package com.github.jlprat.gameserver.fsm.model

/**
 * Sealed Trait that models the FSM state of the Table Actor
 * Created by josep on 6/29/15.
 */
sealed trait TableState

/**
 * Table is not yet initialized
 */
case object Uninitialized extends TableState

/**
 * A player is in turn and there is no special condition to consider
 */
case object DefaultState extends TableState

/**
 * Penalty cards are being accumulated
 */
case object AccumulatingPenalty extends TableState

/**
 * Waiting for Player's input about the new suit of the table
 */
case object WaitingForSuit extends TableState

/**
 * A player is in turn and the suit of the top card is overriden
 */
case object OverridenSuit extends TableState

/**
 * Game is finished
 */
case object EndGame extends TableState