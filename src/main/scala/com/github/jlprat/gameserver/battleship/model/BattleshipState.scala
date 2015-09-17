package com.github.jlprat.gameserver.battleship.model

/**
 * Created by josep on 9/16/15.
 */
sealed trait BattleshipState
case object WaitingForPlayers extends BattleshipState
case object PlacingShips extends BattleshipState
case object CheckingPlacedShips extends BattleshipState
case object WaitingForNextPlayer extends BattleshipState
case object CheckingShot extends BattleshipState
case object HitShip extends BattleshipState
case object EndGame extends BattleshipState
