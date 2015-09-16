package com.github.jlprat.gameserver.battleship.model

/**
 * Created by josep on 9/16/15.
 */
sealed trait BattleshipState
case object WaitingForShips extends BattleshipState
case object PlacingShips extends BattleshipState
case object WaitingForNextPlayer extends BattleshipState
case object CheckingShot extends BattleshipState
case object ShitHit extends BattleshipState
case object EndGame extends BattleshipState
