package com.github.jlprat.gameserver.battleship.model

/**
 * Created by josep on 9/16/15.
 */
sealed trait BattleshipMessages
case class PlaceShip(playerId: Int, id: Short, x: Short, y: Short, size: Short, vertical: Boolean) extends BattleshipMessages
case object PlacedShip extends BattleshipMessages
case object ShipsPlaced extends BattleshipMessages
case object NextPlayer extends BattleshipMessages
case class PlaceShot(playerId: Int, x: Short, y: Short) extends BattleshipMessages
case object Miss extends BattleshipMessages
case object Hit extends BattleshipMessages
case object ShipsAlive extends BattleshipMessages
case object AllShipsSunk extends BattleshipMessages
case object Timeout extends BattleshipMessages

case class Message(msg: String)
