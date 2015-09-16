package com.github.jlprat.gameserver.battleship.model

/**
 * Created by josep on 9/16/15.
 */
sealed trait BattleshipMessages
case class PlaceShip(playerId: Int, id: Short, x: Short, y: Short, size: Short, vertical: Boolean) extends BattleshipMessages
case object ShipsPlaced
case class Shoot(playerId: Int, x: Short, y: Short) extends BattleshipMessages
case object Miss extends BattleshipMessages
case class Hit(playerId: Int, shipId: Short) extends BattleshipMessages
case object ShipsAlive extends BattleshipMessages
case object AllShipsSunk extends BattleshipMessages
