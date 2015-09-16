package com.github.jlprat.gameserver.battleship.model

/**
 * Created by josep on 9/16/15.
 */
sealed trait BattleShipMessages
case class PlaceShip(playerId: Int, id: Short, x: Short, y: Short, size: Short, vertical: Boolean)
case class Shoot(playerId: Int, x: Short, y: Short)
case object Miss
case class Hit(playerId: Int, shipId: Short)
case object ShipsAlive
case object AllShipsSunk
