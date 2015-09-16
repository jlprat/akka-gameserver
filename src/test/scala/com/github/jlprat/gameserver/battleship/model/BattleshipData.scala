package com.github.jlprat.gameserver.battleship.model

import scala.collection.mutable.Map

/**
 * Created by josep on 9/16/15.
 */
case class CellStatus(id: Short, hit: Boolean = false)

case class SimplifiedGrid(grid: Map[(Short,Short), CellStatus]) {
}

case class BattleshipData(grids: Array[Map[(Short,Short), CellStatus]]) {
  def isOccupied(playerIdx: Int, x: Short, y: Short): Boolean = {
    grids(playerIdx).isDefinedAt((x,y))
  }

  def placeShip(playerIdx: Int, shipId: Short, x: Short, y: Short, size: Short, vertical: Boolean): Boolean = vertical match {
    case true if y + size < 10 =>
      val yPositions = List.tabulate(size)(elem => y + elem)
      val canBePlaced = yPositions.forall(yPos => !isOccupied(playerIdx, x, yPos.toShort))
      if (canBePlaced) {
        yPositions.foreach(yPos => grids(playerIdx) + ((x, yPos.toShort) -> CellStatus(shipId)))
      }
      canBePlaced
    case false =>
      val xPositions = List.tabulate(size)(elem => x + elem)
      val canBePlaced = xPositions.forall(xPos => !isOccupied(playerIdx, xPos.toShort, y))
      if (canBePlaced) {
        xPositions.foreach(xPos => grids(playerIdx) + ((xPos.toShort, y) -> CellStatus(shipId)))
      }
      canBePlaced
  }

  def isShipSunk(playerId: Int, shipId: Short): Boolean = {
    !grids(playerId).values.exists(status => status.id == shipId && !status.hit)
  }

  def areShipsAlive(playerId: Int): Boolean = grids(playerId).values.forall(_.hit)

}
