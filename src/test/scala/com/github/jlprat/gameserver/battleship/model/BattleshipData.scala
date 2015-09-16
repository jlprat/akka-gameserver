package com.github.jlprat.gameserver.battleship.model

import scala.collection.mutable

/**
 * Created by josep on 9/16/15.
 */
case class CellStatus(id: Short, hit: Boolean = false)

case class SimplifiedGrid(grid: mutable.Map[(Short,Short), CellStatus]) {
}

case class BattleshipData(grids: Array[mutable.Map[(Short,Short), CellStatus]]) {
  def isOccupied(playerIdx: Int, x: Short, y: Short): Boolean = {
    grids(playerIdx).isDefinedAt((x,y))
  }

  def placeShip(playerIdx: Int, shipId: Short, x: Short, y: Short, size: Short, vertical: Boolean): BattleshipData = vertical match {
    case true if y + size < 10 =>
      val yPositions = List.tabulate(size)(elem => y + elem)
      val canBePlaced = yPositions.forall(yPos => !isOccupied(playerIdx, x, yPos.toShort))
      if (canBePlaced) {
        val clonedGrids = grids.clone()
        yPositions.foreach(yPos => clonedGrids(playerIdx) + ((x, yPos.toShort) -> CellStatus(shipId)))
        BattleshipData(clonedGrids)
      } else
        BattleshipData(grids)
    case false =>
      val xPositions = List.tabulate(size)(elem => x + elem)
      val canBePlaced = xPositions.forall(xPos => !isOccupied(playerIdx, xPos.toShort, y))
      if (canBePlaced) {
        val clonedGrids = grids.clone()
        xPositions.foreach(xPos => clonedGrids(playerIdx) + ((xPos.toShort, y) -> CellStatus(shipId)))
        BattleshipData(clonedGrids)
      } else
        BattleshipData(grids)
  }

  def shoot(playerId: Int, x: Short, y: Short): (Boolean, BattleshipData) = {
    val opponent = if (playerId == 1) 0 else 1
    if (grids(opponent).isDefinedAt((x,y))) {
      val clonedGrids = grids.clone()
      clonedGrids(opponent).get((x,y)).foreach(cellStatus => clonedGrids(opponent).update((x,y), cellStatus.copy(hit = true)))
      (true, BattleshipData(clonedGrids))
    } else {
      (false, BattleshipData(grids))
    }
  }

  def isShipSunk(playerId: Int, shipId: Short): Boolean = {
    !grids(playerId).values.exists(status => status.id == shipId && !status.hit)
  }

  def areShipsAlive(playerId: Int): Boolean = grids(playerId).values.forall(_.hit)

}
