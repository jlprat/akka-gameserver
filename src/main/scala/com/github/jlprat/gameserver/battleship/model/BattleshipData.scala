package com.github.jlprat.gameserver.battleship.model

/**
 * Created by josep on 9/16/15.
 */
case class CellStatus(id: Short, hit: Boolean = false)

case class BattleshipData(grids: Array[Map[(Short,Short), CellStatus]], currentPlayer: Int, pendingShot: Option[(Short, Short)], shipsToPlace: Array[Vector[Short]]) {
  def isOccupied(playerIdx: Int, x: Short, y: Short): Boolean = {
    grids(playerIdx).isDefinedAt((x,y))
  }

  def canShipBePlaced(playerIdx: Int, shipId: Short, x: Short, y: Short, size: Short, vertical: Boolean) : Boolean = {
    case true if y + size < 10 =>
      val yPositions = List.tabulate(size)(elem => y + elem)
       yPositions.forall(yPos => !isOccupied(playerIdx, x, yPos.toShort))
    case false =>
      val xPositions = List.tabulate(size)(elem => x + elem)
      xPositions.forall(xPos => !isOccupied(playerIdx, xPos.toShort, y))
  }

  def placeShip(playerIdx: Int, shipId: Short, x: Short, y: Short, size: Short, vertical: Boolean): BattleshipData = vertical match {
    case true if y + size < 10 =>
      val yPositions = List.tabulate(size)(elem => y + elem)
      val canBePlaced = yPositions.forall(yPos => !isOccupied(playerIdx, x, yPos.toShort))
      if (canBePlaced) {
        val clonedGrids = grids.clone()
        //yPositions.foldLeft(clonedGrids)((theGrids,yPos) => theGrids(playerIdx) + ((x, yPos.toShort) -> CellStatus(shipId)))
        //TODO fix this shit!!!
        yPositions.foreach(yPos => clonedGrids(playerIdx) + ((x, yPos.toShort) -> CellStatus(shipId)))
        BattleshipData(clonedGrids, currentPlayer, pendingShot, shipsToPlace)
      } else
        BattleshipData(grids, currentPlayer, pendingShot, shipsToPlace)
    case false =>
      val xPositions = List.tabulate(size)(elem => x + elem)
      val canBePlaced = xPositions.forall(xPos => !isOccupied(playerIdx, xPos.toShort, y))
      if (canBePlaced) {
        val clonedGrids = grids.clone()
        xPositions.foreach(xPos => clonedGrids(playerIdx) + ((xPos.toShort, y) -> CellStatus(shipId)))
        BattleshipData(clonedGrids, currentPlayer, pendingShot, shipsToPlace)
      } else
        BattleshipData(grids, currentPlayer, pendingShot, shipsToPlace)
  }

  def wouldBeAShot: Boolean = {
    if (grids(opponent).isDefinedAt((pendingShot.get._1, pendingShot.get._2))) true
    else false
  }

  def shoot: (Boolean, BattleshipData) = {
    if (grids(opponent).isDefinedAt((pendingShot.get._1, pendingShot.get._2))) {
      val clonedGrids = grids.clone()
      clonedGrids(opponent).get((pendingShot.get._1, pendingShot.get._2)).foreach(cellStatus => clonedGrids(opponent).updated((pendingShot.get._1, pendingShot.get._2), cellStatus.copy(hit = true)))
      (true, BattleshipData(clonedGrids, currentPlayer, pendingShot, shipsToPlace))
    } else {
      (false, BattleshipData(grids, currentPlayer, pendingShot, shipsToPlace))
    }
  }

  def opponent: Int = {
    if (currentPlayer == 1) 0 else 1
  }

  def isShipSunk(playerId: Int, shipId: Short): Boolean = {
    !grids(playerId).values.exists(status => status.id == shipId && !status.hit)
  }

  def areShipsAlive(playerId: Int): Boolean = grids(playerId).values.forall(_.hit)

}
