package com.github.jlprat.gameserver.battleship.actor

import scala.collection.mutable
import akka.actor.{ActorLogging, FSM}
import com.github.jlprat.gameserver.battleship.model._

/**
 * Created by josep on 9/16/15.
 */
class BattleShipActor extends ActorLogging with FSM[BattleshipState, BattleshipData]{

  startWith(WaitingForPlayers, new BattleshipData(new Array(2)))

  when(WaitingForPlayers) {
    case Event(PlaceShip(playerId, shipId, x, y, size, vertical), data: BattleshipData) =>
      goto(PlacingShips) using data.placeShip(playerId, shipId, x, y, size, vertical)
  }

  when(PlacingShips) {
    case Event(PlaceShip(playerId, shipId, x, y, size, vertical), data: BattleshipData) =>
      stay using data.placeShip(playerId, shipId, x, y, size, vertical)
    case Event(ShipsPlaced, data: BattleshipData) =>
      goto(WaitingForNextPlayer)
  }

  initialize()
}
