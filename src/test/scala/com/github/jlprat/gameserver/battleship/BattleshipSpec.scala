package com.github.jlprat.gameserver.battleship

import akka.actor.ActorSystem
import akka.testkit.{TestFSMRef, TestProbe, ImplicitSender, TestKit}
import com.github.jlprat.gameserver.battleship.actor.BattleShipActor
import com.github.jlprat.gameserver.battleship.model._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
 * Created by josep on 10/5/15.
 */
class BattleshipSpec (_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("FSM"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A Battleship actor" should {
    "start in WaitingForPlayers state and be initialized" in {
      val battleshipFSM = getBattleshipFSM()
      assert(battleshipFSM.stateName === WaitingForPlayers)
      val stateData = battleshipFSM.stateData
      assert(stateData.currentPlayer === 1)
      assert(stateData.grids.forall(_.isEmpty))
      assert(stateData.pendingShot === None)
    }
  }

  "A Battleship actor" when {
    "in WaitingForPlayer state" can {
      "go to PlacingShips state" in {
        val battleshipFSM = getBattleshipFSM()
        assert(battleshipFSM.stateName === WaitingForPlayers)
        battleshipFSM ! PlaceShip(playerId = 1, id = 2, x = 1, y = 1, size = 3, vertical = true)
        assert(battleshipFSM.stateName === CheckingPlacedShips)
        //assert(battleshipFSM.stateData.isOccupied(playerIdx = 1, x = 1, y = 1))
      }
    }
    "in CheckingPlacedShips state" can {
      "go to PlacingShips to come back to same state" in {
        val battleshipFSM = getBattleshipFSM()
        battleshipFSM.setState(CheckingPlacedShips, battleshipFSM.stateData)
        assert(battleshipFSM.stateName === CheckingPlacedShips)
        battleshipFSM ! PlaceShip(playerId = 1, id = 3, x = 2, y = 2, size = 2, vertical = true)
        assert(battleshipFSM.stateName === CheckingPlacedShips)
      }
      "go to WaitingForNextPlayer" in {
        val battleshipFSM = getBattleshipFSM()
        battleshipFSM.setState(CheckingPlacedShips, battleshipFSM.stateData)
        assert(battleshipFSM.stateName === CheckingPlacedShips)
        battleshipFSM ! ShipsPlaced
        assert(battleshipFSM.stateName === WaitingForNextPlayer)
        assert(battleshipFSM.isStateTimerActive)
      }
    }
  }

  def getBattleshipFSM() = {
    val player1Probe = TestProbe()
    val player2Probe = TestProbe()
    TestFSMRef(new BattleShipActor(player1Probe.ref, player2Probe.ref))
  }
}
