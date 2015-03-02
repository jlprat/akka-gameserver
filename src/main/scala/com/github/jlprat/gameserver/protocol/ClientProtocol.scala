package com.github.jlprat.gameserver.protocol

/**
 * Contains the protocol messages between client and server
 * Created by josep on 3/2/15.
 */
object ClientProtocol {

  import com.github.jlprat.gameserver.model.{Card,Hand}

  /**
   * Contains all messages that are sent from Client to Server
   */
  object In {

    /**
     * Client requests to play a card
     * @param card the card to be played
     */
    case class PlayCardRequest(card: Card)

    /**
     * Client requests to get a card from the pile
     */
    case object TakeCardsRequest

    /**
     * Client announces Last Card
     */
    case object AnnounceLastCard

    /**
     * Client requests to change the suit
     * @param suit the suit to be changed to
     */
    case class SelectSuitRequest(suit: String)

    /**
     * Client leaves the game
     */
    case object Leave
  }

  /**
   * Contains all messages that are sent from Server to client
   */
  object Out {

    /**
     * Client is informed about a card being played successfully
     * @param card the card played
     * @param playerId the player who played it
     */
    case class PlayedCardSuccessfully(card: Card, playerId: Int)

    /**
     * Client is informed about a card being played irregularly
     * @param card the card that was played irregularly
     * @param playerId the player who tried to play it
     */
    case class PlayedCardIrregularly(card: Card, playerId: Int)

    /**
     * Client is informed about a card that should be added to its hand. Only sent to this client
     * @param hand the cards that should be added to client's hand
     * @param playerId the intended receiver of the message
     */
    case class ReceiveCard(hand: Hand, playerId: Int)

    /**
     * Client is informed about a card that should be added to some other client's hand
     * @param numberCards the number of cards to be added to the client's hand
     * @param playerId the client who receives the hand
     */
    case class ReceiveCardOpponent(numberCards: Int, playerId: Int)

    /**
     * Client is informed about a player who announces Last Card
     * @param playerId the player who announced Last Card
     */
    case class LastCallCalled(playerId: Int)

    /**
     * Client is informed about a player who forgot to announce Last Card
     * @param playerId the player who forgot to announce Last Card
     */
    case class ForgottenToCallLastCard(playerId: Int)

    /**
     * Client is informed about a change of direction
     * @param clockwise if the direction is clockwise or not
     */
    case class ChangeGameDirection(clockwise: Boolean)

    /**
     * Client is informed about somebody being skipped
     * @param playerId the player that is skipped
     */
    case class SkippedPlayer(playerId: Int)

    /**
     * Client is informed about somebody must select the new suit
     * @param playerId the player who should select the new suit
     */
    case class SelectSuitRequest(playerId: Int)

    /**
     * Client is informed about the new selected suit
     * @param suit the selected suit
     * @param playerId the player who selected the new suit
     */
    case class NewSuitSelected(suit: String, playerId: Int)

    /**
     * Client is informed about who is the next client in turn
     * @param playerId the player in turn
     */
    case class NextPlayerInTurn(playerId: Int)

    /**
     * Client is informed about a client leaving the game
     * @param playerId the player who left
     */
    case class PlayerLeftGame(playerId: Int)

    /**
     * Client is informed about the game results
     * @param results the results of the game
     */
    case class GameResults(results: Map[Int, Int])
  }
}
