package com.github.jlprat.gameserver.model

import org.scalatest.WordSpec

/**
 * Created by josep on 12/23/14.
 */
class HandSpec extends WordSpec {

  def cardOne = Card(1,1,"diamonds")
  def cardTwo = Card(2,2,"diamonds")
  def cardThree = Card(3,3,"diamonds")
  def allCards = List(cardOne, cardTwo, cardThree)
  def cardsOneAndTWo = List(cardOne, cardTwo)
  def emptyHand = Hand()
  def twoCardsHand = Hand(cardsOneAndTWo)
  def oneCardHand = Hand(cardThree)
  def combinedHand = oneCardHand ::: twoCardsHand
  def reversedCombinedHand = Hand(allCards.reverse)

  "A Hand" can {
    "be creatable" when {
      "no cards are provided" in {
        assert(emptyHand.cards.size == 0)
      }
      "a single card is provided" in {
        assert(oneCardHand.cards.size == 1)
        assert(oneCardHand.cards == List(cardThree))
      }
      "multiple cards are provided" in {
        assert(twoCardsHand.cards.size == cardsOneAndTWo.size)
        assert(twoCardsHand.cards == cardsOneAndTWo)
      }
    }
    "be appended to another one" in {
      assert(combinedHand.cards.size == allCards.size)
      assert(combinedHand.cards == allCards)
    }
    "be sortable" in {
      assert(reversedCombinedHand.sort == combinedHand)
    }
  }
  "A Hand" should {
    "be printable" in {
      assert(twoCardsHand.toString() == "Hand [Card {id: 1, rank: 1, suit: diamonds},Card {id: 2, rank: 2, suit: diamonds}]")
    }
  }
}
