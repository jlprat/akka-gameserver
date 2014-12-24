package com.github.jlprat.gameserver.model

import org.scalatest.WordSpec

/**
 * Created by josep on 12/23/14.
 */
class HandSpec extends WordSpec {

  val cardOne = Card(1,1,"diamonds")
  val cardTwo = Card(2,2,"diamonds")
  val cardThree = Card(3,3,"diamonds")
  val cardFour = Card(4,1,"clubs")
  val cardFive = Card(5,2,"spades")
  val threeCards = List(cardOne, cardTwo, cardThree)
  val cardsOneAndTWo = List(cardOne, cardTwo)
  val fiveCards = List(cardTwo, cardThree, cardOne, cardFive, cardFour)
  val emptyHand = Hand()
  val twoCardsHand = Hand(cardsOneAndTWo)
  val oneCardHand = Hand(cardThree)
  val combinedHand = oneCardHand ::: twoCardsHand
  val reversedCombinedHand = Hand(threeCards.reverse)
  val fiveCardHand = Hand(fiveCards)
  
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
      assert(combinedHand.cards.size == threeCards.size)
      assert(combinedHand.cards == threeCards)
    }
    "be sorted" in {
      assert(reversedCombinedHand.sort == combinedHand)
    }
  }
  "A Hand" should {
    "be printable" in {
      assert(twoCardsHand.toString() == "Hand [Card {id: 1, rank: 1, suit: diamonds},Card {id: 2, rank: 2, suit: diamonds}]")
    }
  }
  "Sorting cards" when {
    "using rank sorting" should {
      "lowest card is first" in {
        assert(fiveCardHand.sortByRank.cards.head == cardFour)
      }
      "highest card is last" in {
        assert(fiveCardHand.sortByRank.cards.last == cardThree)
      }
      "have cards sorted correctly" in {
        assert(fiveCardHand.sortByRank.cards == List(cardFour, cardOne, cardTwo, cardFive, cardThree))
      }
    }
    "using suit sorting" should {
      "lowest card is first" in {
        assert(fiveCardHand.sortBySuit.cards.head == cardFour)
      }
      "highest card is last" in {
        assert(fiveCardHand.sortBySuit.cards.last == cardFive)
      }
      "have cards sorted correctly" in {
        assert(fiveCardHand.sortBySuit.cards == List(cardFour, cardOne, cardTwo, cardThree, cardFive))
      }
    }
  }
}
