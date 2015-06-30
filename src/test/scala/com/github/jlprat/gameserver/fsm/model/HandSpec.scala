package com.github.jlprat.gameserver.fsm.model

import org.scalatest.WordSpec

/**
 * Created by josep on 12/23/14.
 */
class HandSpec extends WordSpec {

  import Card._

  val cardOne = Card(CardId(1),Rank(1),Suit("diamonds"))
  val cardTwo = Card(CardId(2), Rank(2), Suit("diamonds"))
  val cardThree = Card(CardId(3), Rank(3), Suit("diamonds"))
  val cardFour = Card(CardId(4), Rank(1), Suit("clubs"))
  val cardFive = Card(CardId(5), Rank(2), Suit("spades"))
  val threeCards = List(cardOne, cardTwo, cardThree)
  val cardsOneAndTWo = List(cardOne, cardTwo)
  val fiveCards = List(cardTwo, cardThree, cardOne, cardFive, cardFour)
  val emptyHand = Hand()
  val twoCardsHand = Hand(cardsOneAndTWo)
  val oneCardHand = Hand(cardThree)
  val combinedHand = Bunch(oneCardHand.cards) ::: twoCardsHand
  val reversedCombinedHand = Hand(threeCards.reverse)
  val fiveCardHand = Hand(fiveCards)
  
  "A Hand" can {
    "be creatable" when {
      "no cards are provided" in {
        assert(emptyHand.size === 0)
      }
      "a single card is provided" in {
        assert(oneCardHand.size === 1)
        assert(oneCardHand.cards == List(cardThree))
      }
      "multiple cards are provided" in {
        assert(twoCardsHand.size === cardsOneAndTWo.size)
        assert(twoCardsHand.cards === cardsOneAndTWo)
      }
    }
    "be appended to another one" in {
      assert(combinedHand.size === threeCards.size)
      assert(combinedHand.cards === threeCards)
    }
    "be added a new card to it" in {
      assert((cardThree :: twoCardsHand).sort === combinedHand.sort)
    }
    "be sorted" in {
      assert(reversedCombinedHand.sort === combinedHand)
    }
  }
  "A Hand" must {
    "be printable" in {
      assert(twoCardsHand.toString() === "Hand [Card {id: 1, rank: 1, suit: diamonds},Card {id: 2, rank: 2, suit: diamonds}]")
    }
    "offer a way to check if a card is present" in {
      assert(twoCardsHand.exists(_ === cardOne))
      assert(!twoCardsHand.exists(_ === cardFive))
    }
  }
  "A Hand" when {
    "not empty" must {
      "offer a way to play a card" when {
        "card is present" in {
          val (cardOption, hand) = combinedHand.play(cardOne)
          assert(hand.size === 2)
          assert(cardOption.isDefined)
          assert(cardOption.forall(_ == PlayedCard(cardOne)))
        }
      }
      "fail to play a card" when {
        "card is not present" in {
          val (cardOption, hand) = combinedHand.play(cardFive)
          assert(hand.size === 3)
          assert(cardOption.isEmpty)
        }
      }
    }
    "empty" must {
      "fail to play a card" in {
        val (cardOption, hand) = emptyHand.play(cardFive)
        assert(hand.size === 0)
        assert(cardOption.isEmpty)
      }
    }
  }
  "Sorting cards" when {
    "using rank sorting" should {
      "lowest card is first" in {
        assert(fiveCardHand.sortByRank.cards.head === cardFour)
      }
      "highest card is last" in {
        assert(fiveCardHand.sortByRank.cards.last === cardThree)
      }
      "have cards sorted correctly" in {
        assert(fiveCardHand.sortByRank.cards === List(cardFour, cardOne, cardTwo, cardFive, cardThree))
      }
    }
    "using suit sorting" should {
      "lowest card is first" in {
        assert(fiveCardHand.sortBySuit.cards.head === cardFour)
      }
      "highest card is last" in {
        assert(fiveCardHand.sortBySuit.cards.last === cardFive)
      }
      "have cards sorted correctly" in {
        assert(fiveCardHand.sortBySuit.cards === List(cardFour, cardOne, cardTwo, cardThree, cardFive))
      }
    }
  }
}
