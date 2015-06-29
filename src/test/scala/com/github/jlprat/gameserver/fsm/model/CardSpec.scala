package com.github.jlprat.gameserver.fsm.model

import org.scalatest.WordSpec

/**
 * Created by josep on 12/23/14.
 */
class CardSpec extends WordSpec {
  import Card._

  val cardAceSpades = Card(CardId(1),Rank(1),Suit("spades"))
  val cardAceSpadesClone = Card(CardId(2), Rank(1), Suit("spades"))
  val cardTwoSpades = Card(CardId(2), Rank(2), Suit("spades"))
  val cardThreeSpades = Card(CardId(2), Rank(3), Suit("spades"))
  val cardTwoHearts = Card(CardId(2), Rank(2), Suit("hearts"))
  "A Card" should {
    "be creatable in short way" in {
      assert(cardAceSpades !== null)
    }
    "be equal" when {
      "compared to itself" in {
        assert(cardAceSpades === cardAceSpades)
        }
      }
    "differ" when {
      "compared to same card but different Id" in {
        assert(cardAceSpades !== cardAceSpadesClone)
      }
      "compared to different rank" in {
        assert(cardTwoSpades !== cardThreeSpades)
      }
      "compared to different suit" in {
        assert(cardTwoSpades !== cardTwoHearts)
      }
    }
    "be printable" in {
      assert(cardAceSpades.toString() === "Card {id: 1, rank: 1, suit: spades}")
    }
  }
  "Sorting by rank" should {
    "be 0" when {
      "cards have the same rank and suit" in {
        assert(RankOrdering.compare(cardAceSpades, cardAceSpadesClone) === 0)
      }
    }
    "be negative" when {
      "first card rank is lower than the second one" in {
        assert(RankOrdering.compare(cardAceSpades, cardTwoSpades) < 0)
      }
      "ranks are the same but first card suit is lower alphabetically" in {
        assert(RankOrdering.compare(cardTwoHearts, cardTwoSpades) < 0)
      }
    }
    "be positive" when {
      "first card rank is higher than the second one" in {
        assert(RankOrdering.compare(cardTwoSpades, cardAceSpades) > 0)
      }
      "ranks are the same but first card suit is higher alphabetically" in {
        assert(RankOrdering.compare(cardTwoSpades, cardTwoHearts) > 0)
      }
    }
  }
  "Sorting by suit" should {
    "be 0" when {
      "cards have the same rank and suit" in {
        assert(SuitOrdering.compare(cardAceSpades, cardAceSpadesClone) === 0)
      }
    }
    "be negative" when {
      "first card suit is lower alphabetically than the second one" in {
        assert(SuitOrdering.compare(cardTwoHearts, cardTwoSpades) < 0)
      }
      "suits are the same but first card rak is lower" in {
        assert(SuitOrdering.compare(cardAceSpades, cardTwoSpades) < 0)
      }
    }
    "be positive" when {
      "first card suit is higher alphabetically than the second one" in {
        assert(SuitOrdering.compare(cardTwoSpades, cardTwoHearts) > 0)
      }
      "suits are the same but first card rank is higher" in {
        assert(SuitOrdering.compare(cardTwoSpades, cardAceSpades) > 0)
      }
    }
  }
}
