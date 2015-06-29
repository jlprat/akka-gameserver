package com.github.jlprat.gameserver.become.model

import org.scalatest.WordSpec

/**
 * Created by josep on 12/23/14.
 */
class CardSpec extends WordSpec {
  val cardAceSpades = Card(1,1,"spades")
  val cardAceSpadesClone = Card(2,1,"spades")
  val cardTwoSpades = Card(2,2,"spades")
  val cardThreeSpades = Card(2,3,"spades")
  val cardTwoHearts = Card(2,2,"hearts")
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
