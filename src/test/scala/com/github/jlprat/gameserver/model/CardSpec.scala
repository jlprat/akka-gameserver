package com.github.jlprat.gameserver.model

import org.scalatest.WordSpec

/**
 * Created by josep on 12/23/14.
 */
class CardSpec extends WordSpec {
  "A Card" should {
    "be creatable in short way" in {
      assert(Card(1, 1, "spades") != null)
    }
    "be equal" when {
      "compared to itself" in {
        assert(Card(1,1, "spades") == Card(1,1, "spades"))
        }
      }
    "differ" when {
      "compared to same card but different Id" in {
        assert(Card(1,1,"spades") != Card(2,1,"spades"))
      }
      "compared to different rank" in {
        assert(Card(1,2,"spades") != Card(1,3,"spades"))
      }
      "compared to different suit" in {
        assert(Card(1,2,"spades") != Card(1,2,"hearts"))
      }
    }
    "be printable" in {
      assert(Card(1,1,"spades").toString() == "Card {id: 1, rank: 1, suit: spades}")
    }
  }
}
