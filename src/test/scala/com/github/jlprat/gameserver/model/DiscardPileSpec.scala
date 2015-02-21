package com.github.jlprat.gameserver.model {

import org.scalatest.WordSpec

/**
 * Created by josep on 2/21/15.
 */
class DiscardPileSpec extends WordSpec {

  "A discard pile" must {
    "be creatable empty" in {
      assert(DiscardPile.empty.cards.size === 0)
    }
    "be creatable from a card" in {
      val singleCardPile = DiscardPile(Card(1, 1, "suit"))
      assert(singleCardPile.cards.size === 1)
      assert(singleCardPile.cards.head === Card(1, 1, "suit"))
    }
    "be creatable from a list of cards " in {
      val discardPile = DiscardPile(List(Card(1, 1, "suit"), Card(2, 2, "suit")))
      assert(discardPile.cards.size === 2)
      assert(discardPile.cards.head === Card(1, 1, "suit"))
      assert(discardPile.cards.last === Card(2, 2, "suit"))
    }
    "extendable with cards" in {
      val discardPile = Card(2, 2, "suit") :: DiscardPile(Card(1, 1, "suit"))
      assert(discardPile.cards.size === 2)
      assert(discardPile.cards.head === Card(2, 2, "suit"))
      assert(discardPile.cards.last === Card(1, 1, "suit"))
    }
  }
}

}

package foo {
  import org.scalatest.WordSpec

  class DiscardPileSpec extends WordSpec {

    import com.github.jlprat.gameserver.model._

    "access to inner members giis protected" in {
      assertDoesNotCompile(
        """
        val discardPile = Card(2, 2, "suit") :: DiscardPile(Card(1, 1, "suit"))
        discardPile.cards
        """.stripMargin)
    }
  }
}

