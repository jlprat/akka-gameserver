package com.github.jlprat.gameserver.fsm.model

import org.scalatest.WordSpec

/**
 * Created by josep on 2/21/15.
 */
class DiscardPileSpec extends WordSpec {

  import Card._

  "A discard pile" must {
    "be creatable empty" in {
      assert(DiscardPile.empty.cards.size === 0)
    }
    val cardOne = PlayedCard(Card(CardId(1), Rank(1), Suit("suit")))
    "be creatable from a card" in {
      val singleCardPile = DiscardPile(cardOne)
      assert(singleCardPile.cards.size === 1)
      assert(singleCardPile.cards.head === cardOne)
    }
    val cardTwo = PlayedCard(Card(CardId(2), Rank(2), Suit("suit")))
    "be creatable from a list of cards " in {
      val discardPile = DiscardPile(List(cardOne, cardTwo))
      assert(discardPile.cards.size === 2)
      assert(discardPile.cards.head === cardOne)
      assert(discardPile.cards.last === cardTwo)
    }
    "extendable with cards" in {
      val discardPile = cardTwo :: DiscardPile(cardOne)
      assert(discardPile.cards.size === 2)
      assert(discardPile.cards.head === cardTwo)
      assert(discardPile.cards.last === cardOne)
    }
  }
}
