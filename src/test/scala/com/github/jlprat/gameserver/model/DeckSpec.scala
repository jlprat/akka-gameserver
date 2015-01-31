package com.github.jlprat.gameserver.model

import org.scalatest.WordSpec

/**
 * Defines the Specification of the Deck domain class
 * Created by josep on 1/31/15.
 */
class DeckSpec extends WordSpec{
  val numberCards = 52
  //TODO think about creating helpers for Deck Creation
  val deck = Deck(List.tabulate(numberCards)(elem => Card(elem, elem, "suit")))
  val cards = List.tabulate(5)(elem => Card(elem, elem, "suit"))

  "An initial Deck" should {
    "have "+ numberCards +" cards" in {
      assert(deck.size === numberCards)
    }
    "have all cards with unique Id's" in {
      assert(deck.deck.map(_.id).distinct.size === numberCards)
    }
  }
  "A deck" can {
    "be created empty" in {
      assert(Deck.empty().size === 0)
    }
    "be created from a list of Cards" in {
      assert(Deck(cards).size === cards.size)
    }
  }

  "A deck" when {
    "not empty" should {
      "let me pick a card" which {
        val takenOption = deck.take(1)
        assert(takenOption.isDefined)
        val (cards, restDeck) = takenOption.get
        assert(cards.size === 1)
        "is the top card of the deck" in {
          assert(cards.exists(_ == deck.deck.head))
        }
        "and the remaining deck has one card less" in {
          assert(restDeck.size === numberCards - 1)
        }
      }
      "prevent me from picking more cards than the deck itself" in {
        assert(deck.take(100).isEmpty)
      }
      "let me draw cards to players" when {
        "2 players are present" must {
          val drawnOption = deck.draw(2,6,1)
          assert(drawnOption.isDefined)
          val (hands, restOfDeck) = drawnOption.get
          "have 2 hands" in {
            assert(hands.size === 2)
            assert(hands.forall(_.size === 6))
          }
          "have 1st player hand" which {
            "has exactly these cards" in {
              assert(hands.head === Hand(List.tabulate(6)(elem => Card(elem * 2, elem * 2,"suit"))))
            }
          }
          "have 2nd player hand" which {
            "has exactly these cards" in {
              assert(hands.last === Hand(List.tabulate(6)(elem => Card(elem * 2 + 1, elem * 2 + 1,"suit"))))
            }
          }
          "the remaining deck" must {
            "have 12 cards less" in {
              assert(restOfDeck.size === numberCards - 12)
            }
            "have the 13th card as a top card" in {
              assert(restOfDeck.take(1).map(_._1).get === Hand(Card(12,12,"suit")))
            }
          }
        }
        "taking 2 cards at a time" must {
          val drawnOption = deck.draw(3,6,2)
          assert(drawnOption.isDefined)
          val (hands, restOfDeck) = drawnOption.get
          "have 3 hands with 6 cards each" in {
            assert(hands.size === 3)
            assert(hands.forall(_.size === 6))
          }
          "have the following cards for 1st player" in {
            assert(hands.head === Hand(List.tabulate(3)(elem => List(Card(elem * 6, elem * 6, "suit"), Card(elem * 6 + 1,elem * 6 + 1,"suit"))).flatten))
          }
          "have the following cards for 2nd player" in {
            assert(hands.tail.head === Hand(List.tabulate(3)(elem => List(Card(elem * 6 + 2, elem * 6 + 2, "suit"), Card(elem * 6 + 3,elem * 6 + 3,"suit"))).flatten))
          }
          "have the following cards for 3rd player" in {
            assert(hands.last === Hand(List.tabulate(3)(elem => List(Card(elem * 6 + 4, elem * 6 + 4, "suit"), Card(elem * 6 + 5,elem * 6 + 5,"suit"))).flatten))
          }
        }
      }
      "prevent me from drawing more cards" when {
        "there are not enough cards in the deck" in {
          val drawnOption = deck.draw(10,6,2)
          assert(drawnOption.isEmpty)
        }
      }
    }
    "empty" should {
      "not let me pick a card" in {
        assert(Deck.empty().take(1).isEmpty)
      }
      "not let me draw cards to players" in {
        assert(Deck.empty().draw(2,4).isEmpty)
      }
    }
  }

}
