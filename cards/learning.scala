//
// See http://old.nabble.com/Performance-in-interactive-mode-(Scala-2.8)-td26877438.html
//
// From http://old.nabble.com/file/p26877647/learning.scala
//      http://old.nabble.com/file/p26877784/learning.scala (updated version)
//

package ScalaTutorial

class Suit(val name: String, val abbreviation: String, val rank: Int) {
}
case object Suit {
  def apply(name: String, abbreviation: String, rank: Int): Suit = new Suit(name, abbreviation, rank) 
  val Clubs = new Suit("Clubs", "c", 0)
  val Diamonds = new Suit("Diamonds", "d", 1)
  val Hearts = new Suit("Hearts", "h", 2)
  val Spades = new Suit("Spades", "s", 3)
  val Suited = new Suit("Suited", "m", 9)
}
import Suit._  


class Card(val denomination: Int, val suit: Suit) {
  val index = denomination - 2 + suit.rank * 13
  def +(otherCard: Card) = Hand(this, otherCard)
  override def toString() = suit.abbreviation + "  23456789TJQKA"(denomination)
}
case object Card {
  def apply(denomination: Int, suit: Suit): Card = new Card(denomination, suit) 
  val A = 14
  val K = 13
  val Q = 12
  val J = 11
  val T = 10

  val c2 = Card(2, Clubs)
  val c3 = Card(3, Clubs)
  val c4 = Card(4, Clubs)
  val c5 = Card(5, Clubs)
  val c6 = Card(6, Clubs)
  val c7 = Card(7, Clubs)
  val c8 = Card(8, Clubs)
  val c9 = Card(9, Clubs)
  val ct = Card(T, Clubs)
  val cj = Card(J, Clubs)
  val cq = Card(Q, Clubs)
  val ck = Card(K, Clubs)
  val ca = Card(A, Clubs)

  val d2 = Card(2, Diamonds)
  val d3 = Card(3, Diamonds)
  val d4 = Card(4, Diamonds)
  val d5 = Card(5, Diamonds)
  val d6 = Card(6, Diamonds)
  val d7 = Card(7, Diamonds)
  val d8 = Card(8, Diamonds)
  val d9 = Card(9, Diamonds)
  val dt = Card(T, Diamonds)
  val dj = Card(J, Diamonds)
  val dq = Card(Q, Diamonds)
  val dk = Card(K, Diamonds)
  val da = Card(A, Diamonds)

  val h2 = Card(2, Hearts)
  val h3 = Card(3, Hearts)
  val h4 = Card(4, Hearts)
  val h5 = Card(5, Hearts)
  val h6 = Card(6, Hearts)
  val h7 = Card(7, Hearts)
  val h8 = Card(8, Hearts)
  val h9 = Card(9, Hearts)
  val ht = Card(T, Hearts)
  val hj = Card(J, Hearts)
  val hq = Card(Q, Hearts)
  val hk = Card(K, Hearts)
  val ha = Card(A, Hearts)

  val s2 = Card(2, Spades)
  val s3 = Card(3, Spades)
  val s4 = Card(4, Spades)
  val s5 = Card(5, Spades)
  val s6 = Card(6, Spades)
  val s7 = Card(7, Spades)
  val s8 = Card(8, Spades)
  val s9 = Card(9, Spades)
  val st = Card(T, Spades)
  val sj = Card(J, Spades)
  val sq = Card(Q, Spades)
  val sk = Card(K, Spades)
  val sa = Card(A, Spades)
}
import Card._


class Hand(card1: Card, card2: Card) {
  val _1 = if (card1.denomination > card2.denomination) card1 else card2
  val _2 = if (card1.denomination > card2.denomination) card2 else card1

  val suited = _1.suit == _2.suit
  val handType = _1.denomination.toString() + _2.denomination + (if (suited) "s" else "")

  def +(otherCard: Card) = List(_1, _2) :+ otherCard
  def vs(hand2: Hand) = Comparison.compare(List(List(_1, _2), List(hand2._1, hand2._2)), List())

  override def toString() = "Hand(" + _1 + ", " + _2 + ")"
}
object Hand {
  def apply(card1: Card, card2: Card): Hand = new Hand(card1, card2)
  implicit def handToList(hand: Hand) = List(hand._1, hand._2)
}
import Hand._


case object Deck {
  val deck = List(c2, c3, c4, c5, c6, c7, c8, c9, ct, cj, cq, ck, ca,
                  d2, d3, d4, d5, d6, d7, d8, d9, dt, dj, dq, dk, da,
                  h2, h3, h4, h5, h6, h7, h8, h9, ht, hj, hq, hk, ha,
                  s2, s3, s4, s5, s6, s7, s8, s9, st, sj, sq, sk, sa)
  def deal = shuffle(deck)
  def shuffle[A](deck: List[A]) = {
    import scala.util.Random
    def _shuffle(deck: List[A], random: Random): List[A] = {
      val index = random.nextInt(deck.size)
      deck match {
  case x :: xs if xs.size > 0 => deck(index) :: _shuffle(deck.zipWithIndex.filter(a => a._2 != index).map(a => a._1), random)
  case x => x
      }
    }
    _shuffle(deck, new Random)
  }
}
import Deck._

class HandRanking(val rank: Int, val denominations: List[Int]) extends AnyRef with Ordered[HandRanking] {
  override def compare(other: HandRanking): Int = 
     rank match {
       case other.rank => 
   denominations.zip(other.denominations).dropWhile((a) => a._1 == a ._2) match {
     case List() => 0
     case x :: xs => x._1.compare(x._2)
   }
       case _ => rank.compare(other.rank)
     }
  override def equals(other: Any) = 
    other.isInstanceOf[HandRanking] match {
      case true => compare(other.asInstanceOf[HandRanking]) == 0
      case false => false
    }
}
class HighCards(denoms: List[Int]) extends HandRanking(1, denoms) {
  override def toString() = "HighCards(" + denominations.map(_.toString).reduceRight(_ + " " + _) + ")"
}
class OnePair(denoms: List[Int]) extends HandRanking(2, denoms)
class TwoPair(denoms: List[Int]) extends HandRanking(3, denoms)
class ThreeOfAKind(denoms: List[Int]) extends HandRanking(4, denoms)
class Straight(denoms: List[Int]) extends HandRanking(5, denoms) {
  override def toString() = "Straight(" + denominations(0) + " high)"
}
class Flush(denoms: List[Int]) extends HandRanking(6, denoms)
class FullHouse(denoms: List[Int]) extends HandRanking(7, denoms) {
  override def toString() = "FullHouse(" + denominations.map(_.toString).reduceRight(_ + " " + _) + ")"
}
class FourOfAKind(denoms: List[Int]) extends HandRanking(8, denoms)
class StraightFlush(denoms: List[Int]) extends HandRanking(9, denoms)

case object HandRanking {
  // Single pass evaluation of 5 to 7 card hands
  // Four eight bit counters for the suits.
  val suitCounters = Array(0x1,0x100,0x10000,0x1000000)

  // Thirteen two bit counters for the denominations, Ace in lowest counter. two leading zeroes, as
  // denominations start at 2, but array indexes start at 0.
  val denominationCounters = Array(0, 0, 0x1000000, 0x400000, 0x100000, 0x40000, 0x10000, 
                                         0x4000, 0x1000, 0x400, 0x100, 0x40, 0x10, 0x4, 0x1)

  // Ten three bit counters for the straights. Ace high to 5 high.  If any counter gets to 5, we have a straight.
  val straightCounters = Array(0, 0, 0x9000000, 0x9200000, 0x9240000, 0x9248000, 0x1249000, 
                                     0x249200, 0x49240, 0x9248, 0x1249, 0x249, 0x49, 0x9, 0x8000001)

  // One bit hit detectors for the 10 3-bit straight counters
  val straightCountHits = 0x9249249

  // Count the number of trailing binary zeros in a non-zero 32-bit number using a de Bruijn sequence.
  def trailingZeroes(v: Int) = {
    val deBruijnOffsets = Array(0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 
        31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9)
    deBruijnOffsets(((v & -v) * 0x077cb531) >>> 27)
  }  

  // Produce a list of the n highest cards in the denomination flag word.
  def highCards(denominations: Int, n: Int): List[Int] = {
    val highest = 14 - trailingZeroes(denominations) / 2
    List(highest) ++ (if (n > 1) highCards(denominations ^ denominationCounters(highest),n - 1) else List())
  }

  // Figure out what hand the cards represent.  From 5 to seven cards can be passed in,
  // with the cards in either input argument.
  def apply(cards: List[Card], board: List[Card] = List()) = {
    var suits = 0x03030303 // Preset to 3, so flush hands of 5, 6, 7 cards give 8, 9 or 0xa
                           // counts, which can be tested by checking the 8 bit only.
    var denominations = 0  // recorder of first hit for denomination
    var denominationMultiples = 0 // counts of repeats, i.e. count of 2 indicates 3 of a kind.
    var straightCounts = 0 // straight counters, if a counter reaches 5, that's a straight.

    // Run through the cards, accumulating counts for denominations, suits and
    // straights.
    for (card <- cards ++ board) {
      suits += suitCounters(card.suit.rank)
      (denominationCounters(card.denomination) & denominations) != 0 match {
  case false => 
    // We haven't seen a card of this denomination yet.
    straightCounts += straightCounters(card.denomination)
          denominations += denominationCounters(card.denomination)
  case true => 
    // We have already seen a card of this denomination.
          denominationMultiples += denominationCounters(card.denomination)
      }
    }

    // Figure out the hand, flushes, straights, then high card combinations.  With
    // at most seven cards, if we have a flush or straight we can't have a better 
    // high card hand like four-of-a-kind or a full house.
    val flushes = suits & 0x08080808
    flushes match {
      case 0 =>
  val straights = straightCounts & (straightCounts >> 2) & straightCountHits
  straights match {
    case 0 => 
      // No flush or straight, figure out the high cards best hand.
      val fourOfAKinds = (denominationMultiples >> 1) & denominationMultiples & 0x55555555
      fourOfAKinds match {
        case 0 => 
    val threeOfAKinds = denominationMultiples & 0xAAAAAAAA
    threeOfAKinds match {
      case 0 =>
        val pairs = denominationMultiples & 0x55555555
                    pairs match {
          case 0 =>
      new HighCards(highCards(denominations, 5))
          case _ => // There is a pair, possibly two or even three.
      val topPair = 14 - trailingZeroes(pairs) / 2
          // Check for another pair to make a two pair hand.
      val secondPairs = denominationMultiples ^ denominationCounters(topPair)
      secondPairs match {
        case 0 => 
          new OnePair(topPair :: highCards(denominations ^ denominationCounters(topPair), 3))
        case _ =>  
          val secondPair = 14 - trailingZeroes(secondPairs) / 2
                      new TwoPair(List(topPair, secondPair) ++ 
            highCards(denominations ^ denominationCounters(topPair) ^ denominationCounters(secondPair), 1))
      }
        }
      case _ => // There is a three-of-a-kind or full house.
        val denomination = 14 - trailingZeroes(threeOfAKinds) / 2
        // Check for another triplet or pair to make a full house.
        val secondPairs = denominationMultiples ^ (denominationCounters(denomination) << 1)
        secondPairs match {
          case 0 =>
      new ThreeOfAKind(denomination :: highCards(denominations ^ denominationCounters(denomination), 2))
          case _ =>  
      val secondPair = 14 - trailingZeroes(secondPairs) / 2
      new FullHouse(List(denomination, secondPair))
        }
    }
        case _ => // There is a four-of-a-kind.  There can only be one as we have at most 7 cards.
    // Count the number of trailing zeroes and adjust to find the denomination.
    val denomination = 14 - trailingZeroes(fourOfAKinds) / 2
                val kicker = 14 - trailingZeroes(denominations ^ fourOfAKinds) / 2
    new FourOfAKind(List(denomination, kicker))
      }
    case s => new Straight(List(14 - trailingZeroes(s) / 3))
  }
      case _ =>
  // We have a flush or straight flush.
  val suitRank = trailingZeroes(flushes) / 8
        // Create a new straight counter to check for straight flush
        // and get new denomination counts for the high card list for this suit.
        var flushDenominations = 0
        var straightFlushCounts = 0
        for (card <- cards ++ board if card.suit.rank == suitRank) {
    flushDenominations += denominationCounters(card.denomination)
    straightFlushCounts += straightCounters(card.denomination)
  }

        val straightFlushes = straightFlushCounts & (straightFlushCounts >> 2) & straightCountHits
        straightFlushes match {
    case 0 => new Flush(highCards(flushDenominations, 5))
    case sf => new StraightFlush(List(14 - trailingZeroes(sf) / 3))
  }
    }
  }
}

/*
List(
  (Hand(ha, h9), List(d5, da, s7, sa, ca)),  // four of a kind, Aces, with 9 kicker
  (Hand(h5, h9), List(d5, s6, s7, s5, c5)),  // four of a kind, fives, with 9 kicker
  (Hand(h5, h9), List(d9, s6, s7, s9, c5)),  // full house, nines over fives
  (Hand(h4, h9), List(d9, s6, s4, s9, c4)),  // full house, nines over fours (two sets)
  (Hand(h4, h9), List(d9, s6, sj, s9, cj)),  // full house, nines over jacks (set and pair)
  (Hand(h4, h9), List(d9, s4, sj, s9, cj)),  // full house, nines over jacks (set and two pairs)
  (Hand(h5, h9), List(d9, s6, s7, cq, c9)),  // three of a kind, nines with q, 7 kickers 
  (Hand(h5, h9), List(d9, s5, s7, s2, c2)),  // two pair, nines and fives with 7 kicker (three pairs)
  (Hand(h5, h9), List(d9, s5, s7, sj, c2)),  // two pair, nines and fives with j kicker
  (Hand(h5, h9), List(d9, s6, s7, cq, c3)),  // one pair, nines with q, 7, 6 kickers 
  (Hand(h5, hk), List(d9, s6, s7, cq, ct)),  // high cards, k, q, t, 9, 7
  (Hand(h5, hk), List(d9, h6, h7, h8, h4)),  // straight flush, 8 high
  (Hand(h5, hk), List(d9, h6, h7, hq, ct)),  // flush, k, q, 7, 6, 5
  (Hand(h5, hk), List(d9, h6, h7, cq, c8))   // straight, 9 high
).foreach((a) => println("ranked " + a._1 + " with board " + a._2 + " as " + HandRanking(a._1, a._2)))
*/
/*
val deck = Array[Card]() ++ Deck.deck
val seenIndex = Array.make[Int](52,0)
var seenCount = 0
val seenSet = Array.make[Int](52,0)

// Mark a card as seen.
def seen(card: Card) {
  val index = card.denomination - 2 + card.suit.rank * 13
  seenIndex(index) match {
    case s if 0 <= s && s < seenCount && seenSet(s) == index => null
    case s =>
      seenSet(seenCount) = index
      seenIndex(index) = seenCount
      seenCount += 1
  }
}

// Check to see whether we've seen a card already.
def isSeen(card: Card): Boolean = {
  val index = card.denomination - 2 + card.suit.rank * 13
  seenIndex(index) match {
    case s if 0 <= s && s < seenCount && seenSet(s) == index => true
    case _ => false
  }
}

def isSeen(index: Int): Boolean = {
  seenIndex(index) match {
    case s if 0 <= s && s < seenCount && seenSet(s) == index => true
    case _ => false
  }
}

val hand1 = Hand(ca, ha)
val hand2 = Hand(sk, dk)
seen(hand1._1)
seen(hand1._2)
seen(hand2._1)
seen(hand2._2)
*/
/*
var results = Array(0,0,0)
for (i<-0 to 47)
  isSeen(i) match {
    case true => null
    case false =>
      for(j<-i+1 to 48)
  isSeen(j) match {
    case true => null
    case false =>
      for(k<-j+1 to 49)
        isSeen(k) match {
    case true => null
    case false =>
      for(l<-k+1 to 50)
        isSeen(l) match {
          case true => null
          case false =>
      for(m<-l+1 to 51) 
        isSeen(m) match {
          case true => null
          case false =>
            val board = List(deck(i), deck(j), deck(k), deck(l), deck(m))
            val h1 = HandRanking(hand1, board)
            val h2 = HandRanking(hand2, board)
            results(1 - h1.compare(h2)) += 1
            // if (h1 == h2) println("tie: " + board)
        }
        }
        }
  }
  }
val wins = results(0)
val ties = results(1)
val losses = results(2)
println(wins + " " + ties + " " + losses)
println((wins + ties * 0.5)/(wins + ties + losses))
*/

object Comparison {
  // Single pass evaluation of 5 to 7 card hands
  // Four eight bit counters for the suits.
  val suitCounters = Array(0x1,0x100,0x10000,0x1000000)

  // Thirteen two bit counters for the denominations, Ace in lowest counter. two leading zeroes, as
  // denominations start at 2, but array indexes start at 0.
  val denominationCounters = Array(0, 0, 0x1000000, 0x400000, 0x100000, 0x40000, 0x10000, 
                                         0x4000, 0x1000, 0x400, 0x100, 0x40, 0x10, 0x4, 0x1)

  // Ten three bit counters for the straights. Ace high to 5 high.  If any counter gets to 5, we have a straight.
  val straightCounters = Array(0, 0, 0x9000000, 0x9200000, 0x9240000, 0x9248000, 0x1249000, 
                                     0x249200, 0x49240, 0x9248, 0x1249, 0x249, 0x49, 0x9, 0x8000001)

  // One bit hit detectors for the 10 3-bit straight counters
  val straightCountHits = 0x9249249

  // Count the number of trailing binary zeros in a non-zero 32-bit number using a de Bruijn sequence.
  def trailingZeroes(v: Int) = {
    val deBruijnOffsets = Array(0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8, 
        31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9)
    deBruijnOffsets(((v & -v) * 0x077cb531) >>> 27)
  }  

  // Produce a list of the n highest cards in the denomination flag word.
  def highCards(denominations: Int, n: Int): List[Int] = 
    n match {
      case 0 => List()
      case _ => 
  val highest = 14 - trailingZeroes(denominations) / 2
  highest :: highCards(denominations ^ denominationCounters(highest), n - 1)
    }

  def timeTest: Double = {
    val startTime = java.lang.System.currentTimeMillis()
    val result = compare(List(ca+da, hk+sk))
    val totalTime = java.lang.System.currentTimeMillis() - startTime
    println("result: " + result)
    println("time: " + totalTime/1000.0)
    totalTime/1000.0
  }

  // Compare several hands, given the known board so far.  The return value is
  // an Array of results Arrays for each hand.  The results are given as wins, losses, 
  // two-way ties, three-way ties, etc.  So for two hands, the result will be
  // and Array of two three-element Arrays (wins, losses, ties for each hand).  As
  // there are more hands, the ties get multiway.  For example, for four hands, there
  // will be four sub-arrays, each with five elements: wins, losses, two-way ties,
  // three-way ties and four-way ties.
  def compare(hands: List[List[Card]], board: List[Card] = List()) : Array[Array[Int]] = {
    
    // Create an Array of the cards we have not seen already.  We will loop
    // through the array to get our cards.
    val remainingDeckList = (List() ++ deck) -- (board ++ hands.flatten)
    val remainingDeck = Array(remainingDeckList: _*)

    // Create state arrays for each hand.  We will precompute each
    // hand with known board cards.
    // handCountState records hands x possible extra board cards x 4 counters, suits, 
    // denominations, denominationMultiples, straightCounts
    val handCountState = Array.fill[Int](hands.size, 8, 4)(0)
    var SUITS = 0 // Preset to 3, so flush hands of 5, 6, 7 cards give 8, 9 or 0xa
                  // counts, which can be tested by checking the 8 bit only.
    var DENOMINATIONS = 1  // recorder of first hit for denomination
    var DENOMINATION_MULTIPLES = 2 // counts of repeats, i.e. count of 2 indicates 3 of a kind.
    var STRAIGHTS = 3 // straight counters, if a counter reaches 5, that's a straight.

    // Initialize starting counters for each hand.
    // Suit counters actually start at 3, as we want to check when they hit 8, 9 or 10
    // using a simple bit-mask.
    handCountState.foreach((subarray) => subarray(0)(SUITS) = 0x03030303)

    // Count up the hand cards and known board cards.
    for (i <- 0 until hands.length) {
      var j = 1
      for (card <- hands(i) ++ board) {
//  println("counting card " + card)
  handCountState(i)(j)(SUITS) = handCountState(i)(j-1)(SUITS) + suitCounters(card.suit.rank)
  val denominationSeen = (handCountState(i)(j-1)(DENOMINATIONS) & denominationCounters(card.denomination)) != 0
  handCountState(i)(j)(STRAIGHTS) = handCountState(i)(j-1)(STRAIGHTS) + 
            (if (denominationSeen) 0 else straightCounters(card.denomination))
        handCountState(i)(j)(DENOMINATIONS) = handCountState(i)(j-1)(DENOMINATIONS) + 
            (if (denominationSeen) 0 else denominationCounters(card.denomination))
        handCountState(i)(j)(DENOMINATION_MULTIPLES) = handCountState(i)(j-1)(DENOMINATION_MULTIPLES) +
            (if (denominationSeen) denominationCounters(card.denomination) else 0)
//  println(i + " " + j + " %x".format(handCountState(i)(j)(0)) + " %x".format(handCountState(i)(j)(1)) + " %x".format(handCountState(i)(j)(2)) + " %x".format(handCountState(i)(j)(3)))
  j += 1
      }
    }

    // Loop through the remainingDeck, with as many nested loops as their
    // are cards to be dealt.
    val remainingCards = 7 - board.length - hands(0).length
    val loopIndex = Array.fill[Int](8)(0)
    // Set initial loop counters.  We're looping at array levels
    // (8 - remainingCards) to 7, but what we're going to do is
    // set the number at 7-remainingCards to -1, then our normal
    // loop system will set up the rest.
    loopIndex(7 - remainingCards) = -1
    // Preset to the non-existant outside loop (the sixth one if we are
    // going to loop through a five card board).  When we get back to
    // increment the counter for this loop, we're finished.
    val outsideLoop = 7 - remainingCards
    var j = outsideLoop + 1
    loopIndex(j) = loopIndex(j - 1) + 1
    var totals = Array.fill[Array[Int]](hands.length)(Array.fill[Int](hands.length+1)(0))
    val handRanking = Array.make[HandRanking](hands.length, new HighCards(List()))
    do {
      do {
  val card = remainingDeck(loopIndex(j))
  //println("loop level: " + j + " index: " + loopIndex(j) + " card: " + card)
  for (i <- 0 until hands.length) {
    handCountState(i)(j)(SUITS) = handCountState(i)(j-1)(SUITS) + suitCounters(card.suit.rank)
    val denominationSeen = (handCountState(i)(j-1)(DENOMINATIONS) & denominationCounters(card.denomination)) != 0
    handCountState(i)(j)(STRAIGHTS) = handCountState(i)(j-1)(STRAIGHTS) + 
          (if (denominationSeen) 0 else straightCounters(card.denomination))
          handCountState(i)(j)(DENOMINATIONS) = handCountState(i)(j-1)(DENOMINATIONS) + 
          (if (denominationSeen) 0 else denominationCounters(card.denomination))
          handCountState(i)(j)(DENOMINATION_MULTIPLES) = handCountState(i)(j-1)(DENOMINATION_MULTIPLES) +
          (if (denominationSeen) denominationCounters(card.denomination) else 0)
    //println(i + " " + j + " %x".format(handCountState(i)(j)(0)) + " %x".format(handCountState(i)(j)(1)) + 
    //" %x".format(handCountState(i)(j)(2)) + " %x".format(handCountState(i)(j)(3)))
  }

  j match {
    case 7 =>
      for (i <- 0 until hands.length) {
        // Figure out the hand, flushes, straights, then high card combinations.  With
        // seven cards, if we have a flush or straight we can't have a better 
        // high card hand like four-of-a-kind or a full house.
        val suits = handCountState(i)(j)(SUITS)
        val straightCounts = handCountState(i)(j)(STRAIGHTS)
        val denominations = handCountState(i)(j)(DENOMINATIONS)
        val denominationMultiples = handCountState(i)(j)(DENOMINATION_MULTIPLES)
        val flushes = suits & 0x08080808
        
        handRanking(i) = flushes match {
    case 0 =>
      val straights = straightCounts & (straightCounts >> 2) & straightCountHits
      straights match {
        case 0 => 
          // No flush or straight, figure out the high cards best hand.
          val fourOfAKinds = (denominationMultiples >> 1) & denominationMultiples & 0x55555555
          fourOfAKinds match {
      case 0 => 
        val threeOfAKinds = denominationMultiples & 0xAAAAAAAA
        threeOfAKinds match {
          case 0 =>
            val pairs = denominationMultiples & 0x55555555
            pairs match {
        case 0 =>
          new HighCards(highCards(denominations, 5))
        case _ => // There is a pair, possibly two or even three.
          val topPair = 14 - trailingZeroes(pairs) / 2
              // Check for another pair to make a two pair hand.
          val secondPairs = denominationMultiples ^ denominationCounters(topPair)
          secondPairs match {
            case 0 => 
              new OnePair(topPair :: highCards(denominations ^ denominationCounters(topPair), 3))
            case _ =>  
              val secondPair = 14 - trailingZeroes(secondPairs) / 2
              new TwoPair(List(topPair, secondPair) ++ 
                highCards(denominations ^ denominationCounters(topPair) ^ denominationCounters(secondPair), 1))
          }
            }
          case _ => // There is a three-of-a-kind or full house.
            val denomination = 14 - trailingZeroes(threeOfAKinds) / 2
            // Check for another triplet or pair to make a full house.
            val secondPairs = denominationMultiples ^ (denominationCounters(denomination) << 1)
            secondPairs match {
        case 0 =>
          new ThreeOfAKind(denomination :: highCards(denominations ^ denominationCounters(denomination), 2))
        case _ =>  
          val secondPair = 14 - trailingZeroes(secondPairs) / 2
          new FullHouse(List(denomination, secondPair))
            }
        }
      case _ => // There is a four-of-a-kind.  There can only be one as we have at most 7 cards.
        // Count the number of trailing zeroes and adjust to find the denomination.
        val denomination = 14 - trailingZeroes(fourOfAKinds) / 2
        val kicker = 14 - trailingZeroes(denominations ^ fourOfAKinds) / 2
        new FourOfAKind(List(denomination, kicker))
          }
        case s => new Straight(List(14 - trailingZeroes(s) / 3))
      }
    case _ =>
      // We have a flush or straight flush.
      val suitRank = trailingZeroes(flushes) / 8
      // Create a new straight counter to check for straight flush
      // and get new denomination counts for the high card list for this suit.
      var flushDenominations = 0
      var straightFlushCounts = 0
                  
      for (card <- hands(i) ++ board ++ ((outsideLoop + 1) to 7).map((j) => remainingDeck(loopIndex(j)))
           if card.suit.rank == suitRank) {
        flushDenominations += denominationCounters(card.denomination)
        straightFlushCounts += straightCounters(card.denomination)
      }
    
      val straightFlushes = straightFlushCounts & (straightFlushCounts >> 2) & straightCountHits
      straightFlushes match {
        case 0 => new Flush(highCards(flushDenominations, 5))
        case sf => new StraightFlush(List(14 - trailingZeroes(sf) / 3))
      }
        }
      }

      // Now compare the list of hand ranking to arrive at win/tie/loss counts.
      hands.length match {
        case 1 =>
    // Simplify this for the usual case, one on one.
    handRanking(0).compare(handRanking(1)) match {
      case 1 => 
        totals(0)(0) += 1
        totals(1)(1) += 1
      case 0 =>
        totals(0)(2) += 1
        totals(1)(2) += 1
      case -1 =>
        totals(0)(1) += 1
        totals(1)(0) += 1
    }
        case _ =>
    // Multiway hand. Figure out the winners.  If there are several, then
    // we have a tie situation.
    val results = List() ++ handRanking
    val winner = results.reduceLeft((a,b) => if (a<b) b else a)
          val ties = results.count(_ == winner)
          ties match {
      case 1 =>
        // A single winner, all the rest are losers.
        results.zip(List.range(0, results.length)).foreach((a) =>
          a._1 match {
      case w if w == winner => totals(a._2)(0) += 1
      case _ => totals(a._2)(1) += 1
          })
      case n =>
        // Multiple tied.  Put tie counts in proper slot, 2-way, 3-way, etc.
        results.zip(List.range(0, results.length)).foreach((a) =>
          a._1 match {
      case w if w == winner => totals(a._2)(n) += 1
      case _ => totals(a._2)(1) += 1
          })
    }
      }

      loopIndex(j) += 1
      while (loopIndex(j) == remainingDeck.length + j - 7) {
        j -= 1
      loopIndex(j) += 1
      }
  
    case _ => 
      j += 1
      loopIndex(j) = loopIndex(j - 1) + 1
  }
      } while (j != outsideLoop)
    } while (j != outsideLoop)

    totals
  }
}
