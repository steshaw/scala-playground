//
// Adapted from http://blog.tmorris.net/the-power-of-type-classes-with-scala-implicit-defs/
//

import sys.error

trait Eq[a] {
  def eq(a1: a)(a2: a): Boolean
}

sealed abstract class Ordering
final case object LT extends Ordering
final case object EQ extends Ordering
final case object GT extends Ordering

trait Ord[a] extends Eq[a] {
  override def eq(a1: a)(a2: a) = compare(a1)(a2) == EQ
  def compare(a1: a)(a2: a): Ordering
}

object Ord {
  implicit def intOrd = new Ord[Int] {
    def compare(a1: Int)(a2: Int) = if(a1 == a2) EQ else if(a1 < a2) LT else GT
  }
}

object Maximum {
  def maximum[a](as: List[a])(implicit o: Ord[a]): a = as match {
    case Nil => error("maximum undefined for empty list")
    case x :: y :: ys => maximum((o.compare(x)(y) match {
      case GT => x
      case _ => y
    }) :: ys)
    case x :: _ => x
  }
}

trait Successor[a] {
  def succ(a: a): Option[a]
}

object Successor {
  implicit def intSuccessor = new Successor[Int] {
    override def succ(a: Int) = a match {
      case Integer.MAX_VALUE => None
      case _ => Some(a + 1)
    }
  }
}

object Range {
  def range[a](from: a)(to: a)(implicit e: Successor[a], o: Ord[a]): List[a] =
    if(o.compare(from)(to) == GT) Nil
    else if(o.eq(from)(to)) List(from)
    else e.succ(from) match {
      case None => List(from)
      case Some(s) => from :: range(s)(to)
    }
}

sealed abstract class Rank
final case object ACE extends Rank
final case object TWO extends Rank
final case object THREE extends Rank
final case object FOUR extends Rank
final case object FIVE extends Rank
final case object SIX extends Rank
final case object SEVEN extends Rank
final case object EIGHT extends Rank
final case object NINE extends Rank
final case object TEN extends Rank
final case object JACK extends Rank
final case object QUEEN extends Rank
final case object KING extends Rank

object Rank {
  implicit def acesHigh = new Successor[Rank] with Ord[Rank] {
    val ranks = Array(TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, JACK, QUEEN, KING, ACE)

    override def compare(a1: Rank)(a2: Rank) = if(a1 == a2) EQ else if(ranks.indexOf(a1) < ranks.indexOf(a2)) LT else GT

    override def succ(a: Rank) = a match {
      case ACE => None
      case _ => Some(ranks(ranks.indexOf(a) + 1))
    }
  }

  implicit def acesLow = new Successor[Rank] {
    val ranks = Array(ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, JACK, QUEEN, KING)

    override def succ(a: Rank) = a match {
      case KING => None
      case _ => Some(ranks(ranks.indexOf(a) + 1))
    }
  }
}

object Main {
  def main(args: Array[String]) = {
    import Ord.intOrd
    Console.println(Maximum.maximum(List.range(1, 10)))

    import Range.range

    Console.println(range(7)(8))
    Console.println(range(7)(7))
    Console.println(range(7)(6))
    Console.println(range(Integer.MAX_VALUE)(Integer.MAX_VALUE))
    Console.println(range(Integer.MAX_VALUE - 1)(Integer.MAX_VALUE))
    Console.println(range(Integer.MAX_VALUE)(7))
    Console.println(range(Integer.MIN_VALUE)(Integer.MIN_VALUE + 1))

    {
      import Rank.acesHigh
      Console.println(range(TWO: Rank)(THREE))
      Console.println(range(TWO: Rank)(ACE))
      Console.println(range(ACE: Rank)(KING))
    }

    {
      import Rank.acesLow
      Console.println(range(TWO: Rank)(THREE))
      Console.println(range(TWO: Rank)(ACE))
      Console.println(range(ACE: Rank)(KING))
    }
  }
}
