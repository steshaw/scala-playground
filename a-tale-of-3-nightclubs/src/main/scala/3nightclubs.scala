//
// From https://gist.github.com/970717
//

/**
 * Part Zero : 10:15 Saturday Night
 *
 * (In which we will see how to let the type system help you handle failure)...
 *
 * First let's define a domain. (All the following requires scala 2.9.x and scalaz 6.0)
 */

import scalaz._
import Scalaz._

object Sobriety extends Enumeration { val Sober, Tipsy, Drunk, Paralytic, Unconscious = Value }

object Gender extends Enumeration { val Male, Female = Value }

case class Person(gender : Gender.Value, age : Int, clothes : Set[String], sobriety : Sobriety.Value)

/**
 * Let's define a trait which will contain the checks that *all* nightclubs make!
 */
trait Nightclub {

  //First CHECK
  def checkAge(p : Person)  : Validation[String, Person]
     = if (p.age < 18)
         "Too Young!".fail
        else if (p.age > 40)
          "Too Old!".fail
        else
            p.success

  //Second CHECK
  def checkClothes(p : Person)  : Validation[String, Person]
    = if (p.gender == Gender.Male && !p.clothes("Tie"))
        "Smarten Up!".fail
      else if (p.gender == Gender.Female && p.clothes("Trainers"))
        "Wear high heels".fail
      else
        p.success

  //Third CHECK
  def checkSobriety(p : Person): Validation[String, Person]
    = if (Set(Sobriety.Drunk, Sobriety.Paralytic, Sobriety.Unconscious) contains p.sobriety)
        "Sober Up!".fail
      else
        p.success
}

/**
 * Part One : Clubbed to Death
 *
 * Now let's compose some validation checks
 *
 */
object ClubbedToDeath extends Nightclub {
  def costToEnter(p : Person) : Validation[String, Double] = {

    //PERFORM THE CHECKS USING Monadic "for comprehension" SUGAR
    for {
      a <- checkAge(p)
      b <- checkClothes(a)
      c <- checkSobriety(b)
    } yield (if (c.gender == Gender.Female) 0D else 5D)
  }
}

// Now let's see these in action
object Test1 {
  val Ken = Person(Gender.Male, 28, Set("Tie", "Shirt"), Sobriety.Tipsy)

  val Dave = Person(Gender.Male, 41, Set("Tie", "Jeans"), Sobriety.Sober)

  val Ruby = Person(Gender.Female, 25, Set("High Heels"), Sobriety.Tipsy)

// Let's go clubbing!

  ClubbedToDeath costToEnter Dave //res0: scalaz.Validation[String,Double] = Failure(Too Old!)

  ClubbedToDeath costToEnter Ken //res1: scalaz.Validation[String,Double] = Success(5.0)

  ClubbedToDeath costToEnter Ruby //res2: scalaz.Validation[String,Double] = Success(0.0)

  ClubbedToDeath costToEnter (Ruby.copy(age = 17)) //res3: scalaz.Validation[String,Double] = Failure(Too Young!)

  ClubbedToDeath costToEnter (Ken.copy(sobriety = Sobriety.Unconscious)) //res5: scalaz.Validation[String,Double] = Failure(Sober Up!)

}
/**
 * The thing to note here is how the Validations can be composed together in a for-comprehension.
 * Scala's type system is making sure that   failures flow through your computation in a safe manner.
 */


/**
 * Part Two : Club Tropicana
 *
 * Part One showed monadic composition, which from the perspective of Validation is *fail-fast*.
 * That is, any failed check shortcircuits subsequent checks. This nicely models nightclubs in the
 * real world, as anyone who has dashed home for a pair of smart shoes and returned, only to be
 * told that your tie does not pass muster, will attest.
 *
 * But what about an ideal nightclub? One that tells you *everything* that is wrong with you.
 *
 * Applicative functors to the rescue!
 *
 */

object ClubTropicana extends Nightclub {
  def costToEnter(p : Person) : ValidationNEL[String, Double] = {

    //PERFORM THE CHECKS USING applicative functors, accumulating failure via a monoid (a NonEmptyList, or NEL)
    (checkAge(p).liftFailNel |@| checkClothes(p).liftFailNel |@| checkSobriety(p).liftFailNel)  {
      case (_, _, c) => if (c.gender == Gender.Female) 0D else 7.5D
    }
  }
}

/**
 *
 * And the use? Dave tried the second nightclub after a few more drinks in the pub
 *
 */
object Test2 {
  import Test1._
  ClubTropicana costToEnter (Dave.copy(sobriety = Sobriety.Paralytic)) //res6: scalaz.Scalaz.ValidationNEL[String,Double] = Failure(NonEmptyList(Too Old!, Sober Up!))

  ClubTropicana costToEnter(Ruby) //res7: scalaz.Scalaz.ValidationNEL[String,Double] = Success(0.0)

}
/**
 *
 * So, what have we done? Well, with a *tiny change* (and no changes to the individual checks themselves),
 * we have completely changed the behaviour to accumulate all errors, rather than halting at the first sign
 * of trouble. Imagine trying to do this in Java, using exceptions, with ten checks.
 *
 */

/**
 *
 * Part Three : Gay Bar
 *
 * And for those wondering how to do this with a *very long list* of checks. Use sequence:
 *   List[ValidationNEL[E, A]] ~> (via sequence) ~> ValidationNEL[E, List[A]]
 *
 * Here we go (unfortunately we need to use a type lambda on the call to sequence):
 *
 */
object GayBar extends Nightclub {

  def checkGender(p : Person) : Validation[String, Person] =
    if (p.gender != Gender.Male)
      "Men Only".fail
    else
      p.success

  def costToEnter(p : Person) : ValidationNEL[String, Double] = {
    val checks = List(checkAge _, checkClothes _, checkSobriety _, checkGender _)
    (checks map {(_ : (Person => Validation[String, Person])).apply(p).liftFailNel}).sequence[({type l[a]=ValidationNEL[String, a]})#l, Person] map { 
       case c :: _ => c.age + 1.5D
    }
  }

  //Interestingly, as traverse is basically map + sequence, we can reduce this even further

  def costToEnter2(p : Person) : ValidationNEL[String, Double] = {
    val checks = List(checkAge _, checkClothes _, checkSobriety _, checkGender _)
    checks.traverse[({type l[a] = ValidationNEL[String, a]})#l, Person](_ andThen (_.liftFailNel) apply p) map { case c :: _ => c.age + 1.5D }
  }

}

object Test3 {
  import GayBar._

  def main(args: Array[String]) {
    costToEnter(Person(Gender.Male, 59, Set("Jeans"), Sobriety.Paralytic))  //Failure(NonEmptyList(Too Old!, Smarten Up!, Sober Up!))
    costToEnter2(Person(Gender.Male, 59, Set("Jeans"), Sobriety.Paralytic)) //Failure(NonEmptyList(Too Old!, Smarten Up!, Sober Up!))
  }
}

/**
 * As always; the point is that our validation functions are "static";
 * we do not need to change the way they have been coded because we want to combine them in different ways
 */
