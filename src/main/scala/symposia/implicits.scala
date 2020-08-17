package symposia

import scala.annotation.implicitNotFound

/**
  * # Explicating Implicits
  *
  * There are really just two flavors of implicits:
  *
  * 1. Implicit Conversions and
  * 2. Implicit Parameters
  *
  * Everything other than these two primary sorts are simply conventions built atop implicits (such as Type Classes),
  * special cases (Implicit Classes), or their composition (see: [[implicit_conversions_as_implicit_parameters]]).
  *
  * At their core, implicits are quite a bit simpler than their reputation suggests. It's just that their
  * implicit nature, being not explicit and all, makes it very difficult to see what's happening. And unless you know
  * the right keyboard shortcuts, your IDE isn't going to be of much use.
  *
  * In this session, we're going to focus on:
  *
  * 1. Learning the two primary sorts of implicits.
  * 2. The common conventions built atop those core sorts.
  * 3. Tactics and IDE features that can save you from going completely mad when implicits aren't behaving.
  */
object explicating_implicits

/**
  * 1. Implicit Conversions
  *
  * [[https://docs.scala-lang.org/tour/implicit-conversions.html]]
  *
  * Implicit conversions are applied in two situations. Here's an example of the first one (quoted from the docs):
  *
  * "If an expression `e` is of type `A`, and `A` does not conform to the expression’s expected type `B`.
  * A conversion `c` is searched for which is applicable to `e` and whose result type conforms to `B`."
  *
  * In other words, when you try to use an `A` where a `B` is expected, the compiler searches for an implicit function
  * of type `A => B` and then applies it to `A`.
  */
object implicit_conversions_1 {

  import scala.languageFeature.implicitConversions

//  /**
//    * Here is our implicit function from `A => B`
//    *
//    * Note: You must have imported [[scala.languageFeature.implicitConversions]] into scope wherever you'd like to
//    * define an implicit conversion, otherwise you'll get a warning.
//    */
  implicit def double2Int(double: Double): Int = double.toInt

  val double: Double = 10.0
  val int: Int       = double // 🐙

  /**
  * EXERCISE
  *
  * 1. Place the text caret inside of `double` (the one next to the arbitrary octopus) and then search for IntelliJ
  *    action called "Show implicit conversions". This should open a list with [[double2Int]] selected.
  *
  * 2. Hit `alt-enter` with your caret over that same `double` to open the hint menu, and select "Make implicit
  *    conversion  explicit". You should see `implicit_conversions_1.double2Int(double)`
  *
  * 2. Comment out [[double2Int]] and recompile. Notice how you immediately (if your CPU pleases IntelliJ) get a type
  *    mismatch error. The implicit conversion cannot fire if it doesn't exist. This error would occur if you moved the
  *    implicit out of scope, say by wrapping it in another object named `Implicit Prison`. Try it if you don't
  *    believe me.
  *
  * 3. Search for the action "Show Implicit Hints" (or attempt to trigger the finger-contorting keyboard shortcut) and
  *    enable it. This shortcut may take a moment to activate, depending on the number and complexity of the implicits
  *    currently being used. You should see a grayed out [[double2Int]] wrapping `double`; the implicit made explicit!
  *
  *    Personally, I don't use this as much as the more targeted shortcuts, like "Show implicit conversions" and
  *    "Show implicit parameters" (as you'll see in [[implicit_parameters]]. But it can useful to know about this
  *    _Sixth Sense_ for implicits. Implicits are truly the (SPOILER) Bruce Willisses of Scala.
  */
}

/**
  * And here's the second breed of implicit conversions (as quoted from the docs):
  *
  * "In a selection `e.m` with `e` of type `A`, if the selector m does not denote a member of `A`.
  * A conversion `c` is searched for which is applicable to `e` and whose result contains a member named `m`."
  *
  * In other words, the compiler searches for a function from `A => T` where type `T` has a member `m`
  */
object implicit_conversions_2 {

  /** The `String` type in Scala has all the methods of the underlying
    * `java.lang.String`, of which it is just an alias.
    * In addition, extension methods in [[scala.collection.StringOps]]
    * are added implicitly through the conversion [[augmentString]].
    *
    * `type String = java.lang.String`
    */
  val normalString: String         = "oh no".toUpperCase
  val augmentStringExample: String = "oh no".appended('!')

  /**
  *
  * EXPLANATION
  *
  * So, in this case `A` is `String` and `m` is `appended`. Thus the `T` is `StringOps` and the method `A => T { m }`
  * is [[augmentString]]:
  *
  * `implicit def augmentString(x: String): StringOps = new StringOps(x)`
  *
  * --
  *
  * EXERCISE
  *
  * 1. Command-click on `appended`. Notice how you're in [[scala.collection.StringOps]] instead of [[String]], which is
  *    itself simply an alias for [[java.lang.String]] (as explained in the comment above).
  *
  *    If you command-click on `toUpperCase`, you'll see that you're in fact in [[java.lang.String]], because this
  *    method exists on the Java's String type, so no conversion is required.
  *
  * 2. Place the text caret inside of `"oh no"` and use the IntelliJ action and type "Show implicit conversions".
  *    This should open a list with [[augmentString]] selected.
  *
  * 3. Hit `alt-enter` with your caret over `"oh no"` to open the hint menu, and select "Make implicit conversion
  *    explicit". You should see `Predef.augmentString("oh no").appended('!')`
  */

}

/**
  * 2. Implicit Parameters
  *
  * "These are passed to method calls like any other parameter, but the compiler tries to fill them in automatically.
  * If it can’t, it will complain"
  */
object implicit_parameters {

  // Are you ready to trust implicits with your life?
  def implicitRussianRoulette(implicit int: Int): String =
    if (int % 6 == 3) "BANG!" else "click."

  // Uh oh.
  implicit val int: Int = 1107

  def main(args: Array[String]): Unit =
    println(implicitRussianRoulette)

  val otherInt = 100

  /**
  * EXERCISE
  *
  * 1. Place your caret over `implicitRussianRoulette` within `println` and press `Cmd-Shift-P` (or search the actions
  *    for "Show implicit arguments".
  *
  * 2. Try adding the `implicit` keyword in front of [[otherInt]] in order to witness the magnificent "ambiguous
  *    implicits" error!
  */
}

/**
  * This is a Type Class, which is really just a trait parameterized by some type `A`.
  *
  * The only thing is, that instead of extending this trait, one defines implicit instances of it. This allows it to
  * be used with all of the implicit magic we've already discussed. You essentially get to summon a custom bundle of
  * functionality, in the form of this implicit trait, for free.
  */
@implicitNotFound("NO! NO! NO! I absolutely loathe ${A}s! Get it away from me!")
trait FavoriteType[A] {
  def explanation: String
}

object FavoriteType {
  implicit val stringFavorite: FavoriteType[String] = new FavoriteType[String] {
    override def explanation: String = "Strings are my favorite because they're what this is!"
  }

  implicit val booleanFavorite: FavoriteType[Boolean] = new FavoriteType[Boolean] {
    override def explanation: String = "Booleans are not not my favorite because they just are, okay!!!"
  }
}

object type_classes {

  /**
    * Type classes are often used as implicit parameters.
    */
  def bragAboutSomeValue[A](value: A)(implicit favoriteType: FavoriteType[A]): Unit =
    println(s"Bro! Check out this sick '$value'! " + favoriteType.explanation)

  /**
    * It's not required to use a value of type `A`, if you just want the implicit.
    * However, you'll then need to explicitly annotate the function call. (see `bragAboutType[String]` in [[main]])
    */
  def bragAboutType[A](implicit favoriteType: FavoriteType[A]): Unit =
    println("Sup! You know what I was just thinking? " + favoriteType.explanation)

  def main(args: Array[String]): Unit = {
    bragAboutSomeValue("Funky Fresh")
    bragAboutSomeValue(true)

    bragAboutType[String]
    bragAboutType[Boolean]
  }
}

object context_bound {

  /** The "Context Bound" is `[A: FavoriteType]`, which is merely syntactic sugar for:
    *
    * `case class FavoritesList[A](list: List[A])(implicit favoriteType: FavoriteType[A])`
    */
  case class FavoritesList[A: FavoriteType](list: List[A])

  val myFavoriteStrings: FavoritesList[String]   = FavoritesList(List("Foo", "Bar", "Baz", "Beelzebub"))
  val myFavoriteBooleans: FavoritesList[Boolean] = FavoritesList(List(true, false))

  //  val myFavoriteRuntimeExceptions: FavoritesList[RuntimeException] = FavoritesList(
  //    List(new RuntimeException("You forgot to plug it in.")))

  /**
  * EXERCISE
  *
  * 1. Uncomment `myFavoriteRuntimeExceptions` and behold the ribald "implicit not found" error.
  */
}

/**
  * From [[https://docs.scala-lang.org/overviews/core/implicit-classes.html]]:
  *
  * Scala 2.10 introduced a new feature called implicit classes. An implicit class is a class marked with the implicit
  * keyword. This keyword makes the class’s primary constructor available for implicit conversions when the class is
  * in scope.
  */
object implicit_classes {

  /**
    * As long as [[SillyString]] is in scope. [[String]] will be augmented with all of its members.
    *
    * @param string the wrapped value.
    */
  implicit class SillyString(string: String) {
    def sPoNgEbOb: String =
      string.zipWithIndex.map {
        case (c, i) if i % 2 == 0 => c.toUpper
        case (c, _) => c.toLower
      }.mkString
  }

  val importantMessage: String =
    "Fourscore and seven years ago our fathers brought forth, on this continent, a new nation," +
      " conceived in liberty, and dedicated to the proposition that all men are created equal."

  def main(args: Array[String]): Unit =
    println(importantMessage.sPoNgEbOb)
}

object implicit_conversions_as_implicit_parameters {

  import scala.languageFeature.implicitConversions

  def uppercase[A](input: A)(implicit conv: A => String): String = input.toUpperCase

  implicit def intToString(int: Int): String = int.toString

  def main(args: Array[String]): Unit =
    uppercase(12)
}

/**
  * Where does the compiler search for implicits? How does it prioritize them if there are multiple candidates?
  */
object implicit_resolution {

  case class Rabbit(name: String)

  object Rabbit {
    implicit val rabbitInCompanion: Rabbit = Rabbit("Lady Northrup")
  }

  /**
    * Scala will first look for implicit definitions and implicit parameters that can be accessed directly
    * (without a prefix) at the point the method with the implicit parameter block is called.
    * Then it looks for members marked implicit in all the companion objects associated with the implicit candidate type.
    */
  def reachInToHat(context: String)(implicit rabbit: Rabbit): Unit = println(s"Pulled $rabbit out of the $context")

  object lexical_scope {
    implicit val rabbitInScope: Rabbit = Rabbit("Mr. Bumpy")

    def run(): Unit = reachInToHat("lexical scope")
  }

  def main(args: Array[String]): Unit = {
    lexical_scope.run()
    reachInToHat("companion object")
  }
}

// WARNING: Everything from here on out is a little wacky.

/**
  * Chaining implicits [[https://docs.scala-lang.org/tutorials/FAQ/chaining-implicits.html]]
  */
object chaining_implicits {
  sealed trait Power { self =>
    def exert: String = self match {
      case Flight          => "I'm flying! I'm really flying!"
      case SpontaneousBees => "Ahhhhhhhhhhhh! Oh no! My eyes!"
    }
  }
  case object Flight          extends Power
  case object SpontaneousBees extends Power

  implicit def int2String(int: Int): String                                 = int.toString
  implicit def string2Bool[A](string: A)(implicit ev: A => String): Boolean = string.length % 2 == 0
  implicit def bool2Sign[A](bool: A)(implicit ev: A => Boolean): Power =
    if (bool) Flight else SpontaneousBees

  def main(args: Array[String]): Unit = {
    println(9001.exert)
    println(123.exert)
  }
}

object type_level_functions {
  sealed trait Bit
  case object Zero extends Bit
  case object One  extends Bit

  type Zero = Zero.type
  type One  = One.type

  /**
    * Let's pretend we want to write a type-aware multiplexing function, which, given a tuple `(A,B)` selects either the
    * `A` or `B` value based on a `Bit`.
    *
    * Even though we know we're always returning `A` if the bit is `Zero` and `B`` if the bit is `One`, the compiler
    * has no idea. The question is, can we somehow convince it.
    */
  def typeUnsafeMultiplexer[A, B](bit: Bit, pair: (A, B)): Any =
    bit match {
      case Zero => pair._1
      case One  => pair._2
    }

  // What if we could make this compile?
//  val unsafeZero: String = typeUnsafeMultiplexer(Zero, (1, "Help"))
//  val unsafeOne: Int     = typeUnsafeMultiplexer(One, (1, "Help"))

  /**
    * Type-level functions are expressed as traits, whose overloaded implementations are defined via implicits.
    *
    * The interface, defined as a trait:
    * `def mux(bit: Bit, left: L, right: R) : Output = ???`
    */
  trait Mux[B <: Bit, L, R] {
    type Output

    def apply(pair: (L, R)): Output
  }

  /**
    * the overloads, defined as implicits:
    * `def mux(bit: Zero, pair: (L, R)) : L = ???`
    * `def mux(bit: One, pair: (L, R)) : R = ???`
    */
  object Mux {
    type Aux[B <: Bit, L, R, O] = Mux[B, L, R] { type Output = O }

    //`def mux(bit: Zero, pair: (L, R)) : L = pair._1`
    //               |           |  |     |=====|
    //               |           |  |========|  |
    //               |           |========|  |  |
    //               |===============|    |  |  |
    implicit def zeroMux[L, R]: Aux[Zero, L, R, L] = new Mux[Zero, L, R] {
      type Output = L

      def apply(pair: (L, R)): L = pair._1
    }

    //`def mux(bit: Zero, pair: (L, R)) : R = pair._2`
    //               |           |  |     |===|
    //               |           |  |======|  |
    //               |           |======|  |  |
    //               |==============|   |  |  |
    implicit def oneMux[L, R]: Aux[One, L, R, R] = new Mux[One, L, R] {
      type Output = R

      def apply(pair: (L, R)): R = pair._2
    }
  }

  import Mux._

  def multiplexer[B <: Bit, L, R, O](bit: B, pair: (L, R))(implicit mux: Mux.Aux[B, L, R, O]): O =
    mux.apply(pair)

  val pair: (Int, String) = (1, "hello")
  val zero: Int           = multiplexer(Zero, pair)
  val one: String         = multiplexer(One, pair)

// The true types are known at compile time
//  val zeroFail: String = multiplexer(Zero, pair)
//  val oneFail: Int     = multiplexer(One, pair)

  /**
    * Bonus. A weird way to ditch the unused proxy `bit`.
    */
  def multiplexer[B <: Bit] = new MultiplexApply[B]()

  final class MultiplexApply[B <: Bit](val dummy: Boolean = true) extends AnyVal {
    def apply[L, R, O](pair: (L, R))(implicit mux: Mux.Aux[B, L, R, O]): O = mux.apply(pair)
  }

  val zeroAgain: Int   = multiplexer[Zero](pair)
  val oneAgain: String = multiplexer[One](pair)
}

/**
  * TODO:
  * 1. Chaining Implicits [[https://docs.scala-lang.org/tutorials/FAQ/chaining-implicits.html]]
  * 2. Read about `breakOut` && `CanBuildFrom` [[https://docs.scala-lang.org/tutorials/FAQ/breakout.html]]
  * 3. ...
  */
