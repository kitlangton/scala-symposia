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
object explicating_implicits {}

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
  * In other words, the compiler searches for an implicit function of type `A => B`.
  */
object implicit_conversions_1 {

  import scala.languageFeature.implicitConversions

  /**
    * Here is our implicit function from `A => B`
    *
    * Note: You must have imported [[scala.languageFeature.implicitConversions]] into scope wherever you'd like to
    * define an implicit conversion, otherwise you'll get a warning.
    */
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

  val otherInt: Int = 100

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

/**
  * TODO:
  * 1. Chaining Implicits [[https://docs.scala-lang.org/tutorials/FAQ/chaining-implicits.html]]
  * 2. Read about `breakOut` && `CanBuildFrom` [[https://docs.scala-lang.org/tutorials/FAQ/breakout.html]]
  */
