package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s12 = union(s1, s2)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(s12, 1), "Union 1")
      assert(contains(s12, 2), "Union 2")
      assert(!contains(s12, 3), "Union 3")
    }
  }

  test("intersect contains only the common elements of each set") {
    new TestSets {
      val sNone = intersect(s1, s2)
      assert(!contains(sNone, 1), "Intersection 1")
      assert(!contains(sNone, 2), "Intersection 2")
      assert(!contains(sNone, 3), "Intersection 3")

      val sIntersect1 = intersect(s1, s12)
      assert(contains(sIntersect1, 1), "Intersection 4")
      assert(!contains(sIntersect1, 2), "Intersection 5")
      assert(!contains(sIntersect1, 3), "Intersection 6")
    }
  }

  test("diff contains only the elements of the first minus elements of the second") {
    new TestSets {
      val sNone = diff(s1, s1)
      assert(!contains(sNone, 1), "Diff 1")
      assert(!contains(sNone, 2), "Diff 2")
      assert(!contains(sNone, 3), "Diff 3")

      val sDiff1 = diff(s12, s2)
      assert(contains(sDiff1, 1), "Diff 4")
      assert(!contains(sDiff1, 2), "Diff 5")
      assert(!contains(sDiff1, 3), "Diff 6")

      val sDiffAll = diff(s12, s3)
      assert(contains(sDiffAll, 1), "Diff 7")
      assert(contains(sDiffAll, 2), "Diff 8")
      assert(!contains(sDiffAll, 3), "Diff 9")
    }
  }

  test("filter contains only the common elements matching function p") {
    new TestSets {
      val sNone = filter(s1, x => x == 2)
      assert(!contains(sNone, 1), "Filter 1")
      assert(!contains(sNone, 2), "Filter 2")
      assert(!contains(sNone, 3), "Filter 3")

      val sIntersect1 = filter(s12, x => x == 1 || x == 2)
      assert(contains(sIntersect1, 1), "Filter 4")
      assert(contains(sIntersect1, 2), "Filter 5")
      assert(!contains(sIntersect1, 3), "Filter 6")
    }
  }

  test("forall returns true if all elements in s match function p") {
    new TestSets {
      assert(forall(s1, x => x < 2), "Forall 1")
      assert(!forall(s2, x => x < 2), "Forall 2")
      assert(!forall(s3, x => x < 2), "Forall 3")

      assert(!forall(s12, x => x < 1), "Forall 4")
      assert(!forall(s12, x => x < 2), "Forall 5")
      assert(forall(s12, x => x < 3), "Forall 6")
    }
  }

  test("exists returns true if one element in s match function p") {
    new TestSets {
      assert(exists(s1, x => x < 2), "exists 1")
      assert(!exists(s2, x => x < 2), "exists 2")
      assert(!exists(s3, x => x < 2), "exists 3")

      assert(!exists(s12, x => x < 1), "exists 4")
      assert(exists(s12, x => x < 2), "exists 5")
      assert(exists(s12, x => x < 3), "exists 6")
    }
  }

  test("map returns a set where function f is applied") {
    new TestSets {
      def double(x: Int) : Int = x * 2
      val s1Mapped = map(s1, double)
      val s2Mapped = map(s2, double)
      val s3Mapped = map(s3, double)
      val s12Mapped = map(s12, double)

      assert(forall(s1Mapped, x => x < 3), "Map Forall 1")
      assert(!forall(s2Mapped, x => x < 3), "Map Forall 2")
      assert(!forall(s3Mapped, x => x < 3), "Map Forall 3")

      assert(!forall(s12Mapped, x => x < 3), "Map Forall 4")
      assert(!forall(s12Mapped, x => x < 4), "Map Forall 5")
      assert(forall(s12Mapped, x => x < 5), "Map Forall 6")

      assert(exists(s1Mapped, x => x < 3), "Map exists 1")
      assert(!exists(s2Mapped, x => x < 3), "Map exists 2")
      assert(!exists(s3Mapped, x => x < 3), "Map exists 3")

      assert(!exists(s12Mapped, x => x < 2), "Map exists 4")
      assert(exists(s12Mapped, x => x < 4), "Map exists 5")
      assert(exists(s12Mapped, x => x < 5), "Map exists 6")
    }
  }
}
