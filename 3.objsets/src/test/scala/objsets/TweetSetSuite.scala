package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val eq1 = new NonEmpty(new Tweet("b", "b body", 20), new Empty, new Empty)
    val eq2 = eq1.incl(new Tweet("a", "a body", 10))
    val eq3 = eq2.incl(new Tweet("c", "c body", 30))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: each user on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
      assert(size(set5.filter(tw => tw.user == "b")) === 1)
      assert(size(set5.filter(tw => tw.user == "c")) === 1)
      assert(size(set5.filter(tw => tw.user == "d")) === 1)
      assert(size(eq3.filter(tw => tw.user == "a")) === 1)
      assert(size(eq3.filter(tw => tw.user == "b")) === 1)
      assert(size(eq3.filter(tw => tw.user == "c")) === 1)
      assert(size(eq3.filter(tw => tw.user != "a")) === 2)
      assert(size(eq3.filter(tw => tw.user != "b")) === 2)
      assert(size(eq3.filter(tw => tw.user != "c")) === 2)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("most retwteeted : null") {
    new TestSets {
      private val tweet = set1.mostRetweeted
      assert(tweet == null)
    }
  }

  test("most retwteeted : set5") {
    new TestSets {
      private val user: String = set5.mostRetweeted.user
      assert(user == "a" || user == "b")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  }
