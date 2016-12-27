import org.scalatest._

class ComparatorSuite extends FunSuite {

  val comparator = new NumberAwareComparator()

  test("strings with one number compare numerically") {
    assert(comparator.compare("abc 11", "abc 2") == 1, "abc 11 > abc 2")
    assert(comparator.compare("abc 11", "abc 12") == -1, "abc 11 < abc 12")
    assert(comparator.compare("abc 1", "abc 2") == -1, "abc 1 < abc 2")
    assert(comparator.compare("abc 2", "abc 2") == 0, "abc 2 = abc 2")
    assert(comparator.compare("abc 23", "abc 2") == 1, "abc 23 > abc 2")
  }

  test("strings without numbers compare as usual") {
    assert(comparesDefault("abc", "def"), "abc < def")
    assert(comparesDefault("abc", "abc"), "abc = abc")
    assert(comparesDefault("abc", "abcd"), "abc < abcd")
    assert(comparesDefault("abcd", "abc"), "abcd > abc")
    assert(comparesDefault("abcd", "abcf"), "abcd < abcf")
  }

  test("single chars compare as usual") {
    assert(comparator.compare("a", "b") == -1, "a < b")
    assert(comparator.compare("b", "a") == 1, "b > a")
    assert(comparator.compare("b", "b") == 0, "b = b")
  }

  test("number strings compare numerically") {
    assert(comparator.compare("123", "123") == 0, "123 = 123")
    assert(comparator.compare("123", "12") == 1, "123 > 12")
    assert(comparator.compare("123", "1234") == -1, "123 < 1234")
  }

  private def comparesDefault(lhs: String, rhs: String) =
    comparator.compare(lhs, rhs) == lhs.compareTo(rhs)

}

