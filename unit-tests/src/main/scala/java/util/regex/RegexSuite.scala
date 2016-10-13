package java.util.regex

object RegexSuite extends tests.Suite {

  private def getMatcher(regex: String, string: String): Matcher =
    Pattern.compile(regex).matcher(string)

  private def expectMatch(regex: String, string: String): Unit =
    assert(getMatcher(regex, string).matches())

  private def expectNoMatch(regex: String, string: String): Unit =
    assert(!getMatcher(regex, string).matches())

  private def expectGroups(regex: String, string: String, groups: String*): Unit = {
    val matcher: Matcher = getMatcher(regex, string)
    assert(matcher.matches())
    assert(matcher.groupCount() == groups.length)
    var i = 1
    while (i <= groups.length) {
      if (groups(i - 1) == null)
        assert(matcher.group(i) == null)
      else
        assert(groups(i - 1).equals(matcher.group(i)))
      i += 1
    }
  }

  private def expectFind(regex: String, string: String, matches: String*): Unit = {
    val matcher: Matcher = getMatcher(regex, string)
    var i = 0;
    while (i < matches.length) {
      assert(matcher.find())
      assert(matches(i).equals(matcher.group()))
      i += 1
    }
    assert(!matcher.find())
  }

  private def expectSplit(regex: String, string: String, list: String*): Unit = {
    val array: Array[String] = Pattern.compile(regex).split(string)
    assert(array.length == list.length)
    var i = 0
    while(i < list.length) {
      i += 1
      assert(list(i).equals(array(i)))
    }
  }

  test("regex tests") {
    expectMatch("a(bb)?a", "abba")
    /*
    expectNoMatch("a(bb)?a", "abbba")
    expectNoMatch("a(bb)?a", "abbaa")
    expectGroups("a(a*?)(a?)(a??)(a+)(a*)a", "aaaaaa", "", "a", "", "aaa", "")
    expectMatch("...", "abc")
    expectNoMatch(".", "\n")

    expectGroups("a(bb)*a", "abbbba", "bb")
    expectGroups("a(bb)?(bb)+a", "abba", null, "bb")
    expectFind(" +", "Hello  ,   world! ", "  ", "   ", " ")
    expectMatch("[0-9A-Fa-f]+", "08ef")
    expectNoMatch("[0-9A-Fa-f]+", "08@ef")
    expectGroups("(?:a)", "a")
    expectGroups("a|(b|c)", "a", null.asInstanceOf[String])
    expectGroups("a|(b|c)", "c", "c")
    expectGroups("(?=a)a", "a")
    expectGroups(".*(o)(?<=[A-Z][a-z]{1,4})", "Hello", "o")
    expectNoMatch("(?!a).", "a")
    expectMatch("[\\d]", "0")
    expectMatch("\\0777", "?7")
    expectMatch("\\a", "\u0007")
    expectMatch("\\\\", "\\")
    expectMatch("\\x4A", "J")
    expectMatch("\\x61", "a")
    expectMatch("\\078", "\u00078")
    expectSplit("(?<=\\w)(?=\\W)|(?<=\\W)(?=\\w)", "a + b * x", "a", " + ", "b", " * ", "x")
    expectMatch("[0-9[def]]", "f")
    expectNoMatch("[a-z&&[^d-f]]", "f")
    expectSplit("^H", "Hello\nHobbes!", "", "ello\nHobbes!")
    expectSplit("o.*?$", "Hello\r\nHobbes!", "Hello\r\nH")
    try {
      expectSplit("\\b", "a+ b + c\nd", "", "a", "+ ", "b", " + ", "c", "\n", "d")
    } catch {
      case e: RuntimeException =>
        // Java 8 changed the semantics of split, so if we're on 8, the
        // above will fail and this will succeed:
        expectSplit("\\b", "a+ b + c\nd", "a", "+ ", "b", " + ", "c", "\n", "d")
    }
    expectSplit("\\B", "Hi Cal!", "H", "i C", "a", "l!")
    expectMatch("a{2,5}", "aaaa")
    expectGroups("a??(a{2,5}?)", "aaaa", "aaaa")
    expectGroups("a??(a{3}?)", "aaaa", "aaa")
    expectNoMatch("a(a{3}?)", "aaaaa")
    expectMatch("a(a{3,}?)", "aaaaa")
    */
  }
}
