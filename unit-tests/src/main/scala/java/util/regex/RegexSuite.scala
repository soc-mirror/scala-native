package java.util.regex

object RegexSuite extends tests.Suite {

  private def getMatcher(regex: String, string: String): Matcher =
    Pattern.compile(regex).matcher(string)

  private def expectMatch(regex: String, string: String): Unit =
    assert(getMatcher(regex, string).matches())

  private def expectNoMatch(regex: String, string: String): Unit =
    assertNot(getMatcher(regex, string).matches())

  private def expectGroups(regex: String,
                           string: String,
                           groups: String*): Unit = {
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

  private def expectFind(regex: String,
                         string: String,
                         matches: String*): Unit = {
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
    while (i < list.length) {
      i += 1
      assert(list(i).equals(array(i)))
    }
  }

  test("""expectMatch("a(bb)?a", "abba")""") {
    expectMatch("a(bb)?a", "abba")
  }

  test("""expectNoMatch("a(bb)?a", "abbba")""") {
    expectNoMatch("a(bb)?a", "abbba")
  }

  test("""expectGroups("a(a*?)(a?)(a??)(a+)(a*)a", "aaaaaa", "", "a", "", "aaa", "")""") {
    expectGroups("a(a*?)(a?)(a??)(a+)(a*)a", "aaaaaa", "", "a", "", "aaa", "")
  }

  test("""expectMatch("...", "abc")""") {
    expectMatch("...", "abc")
  }

  test("""expectNoMatch(".", "\n")""") {
    expectNoMatch(".", "\n")
  }

  test("""expectGroups("a(bb)*a", "abbbba", "bb")""") {
    expectGroups("a(bb)*a", "abbbba", "bb")
  }

  test("""expectGroups("a(bb)?(bb)+a", "abba", null, "bb")""") {
    expectGroups("a(bb)?(bb)+a", "abba", null, "bb")
  }

  test("""expectFind(" +", "Hello  ,   world! ", "  ", "   ", " ")""") {
    expectFind(" +", "Hello  ,   world! ", "  ", "   ", " ")
  }

  test("""expectMatch("[0-9A-Fa-f]+", "08ef")""") {
    expectMatch("[0-9A-Fa-f]+", "08ef")
  }

  test("""expectNoMatch("[0-9A-Fa-f]+", "08@ef")""") {
    expectNoMatch("[0-9A-Fa-f]+", "08@ef")
  }

  test("""expectGroups("(?:a)", "a")""") {
    expectGroups("(?:a)", "a")
  }

  test("""expectGroups("a|(b|c)", "a", null.asInstanceOf[String])""") {
    expectGroups("a|(b|c)", "a", null.asInstanceOf[String])
  }

  test("""expectGroups("a|(b|c)", "c", "c")""") {
    expectGroups("a|(b|c)", "c", "c")
  }

  test("""expectGroups("(?=a)a", "a")""") {
    expectGroups("(?=a)a", "a")
  }

  test("""expectGroups(".*(o)(?<=[A-Z][a-z]{1,4})", "Hello", "o")""") {
    expectGroups(".*(o)(?<=[A-Z][a-z]{1,4})", "Hello", "o")
  }

  test("""expectNoMatch("(?!a).", "a")""") {
    expectNoMatch("(?!a).", "a")
  }

  test("""expectMatch("[\\d]", "0")""") {
    expectMatch("[\\d]", "0")
  }

    /*
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
