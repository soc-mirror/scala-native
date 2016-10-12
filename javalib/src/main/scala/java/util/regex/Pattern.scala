/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. *//* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */
package java.util.regex

/** This is a work in progress.
  *
  * @author zsombor and others
  */
object Pattern {
  val UNIX_LINES: Int = 1
  val CASE_INSENSITIVE: Int = 2
  val COMMENTS: Int = 4
  val MULTILINE: Int = 8
  val LITERAL: Int = 16
  val DOTALL: Int = 32
  val UNICODE_CASE: Int = 64
  val CANON_EQ: Int = 128

  def compile(regex: String): Pattern = compile(regex, 0)

  def compile(regex: String, flags: Int): Pattern = {
    if (flags != 0) {
      throw new UnsupportedOperationException("TODO")
    }
    new Compiler().compile(regex)
  }

  def matches(regex: String, input: CharSequence): Boolean = Pattern.compile(regex).matcher(input).matches
}

abstract class Pattern protected(val pattern: String, val patternFlags: Int) {
  def flags: Int = patternFlags

  def matcher(input: CharSequence): Matcher

  def split(input: CharSequence): Array[String] = split(input, 0)

  def split(input: CharSequence, limit: Int): Array[String] = {
    var _limit = limit
    if (_limit <= 0) {
      _limit = Int.MaxValue
    }
    val _matcher: Matcher = matcher(input)
    val result: java.util.List[String] = new java.util.ArrayList[String]
    var offset: Int = 0

    scala.util.control.Breaks.breakable {
      while (true) {
        if (result.size >= _limit || !_matcher.find) {
          scala.util.control.Breaks.break()
        }
        result.add(input.subSequence(offset, _matcher._start).toString)
        offset = _matcher._end
      }
    }
    if (offset == 0 || offset < input.length) {
      result.add(input.subSequence(offset, input.length).toString)
    }
    result.toArray(new Array[String](result.size))
  }
}
