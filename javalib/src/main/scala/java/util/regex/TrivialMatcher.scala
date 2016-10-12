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

/**
  * This is a work in progress.
  *
  * @author zsombor and others
  */
class TrivialMatcher private[regex](val pattern: String, _input: CharSequence) extends Matcher(_input) {
  override def matches: Boolean = {
    if (pattern == input.toString) {
      _start = 0
      _end = input.length
      true
    } else {
      false
    }
  }

  override def find(start: Int): Boolean = {
    val p: String = pattern
    val i: Int = TrivialPattern.indexOf(input, p, start)
    if (i >= 0) {
      this._start = i
      this._end = i + p.length
      true
    } else {
      false
    }
  }
}
