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
abstract class Matcher(protected var input: CharSequence) {
  reset(input)
  protected[regex] var _start: Int = 0
  protected[regex] var _end: Int = 0

  def matches: Boolean

  def find: Boolean = find(_end)

  def find(start: Int): Boolean

  def reset: Matcher = reset(input)

  def reset(_input: CharSequence): Matcher = {
    this.input = _input
    _start = 0
    _end = 0
    this
  }

  def replaceAll(replacement: String): String = replace(replacement, Integer.MAX_VALUE)

  def replaceFirst(replacement: String): String = replace(replacement, 1)

  protected def replace(replacement: String, limit: Int): String = {
    reset
    var sb: StringBuilder = null
    var index: Int = 0
    var count: Int = 0
    scala.util.control.Breaks.breakable {
      while (count < limit && index < input.length) {
        if (find(index)) {
          if (sb == null) {
            sb = new StringBuilder
          }
          if (_start > index) {
            sb.append(input.subSequence(index, _start))
          }
          sb.append(replacement)
          index = _end
          count += 1
        }
        else if (index == 0) {
          return input.toString
        }
        else {
          scala.util.control.Breaks.break()
        }
      }
    }
    if (index < input.length) {
      sb.append(input.subSequence(index, input.length))
    }
    sb.toString
  }

  def group: String = input.subSequence(_start, _end).toString

  def start(group: Int): Int = {
    if (group == 0) {
      return _start
    }
    throw new UnsupportedOperationException
  }

  def end(group: Int): Int = {
    if (group == 0) {
      return _end
    }
    throw new UnsupportedOperationException
  }

  def group(_group: Int): String = {
    if (_group == 0) {
      return group
    }
    throw new UnsupportedOperationException
  }

  def groupCount: Int = 0
}
