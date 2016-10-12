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
  * A minimal implementation of a regular expression matcher.
  *
  * @author Johannes Schindelin
  */
class RegexMatcher private[regex](val vm: PikeVM, val string: CharSequence) extends Matcher(string) {
  private var array: Array[Char] = null
  private[regex] var groupStart: Array[Int] = null
  private[regex] var groupEnd: Array[Int] = null
  final private val adapter: PikeVM.Result = new PikeVM.Result() {
    def set(start: Array[Int], end: Array[Int]): Unit = {
      RegexMatcher.this._start = start(0)
      RegexMatcher.this._end = end(0)
      RegexMatcher.this.groupStart = start
      RegexMatcher.this.groupEnd = end
    }
  }

  override def reset: Matcher = {
    _start = -1
    _end = -1
    this
  }

  override def reset(input: CharSequence): Matcher = {
    this.input = input
    array = input.toString.toCharArray
    reset
  }

  override def matches: Boolean = {
    vm.matches(array, 0, array.length, true, true, adapter)
  }

  override def find: Boolean = {
    find(_end + (if ((_start == _end)) 1
    else 0))
  }

  override def find(offset: Int): Boolean = {
    vm.matches(array, offset, array.length, false, false, adapter)
  }

  override def start(group: Int): Int = {
    groupStart(group)
  }

  override def end(group: Int): Int = {
    groupEnd(group)
  }

  override def group(group: Int): String = {
    val offset: Int = start(group)
    if (offset < 0) {
      return null
    }
    val length: Int = end(group) - offset
    new String(array, offset, length)
  }

  override def groupCount: Int = {
    groupStart.length - 1
  }
}
