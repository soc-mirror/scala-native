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
object TrivialPattern {
  private[regex] def indexOf(haystack: CharSequence, needle: CharSequence, start: Int): Int = {
    if (needle.length == 0) return start
    var i: Int = start
    while (i < haystack.length - needle.length + 1) {
       var j: Int = 0

       scala.util.control.Breaks.breakable {
         while (j < needle.length) {
           if (haystack.charAt(i + j) != needle.charAt(j)) {
             scala.util.control.Breaks.break()
           }
           j += 1
         }
       }
       if (j == needle.length) {
         return i
       }
       i += 1
    }
    -1
  }
}

class TrivialPattern private[regex](override val pattern: String, val unescaped: String, override val flags: Int) extends Pattern(pattern, flags) {
  override def matcher(input: CharSequence): Matcher = new TrivialMatcher(unescaped, input)

  override def split(input: CharSequence, limit: Int): Array[String] = {
    var _limit = limit
    var strip: Boolean = false
    if (_limit < 0) {
      strip = false
      _limit = Int.MaxValue
    }
    else if (_limit == 0) {
      strip = true
      _limit = Int.MaxValue
    }
    else {
      strip = false
    }
    val list: java.util.List[CharSequence] = new java.util.LinkedList[CharSequence]
    var index: Int = 0
    var trailing: Int = 0
    val patternLength: Int = unescaped.length
    scala.util.control.Breaks.breakable {
      while (index < input.length && list.size < _limit - 1) {
        var i: Int = 0
        if (patternLength == 0) {
          if (list.size == 0) {
            i = 0
          }
          else {
            i = index + 1
          }
        }
        else {
          i = TrivialPattern.indexOf(input, unescaped, index)
        }
        if (i >= 0) {
          if (patternLength != 0 && i == index) {
            trailing += 1
          }
          else {
            trailing = 0
          }
          list.add(input.subSequence(index, i))
          index = i + patternLength
        }
        else {
          scala.util.control.Breaks.break()
        }
      }
    }
    if (strip && index > 0 && index == input.length) {
      trailing += 1
    }
    else {
      trailing = 0
    }
    list.add(input.subSequence(index, input.length))
    val result: Array[String] = new Array[String](list.size - trailing)
    var i: Int = 0
    val it: java.util.Iterator[CharSequence] = list.iterator
    while (it.hasNext && i < result.length) {
      result(i) = it.next.toString
      i += 1
    }
    result
  }
}