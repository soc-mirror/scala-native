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

/** A class to match classes of characters.
  *
  * This class is intended to be the working horse behind character classes
  * such as `[a-z]`.
  *
  * @author Johannes Schindelin
  */
object CharacterMatcher {
  def parse(description: String): CharacterMatcher = parse(description.toCharArray)

  def parse(description: Array[Char]): CharacterMatcher = {
    val parser: CharacterMatcher.Parser = new CharacterMatcher.Parser(description)
    val result: CharacterMatcher = parser.parseClass
    if (parser.getEndOffset != description.length)
      throw new RuntimeException("Short character class @" + parser.getEndOffset + ": " + new String(description))
    result
  }

  private def specialClass(c: Int): String = {
    if ('d' == c) {
      return "[0-9]"
    }
    if ('D' == c) {
      return "[^0-9]"
    }
    if ('s' == c) {
      return "[ \\t\\n\\x0B\\f\\r]"
    }
    if ('S' == c) {
      return "[^ \\t\\n\\x0B\\f\\r]"
    }
    if ('w' == c) {
      return "[a-zA-Z_0-9]"
    }
    if ('W' == c) {
      return "[^a-zA-Z_0-9]"
    }
    null
  }

  private[regex] class Parser(val description: Array[Char]) {
    private var offset: Int = 0

    def getEndOffset: Int = offset

    /** Parses an escaped character.
      *
      * @param start the offset <u>after</u> the backslash
      * @return the escaped character, or -1 if no character was recognized
      */
    def parseEscapedCharacter(start: Int): Int = {
      offset = start
      parseEscapedCharacter
    }

    private def parseEscapedCharacter: Int = {
      if (offset == description.length) {
        throw new IllegalArgumentException("Short escaped character")
      }
      var c: Char = description(offset)
      offset += 1
      if (c == '0') {
        var len: Int = digits(offset, 3, 8)
        if (len == 3 && description(offset) > '3') {
          len -= 1
        }
        c = Integer.parseInt(new String(description, offset, len), 8).toChar
        offset += len
        return c
      }
      if (c == 'x' || c == 'u') {
        val len: Int = digits(offset, 4, 16)
        c = Integer.parseInt(new String(description, offset, len), 16).toChar
        offset += len
        return c
      }
      c match {
        case 'a' => 0x0007
        case 'e' => 0x001B
        case 'f' => 0x000C
        case 'n' => 0x000A
        case 'r' => 0x000D
        case 't' => 0x0009
        case '\\'
           | '.'
           | '*'
           | '+'
           | '?'
           | '|'
           | '['
           | ']'
           | '{'
           | '}'
           | '('
           | ')'
           | '^'
           | '$' => c
        case _   => -1
      }
    }

    def digits(offset: Int, maxLength: Int, base: Int): Int = {
      var i: Int = 0
      while (true) {
        if (i == maxLength || offset + i >= description.length) {
           return i
         }
         var value: Int = description(offset + i) - '0'
         if (value < 0) {
           return i
         }
         if (base > 10 && value >= 10) {
           value += 10 - (if (value >= 'a' - '0') 'a' - '0'
           else 'A' - '0')
         }
         if (value >= base) {
           return i
         }
         i += 1
      }
      throw new Error // we should never get here
    }

    def parseClass(start: Int): CharacterMatcher = {
      offset = start
      parseClass
    }

    def parseClass: CharacterMatcher = {
      if (description(offset) != '[') {
        if (description(offset) == '\\') {
          val range: String = specialClass(description({
            offset += 1; offset
          }))
          if (range != null) {
            offset += 1
            return CharacterMatcher.parse(range)
          }
        }
        return null
      }
      val matcher: CharacterMatcher = new CharacterMatcher(new Array[Boolean](0), description({
        offset += 1; offset
      }) == '^')
      if (matcher.inversePattern) {
        offset += 1
      }
      var previous: Int = -1
      var firstCharacter: Boolean = true

      scala.util.control.Breaks.breakable {
        while (true) {
          if (offset >= description.length) {
            unsupported("short regex")
          }
          val c: Char = description({
            offset += 1; offset - 1
          })
          if (c == '-' && !firstCharacter && description(offset) != ']') {
            if (previous < 0) {
              unsupported("invalid range")
            }
            var rangeEnd: Int = description(offset)
            if ('\\' == rangeEnd) {
              rangeEnd = parseEscapedCharacter
              if (rangeEnd < 0) {
                unsupported("invalid range")
              }
            }
            matcher.ensureCapacity(rangeEnd + 1)
            var j: Int = previous + 1
            while (j <= rangeEnd) {
              matcher.map(j) = true
              j += 1
            }
          }
          else if (c == '\\') {
            val saved: Int = offset
            previous = parseEscapedCharacter
            if (previous < 0) {
              offset = saved - 1
              val clazz: CharacterMatcher = parseClass
              if (clazz == null) {
                unsupported("escape")
              }
              matcher.merge(clazz)
            }
            else {
              matcher.setMatch(previous)
            }
          }
          else if (c == '[') {
            val parser: CharacterMatcher.Parser = new CharacterMatcher.Parser(description)
            val other: CharacterMatcher = parser.parseClass(offset - 1)
            if (other == null) {
              unsupported("invalid merge")
            }
            matcher.merge(other)
            offset = parser.getEndOffset
            previous = -1
          }
          else if (c == '&') {
            if (offset + 2 > description.length || description(offset) != '&' || description(offset + 1) != '[') {
              unsupported("operation")
            }
            val parser: CharacterMatcher.Parser = new CharacterMatcher.Parser(description)
            val other: CharacterMatcher = parser.parseClass(offset + 1)
            if (other == null) {
              unsupported("invalid intersection")
            }
            matcher.intersect(other)
            offset = parser.getEndOffset
            previous = -1
          }
          else if (c == ']') {
            scala.util.control.Breaks.break()
          }
          else {
            previous = c
            matcher.setMatch(previous)
          }
          firstCharacter = false
        }
      }
      matcher
    }

    @throws[UnsupportedOperationException]
    private def unsupported(msg: String): Unit =
      throw new UnsupportedOperationException(s"Unsupported $msg @$offset: ${new String(description, 0, description.length)}")
  }

}

class CharacterMatcher private(var map: Array[Boolean], var inversePattern: Boolean) {
  def matches(c: Char): Boolean = {
    val index: Int = c
    (map.length > index && map(index)) ^ inversePattern
  }

  override def toString: String = {
    val builder: StringBuilder = new StringBuilder
    builder.append("[")
    if (inversePattern) {
      builder.append("^")
    }
    var i: Int = 0
    while (i < map.length) {
      i += 1
      def loop(): Unit = {
        if (!map(i)) {
          return
        }
        builder.append(if (i >= ' ' && i <= 0x7f) "" + i.toChar
        else "\\x" + Integer.toHexString(i))
        var j: Int = i + 1
        while (j < map.length && map(j)) {
          j += 1
        }
        j -= 1
        if (j > i) {
          if (j > i + 1) {
            builder.append('-')
          }
          builder.append(if (j >= ' ' && j <= 0x7f) "" + j.toChar
          else "\\x" + Integer.toHexString(j))
          i = j
        }
      }
      loop()
    }
    builder.append("]")
    builder.toString
  }

  private def setMatch(c: Int): Unit = {
    ensureCapacity(c + 1)
    map(c) = true
  }

  private def ensureCapacity(length: Int): Unit = {
    if (map.length >= length) {
      return
    }
    var size: Int = map.length
    if (size < 32) {
      size = 32
    }
    while (size < length) {
      size <<= 1
    }
    map = java.util.Arrays.copyOf(map, size)
  }

  private def merge(other: CharacterMatcher): Unit = {
    val inversePattern: Boolean = this.inversePattern || other.inversePattern
    if ((map.length < other.map.length) ^ inversePattern) {
      map = java.util.Arrays.copyOf(map, other.map.length)
    }
    var i: Int = 0
    while (i < map.length) {
      map(i) = (matches(i.toChar) || other.matches(i.toChar)) ^ inversePattern
      i += 1
    }
    this.inversePattern = inversePattern
  }

  private def intersect(other: CharacterMatcher): Unit = {
    val inversePattern: Boolean = this.inversePattern && other.inversePattern
    if ((map.length > other.map.length) ^ inversePattern) {
      map = java.util.Arrays.copyOf(map, other.map.length)
    }
    var i: Int = 0
    while (i < map.length) {
      map(i) = (matches(i.toChar) && other.matches(i.toChar)) ^ inversePattern
      i += 1
    }
    this.inversePattern = inversePattern
  }
}
