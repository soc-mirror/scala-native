/* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */ /* Copyright (c) 2008-2015, Avian Contributors

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   There is NO WARRANTY for this software.  See license.txt for
   details. */
package java.util.regex

/** Compiles regular expressions into `PikeVM`s.
 *
 * @author Johannes Schindelin
 */
object Compiler {
  private val regularCharacter: CharacterMatcher =
    CharacterMatcher.parse("[^\\\\.*+?|\\[\\]{}()^$]")

  private class Output(val expr: Compiler#Expression) {
    // try-run to determine the code size
    expr.writeCode(this)

    private[regex] var offset: Int     = 0
    private[regex] var groupCount: Int = -1
    private var program: Array[Int]    = new Array[Int](offset)
    private var findPreambleSize: Int  = 0
    private var classes: java.util.ArrayList[CharacterMatcher] =
      new java.util.ArrayList[CharacterMatcher]
    private var lookarounds: java.util.ArrayList[PikeVM] =
      new java.util.ArrayList[PikeVM]

    // write it out!
    expr.writeCode(this)

    def add(opcode: Int): Unit = {
      if (program != null)
        program(offset) = opcode
      offset += 1
    }

    def markJump: Int = {
      offset += 1
      offset - 1
    }

    def setJump(mark: Int): Unit =
      if (program != null)
        program(mark) = offset

    def markFindPreambleEnd(): Unit = findPreambleSize = offset

    def toVM: PikeVM = {
      val classes: Array[CharacterMatcher] =
        new Array[CharacterMatcher](this.classes.size)
      this.classes.toArray(classes)
      val lookarounds: Array[PikeVM] = new Array[PikeVM](this.lookarounds.size)
      this.lookarounds.toArray(lookarounds)
      new PikeVM(program, findPreambleSize, groupCount, classes, lookarounds)
    }

    def addClass(characterClass: CharacterMatcher): Int =
      if (program == null) {
        -1
      } else {
        val result: Int = classes.size
        classes.add(characterClass)
        result
      }

    def addLookaround(lookaround: PikeVM): Int =
      if (program == null) {
        -1
      } else {
        val result: Int = lookarounds.size
        lookarounds.add(lookaround)
        result
      }
  }

}

class Compiler() {

  private var root: Compiler#Group0 = new Group0
  private var groups: java.util.ArrayList[Compiler#Group] =
    new java.util.ArrayList[Compiler#Group]

  groups.add(root.group)

  abstract private[regex] class Expression {
    protected[regex] def writeCode(output: Compiler.Output): Unit
  }

  private class CharacterRange(val characterClass: CharacterMatcher)
      extends Expression {
    protected[regex] def writeCode(output: Compiler.Output): Unit = {
      output.add(PikeVMOpcodes.CHARACTER_CLASS)
      output.add(output.addClass(characterClass))
    }

    override def toString: String = characterClass.toString
  }

  private class Repeat(var expr: Compiler#Expression,
                       var minCount: Int,
                       var maxCount: Int,
                       var greedy: Boolean)
      extends Expression {
    if (minCount < 0)
      throw new RuntimeException(s"Unexpected min count: $minCount")
    if (maxCount != -1) {
      if (maxCount == 0)
        throw new RuntimeException(s"Unexpected max count: $maxCount")
      if (minCount > maxCount)
        throw new RuntimeException(s"Unexpected range: $minCount, $maxCount")
    }

    protected[regex] def writeCode(output: Compiler.Output): Unit = {
      val start: Int = output.offset
      val splitJmp: Int =
        if (greedy) PikeVMOpcodes.SPLIT_JMP
        else PikeVMOpcodes.SPLIT
      val split: Int =
        if (greedy) PikeVMOpcodes.SPLIT
        else PikeVMOpcodes.SPLIT_JMP
      var i: Int = 1
      while (i < minCount) {
        expr.writeCode(output)
        i += 1
      }
      if (maxCount == -1) {
        if (minCount > 0) {
          val jump: Int = output.offset
          expr.writeCode(output)
          output.add(splitJmp)
          output.add(jump)
        } else {
          output.add(split)
          val jump: Int = output.markJump
          expr.writeCode(output)
          output.add(splitJmp)
          output.add(start + 2)
          output.setJump(jump)
        }
      } else {
        if (minCount > 0)
          expr.writeCode(output)
        if (maxCount > minCount) {
          val jumps: Array[Int] = new Array[Int](maxCount - minCount)
          var i: Int = 0
          while (i < jumps.length) {
            output.add(split)
            jumps(i) = output.markJump
            expr.writeCode(output)
            i += 1
          }
          var j: Int = 0
          while(j < jumps.length) {
            output.setJump(jumps(j))
            j += 1
          }
        }
      }
    }

    override def toString: String = {
      val qualifier: String = if (greedy) "" else "?"
      if (minCount == 0 && maxCount < 2) {
        val minCountString = if (minCount < 0) "*" else "?"
        s"$expr$minCountString$qualifier"
      } else if (minCount == 1 && maxCount < 0) {
        s"$expr+$qualifier"
      } else {
        val maxCountString = if (maxCount < 0) "" else s"$maxCount"
        s"$expr{$minCount,$maxCountString}$qualifier"
      }
    }
  }

  private class Group(
      val capturing: Boolean,
      val initialList: java.util.ArrayList[Compiler#Expression])
      extends Expression {

    private val list: java.util.ArrayList[Compiler#Expression] =
      new java.util.ArrayList[Compiler#Expression]
    private var alternatives: java.util.ArrayList[Compiler#Group] = null

    if (initialList != null)
      list.addAll(initialList)

    def push(expr: Compiler#Expression): Unit = list.add(expr)

    def push(c: Int): Unit =
      push(new Expression() {
        def writeCode(output: Compiler.Output): Unit = output.add(c)

        override def toString: String = c match {
          case _ if c >= 0                     => "" + c.toChar
          case PikeVMOpcodes.DOT               => "."
          case PikeVMOpcodes.WORD_BOUNDARY     => "\\b"
          case PikeVMOpcodes.NON_WORD_BOUNDARY => "\\B"
          case PikeVMOpcodes.LINE_START        => "^"
          case PikeVMOpcodes.LINE_END          => "$"
          case _ =>
            throw new RuntimeException(s"Unhandled opcode: $c")
        }
      })

    def startAlternative(): Unit = {
      if (alternatives == null)
        alternatives = new java.util.ArrayList[Compiler#Group]
      alternatives.add(new Group(false, list))
      list.clear()
    }

    def pop: Compiler#Expression = list.remove(list.size - 1)

    protected[regex] def writeCode(output: Compiler.Output): Unit = {
      var groupIndex: Int = -1
      if (capturing) {
        output.groupCount += 1
        groupIndex = output.groupCount
        output.add(PikeVMOpcodes.SAVE_OFFSET)
        output.add(2 * groupIndex)
      }
      var jumps: Array[Int] = null
      if (alternatives != null) {
        jumps = new Array[Int](alternatives.size)
        var i: Int = 0
        val alternativesIterator = alternatives.iterator
        while (alternativesIterator.hasNext) {
          val alternative = alternativesIterator.next()
          output.add(PikeVMOpcodes.SPLIT)
          val jump: Int = output.markJump
          alternative.writeCode(output)
          output.add(PikeVMOpcodes.JMP)
          jumps(i) = output.markJump
          i += 1
          output.setJump(jump)
        }
      }
      val listIterator = list.iterator
      while (listIterator.hasNext) {
        val expr = listIterator.next()
        expr.writeCode(output)
      }
      if (jumps != null) {
        var i: Int = 0
        while (i < jumps.length) {
          output.setJump(jumps(i))
          i += 1
        }
      }
      if (capturing) {
        output.add(PikeVMOpcodes.SAVE_OFFSET)
        output.add(2 * groupIndex + 1)
      }
    }

    override def toString: String = {
      val builder: StringBuilder = new StringBuilder
      if (alternatives != null || list.size > 1) {
        builder.append('(')
        if (!capturing)
          builder.append("?:")
      }
      if (alternatives != null) {
        val alternativesIterator = alternatives.iterator()
        while (alternativesIterator.hasNext) {
          val alternative = alternativesIterator.next()
          builder.append(alternative).append('|')
        }
      }
      val listIterator = list.iterator
      while (listIterator.hasNext) {
        val expr = listIterator.next()
        builder.append(expr)
      }
      if (alternatives != null || list.size > 1)
        builder.append(')')
      builder.toString
    }
  }

  private class Lookaround(val forward: Boolean, val negative: Boolean)
      extends Expression {
    final private[regex] val group: Compiler#Group = new Group(false, null)

    protected[regex] def writeCode(output: Compiler.Output): Unit = {
      val vm: PikeVM = new Compiler.Output(group).toVM
      if (!forward) {
        vm.reverse()
      }
      output.add(
          if (forward)
            (if (negative) PikeVMOpcodes.NEGATIVE_LOOKAHEAD
             else PikeVMOpcodes.LOOKAHEAD)
          else (if (negative) PikeVMOpcodes.NEGATIVE_LOOKAHEAD
                else PikeVMOpcodes.LOOKBEHIND))
      output.add(output.addLookaround(vm))
    }

    override def toString: String = {
      var inner: String = group.toString
      if (inner.startsWith("(?:")) {
        inner = inner.substring(3)
      } else {
        inner += ")"
      }
      "(?=" + inner
    }
  }

  private class Group0() extends Expression {

    final private[regex] var group: Compiler#Group = new Group(true, null)

    def writeCode(output: Compiler.Output): Unit = {
      // find() preamble
      val start: Int = output.offset
      output.add(PikeVMOpcodes.SPLIT_JMP)
      output.add(start + 5)
      output.add(PikeVMOpcodes.DOTALL)
      output.add(PikeVMOpcodes.SPLIT)
      output.add(start + 2)
      output.markFindPreambleEnd()
      group.writeCode(output)
    }

    override def toString: String = {
      val inner: String = group.toString
      if (inner.startsWith("(?:") && inner.endsWith(")"))
        inner.substring(1, inner.length - 1)
      else inner
    }
  }

  def compile(regex: String): Pattern = {
    val array: Array[Char] = regex.toCharArray
    val characterClassParser: CharacterMatcher.Parser =
      new CharacterMatcher.Parser(array)
    var index: Int = 0
    while (index < array.length) {
      def step(): Unit = {
        var c: Char = array(index)
        val current: Compiler#Group = groups.get(groups.size - 1) // peek
        if (Compiler.regularCharacter.matches(c)) {
          current.push(c)
          return
        }
        c match {
          case '.' =>
            current.push(PikeVMOpcodes.DOT)
          case '\\' =>
            val unescaped: Int =
              characterClassParser.parseEscapedCharacter(index + 1)
            if (unescaped >= 0) {
              index = characterClassParser.getEndOffset - 1
              current.push(unescaped.toChar)
              return
            }
            val characterClass: CharacterMatcher =
              characterClassParser.parseClass(index)
            if (characterClass != null) {
              index = characterClassParser.getEndOffset - 1
              current.push(new CharacterRange(characterClass))
              return
            }
            array(index + 1) match {
              case 'b' =>
                index += 1
                current.push(PikeVMOpcodes.WORD_BOUNDARY)
                return
              case 'B' =>
                index += 1
                current.push(PikeVMOpcodes.NON_WORD_BOUNDARY)
                return
            }
            throw new RuntimeException(s"Parse error @$index: $regex")
          case '?' | '*' | '+' =>
            var greedy: Boolean = true
            if (index + 1 < array.length && array(index + 1) == '?') {
              greedy = false
              index += 1
            }
            val minCount = if (c == '+') 1 else 0
            val maxCount = if (c == '?') 1 else -1
            current.push(new Repeat(current.pop, minCount, maxCount, greedy))
            return
          case '{' =>
            index += 1
            var length: Int = characterClassParser.digits(index, 8, 10)
            val min: Int = regex.substring(index, index + length).toInt
            var max: Int = min
            index += length - 1
            c =
              if (index + 1 < array.length) array(index + 1)
              else 0
            if (c == ',') {
              index += 1
              length = characterClassParser.digits(index + 1, 8, 10)
              max =
                if (length == 0) -1
                else regex.substring(index + 1, index + 1 + length).toInt
              index += length
              c =
                if (index + 1 < array.length) array(index + 1)
                else 0
            }
            if (c != '}')
              throw new RuntimeException(s"Invalid quantifier @$index: $regex")
            index += 1
            var greedy: Boolean = true
            if (index + 1 < array.length && array(index + 1) == '?') {
              index += 1
              greedy = false
            }
            current.push(new Repeat(current.pop, min, max, greedy))
            return
          case '(' =>
            var capturing: Boolean = true
            if (index + 1 < array.length && array(index + 1) == '?') {
              index += 2
              if (index >= array.length)
                throw new RuntimeException(s"Short pattern @$index: $regex")
              c = array(index)
              var lookAhead: Boolean = true
              if (c == '<') {
                index += 1
                if (index >= array.length)
                  throw new RuntimeException(s"Short pattern @$index: $regex")
                lookAhead = false
                c = array(index)
                if (c != '=' && c != '!')
                  throw new IllegalArgumentException(
                      s"Named groups not supported @$index: $regex")
              }
              c match {
                case ':' =>
                  capturing = false
                case '!' | '=' => {
                  capturing = false
                  val lookaround: Compiler#Lookaround =
                    new Lookaround(lookAhead, c == '!')
                  current.push(lookaround)
                  groups.add(lookaround.group) // push
                  return
                }
                case _ =>
                  throw new UnsupportedOperationException(
                      "Not yet supported: " + regex.substring(index))
              }
            }
            val newGroup = new Group(capturing, null)
            groups.add(newGroup) // push
            current.push(newGroup)
            return
          case ')' =>
            if (groups.size < 2)
              throw new RuntimeException(s"Invalid group close @$index: $regex")
            groups.remove(groups.size - 1) // pop
            return
          case '[' => {
            val matcher: CharacterMatcher =
              characterClassParser.parseClass(index)
            if (matcher == null)
              throw new RuntimeException(s"Invalid range @$index: $regex")
            current.push(new CharacterRange(matcher))
            index = characterClassParser.getEndOffset - 1
            return
          }
          case '|' =>
            current.startAlternative()
            return
          case '^' =>
            current.push(PikeVMOpcodes.LINE_START)
            return
          case '$' =>
            current.push(PikeVMOpcodes.LINE_END)
            return
          case _ =>
            throw new RuntimeException(s"Parse error @$index: $regex")
        }
      }
      step()
      index += 1
    }
    if (groups.size != 1)
      throw new IllegalArgumentException(
        s"Unclosed groups: (${groups.size - 1}): $regex")
    val vm: PikeVM    = new Compiler.Output(root).toVM
    val plain: String = vm.isPlainString
    if (plain != null)
      new TrivialPattern(regex, plain, 0)
    else
      new RegexPattern(regex, 0, vm)
  }
}
