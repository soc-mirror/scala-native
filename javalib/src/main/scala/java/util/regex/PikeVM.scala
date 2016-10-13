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

import PikeVMOpcodes._
/**
  * A minimal implementation of a regular expression engine.
  *
  * @author Johannes Schindelin
  */
object PikeVM {
  private val wordCharacter: CharacterMatcher = CharacterMatcher.parse("\\w")
  private val lineTerminator: CharacterMatcher = CharacterMatcher.parse("[\n\r\u0085\u2028\u2029]")

  trait Result {
    def set(start: Array[Int], end: Array[Int]): Unit
  }

  private def length(opcode: Int): Int = {
    if (opcode <= SINGLE_ARG_START && opcode >= SINGLE_ARG_END) 2
    else 1
  }

  private def isJump(opcode: Int): Boolean = {
    opcode <= SPLIT && opcode >= JMP
  }
}

 /* For `find()`, we do not want to anchor the match at the start offset.
  * Our compiler allows this by prefixing the code with an implicit '(?:.*?)'.
  * For regular `matches()` calls, we want to skip that code and start at
  * `findPrefixLength` instead.
  */
class PikeVM protected[regex](val program: Array[Int], val findPrefixLength: Int, val groupCount: Int, val classes: Array[CharacterMatcher], val lookarounds: Array[PikeVM]) {
  final private var offsetsCount = 2 * groupCount + 2
  private val multiLine: Boolean = false

  /**
    * The current thread states.
    *
    * The threads are identified by their program counter. The rationale: as all
    * threads are executed in lock-step, i.e. for the same character in the
    * string to be matched, it does not make sense for two threads to be at the
    * same program counter -- they would both do exactly the same for the rest of
    * the execution.
    *
    * For efficiency, the threads are kept in a linked list that actually lives
    * in an array indexed by the program counter, pointing to the next thread's
    * program counter, in the order of high to low priority.
    *
    * Program counters which have no thread associated thread are marked as -1.
    * The program counter associated with the least-priority thread (the last one
    * in the linked list) is marked as -2 to be able to tell it apart from
    * unscheduled threads.
    *
    * We actually never need to have an explicit value for the priority, the
    * ordering is sufficient: whenever a new thread is to be scheduled and it is
    * found to be scheduled already, it was already scheduled by a
    * higher-priority thread.
    */
  private class ThreadQueue() {
    private var head: Int = -1
    private var tail: Int = -1
    // next[pc] is 1 + the next thread's pc
    private var _next: Array[Int] = new Array[Int](program.length + 1)
    // offsets[pc][2 * group] is 1 + start offset
    private var offsets: Array[Array[Int]] = new Array[Array[Int]](program.length + 1)

    def this(startPC: Int) {
      this()
      head = startPC
      tail = startPC
      offsets(head) = new Array[Int](offsetsCount)
    }

    def queueOneImmediately(into: PikeVM#ThreadQueue): Int = {
      while (true) {
        {
          if (head < 0) {
            return -1
          }
          val wasQueued: Boolean = queueNext(head, head, into)
          val pc: Int = head
          if (head == tail) {
            head = -1
            tail = -1
          }
          else {
            head = next(pc) - 1
            _next(pc) = 0
          }
          offsets(pc) = null
          if (wasQueued) {
            into.tail = pc
            return pc
          }
        }
      }
      throw new Error // should never happen
    }

    /**
      * Schedules the instruction at {@code nextPC} to be executed immediately.
      * <p>
      * For non-matching steps (SPLIT, SAVE_STATE, etc) we need to schedule the
      * corresponding program counter(s) to be handled right after this opcode,
      * before advancing to the next character.
      * </p>
      * <p>
      * To achieve this, we insert the program counter to-be-scheduled in the
      * linked thread list at the current position, but only if it has not been
      * scheduled yet: if it has, a higher-priority thread already reached that
      * state.
      * </p>
      * <p>
      * In contrast to {@link #queueNext(int, int, ThreadQueue)}, this method
      * works on the current step's thread list.
      * </p>
      *
      * @param currentPC
      * the current program counter
      * @param nextPC
      * the program counter to schedule
      * @param copyThreadState
      * whether to spawn off a new thread
      * @return whether the step was queued (i.e. no thread was queued for the
      *         same { @code nextPC} already)
      */
    def queueImmediately(currentPC: Int, nextPC: Int, copyThreadState: Boolean): Boolean = {
      if (isScheduled(nextPC)) {
        return false
      }
      var offsets: Array[Int] = this.offsets(currentPC)
      if (copyThreadState) {
        offsets = java.util.Arrays.copyOf(offsets, offsetsCount)
      }
      if (currentPC == tail) {
        tail = nextPC
      }
      else {
        _next(nextPC) = next(currentPC)
      }
      this.offsets(nextPC) = offsets
      _next(currentPC) = nextPC + 1
      true
    }

    /**
      * Schedules the instruction at {@code nextPC} to be executed in the next
      * step.
      * <p>
      * This method advances the current thread to the next program counter, to
      * be executed after reading the next character.
      * </p>
      *
      * @param currentPC
      * the current program counter
      * @param nextPC
      * the program counter to schedule
      * @param next
      * the thread state of the next step
      * @return whether the step was queued (i.e. no thread was queued for the
      *         same { @code nextPC} already)
      */
    private[regex] def queueNext(currentPC: Int, nextPC: Int, next: PikeVM#ThreadQueue): Boolean = {
      if (next.tail < 0) {
        next.head = nextPC
      }
      else if (next.isScheduled(nextPC)) {
        return false
      }
      else {
        next._next(next.tail) = nextPC + 1
      }
      next.offsets(nextPC) = offsets(currentPC)
      next.tail = nextPC
      true
    }

    def saveOffset(pc: Int, index: Int, offset: Int): Unit = {
      println(s"offsets($pc)($index)")
      println(s"offsets.length: ${offsets.length}")
      println(s"offsets(pc).length: ${offsets(pc).length}")
      offsets.mkString("[", ",", "]")
      offsets(pc)(index) = offset + 1
    }

    def setResult(result: PikeVM.Result): Unit = {
      // copy offsets
      val offsets: Array[Int] = this.offsets(program.length)
      val groupStart: Array[Int] = new Array[Int](groupCount + 1)
      val groupEnd: Array[Int] = new Array[Int](groupCount + 1)
      var j: Int = 0
      while (j <= groupCount) {
        groupStart(j) = offsets(2 * j) - 1
        groupEnd(j) = offsets(2 * j + 1) - 1
        j += 1
      }
      result.set(groupStart, groupEnd)
    }

    private[regex] def mustStartMatchAt(start: Int): Unit = {
      var previous: Int = -1
      var pc: Int = head
      while (pc >= 0) {
        val nextPC: Int = _next(pc) - 1
        if (start + 1 == offsets(pc)(0)) {
          previous = pc
        }
        else {
          _next(pc) = 0
          offsets(pc) = null
          if (pc == tail) {
            head = -1
            tail = -1
          }
          else if (previous < 0) {
            head = nextPC
          }
          else {
            _next(previous) = 1 + nextPC
          }
        }
        pc = nextPC
      }
    }

    private[regex] def startOffset(pc: Int): Int = {
      offsets(pc)(0) - 1
    }

    def isEmpty: Boolean = {
      head < 0
    }

    def isScheduled(pc: Int): Boolean = {
      pc == tail || _next(pc) > 0
    }

    def next(pc: Int): Int = {
      if (pc < 0) head
      else _next(pc) - 1
    }

    def clean(): Unit = {
      var pc: Int = head
      while (pc >= 0) {
        val nextPC: Int = _next(pc) - 1
        _next(pc) = 0
        offsets(pc) = null
        pc = nextPC
      }
      head = -1
      tail = -1
    }
  }

  /**
    * Executes the Pike VM defined by the program.
    * <p>
    * The idea is to execute threads in parallel, at each step executing them
    * from the highest priority thread to the lowest one. In contrast to most
    * regular expression engines, the Thompson/Pike one gets away with linear
    * complexity because the string is matched from left to right, at each step
    * executing a number of threads bounded by the length of the program: if two
    * threads would execute at the same instruction pointer of the program, we
    * need only consider the higher-priority one.
    * </p>
    * <p>
    * This implementation is based on the description of <a
    * href="http://swtch.com/%7Ersc/regexp/regexp2.html">Russ Cox</a>.
    * </p>
    *
    * @param characters
    * the { @link String} to match
    * @param start
    * the start offset where to match
    * @param end
    * the end offset
    * @param anchorStart
    * whether the match must start at { @code start}
    * @param anchorEnd
    * whether the match must start at { @code end}
    * @param result
    * the { @link Matcher} to store the groups' offsets in, if successful
    * @return whether a match was found
    */
  def matches(characters: Array[Char], start: Int, end: Int, anchorStart: Boolean, anchorEnd: Boolean, result: PikeVM.Result): Boolean = {
    val current: PikeVM#ThreadQueue = new ThreadQueue
    var next: PikeVM#ThreadQueue = new ThreadQueue
    // initialize the first thread
    val startPC: Int = if (anchorStart) findPrefixLength
    else 0
    var queued: PikeVM#ThreadQueue = new ThreadQueue(startPC)
    var foundMatch: Boolean = false
    val step: Int = if (end > start) +1
    else -1
    var i: Int = start
    while (i != end + step) {
      if (queued.isEmpty) {
        // no threads left
        return foundMatch
      }
      val c: Char = if (i != end) characters(i)
      else 0
      var pc: Int = -1

      scala.util.control.Breaks.breakable {
        while (true) {
          // true:  we were only interested in a match, found one, success!
          // false: go on.
          def _step(): Boolean = {
            pc = current.next(pc)
            if (pc < 0) {
              pc = queued.queueOneImmediately(current)
            }
            if (pc < 0) {
              scala.util.control.Breaks.break()
            }
            // pc == program.length is a match!
            if (pc == program.length) {
              if (anchorEnd && i != end) {
                return false
              }
              if (result == null) {
                // only interested in a match, no need to go on
                return true
              }
              current.setResult(result)
              // now that we found a match, even higher-priority matches must match
              // at the same start offset
              if (!anchorStart) {
                next.mustStartMatchAt(current.startOffset(pc))
              }
              foundMatch = true
              scala.util.control.Breaks.break()
            }
            val opcode: Int = program(pc)
            opcode match {
              case DOT =>
                if (c != '\u0000' && c != '\r' && c != '\n') {
                  current.queueNext(pc, pc + 1, next)
                }
              case DOTALL =>
                current.queueNext(pc, pc + 1, next)
              case WORD_BOUNDARY
                 | NON_WORD_BOUNDARY => {
                val i2: Int = i - step
                val c2: Int = if (i2 < 0 || i2 >= characters.length) -1
                else characters(i2)
                opcode match {
                  case WORD_BOUNDARY =>
                    if (c2 < 0 || !PikeVM.wordCharacter.matches(c2.toChar)) {
                      if (PikeVM.wordCharacter.matches(c)) {
                        current.queueImmediately(pc, pc + 1, false)
                      }
                    }
                    else if (i >= 0 && i < characters.length && !PikeVM.wordCharacter.matches(c)) {
                      current.queueImmediately(pc, pc + 1, false)
                    }
                  case NON_WORD_BOUNDARY =>
                    if (c2 < 0 || !PikeVM.wordCharacter.matches(c2.toChar)) {
                      if (i >= 0 && i < characters.length && !PikeVM.wordCharacter.matches(c)) {
                        current.queueImmediately(pc, pc + 1, false)
                      }
                    }
                    else if (PikeVM.wordCharacter.matches(c)) {
                      current.queueImmediately(pc, pc + 1, false)
                    }
                }
              }
              case LINE_START =>
                if (i == 0 || (multiLine && PikeVM.lineTerminator.matches(characters(i - 1)))) {
                  current.queueImmediately(pc, pc + 1, false)
                }
              case LINE_END =>
                if (i == characters.length || (multiLine && PikeVM.lineTerminator.matches(c))) {
                  current.queueImmediately(pc, pc + 1, false)
                }
              case CHARACTER_CLASS =>
                if (classes(program(pc + 1)).matches(c)) {
                  current.queueNext(pc, pc + 2, next)
                }
              case LOOKAHEAD =>
                if (lookarounds(program(pc + 1)).matches(characters, i, characters.length, true, false, null)) {
                  current.queueImmediately(pc, pc + 2, false)
                }
              case LOOKBEHIND =>
                if (lookarounds(program(pc + 1)).matches(characters, i - 1, -1, true, false, null)) {
                  current.queueImmediately(pc, pc + 2, false)
                }
              case NEGATIVE_LOOKAHEAD =>
                if (!lookarounds(program(pc + 1)).matches(characters, i, characters.length, true, false, null)) {
                  current.queueImmediately(pc, pc + 2, false)
                }
              case NEGATIVE_LOOKBEHIND =>
                if (!lookarounds(program(pc + 1)).matches(characters, i - 1, -1, true, false, null)) {
                  current.queueImmediately(pc, pc + 2, false)
                }
              /* immediate opcodes, i.e. thread continues within the same step */
              case SAVE_OFFSET =>
                if (result != null) {
                  val index: Int = program(pc + 1)
                  println(current)
                  current.saveOffset(pc, index, i)
                }
                current.queueImmediately(pc, pc + 2, false)
              case SPLIT =>
                current.queueImmediately(pc, program(pc + 1), true)
                current.queueImmediately(pc, pc + 2, false)
              case SPLIT_JMP =>
                current.queueImmediately(pc, pc + 2, true)
                current.queueImmediately(pc, program(pc + 1), false)
              case JMP =>
                current.queueImmediately(pc, program(pc + 1), false)
              case _ if program(pc) >= 0 && program(pc) <= 0xffff =>
                if (program(pc) >= 0 && program(pc) <= 0xffff) {
                  if (c == program(pc).toChar) {
                    current.queueNext(pc, pc + 1, next)
                  }
                }
              case _ =>
                throw new RuntimeException("Invalid opcode: " + opcode + " at pc " + pc)
            }
            false
          }
          val done = _step()
          if (done)
            return true
        }
      }
      // clean linked thread list (and states)
      current.clean()
      // prepare for next step
      val swap: PikeVM#ThreadQueue = queued
      queued = next
      next = swap
      i += step
    }
    foundMatch
  }

  /**
    * Determines whether this machine recognizes a pattern without special
    * operators.
    * <p>
    * In case that the regular expression is actually a plain string without any
    * special operators, we can avoid using a full-blown Pike VM and instead fall
    * back to using the much faster {@link TrivialPattern}.
    * </p>
    *
    * @return the string to match, or null if the machine recognizes a
    *         non-trivial pattern
    */
  def isPlainString: String = {
    // we expect the machine to start with the find preamble and SAVE_OFFSET 0
    // end with SAVE_OFFSET 1
    var start: Int = findPrefixLength
    if (start + 1 < program.length && program(start) == SAVE_OFFSET && program(start + 1) == 0) {
      start += 2
    }
    var end: Int = program.length
    if (end > start + 1 && program(end - 2) == SAVE_OFFSET && program(end - 1) == 1) {
      end -= 2
    }
    var i: Int = start
    while (i < end) {
      if (program(i) < 0) {
        return null
      }
      i += 1
    }
    val array: Array[Char] = new Array[Char](end - start)
    var j: Int = start
    while (j < end) {
      array(j - start) = program(j).toChar
      j += 1
    }
    new String(array)
  }

  /**
    * Reverses the program (effectively matching the reverse pattern).
    * <p>
    * It is a well-known fact that any regular expression can be reordered
    * trivially into an equivalent regular expression to be applied in backward
    * direction (coming in real handy for look-behind expressions).
    * </p>
    * <p>
    * Example: instead of matching the sequence "aaaabb" with the pattern "a+b+",
    * we can match the reverse sequence "bbaaaa" with the pattern "b+a+".
    * </p>
    * <p>
    * One caveat: while the reverse pattern is equivalent in the sense that it
    * matches if, and only if, the original pattern matches the forward
    * direction, the same is not true for submatches. Consider the input "a" and
    * the pattern "(a?)a?": when matching in forward direction the captured group
    * is "a", while the backward direction will yield the empty string. For that
    * reason, Java dictates that capturing groups in look-behind patterns are
    * ignored.
    * </p>
    */
  def reverse(): Unit = reverse(findPrefixLength, program.length)

  /**
    * Reverses a specific part of the program (to match in reverse direction).
    * <p>
    * This is the work-horse of {@link #reverse()}.
    * </p>
    * <p>
    * To visualize the process of reversing a program, let's look at it as a
    * directed graph (each jump is represented by an "<tt>X</tt>
    * ", non-jumping steps are represented by a "<tt>o</tt>"s, arrows show the
    * direction of the flow, <code>SPLIT</code>s spawn two arrows):
    *
    * <pre>
    * o -> X -> X -> o -> X    o -> o
    * ^    |     \         \___^____^
    * \__/       \____________|
    * </pre>
    *
    * The concept of reversing the program is easiest explained as following: if
    * we insert auxiliary nodes "<tt>Y</tt>" for jump targets, the graph looks
    * like this instead:
    *
    * <pre>
    * Y -> o -> X -> X -> o -> X    Y -> o -> Y -> o
    * ^         |     \         \___^_________^
    * \_______/       \____________|
    * </pre>
    *
    * It is now obvious that reversing the program is equivalent to reversing all
    * arrows, simply deleting all <tt>X</tt>s and substituting each <tt>Y</tt>
    * with a jump. Note that the reverse program will have the same number of
    * <tt>JMP</tt>, but they will not be associated with the same arrows!:
    *
    * <pre>
    * X <- o <- o    X <- o <- X <- o
    * |    ^    ^____|________/
    * \__/ \_______/
    * </pre>
    *
    * </p>
    *
    * @param start
    * start reversing the program with this instruction
    * @param end
    * stop reversing at this instruction (this must be either an index
    * aligned exactly with an instruction, or exactly
    * { @code program.length}.
    */
  private def reverse(start: Int, end: Int): Unit = {

    // Pass 1: build the list of jump targets
    val newJumps: Array[Int] = new Array[Int](end + 1)
    val brokenArrows: Array[Boolean] = new Array[Boolean](end + 1)

    {
      var pc: Int = start
      while (pc < end) {
        {
          if (PikeVM.isJump(program(pc))) {
            val target: Int = program(pc + 1)
            newJumps(pc + 1) = newJumps(target)
            newJumps(target) = pc + 1
            if (program(pc) == JMP) {
              brokenArrows(pc + 2) = true
            }
          }
        }
        pc += PikeVM.length(program(pc))
      }
    }
    // Pass 2: determine mapped program counters
    val mapping: Array[Int] = new Array[Int](end)

    {
      var pc: Int = start
      var mappedPC: Int = end
      while (mappedPC > 0 && pc < end) {
        {
          var jump: Int = newJumps(pc)
          while (jump > 0) {
            {
              mappedPC -= 2
            }
            jump = newJumps(jump)
          }
          if (!PikeVM.isJump(program(pc))) {
            mappedPC -= PikeVM.length(program(pc))
          }
          mapping(pc) = mappedPC
        }
        pc += PikeVM.length(program(pc))
      }
    }
    // Pass 3: write the new program
    val reverse: Array[Int] = new Array[Int](end)

    {
      var pc: Int = start
      var mappedPC: Int = end
      scala.util.control.Breaks.breakable {
        while (mappedPC > 0) {
          var brokenArrow: Boolean = brokenArrows(pc)
          var jump: Int = newJumps(pc)
          while (jump > 0) {
            {
              reverse({
                mappedPC -= 1;
                mappedPC
              }) = mapping(jump - 1)
              if (brokenArrow) {
                reverse({
                  mappedPC -= 1;
                  mappedPC
                }) = JMP
                brokenArrow = false
              }
              else {
                reverse({
                  mappedPC -= 1;
                  mappedPC
                }) = if (program(jump - 1) == SPLIT_JMP) SPLIT_JMP
                else SPLIT
              }
            }
            jump = newJumps(jump)
          }
          if (pc == end) {
            scala.util.control.Breaks.break()
          }
          if (!PikeVM.isJump(program(pc))) {
            var i: Int = PikeVM.length(program(pc))
            while ( {
              i -= 1;
              i + 1
            } > 0) {
              {
                reverse({
                  mappedPC -= 1;
                  mappedPC
                }) = program(pc + i)
              }
            }
          }
        }
        pc += PikeVM.length(program(pc))
      }
    }
    System.arraycopy(reverse, start, program, start, end - start)
  }
}
