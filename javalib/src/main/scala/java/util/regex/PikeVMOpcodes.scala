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

/** Opcodes for the Pike VM.
 *
 * See `PikeVM`.
 *
 * @author Johannes Schindelin
 */
object PikeVMOpcodes {
  val DOT: Int                 = -1
  val DOTALL: Int              = -2
  val WORD_BOUNDARY: Int       = -10
  val NON_WORD_BOUNDARY: Int   = -11
  val LINE_START: Int          = -12
  val LINE_END: Int            = -13
  val CHARACTER_CLASS: Int     = -20
  val LOOKAHEAD: Int           = -30
  val LOOKBEHIND: Int          = -31
  val NEGATIVE_LOOKAHEAD: Int  = -32
  val NEGATIVE_LOOKBEHIND: Int = -33
  val SAVE_OFFSET: Int         = -40
  val SPLIT: Int               = -50
  val SPLIT_JMP: Int           = -51 // this split prefers to jump
  val JMP: Int                 = -52
  val SINGLE_ARG_START: Int    = CHARACTER_CLASS
  val SINGLE_ARG_END: Int      = JMP
}
