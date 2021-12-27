# Honeycomb v0.1.0
# Created by KatrinaKitten

## Honeycomb is a parser combinator library written in pure Nim. It's designed to be simple, straightforward, and easy to expand.
##
## Key functions and types
## ***********************
##
## Core parser constructors
## ========================
## - [s](#s,string) - parse a literal string
## - [c](#c,char) - parse a literal character, or one of a list of characters
## - [regex](#regex,string) - parse a regular expression match
## - [fwdcl](#fwdcl.t) - create a forward-declared parser which can be initialized later
##
## Predefined parsers
## ==================
## - [eof](#eof) - A parser that fails if there is any remaining input.
## - [anyChar](#anyChar) - A parser that succeeds for one character of any non-empty input.
## - [whitespace](#whitespace) - A parser that expects at least one whitespace character.
## - [letter](#letter) - A parser that expects one ASCII alphabetical character.
## - [digit](#digit) - A parser that expects one ASCII digit character.
## - [alphanumeric](#alphanumeric) - A parser that expects one ASCII alphanumeric character.
##
## Parser combinators
## ==================
## - [&](#&,Parser[T],Parser[T]) or [chain](#chain.t,Parser[T],Parser[T],varargs[Parser[T]]) - expect multiple parsers one after the other
## - [|](#|,Parser[T],Parser[T]) or [oneOf](#oneOf.t,Parser[T],Parser[T],varargs[Parser[T]]) - expect one of multiple parsers, preferring the left
## - [>>](#>>,Parser[T],Parser) or [then](#then.t,Parser,Parser) - same as `&`, but discards the left-hand parser's result instead of creating a `seq`
## - [<<](#<<,Parser[T],Parser) or [skip](#skip.t,Parser,Parser) - same as `&`, but discards the right-hand parser's result instead of creating a `seq`
## - [*](#*,Parser[T],int) or [times](#times.t,Parser,auto) - expect a parser multiple times in a row, or a range of times
## - [many](#many.t,Parser[T]) - expect a parser 0 or more times
## - [atLeast](#atLeast.t,Parser[T],int) - expect a parser at least `n` times
## - [atMost](#atMost.t,Parser[T],int) - expect a parser 0 to `n` times
## - [optional](#optional.t,Parser[T]) - expect a parser 0 or 1 times
## - [map](#map,Parser[T],proc(T)) - run a custom function on the value of a successful parse
## - [result](#result.t,Parser,T) - replace the value of a successful parse with a constant value
##
## Execution and results
## =====================
## - [parse](#parse,Parser[T],string) - execute a parser and convert it into a `ParseResult`
## - [error](#error,ParseResult) - generate an error message from a failed `ParseResult`
## - [raiseIfFailed](#raiseIfFailed.t,ParseResult) - raise a `ParseError` from a failed `ParseResult`
##
## Advanced parser construction tools
## ==================================
## - [createParserTo](#createParserTo.t,typedesc,untyped) - manually create a parser by defining its processing function
## - [createParser](#createParser.t,untyped) - shortcut for `createParserTo(string)`
## - [succeed](#succeed.t,string,T,string) - create a successful `ParseResult`
## - [fail](#fail.t,string,seq[string]) - create a failed `ParseResult`
## - [applyParser](#applyParser.m,untyped,untyped,untyped) - apply a parser in the definition of a combinator, failing if the given parser fails


import strutils
import sequtils
import re
import macros

from sugar import `=>`


# === Core Types ===

type
  Parser*[T] = ref object
    ## A constructed parser.
    ##
    ## See also:
    ## - [createParserTo](#createParserTo.t,typedesc,untyped)
    ## - [createParser](#createParser.t,untyped)
    ## - [fwdcl](#fwdcl.t)
    ## - [parse](#parse,Parser[T],string)
    body: proc(input: string): ParseResult[T]

  ParseResultKind* = enum success, failure
  ParseResult*[T] = object
    ## The result of a parser run.
    ##
    ## See also:
    ## - [error](#error,ParseResult)
    ## - [raiseIfFailed](#raiseIfFailed.t,ParseResult)

    case kind*: ParseResultKind
    of success: value*: T                 ## The value of the successful parse.
    of failure: expected*: seq[string]    ## A `seq` of expected values for a failed parse.

    tail*: string                         ## The remaining unparsed input.
    fromInput*: string                    ## The input from which this result was generated.

  ParseError* = object of CatchableError

template succeed*[T](inputIn: string, valueIn: T, tailIn: string): ParseResult[T] =
  ## Creates a successful `ParseResult`.
  ##
  ## See also:
  ## - [fail](#fail.t,string,seq[string])
  ## - [createParserTo](#createParserTo.t,typedesc,untyped)
  ## - [createParser](#createParser.t,untyped)
  ParseResult[T](kind: success, value: valueIn, tail: tailIn, fromInput: inputIn)

template fail*[T](inputIn: string, expectedIn: seq[string]): ParseResult[T] =
  ## Creates a failed `ParseResult`. Requires `T` to be explicitly specified, except inside of `createParserTo` in some cases.
  ##
  ## See also:
  ## - [succeed](#succeed.t,string,T,string)
  ## - [createParserTo](#createParserTo.t,typedesc,untyped)
  ## - [createParser](#createParser.t,untyped)
  ParseResult[T](kind: failure, expected: expectedIn, tail: inputIn, fromInput: inputIn)

template fail[T](inputIn: string, expectedIn: seq[string], tailIn: string): ParseResult[T] =
  ## Internal version of [fail](#fail.t,string,seq[string]) that allows setting a tail which isn't the same as the full input.
  ## Used in the implementation of [applyParser](#applyParser.m,untyped,untyped,untyped) to allow chained parsers to report errors correctly.
  ParseResult[T](kind: failure, expected: expectedIn, tail: tailIn, fromInput: inputIn)

proc parse*[T](p: Parser[T], input: string): ParseResult[T] =
  ## Execute a parser on the given `input`.
  if p.body == nil: return fail[T](input, @["forward-declared parser to be initialized with .become(Parser)"])
  p.body(input)

func error*(result1: ParseResult): string =
  ## Generate an error message from a failed `ParseResult`. Returns an empty string if passed a successful result.
  ##
  ## See also:
  ## - [raiseIfFailed](#raiseIfFailed.t,ParseResult)

  if result1.kind == success: return ""
  let 
    expected = result1.expected.deduplicate
    prior    = result1.fromInput[0..^result1.tail.len+1]
    lineNum  = prior.countLines
    lines    = prior.splitLines
    colmNum  = lines[lines.len-1].len+1
  case expected.len:
    of 1: return "[$1:$2] Expected $3" % [$lineNum, $colmNum, expected[0]]
    else: return "[$1:$2] Expected one of $3" % [$lineNum, $colmNum, expected.join(", ")]

template raiseIfFailed*(result1: ParseResult) =
  # Raise an exception if the given `ParseResult` is failed.
  ##
  ## See also:
  ## - [error](#error,ParseResult)
  if result1.kind == failure: raise newException(ParseError, result1.error)


# === Combinator Definition Utilities ===

template createParserTo*(T: typedesc, parser_body: untyped): Parser[T] =
  ## Convenience method for creating a custom `Parser`. Expects the parser's wrapped type as a parameter (not a generic!) and a block which will form the body of the parser.
  ##
  ## Inside the given block, `input` is the input string to be parsed. The block should evaluate to a `ParseResult` created by either `succeed` or `fail`, with the `tail` consisting of the remaining unparsed input.
  ##
  ## See also:
  ## - [succeed](#succeed.t,string,T,string)
  ## - [fail](#fail.t,string,seq[string])
  ## - [createParser](#createParser.t,untyped)
  Parser[T](body: (proc(input {.inject.}: string): ParseResult[T] =
    let fail {.inject, used.} = (i: string, e: seq[string]) => fail[T](i,e)
    parser_body))

template createParser*(parser_body: untyped): Parser[string] =
  ## Same as [createParserTo](#createParserTo.t,typedesc,untyped), but always creates a `Parser[string]`.
  createParserTo(string, parser_body)

macro applyParser*(parser, input, T: untyped) =
  ## Applies the given parser, evaluating to its result if successful or returning from the containing function if failed.
  ##
  ## This is primarily a tool for simplifying the creation of custom combinators, and should only be used if you know what you're doing. It is only designed to function properly in the context of a block passed to [createParserTo](#createParserTo.t,typedesc,untyped), and will raise a compile warning if used outside of this module.
  result = quote do:
    block:
      when instantiationInfo().filename != "honeycomb.nim":
        {.warning: "applyParser is designed for internal use, and should only be used if you know what you're doing.".}
      let temp = `parser`.parse(`input`)
      if temp.kind == failure: return fail[`T`](input, temp.expected, temp.tail)
      temp


# === Core Parsers ===

func s*(expect: string): Parser[string] = 
  ## Creates a parser matching exactly the given string.
  ##
  ## See also:
  ## - [c](#c,char)
  ## - [regex](#regex,string)
  createParser:
    if input.startsWith(expect): return succeed(input, expect, input[expect.len..^1])
    fail(input, @["'$1'" % expect])

func c*(expect: char): Parser[char] = 
  ## Creates a parser matching exactly the given character.
  ##
  ## See also:
  ## - [s](#s,string)
  ## - [regex](#regex,string)
  createParserTo(char):
    if input.len > 0 and input[0] == expect: return succeed(input, expect, input[1..^1])
    fail(input, @["'$1'" % $expect])

func c*(expect: string): Parser[char] = 
  ## Creates a parser matching any one character from the given string.
  createParserTo(char):
    if input.len > 0 and input[0] in expect: return succeed(input, input[0], input[1..^1])
    fail(input, @["character from '$1'" % $expect])

func regex*(expect: string): Parser[string] = 
  ## Creates a parser matching the given regex. The regex must match from the start of the input.
  ##
  ## See also:
  ## - [s](#s,string)
  ## - [c](#c,char)
  let expreg = expect.re
  createParser:
    let bounds = input.findBounds(expreg)
    if bounds[0] != 0: return fail(input, @["'$1'" % $expect])
    succeed(input, input[0..bounds[1]], input[bounds[1]+1..^1])


# === Combinators ===

func map*[T,U](a: Parser[T], fn: proc(x: T): U): Parser[U] = 
  ## If the parser is successful, calls `fn` on the parsed value and succeeds with its return value.
  ##
  ## See also:
  ## - [result](#result.t,Parser,T)
  createParserTo(U):
    let result1 = applyParser(a, input, U)
    return succeed(input, fn(result1.value), result1.tail)

template result*[T](a: Parser, r: T): Parser[T] = 
  ## If the parser is successful, succeeds with the given `r` as value.
  ##
  ## See also:
  ## - [map](#map,Parser[T],proc(T))
  a.map(x => r)

func `|`*[T](a, b: Parser[T]): Parser[T] = 
  ## Succeeds if either parser succeeds, attempting them from left to right.
  ## 
  ## See also:
  ## - [oneOf](#oneOf.t,Parser[T],Parser[T],varargs[Parser[T]])
  createParserTo(T):
    let result1 = a.parse(input)
    if result1.kind == success: return result1
    let result2 = b.parse(input)
    if result2.kind == success: return result2
    fail[T](input, result1.expected & result2.expected)

func `&`*[T](a, b: Parser[T]): Parser[seq[T]] =
  ## Expects each parser in sequence from left to right, creating a `seq` of their results.
  ## 
  ## See also:
  ## - [chain](#chain.t,Parser[T],Parser[T],varargs[Parser[T]])
  createParserTo(seq[T]):
    let result1 = applyParser(a, input, seq[T])
    let result2 = applyParser(b, result1.tail, seq[T])
    return succeed(input, @[result1.value, result2.value], result2.tail)

func `&`*[T](a: Parser[seq[T]], b: Parser[T]): Parser[seq[T]] =
  ## Same as [&](#&,Parser[T],Parser[T]), but merges `seq`s when the left parser is already one.
  createParserTo(seq[T]):
    let result1 = applyParser(a, input, seq[T])
    let result2 = applyParser(b, result1.tail, seq[T])
    return succeed(input, result1.value & result2.value, result2.tail)

func `&`*[T](a: Parser[T], b: Parser[seq[T]]): Parser[seq[T]] =
  ## Same as [&](#&,Parser[T],Parser[T]), but merges `seq`s when the right parser is already one.
  createParserTo(seq[T]):
    let result1 = applyParser(a, input, seq[T])
    let result2 = applyParser(b, result1.tail, seq[T])
    return succeed(input, @[result1.value] & result2.value, result2.tail)

func `&`*[T](a: Parser[seq[T]], b: Parser[seq[T]]): Parser[seq[T]] =
  ## Same as [&](#&,Parser[T],Parser[T]), but merges `seq`s when both parsers already are.
  createParserTo(seq[T]):
    let result1 = applyParser(a, input, seq[T])
    let result2 = applyParser(b, result1.tail, seq[T])
    return succeed(input, result1.value & result2.value, result2.tail)

func `<<`*[T](a: Parser[T], b: Parser): Parser[T] =
  ## Expects each parser in sequence from left to right, ignoring the result of the right parser if successful.
  ##
  ## See also:
  ## - [>>](#>>,Parser[T],Parser)
  ## - [skip](#skip.t,Parser,Parser)
  createParserTo(T):
    let result1 = applyParser(a, input, T)
    let result2 = applyParser(b, result1.tail, T)
    return succeed(input, result1.value, result2.tail)

func `>>`*[T](a: Parser[T], b: Parser): Parser[T] =
  ## Expects each parser in sequence from left to right, ignoring the result of the left parser if successful.
  ##
  ## See also:
  ## - [<<](#<<,Parser[T],Parser)
  ## - [then](#then.t,Parser,Parser)
  createParserTo(T):
    let result1 = applyParser(a, input, T)
    let result2 = applyParser(b, result1.tail, T)
    return succeed(input, result2.value, result2.tail)

func `*`*[T](a: Parser[T], n: int): Parser[seq[T]] =
  ## Expects the parser a given number of times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [times](#times.t,Parser,auto)
  ## - [many](#many.t,Parser[T])
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [atMost](#atMost.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  case n:
    of 0: return Parser[seq[T]](body: (input: string) => succeed(input, newSeq[T](), input))
    of 1: return a.map(x => @[x])
    else: 
      let parsers = a.repeat(n)
      return parsers[1..^1].foldl(a & b, parsers[0].map(x => @[x]))

func `*`*[T](a: Parser[T], n: Slice[int]): Parser[seq[T]] =
  ## Expects the parser a number of times in the given range, returning a `seq` of the matches.
  createParserTo(seq[T]):
    let 
      initial = a * n.a
      result1 = applyParser(initial, input, seq[T])
    var 
      result2 = a.parse(result1.tail)
      outputs = newSeq[T]()
    for i in n.a ..< n.b:
      if result2.kind == failure: break
      outputs.add(result2.value)
      if i < n.b-1: result2 = a.parse(result2.tail)

    succeed(input, result1.value & outputs, result2.tail)

template atLeast*[T](a: Parser[T], n: int): Parser[seq[T]] = 
  ## Expects the parser `n` or more times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],int)
  ## - [times](#times.t,Parser,auto)
  ## - [many](#many.t,Parser[T])
  ## - [atMost](#atMost.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  a.times(n..high(int))

template atMost*[T](a: Parser[T], n: int): Parser[seq[T]] = 
  ## Expects the parser `n` or fewer times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],int)
  ## - [times](#times.t,Parser,auto)
  ## - [many](#many.t,Parser[T])
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  a.times(0..n)

template many*[T](a: Parser[T]): Parser[seq[T]] =
  ## Expects the parser 0 or more times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],int)
  ## - [times](#times.t,Parser,auto)
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [atMost](#atMost.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  a.atLeast(0)

template optional*[T](a: Parser[T]): Parser[seq[T]] =
  ## Expects the parser 0 or 1 times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],int)
  ## - [times](#times.t,Parser,auto)
  ## - [many](#many.t,Parser[T])
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [atMost](#atMost.t,Parser[T],int)
  a.atMost(1)

template flatten*[T](p: Parser[seq[seq[T]]]): Parser[seq[T]] =
  ## Remove one level of nested `seq`s from a parser.
  p.map(x => x.foldl(a & b, newSeq[T]()))


# === String Parser Specific Combinators ===

template join*(a: Parser[seq[string or char]]): Parser[string] = 
  ## Joins a `seq[string]` parser into a single string.
  a.map(x => x.join(""))

template join*(a: Parser[seq[string or char]], delim: string or char): Parser[string] = 
  ## Joins a `seq[string]` parser into a single string, using the given delimiter.
  a.map(x => x.join($delim))



# === Textual Combinator Alternatives ===

template then*(a, b: Parser): auto = a >> b         ## Textual alternative to [>>](#>>,Parser[T],Parser).
template skip*(a, b: Parser): auto = a << b         ## Textual alternative to [<<](#<<,Parser[T],Parser).
template times*(a: Parser, n: auto): auto = a * n   ## Textual alternative to [*](#*,Parser[T],int).

template chain*[T](p1, p2: Parser[T], ps: varargs[Parser[T]]): Parser[seq[T]] =
  ## Expects the passed parsers in sequence from left to right, creating a `seq` of their results.
  ## 
  ## See also:
  ## - [&](#&,Parser[T],Parser[T])
  var outp = p1 & p2
  for p in ps: outp = outp & p
  outp
  
template oneOf*[T](p1, p2: Parser[T], ps: varargs[Parser[T]]): Parser[T] = 
  ## Succeeds if any of the passed parsers succeed, attempting them from left to right.
  ## 
  ## See also:
  ## - [|](#|,Parser[T],Parser[T])
  var outp = p1 | p2
  for p in ps: outp = outp | p
  outp


# === Forward Declarations ===

template fwdcl*[T](): var Parser[T] = 
  ## Create a forward-declared parser. 
  ##
  ## A forward-declared parser can be used normally, but must be initialized with [become](#become.t,Parser[T],Parser[T]) before you can call [parse](#parse,Parser[T],string) on it. A variable containing a forward-declared parser must be declared with `var` for `become` to function.
  ##
  ## You should only use a forward-declared parser if you absolutely need one. Their primary use case is for creating mutually recursive definitions. If you don't need to use the parser in combinators before it's possible to define it, use [createParserTo](#createParserTo.t,typedesc,untyped) instead.
  Parser[T](body: nil)

template become*[T](a: var Parser[T], b: Parser[T]) = 
  ## Initialize a forward-declared parser created with [fwdcl](#fwdcl.t), after which it can be used.
  a.body = b.body


# === Converters ===

converter toStringParser*(a: Parser[char]): Parser[string] = 
  ## Implicitly converts `char` parsers to `string` parsers for ease of use.
  a.map(x => $x)


# === Predefined Parsers ===

let 
  eofImpl = createParser:   
    if input.len > 0: return fail(input, @["EOF"])
    succeed(input, "", "")

  anyCharImpl = createParserTo(char):
    if input.len > 0: return succeed(input, input[0], input[1..^1])
    fail(input, @["any character"])

let 
  eof*          = eofImpl                                                     ## A parser that fails if there is any remaining input.
  anyChar*      = anyCharImpl                                                 ## A parser that succeeds for one character of any non-empty input.
  whitespace*   = regex(r"\s+")                                               ## A parser that expects at least one whitespace character.
  letter*       = c("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")   ## A parser that expects one ASCII alphabetical character.
  digit*        = c("0123456789")                                             ## A parser that expects one ASCII digit character.
  alphanumeric* = letter | digit                                              ## A parser that expects one ASCII alphanumeric character.
