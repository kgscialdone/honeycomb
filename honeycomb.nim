# Honeycomb v0.1.1
# Created by KatrinaKitten

## Honeycomb is a parser combinator library written in pure Nim. It's designed to be simple, straightforward, and easy to expand, while relying on zero dependencies from outside of Nim's standard library.
##
## Honeycomb was heavily inspired by the excellent Python library [parsy](https://github.com/python-parsy/parsy), as well as the existing but unmaintained [combparser](https://github.com/PMunch/combparser).

runnableExamples:
  let 
    parser  = ((s("Hello") | s("Greetings")) << c(',') << whitespace) & (regex(r"\w+") << c("!."))
    result1 = parser.parse("Hello, world!")
    result2 = parser.parse("Greetings, peasants.")

  assert result1.kind  == success
  assert result1.value == @["Hello", "world"]

  assert result2.kind  == success
  assert result2.value == @["Greetings", "peasants"]

## Honeycomb supports the following key features:
## - Predefined parsers and parser constructors for numerous basic parsing needs
## - An extensive library of combinators with which to combine them
## - Support for manually defining custom parsers / combinators
## - Forward-declared parsers to support mutually recursive parser definitions
## 
## Key functions and types
## ***********************
##
## Core parser constructors
## ========================
## - [s](#s,string) - parse a literal string
## - [c](#c,char) - parse a literal character, or one of a list or range of characters
## - [regex](#regex,string) - parse a regular expression match
## - [nop](#nop) - always succeed, consuming no input
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
## - [&](#&,Parser[seq[T]],Parser[seq[T]]) or [chain](#chain.t,Parser[T],Parser[T],varargs[Parser[T]]) - expect multiple parsers one after the other
## - [|](#|,Parser[T],Parser[T]) or [oneOf](#oneOf.t,Parser[T],Parser[T],varargs[Parser[T]]) - expect one of multiple parsers, preferring the left
## - [>>](#>>,Parser[T],Parser) or [then](#then.t,Parser,Parser) - same as `&`, but discards the left-hand parser's result instead of creating a `seq`
## - [<<](#<<,Parser[T],Parser) or [skip](#skip.t,Parser,Parser) - same as `&`, but discards the right-hand parser's result instead of creating a `seq`
## - [*](#*,Parser[T],int) or [times](#times.t,Parser,auto) - expect a parser multiple times in a row, or a range of times
## - [!](#!,Parser[T]) or [negla](#negla.t,Parser) - negative lookahead; expect a parser to fail, but consume no input
## - [many](#many.t,Parser[T]) - expect a parser 0 or more times
## - [atLeast](#atLeast.t,Parser[T],int) - expect a parser at least `n` times
## - [atMost](#atMost.t,Parser[T],int) - expect a parser 0 to `n` times
## - [optional](#optional.t,Parser[T]) - expect a parser optionally, returning the default value of its result type if it doesn't match
## - [orEmpty](#orEmpty.t,Parser[T]) - expect a parser optionally, returning it in a `seq` or an empty `seq` if it doesn't match
## - [map](#map,Parser[T],proc(T)) - run a custom function on the value of a successful parse
## - [mapEach](#mapEach,Parser[seq[T]],proc(T)) - run a custom function on each value of a successful parse containing a `seq`
## - [result](#result.t,Parser,T) - replace the value of a successful parse with a constant value
## - [filter](#filter.t,Parser[seq[T]],proc(T)) - filter the results of a successful parse by a predicate function
## - [flatten](#flatten.t,Parser[seq[seq[T]]]) - remove a level of nested `seq`s from a parser
## - [removeEmpty](#removeEmpty.t,Parser[seq[seq[T]]]) - remove empty `seq`s from a parser resulting in nested `seqs`
## - [desc](#desc,Parser[T],string) - set a custom description to be shown when a parser fails
## - [asSeq](#asSeq.t,Parser[T]) - wrap a parser's result in a `seq`
## - [asString](#asString.t,Parser) - convert a parser's result to a `string` via `$`
## - [validate](#validate,Parser[T],proc(T),string) - validate a result, reject if it doesn't fulfill a given conditiom
##
## Execution and results
## =====================
## - [parse](#parse,Parser[T],string) - execute a parser and convert it into a `ParseResult`
## - [error](#error,ParseResult) - generate an error message from a failed `ParseResult`
## - [raiseIfFailed](#raiseIfFailed.t,ParseResult) - raise a `ParseError` from a failed `ParseResult`
## - [lineInfo](#lineInfo,ParseResult) - get the line and column at which a parser ended
##
## Advanced parser construction tools
## ==================================
## - [createParser](#createParser.t,typedesc,untyped) - manually create a parser by defining its processing function
## - [succeed](#succeed.t,string,T,string) - create a successful `ParseResult`
## - [fail](#fail.t,string,seq[string]) - create a failed `ParseResult`
## - [applyParser](#applyParser.m,untyped,untyped,untyped) - apply a parser in the definition of a combinator, failing if the given parser fails
## - [fwdcl](#fwdcl.t) - create a forward-declared parser which can be initialized later


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
    ## - [createParser](#createParser.t,typedesc,untyped)
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
    ## - [lineInfo](#lineInfo,ParseResult)

    case kind*: ParseResultKind
    of success: value*: T                 ## The value of the successful parse.
    of failure: expected*: seq[string]    ## A `seq` of expected values for a failed parse.

    tail*: string                         ## The remaining unparsed input.
    fromInput*: string                    ## The input from which this result was generated.

  ParseError* = object of CatchableError

proc parse*[T](p: Parser[T], input: string): ParseResult[T] =
  ## Execute a parser on the given `input`.
  runnableExamples:
    let 
      parser = s("Hello, world!")
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == "Hello, world!"

  if p.body == nil: return ParseResult[T](kind: failure, expected: @["forward-declared parser to be initialized with .become(Parser)"], tail: input, fromInput: input)
  p.body(input)

func lineInfo*(result1: ParseResult): (int,int) =
  ## Get the line and column at which a `ParseResult`'s tail begins.
  ##
  ## See also:
  ## - [error](#error,ParseResult,bool)
  ## - [raiseIfFailed](#raiseIfFailed.t,ParseResult)
  let 
    prior   = result1.fromInput[0..^result1.tail.len+1]
    lineNum = prior.countLines
    lines   = prior.splitLines
    colmNum = lines[lines.len-1].len+1
  (lineNum, colmNum)

func error*(result1: ParseResult, showPos: bool = true): string =
  ## Generate an error message from a failed `ParseResult`. Returns an empty string if passed a successful result.
  ##
  ## If `showPos` is `true`, the error message is prepended with the line and column number at which the error occurred.
  ##
  ## See also:
  ## - [raiseIfFailed](#raiseIfFailed.t,ParseResult)
  ## - [lineInfo](#lineInfo,ParseResult)
  runnableExamples:
    let 
      parser = s("Hello, world!")
      result = parser.parse("Greetings, peasants!")

    assert result.kind  == failure
    assert result.error == "[1:1] Expected 'Hello, world!'"

  if result1.kind == success: return ""
  let 
    expected   = result1.expected.deduplicate
    (line,col) = result1.lineInfo
    posStr     = (if showPos: "[$1:$2] " % [$line, $col] else: "")
  case expected.len:
    of 1: return "$1Expected $2" % [posStr, expected[0]]
    else: return "$1Expected one of $2" % [posStr, expected.join(", ")]

template raiseIfFailed*(result1: ParseResult) =
  ## Raise an exception if the given `ParseResult` is failed.
  ##
  ## See also:
  ## - [error](#error,ParseResult,bool)
  ## - [lineInfo](#lineInfo,ParseResult)
  runnableExamples:
    let 
      parser = s("Hello, world!")
      result = parser.parse("Greetings, peasants!")

    try:
      result.raiseIfFailed   #! Error: [1:1] Expected 'Hello, world!'
    except ParseError:
      discard

  if result1.kind == failure: raise newException(ParseError, result1.error)


# === Combinator Definition Utilities ===

template createParser*(T: typedesc, parser_body: untyped): Parser[T] =
  ## Convenience method for creating a custom `Parser`. Expects the parser's result type as a parameter (not a generic!) and a block which will form the body of the parser.
  ##
  ## Inside the given block, the following bindings are exposed:
  ## - | `let input: string` 
  ##   | The input string to be parsed.
  ## - | `func succeed(input: string, value: T, tail: string)`
  ##   | Creates a successful `ParseResult` with the given `value`.
  ## - | `func fail(input: string, expected: seq[string], tail: string)`
  ##   | Creates a failed `ParseResult` with the given `expected`.
  ## 
  ## The block should return a `ParseResult` created by either `succeed` or `fail`, with the tail consisting of the remaining unparsed input. If the parser failed, the tail should almost always be the entire input; this should only not be the case when a combinator needs to partially consume the input, for example the [&](#&,Parser[seq[T]],Parser[seq[T]]) operator.
  runnableExamples:
    # This is just a contrived example; for this exact interaction,
    #   `s("power level").result(9001)` would be better.
    let
      parser = createParser(int):
        if input == "power level": return succeed(input, 9001, "")
        fail(input, @["'power level'"], input)

      result = parser.parse("power level")

    assert result.kind  == success
    assert result.value == 9001

  block:
    func succeedImpl(inputIn: string, valueIn: T, tailIn: string): ParseResult[T] =
      ParseResult[T](kind: success, value: valueIn, tail: tailIn, fromInput: inputIn)

    func failImpl(inputIn: string, expectedIn: seq[string], tailIn: string): ParseResult[T] =
      ParseResult[T](kind: failure, expected: expectedIn, tail: tailIn, fromInput: inputIn)

    Parser[T](body: (proc(input {.inject.}: string): ParseResult[T] =
      let succeed {.inject, used.} = succeedImpl
      let fail {.inject, used.} = failImpl
      parser_body))

macro applyParser*(parser, input, T: untyped) =
  ## Applies the given parser, evaluating to its result if successful or returning from the containing function if failed.
  ##
  ## This is primarily a tool for simplifying the creation of combinators internally, and should only be used if you know what you're doing. It is only designed to function properly in the context of a block passed to [createParser](#createParser.t,typedesc,untyped), and will raise a compile warning if used outside of this module.
  runnableExamples:
    # Honeycomb exports roughly this definition as the `&` operator.
    func sequence[T](a, b: Parser[T]): Parser[seq[T]] =
      createParser(seq[T]):
        {.push warnings: off.} # Hide the warnings, we know what we're doing.
        let result1 = applyParser(a, input, seq[T])
        let result2 = applyParser(b, result1.tail, seq[T])
        {.pop.}
        return succeed(input, @[result1.value, result2.value], result2.tail)

    let 
      parser = sequence(s("Hello, "), s("world!"))
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == @["Hello, ", "world!"]

  result = quote do:
    block:
      when instantiationInfo().filename != "honeycomb.nim":
        {.warning: "applyParser is designed for internal use, and should only be used if you know what you're doing.".}
      let temp = `parser`.parse(`input`)
      if temp.kind == failure: return ParseResult[`T`](kind: failure, fromInput: input, expected: temp.expected, tail: temp.tail)
      temp


# === Core Parsers ===

func s*(expect: string): Parser[string] = 
  ## Creates a parser matching exactly the given string.
  ##
  ## See also:
  ## - [c](#c,char)
  ## - [regex](#regex,string)
  runnableExamples:
    let 
      parser = s("Hello, world!")
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == "Hello, world!"

  createParser(string):
    if input.startsWith(expect): return succeed(input, expect, input[expect.len..^1])
    fail(input, @["'$1'" % expect], input)

func c*(expect: char): Parser[char] = 
  ## Creates a parser matching exactly the given character.
  ##
  ## See also:
  ## - [s](#s,string)
  ## - [regex](#regex,string)
  runnableExamples:
    let 
      parser = c('H')
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == 'H'

  createParser(char):
    if input.len > 0 and input[0] == expect: return succeed(input, expect, input[1..^1])
    fail(input, @["'$1'" % $expect], input)

func c*(expect: string | set): Parser[char] = 
  ## Creates a parser matching any one character from the given string.
  runnableExamples:
    let 
      parser = c("HIJK")
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == 'H'

  createParser(char):
    if input.len > 0 and input[0] in expect: return succeed(input, input[0], input[1..^1])
    fail(input, @["character from '$1'" % $expect], input)

func c*(expect: Slice[char]): Parser[char] = 
  ## Creates a parser matching any one character from the given range.
  runnableExamples:
    let 
      parser = c('H'..'K')
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == 'H'

  createParser(char):
    if input.len > 0 and input[0] in expect: return succeed(input, input[0], input[1..^1])
    fail(input, @["character from $1..$2" % [$expect.a, $expect.b]], input)

func regex*(expect: string): Parser[string] = 
  ## Creates a parser matching the given regex. The regex must match from the start of the input.
  ##
  ## See also:
  ## - [s](#s,string)
  ## - [c](#c,char)
  runnableExamples:
    let 
      parser = regex(r"\w+, \w+!")
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == "Hello, world!"

  let expreg = expect.re
  createParser(string):
    let bounds = input.findBounds(expreg)
    if bounds[0] != 0: return fail(input, @["match for regex '$1'" % $expect], input)
    succeed(input, input[0..bounds[1]], input[bounds[1]+1..^1])

func nop*[T](): Parser[T] =
  ## Creates a parser for the given type which always succeeds, consumes no input, and has a `value` of the default for type `T`.
  createParser(T):
    succeed(input, default(T), input)

# === Combinators ===

func map*[T,U](a: Parser[T], fn: proc(x: T): U): Parser[U] = 
  ## If the parser is successful, calls `fn` on the parsed value and succeeds with its return value.
  ##
  ## See also:
  ## - [mapEach](#mapEach,Parser[seq[T]],proc(T))
  ## - [result](#result.t,Parser,T)
  ## - [filter](#filter.t,Parser[seq[T]],proc(T))
  runnableExamples:
    from std/sugar import `=>`
    let 
      parser = (s("Hello, ") & s("world!")).map(x => (x[0], x[1]))
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == ("Hello, ", "world!")

  createParser(U):
    let result1 = applyParser(a, input, U)
    return succeed(input, fn(result1.value), result1.tail)

template mapEach*[T,U](a: Parser[seq[T]], fn: proc(x: T): U): Parser[seq[U]] =
  ## If the parser is successful, calls `fn` on each value in the resulting `seq`, succeeding with a `seq` of the results.
  ##
  ## See also:
  ## - [map](#map,Parser[T],proc(T))
  ## - [result](#result.t,Parser,T)
  ## - [filter](#filter.t,Parser[seq[T]],proc(T))
  runnableExamples:
    from std/strutils import toUpperAscii
    let 
      parser = (s("Hello, ") & s("world!")).mapEach(toUpperAscii)
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == @["HELLO, ", "WORLD!"]

  a.map(x => x.map(fn))

template result*[T](a: Parser, r: T): Parser[T] = 
  ## If the parser is successful, succeeds with the given `r` as value.
  ##
  ## See also:
  ## - [map](#map,Parser[T],proc(T))
  ## - [mapEach](#mapEach,Parser[seq[T]],proc(T))
  ## - [filter](#filter.t,Parser[seq[T]],proc(T))
  runnableExamples:
    let 
      parser = s("power level").result(9001)
      result = parser.parse("power level")

    assert result.kind  == success
    assert result.value == 9001

  a.map(x => r)

func filter*[T](a: Parser[seq[T]], fn: proc(x: T): bool): Parser[seq[T]] =
  ## Filter the results of a successful parse to `seq` by the given predicate, keeping only those results for which it returns `true`.
  ##
  ## See also:
  ## - [map](#map,Parser[T],proc(T))
  ## - [mapEach](#mapEach,Parser[seq[T]],proc(T))
  ## - [result](#result.t,Parser,T)
  a.mapEach((x: T) => (if fn(x): @[x] else: newSeq[T]())).flatten

func `|`*[T](a, b: Parser[T]): Parser[T] = 
  ## Succeeds if either parser succeeds, attempting them from left to right.
  ## 
  ## See also:
  ## - [oneOf](#oneOf.t,Parser[T],Parser[T],varargs[Parser[T]]) - textual equivalent to this operator
  runnableExamples:
    let 
      parser  = s("Hello") | s("Greetings")
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")

    assert result1.kind  == success
    assert result1.value == "Hello"
    assert result2.kind  == success
    assert result2.value == "Greetings"

  createParser(T):
    let result1 = a.parse(input)
    if result1.kind == success: return result1
    let result2 = b.parse(input)
    if result2.kind == success: return result2
    fail(input, result1.expected & result2.expected, input)

func `&`*[T](a, b: Parser[seq[T]]): Parser[seq[T]] =
  ## Expects each parser in sequence from left to right, creating a `seq` of their results. If one or both of the parsers already results in a `seq` of the other's type, the two `seq`s will be merged.
  ## 
  ## See also:
  ## - [chain](#chain.t,Parser[T],Parser[T],varargs[Parser[T]]) - textual equivalent to this operator
  runnableExamples:
    let 
      parser = s("Hello, ") & s("world!")
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == @["Hello, ", "world!"]

  createParser(seq[T]):
    let result1 = applyParser(a, input, seq[T])
    let result2 = applyParser(b, result1.tail, seq[T])
    return succeed(input, result1.value & result2.value, result2.tail)

template `&`*[T](a, b: Parser[T]): Parser[seq[T]] =
  ## Same as [&](#&,Parser[seq[T]],Parser[seq[T]]), exists to wrap non-`seq` parsers in `seq`s.
  a.asSeq & b.asSeq

template `&`*[T](a: Parser[seq[T]], b: Parser[T]): Parser[seq[T]] =
  ## Same as [&](#&,Parser[seq[T]],Parser[seq[T]]), exists to wrap non-`seq` parsers in `seq`s.
  a & b.asSeq

template `&`*[T](a: Parser[T], b: Parser[seq[T]]): Parser[seq[T]] =
  ## Same as [&](#&,Parser[seq[T]],Parser[seq[T]]), exists to wrap non-`seq` parsers in `seq`s.
  a.asSeq & b

func `<<`*[T](a: Parser[T], b: Parser): Parser[T] =
  ## Expects each parser in sequence from left to right, ignoring the result of the right parser if successful.
  ##
  ## See also:
  ## - [skip](#skip.t,Parser,Parser) - textual equivalent to this operator
  ## - [>>](#>>,Parser[T],Parser) / [then](#then.t,Parser,Parser)
  runnableExamples:
    let 
      parser = s("Hello, ") << s("world!")
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == "Hello, "

  createParser(T):
    let result1 = applyParser(a, input, T)
    let result2 = applyParser(b, result1.tail, T)
    return succeed(input, result1.value, result2.tail)

func `>>`*[T](a: Parser, b: Parser[T]): Parser[T] =
  ## Expects each parser in sequence from left to right, ignoring the result of the left parser if successful.
  ##
  ## See also:
  ## - [then](#then.t,Parser,Parser) - textual equivalent to this operator
  ## - [<<](#<<,Parser[T],Parser) / [skip](#skip.t,Parser,Parser)
  runnableExamples:
    let 
      parser = s("Hello, ") >> s("world!")
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == "world!"

  createParser(T):
    let result1 = applyParser(a, input, T)
    let result2 = applyParser(b, result1.tail, T)
    return succeed(input, result2.value, result2.tail)

func `*`*[T](a: Parser[T], n: int): Parser[seq[T]] =
  ## Expects the parser a given number of times, returning a `seq` of the matches. Also supports slices as ranges of valid amounts (see [*](#*.t,Parser[T],Slice[int])).
  ##
  ## Note that this will succeed early if the given parser succeeds but doesn't consume any input, in order to prevent infinite loops caused by parsers like [nop](#nop) or [atMost](#atMost.t,Parser[T],int). This means it may not work correctly on parsers with non-deterministic behavior or which use/modify external state; this is intentionally undefined behavior.
  ##
  ## See also:
  ## - [times](#times.t,Parser,auto) - textual equivalent to this operator
  ## - [many](#many.t,Parser[T])
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [atMost](#atMost.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  runnableExamples:
    let 
      parser = s("Hello ") * 3
      result = parser.parse("Hello Hello Hello ")

    assert result.kind  == success
    assert result.value == @["Hello ", "Hello ", "Hello "]

  case n:
    of 0: return nop[seq[T]]()
    of 1: return a.asSeq
    else: 
      createParser(seq[T]):
        var 
          result1  = applyParser(a, input, seq[T])
          outputs  = @[result1.value]
          lastTail = result1.tail
        for i in countup(2, n):
          result1 = applyParser(a, result1.tail, seq[T])
          if result1.tail == lastTail: break
          lastTail = result1.tail
          outputs.add(result1.value)
        succeed(input, outputs, result1.tail)

template `*`*[T](p: Parser[T], n: Slice[int]): Parser[seq[T]] =
  ## Same as [*](#*,Parser[T],int), but takes a range of possible amounts, expecting at least the lower bound and at most the higher bound.
  ((p * n.a) & (p.orEmpty * (n.b - n.a))).flatten

func `!`*[T](a: Parser[T]): Parser[T] =
  ## Succeeds if the given parser fails and fails if it succeeds, consuming no input regardless. The resulting value if successful will be the default for type `T`.
  ##
  ## See also:
  ## - [negla](#negla.t,Parser) - textual equivalent to this operator
  runnableExamples:
    let
      parser  = !s("Hello")
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")

    assert result1.kind  == failure
    assert result1.error == "[1:1] Expected successful negative lookahead"
    assert result2.kind  == success
    assert result2.value == ""

  createParser(T):
    let result1 = a.parse(input)
    case result1.kind:
      of success: return fail(input, @["successful negative lookahead"], input)
      of failure: return succeed(input, default(T), input)

template atLeast*[T](a: Parser[T], n: int): Parser[seq[T]] = 
  ## Expects the parser `n` or more times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],int) / [times](#times.t,Parser,auto)
  ## - [many](#many.t,Parser[T])
  ## - [atMost](#atMost.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  a.times(n..high(int))

template atMost*[T](a: Parser[T], n: int): Parser[seq[T]] = 
  ## Expects the parser `n` or fewer times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],int) / [times](#times.t,Parser,auto)
  ## - [many](#many.t,Parser[T])
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  a.times(0..n)

template many*[T](a: Parser[T]): Parser[seq[T]] =
  ## Expects the parser 0 or more times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],int) / [times](#times.t,Parser,auto)
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [atMost](#atMost.t,Parser[T],int)
  ## - [optional](#optional.t,Parser[T])
  a.atLeast(0)

template optional*[T](a: Parser[T]): Parser[T] =
  ## Expects the parser optionally, returning the default value of type `T` if it doesn't match.
  ##
  ## See also:
  ## - [orEmpty](#orEmpty.t,Parser[T])
  a | nop[T]()

template orEmpty*[T](a: Parser[T]): Parser[seq[T]] =
  # Expects the parser optionally, returning it wrapped in a `seq`, or an empty `seq` if it doesn't match.
  ##
  ## See also:
  ## - [optional](#optional.t,Parser[T])
  a.asSeq.optional

template flatten*[T](p: Parser[seq[seq[T]]]): Parser[seq[T]] =
  ## Remove one level of nested `seq`s from a parser.
  ##
  ## See also:
  ## - [removeEmpty](#removeEmpty.t,Parser[seq[seq[T]]])
  runnableExamples:
    let
      parser = digit.atLeast(3).atLeast(1).flatten
      result = parser.parse("127456")

    assert result.kind  == success
    assert result.value == @['1', '2', '7', '4', '5', '6']

  p.map(x => x.foldl(a & b, newSeq[T]()))

template removeEmpty*[T](p: Parser[seq[seq[T]]]): Parser[seq[seq[T]]] =
  ## Remove empty `seq`s from a parser returning nested `seq`s.
  ##
  ## See also:
  ## - [flatten](#flatten.t,Parser[seq[seq[T]]])
  runnableExamples:
    let
      parser = digit.atMost(3).atLeast(4).removeEmpty
      result = parser.parse("127456")

    assert result.kind  == success
    assert result.value == @[@['1', '2', '7'], @['4', '5', '6']]

  p.filter(x => x.len > 0)

template join*(a: Parser[seq[string or char]], delim: string or char = ""): Parser[string] = 
  ## Joins a `seq[string]` parser into a single string, using the given delimiter.
  runnableExamples:
    let
      parser = (s("Hello, ") & s("world!")).join
      result = parser.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == "Hello, world!"

  a.map(x => x.join($delim))

template asString*(a: Parser): Parser[string] = 
  ## Converts a parser to a `string` parser via `$`.
  a.map(`$`)

template asSeq*[T](a: Parser[T]): Parser[seq[T]] =
  ## Wraps a parser's result in a `seq`.
  a.map(x => @[x])

func desc*[T](a: Parser[T], description: string): Parser[T] =
  ## Add a custom description to a parser, which is shown when the parser fails instead of the default expectation.
  runnableExamples:
    let
      parser = s("Hello, world!").desc("a nice greeting")
      result = parser.parse("Greetings, peasants!")

    assert result.kind == failure
    assert result.error == "[1:1] Expected a nice greeting"

  createParser(T):
    let result1 = a.parse(input)
    if result1.kind == failure: return fail(input, @[description], input)
    result1

proc validate*[T](p: Parser[T]; cond: proc(a: T): bool; errorMessage: string = "Cannot parse"): Parser[T] =
  ## Validate the results of a successful parse with a given condition.
  runnableExamples:
    from sugar import `=>`
    import strutils
    let
      parser1 = digit.atLeast(3).map(a => strutils.join(a).parseInt)
      parser2 = digit.atLeast(3).map(a => strutils.join(a).parseInt).
                  validate(a => a < 500, "integer less than 500")
      result1 = parser1.parse("345")
      result2 = parser1.parse("678")
      result3 = parser2.parse("345")
      result4 = parser2.parse("678")
    assert result1.kind      == success
    assert result1.value     == 345
    assert result2.kind      == success
    assert result2.value     == 678
    assert result3.kind      == success
    assert result3.value     == 345
    assert result4.kind      == failure
    assert result4.error     == "[1:1] Expected integer less than 500"

  createParser(T):
    let interimResult = p.parse(input)
    if interimResult.kind == ParseResultKind.success and cond(interimResult.value):
      return succeed(input, interimResult.value, interimResult.tail)
    else:
      return fail(input, @[errorMessage], input)

# === Textual Combinator Alternatives ===

template then*(a, b: Parser): auto = a >> b         ## Textual alternative to [>>](#>>,Parser[T],Parser).
template skip*(a, b: Parser): auto = a << b         ## Textual alternative to [<<](#<<,Parser[T],Parser).
template times*(a: Parser, n: auto): auto = a * n   ## Textual alternative to [*](#*,Parser[T],int).
template negla*(a: Parser): auto = !a               ## Textual alternative to [!](#!,Parser[T]).

template chain*[T](p1, p2: Parser[T], ps: varargs[Parser[T]]): Parser[seq[T]] =
  ## Textual alternative to [&](#&,Parser[seq[T]],Parser[seq[T]]). Accepts more than two parsers for convenience, chaining them in order.
  var outp = p1 & p2
  for p in ps: outp = outp & p
  outp
  
template oneOf*[T](p1, p2: Parser[T], ps: varargs[Parser[T]]): Parser[T] = 
  ## Textual alternative to [|](#|,Parser[T],Parser[T]). Accepts more than two parsers for convenience, attempting them in order.
  var outp = p1 | p2
  for p in ps: outp = outp | p
  outp


# === Forward Declarations ===

template fwdcl*[T](): var Parser[T] = 
  ## Create a forward-declared parser. 
  ##
  ## A forward-declared parser can be used normally, but must be initialized with [become](#become.t,Parser[T],Parser[T]) before you can call [parse](#parse,Parser[T],string) on it. A variable containing a forward-declared parser must be declared with `var` for `become` to function.
  ##
  ## You should only use a forward-declared parser if you absolutely need one. Their primary use case is for creating mutually recursive definitions. If you don't need to use the parser in combinators before it's possible to define it, you probably don't need a forward-declared parser.
  runnableExamples:
    var parser1 = fwdcl[string]()
    let parser2 = s("Hello, ") >> parser1 << c('!')

    parser1.become(s("world"))

    let result = parser2.parse("Hello, world!")

    assert result.kind  == success
    assert result.value == "world"


  Parser[T](body: nil)

template become*[T](a: var Parser[T], b: Parser[T]) = 
  ## Initialize a forward-declared parser created with [fwdcl](#fwdcl.t), after which it can be used.
  a.body = b.body


# === Converters ===

converter asString*(a: Parser[char]): Parser[string] = 
  ## Implicitly converts `char` parsers to `string` parsers for ease of use.
  a.map(x => $x)


# === Predefined Parsers ===

let 
  eofImpl = createParser(string):   
    if input.len > 0: return fail(input, @["EOF"], input)
    succeed(input, "", "")

  anyCharImpl = createParser(char):
    if input.len > 0: return succeed(input, input[0], input[1..^1])
    fail(input, @["any character"], input)

let 
  eof*          = eofImpl                     ## A parser that fails if there is any remaining input.
  anyChar*      = anyCharImpl                 ## A parser that succeeds for one character of any non-empty input.
  whitespace*   = regex(r"\s+")               ## A parser that expects at least one whitespace character.
  letter*       = c('a'..'z') | c('A'..'Z')   ## A parser that expects one ASCII alphabetical character.
  digit*        = c('0'..'9')                 ## A parser that expects one ASCII digit character.
  alphanumeric* = letter | digit              ## A parser that expects one ASCII alphanumeric character.
