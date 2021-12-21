
import strutils
import re
import macros

from sugar import `=>`


# === Core Types ===

type
  Parser*[T] = proc(input: string): ParseResult[T] 
    ## A simple parser. Returns a `ParseResult` with the appropriate kind, depending on whether it successfully parsed.
    ##
    ## See also:
    ## - [createParserTo](#createParserTo.t,typedesc,untyped)
    ## - [createParser](#createParser.t,untyped)

  ParseResultKind* = enum success, failure
  ParseResult*[T] = object
    ## The result of a parser run.

    case kind*: ParseResultKind
    of success: value*: T         ## The value of the successful parse.
    of failure: error*: string    ## The error message of the failed parse.

    tail*: string                 ## The remaining unparsed input, or all input when failed.
    fromInput*: string            ## The input from which this result was generated.


# === Combinator Definition Utilities ===

template succeed*[T](inputIn: string, valueIn: T, tailIn: string): ParseResult[T] =
  ## Creates a successful `ParseResult`.
  ##
  ## See also:
  ## - [fail](#fail.t,string,string)
  ## - [createParserTo](#createParserTo.t,typedesc,untyped)
  ## - [createParser](#createParser.t,untyped)
  ParseResult[T](kind: success, value: valueIn, tail: tailIn, fromInput: inputIn)

template fail*[T](inputIn, errorIn: string): ParseResult[T] =
  ## Creates a failed `ParseResult`. Requires `T` to be explicitly specified, except inside of `createParserTo` in some cases.
  ##
  ## See also:
  ## - [succeed](#succeed.t,string,T,string)
  ## - [createParserTo](#createParserTo.t,typedesc,untyped)
  ## - [createParser](#createParser.t,untyped)
  ParseResult[T](kind: failure, error: errorIn, tail: inputIn, fromInput: inputIn)

template createParserTo*(T: typedesc, parser_body: untyped): Parser[T] =
  ## Convenience method for creating a custom `Parser`. Expects the parser's wrapped type as a parameter (not a generic!) and a block which will form the body of the parser.
  ##
  ## Inside the given block, `input` is the input string to be parsed. The block should evaluate to a `ParseResult` created by either `succeed` or `fail`, with the `tail` consisting of the remaining unparsed input.
  ##
  ## See also:
  ## - [succeed](#succeed.t,string,T,string)
  ## - [fail](#fail.t,string,string)
  ## - [createParser](#createParser.t,untyped)

  (proc(input {.inject.}: string): ParseResult[T] =
    let fail {.inject, used.} = (i,e: string) => fail[T](i,e)
    parser_body)

template createParser*(parser_body: untyped): Parser[string] =
  ## Same as [createParserTo](#createParserTo.t,typedesc,untyped), but always creates a `Parser[string]`.
  createParserTo(string, parser_body)

macro applyParser*(parser, input, failType: untyped) =
  ## Applies the given parser, evaluating to its result if successful or returning from the containing function if failed.
  ##
  ## This is primarily a tool for simplifying the creation of custom combinators, and should only be used if you know what you're doing. It is only designed to function properly in the context of a block passed to [createParserTo](#createParserTo.t,typedesc,untyped), and will raise a compile warning if used outside of this module.

  result = quote do:
    block:
      when instantiationInfo().filename != "honeycomb.nim":
        {.warning: "applyParser is designed for internal use, and should only be used if you know what you're doing.".}
      let temp = `parser`(`input`)
      if temp.kind == failure: return fail[`failType`](input, temp.error)
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
    fail(input, "Expected '$1'" % expect)

func c*(expect: char): Parser[char] = 
  ## Creates a parser matching exactly the given character.
  ##
  ## See also:
  ## - [s](#s,string)
  ## - [regex](#regex,string)
  createParserTo(char):
    if input.len > 0 and input[0] == expect: return succeed(input, expect, input[1..^1])
    fail(input, "Expected '$1'" % $expect)

func c*(expect: string): Parser[char] = 
  ## Creates a parser matching any one character from the given string.
  createParserTo(char):
    if input.len > 0 and input[0] in expect: return succeed(input, input[0], input[1..^1])
    fail(input, "Expected one of '$1'" % $expect)

func regex*(expect: string): Parser[string] = 
  ## Creates a parser matching the given regex. The regex must match from the start of the input.
  ##
  ## See also:
  ## - [s](#s,string)
  ## - [c](#c,char)
  let expreg = expect.re
  createParser:
    let bounds = input.findBounds(expreg)
    if bounds[0] != 0: return fail(input, "Expected '$1'" % $expect)
    succeed(input, input[0..bounds[1]], input[bounds[1]+1..^1])


# === Combinators ===

func `|`*[T](a, b: Parser[T]): Parser[T] = 
  ## Succeeds if either parser succeeds, attempting them from left to right.
  ## 
  ## See also:
  ## - [oneOf](#oneOf.t,Parser[T],Parser[T],varargs[Parser[T]])

  createParserTo(T):
    let result1 = a(input)
    case result1.kind:
      of success: return result1
      of failure: return b(input)

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

func `<<`*[T](a: Parser[T], b: Parser): Parser[T] =
  ## Expects each parser in sequence from left to right, ignoring the result of the right parser if successful.
  ##
  ## See also:
  ## - [>>](#>>,Parser[T],Parser)
  ## - [then](#then,Parser,Parser)

  createParserTo(T):
    let result1 = applyParser(a, input, T)
    let result2 = applyParser(b, result1.tail, T)
    return succeed(input, result1.value, result2.tail)

func `>>`*[T](a: Parser[T], b: Parser): Parser[T] =
  ## Expects each parser in sequence from left to right, ignoring the result of the left parser if successful.
  ##
  ## See also:
  ## - [<<](#<<,Parser[T],Parser)
  ## - [skip](#skip,Parser,Parser)

  createParserTo(T):
    let result1 = applyParser(a, input, T)
    let result2 = applyParser(b, result1.tail, T)
    return succeed(input, result2.value, result2.tail)

func `*`*[T](a: Parser[T], n: int): Parser[seq[T]] =
  ## Expects the parser a given number of times, returning a `seq` of the matches.

  createParserTo(seq[T]):
    var result1 = a.many()(input)
    if result1.value.len != n: 
      return fail[seq[T]](input, "Expected $1 value(s)" % $n)
    result1

func `*`*[T](a: Parser[T], n: HSlice[int,int]): Parser[seq[T]] =
  ## Expects the parser a number of times in the given range, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [times](#times.t,Parser,auto)
  ## - [many](#many,Parser[T])
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [atMost](#atMost.t,Parser[T],int)

  createParserTo(seq[T]):
    var result1 = a.many()(input)
    if result1.value.len notin n: 
      return fail[seq[T]](input, "Expected $1 to $2 value(s)" % [$n.a, $n.b])
    result1

func many*[T](a: Parser[T]): Parser[seq[T]] =
  ## Expects the parser 0 or more times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],HSlice[int,int])
  ## - [times](#times.t,Parser,auto)
  ## - [atLeast](#atLeast.t,Parser[T],int)
  ## - [atMost](#atMost.t,Parser[T],int)

  createParserTo(seq[T]):
    var 
      result1 = a(input)
      outputs: seq[T] = @[]
    while result1.kind == success:
      outputs.add(result1.value)
      result1 = a(result1.tail)
    succeed(input, outputs, result1.tail)

template atLeast*[T](a: Parser[T], n: int): Parser[seq[T]] = 
  ## Expects the parser `n` or more times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],HSlice[int,int])
  ## - [times](#times.t,Parser,auto)
  ## - [many](#many,Parser[T])
  ## - [atMost](#atMost.t,Parser[T],int)
  a.times(n..high(int))

template atMost*[T](a: Parser[T], n: int): Parser[seq[T]] = 
  ## Expects the parser `n` or fewer times, returning a `seq` of the matches.
  ##
  ## See also:
  ## - [*](#*,Parser[T],HSlice[int,int])
  ## - [times](#times.t,Parser,auto)
  ## - [many](#many,Parser[T])
  ## - [atLeast](#atLeast.t,Parser[T],int)
  a.times(0..n)


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
template times*(a: Parser, n: auto): auto = a * n   ## Textual alternative to [*](#*,Parser[T],HSlice[int,int]).

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



# === Converters ===

converter toStringParser*(a: Parser[char]): Parser[string] = 
  ## Implicitly converts `char` parsers to `string` parsers for ease of use.
  a.map(x => $x)


# === Predefined Parsers ===

let 
  eofImpl = createParser:   
    if input.len > 0: return fail(input, "Expected EOF")
    succeed(input, "", "")

  anyCharImpl = createParserTo(char):
    if input.len > 0: return succeed(input, input[0], input[1..^1])
    fail(input, "Expected any character, got EOF")

let 
  eof*          = eofImpl                                                     ## A parser that fails if there is any remaining input.
  anyChar*      = anyCharImpl                                                 ## A parser that succeeds for one character of any non-empty input.
  whitespace*   = regex(r"\s+")                                               ## A parser that expects at least one whitespace character.
  letter*       = c("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")   ## A parser that expects one ASCII alphabetical character.
  digit*        = c("0123456789")                                             ## A parser that expects one ASCII digit character.
  alphanumeric* = letter | digit                                              ## A parser that expects one ASCII alphanumeric character.
