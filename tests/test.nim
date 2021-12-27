
import unittest
import honeycomb
import strutils
import sequtils
import math

from sugar import `=>`

suite "core parsers":
  
  test "literal string":
    let 
      parser  = s("Hello")
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == "Hello"
    check result1.tail      == ", world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected 'Hello'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

  test "single character":
    let 
      parser  = c('H')
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected 'H'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

  test "character of string":
    let 
      parser  = c("HG")
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")
      result3 = parser.parse("Ahoy there!")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == success
    check result2.value     == 'G'
    check result2.tail      == "reetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

    check result3.kind      == failure
    check result3.error     == "[1:1] Expected character from 'HG'"
    check result3.tail      == "Ahoy there!"
    check result3.fromInput == "Ahoy there!"

  test "character of range":
    let 
      parser  = c('G'..'H')
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")
      result3 = parser.parse("Ahoy there!")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == success
    check result2.value     == 'G'
    check result2.tail      == "reetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

    check result3.kind      == failure
    check result3.error     == "[1:1] Expected character from G..H"
    check result3.tail      == "Ahoy there!"
    check result3.fromInput == "Ahoy there!"

  test "regex":
    let 
      parser  = regex(r"H\w+")
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == "Hello"
    check result1.tail      == ", world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == r"[1:1] Expected match for regex 'H\w+'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"
  
  test "no-op":
    let 
      parser  = nop[string]()
      result1 = parser.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == ""
    check result1.tail      == "Hello, world!"
    check result1.fromInput == "Hello, world!"

  test "raiseIfFailed":
    let
      parser  = s("Hello")
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")

    result1.raiseIfFailed
    expect ParseError: result2.raiseIfFailed
  
  test "forward declarations":
    var parser1 = fwdcl[string]()
    let 
      parser2 = (s("Hello, ") & parser1 & c('!'))
      result1 = parser2.parse("Hello, world!")

    parser1.become(s("world"))

    let
      result2 = parser2.parse("Hello, world!")
      result3 = parser2.parse("Hello, peasants!")

    check result1.kind      == failure
    check result1.error     == "[1:8] Expected forward-declared parser to be initialized with .become(Parser)"
    check result1.tail      == "world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == success
    check result2.value     == @["Hello, ", "world", "!"]
    check result2.tail      == ""
    check result2.fromInput == "Hello, world!"

    check result3.kind      == failure
    check result3.error     == "[1:8] Expected 'world'"
    check result3.tail      == "peasants!"
    check result3.fromInput == "Hello, peasants!"

suite "predefined parsers":
  
  test "eof":
    let
      result1 = eof.parse("")
      result2 = eof.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == ""
    check result1.tail      == ""
    check result1.fromInput == ""

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected EOF"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"
  
  test "anyChar":
    let
      result1 = anyChar.parse("Hello, world!")
      result2 = anyChar.parse("")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected any character"
    check result2.tail      == ""
    check result2.fromInput == ""
  
  test "whitespace":
    let
      result1 = whitespace.parse("  \t \n  \r  Hello, world!")
      result2 = whitespace.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == "  \t \n  \r  "
    check result1.tail      == "Hello, world!"
    check result1.fromInput == "  \t \n  \r  Hello, world!"

    check result2.kind      == failure
    check result2.error     == r"[1:1] Expected match for regex '\s+'"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"
  
  test "letter":
    let
      result1 = letter.parse("Hello, world!")
      result2 = letter.parse("127")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected one of character from a..z, character from A..Z"
    check result2.tail      == "127"
    check result2.fromInput == "127"
  
  test "digit":
    let
      result1 = digit.parse("127")
      result2 = digit.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == '1'
    check result1.tail      == "27"
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected character from 0..9"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"
  
  test "alphanumeric":
    let
      result1 = alphanumeric.parse("Hello, world!")
      result2 = alphanumeric.parse("127")
      result3 = alphanumeric.parse("!@%$!^#")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == success
    check result2.value     == '1'
    check result2.tail      == "27"
    check result2.fromInput == "127"

    check result3.kind      == failure
    check result3.error     == "[1:1] Expected one of character from a..z, character from A..Z, character from 0..9"
    check result3.tail      == "!@%$!^#"
    check result3.fromInput == "!@%$!^#"

suite "general combinators":

  test "map":
    let
      parser  = digit.atLeast(1).join.map(parseInt)
      result1 = parser.parse("127")
      result2 = parser.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == 127
    check result1.tail      == ""
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected character from 0..9"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"

  test "mapEach":
    let
      parser  = digit.asString.atLeast(1).mapEach(parseInt)
      result1 = parser.parse("127")
      result2 = parser.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == @[1,2,7]
    check result1.tail      == ""
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected character from 0..9"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"

  test "result":
    let
      parser  = digit.atLeast(1).result("Successfully parsed number")
      result1 = parser.parse("127")
      result2 = parser.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == "Successfully parsed number"
    check result1.tail      == ""
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected character from 0..9"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"

  test "oneOf":
    let parsers = [
      s("Hello") | s("Greetings"),
      oneOf(s("Hello"), s("Greetings"))
    ]

    for parser in parsers:
      let 
        result1 = parser.parse("Hello, world!")
        result2 = parser.parse("Greetings, peasants!")
        result3 = parser.parse("Ahoy there!")

      check result1.kind      == success
      check result1.value     == "Hello"
      check result1.tail      == ", world!"
      check result1.fromInput == "Hello, world!"

      check result2.kind      == success
      check result2.value     == "Greetings"
      check result2.tail      == ", peasants!"
      check result2.fromInput == "Greetings, peasants!"

      check result3.kind      == failure
      check result3.error     == "[1:1] Expected one of 'Hello', 'Greetings'"
      check result3.tail      == "Ahoy there!"
      check result3.fromInput == "Ahoy there!"
  
  test "chain":
    let parsers = [
      s("Hello") & c(',') & whitespace & letter.atLeast(1).join & c('!'),
      chain(s("Hello"), c(','), whitespace, letter.atLeast(1).join, c('!'))
    ]

    for parser in parsers:
      let 
        result1 = parser.parse("Hello, world!")
        result2 = parser.parse("Greetings, peasants!")
        result3 = parser.parse("Hello,world!")

      check result1.kind      == success
      check result1.value     == @["Hello", ",", " ", "world", "!"]
      check result1.tail      == ""
      check result1.fromInput == "Hello, world!"

      check result2.kind      == failure
      check result2.error     == "[1:1] Expected 'Hello'"
      check result2.tail      == "Greetings, peasants!"
      check result2.fromInput == "Greetings, peasants!"

      check result3.kind      == failure
      check result3.error     == r"[1:7] Expected match for regex '\s+'"
      check result3.tail      == "world!"
      check result3.fromInput == "Hello,world!"

  test "then":
    let parsers = [
      s("Hello") >> c(',').asString >> whitespace >> letter.atLeast(1).join,
      s("Hello").then(c(',')).then(whitespace).then(letter.atLeast(1).join)
    ]

    for parser in parsers:
      let 
        result1 = parser.parse("Hello, world!")
        result2 = parser.parse("Greetings, peasants!")

      check result1.kind      == success
      check result1.value     == "world"
      check result1.tail      == "!"
      check result1.fromInput == "Hello, world!"

      check result2.kind      == failure
      check result2.error     == "[1:1] Expected 'Hello'"
      check result2.tail      == "Greetings, peasants!"
      check result2.fromInput == "Greetings, peasants!"

  test "skip":
    let parsers = [
      s("Hello") << c(',').asString << whitespace << letter.atLeast(1).join,
      s("Hello").skip(c(',')).skip(whitespace).skip(letter.atLeast(1).join)
    ]

    for parser in parsers:
      let 
        result1 = parser.parse("Hello, world!")
        result2 = parser.parse("Greetings, peasants!")

      check result1.kind      == success
      check result1.value     == "Hello"
      check result1.tail      == "!"
      check result1.fromInput == "Hello, world!"

      check result2.kind      == failure
      check result2.error     == "[1:1] Expected 'Hello'"
      check result2.tail      == "Greetings, peasants!"
      check result2.fromInput == "Greetings, peasants!"

  test "times":
    let parsers = [
      s("Hello ") * 3,
      s("Hello ").times(3)
    ]

    for parser in parsers:
      let 
        result1 = parser.parse("Hello Hello Hello ")
        result2 = parser.parse("Hello Hello ")

      check result1.kind      == success
      check result1.value     == @["Hello ","Hello ","Hello "]
      check result1.tail      == ""
      check result1.fromInput == "Hello Hello Hello "

      check result2.kind      == failure
      check result2.error     == "[1:13] Expected 'Hello '"
      check result2.tail      == ""
      check result2.fromInput == "Hello Hello "

  test "times range":
    let parsers = [
      s("Hello ") * (3..4) << eof,
      s("Hello ").times(3..4) << eof
    ]

    for parser in parsers:
      let 
        result1 = parser.parse("Hello Hello Hello ")
        result2 = parser.parse("Hello Hello Hello Hello ")
        result3 = parser.parse("Hello Hello ")
        result4 = parser.parse("Hello Hello Hello Hello Hello ")

      check result1.kind      == success
      check result1.value     == @["Hello ","Hello ","Hello "]
      check result1.tail      == ""
      check result1.fromInput == "Hello Hello Hello "

      check result2.kind      == success
      check result2.value     == @["Hello ","Hello ","Hello ","Hello "]
      check result2.tail      == ""
      check result2.fromInput == "Hello Hello Hello Hello "

      check result3.kind      == failure
      check result3.error     == "[1:13] Expected 'Hello '"
      check result3.tail      == ""
      check result3.fromInput == "Hello Hello "

      check result4.kind      == failure
      check result4.error     == "[1:25] Expected EOF"
      check result4.tail      == "Hello "
      check result4.fromInput == "Hello Hello Hello Hello Hello "

  test "negla":
    let parsers = [
      !eof,
      eof.negla
    ]

    for parser in parsers:
      let 
        result1 = parser.parse("Hello, world!")
        result2 = parser.parse("")

      check result1.kind      == success
      check result1.value     == ""
      check result1.tail      == "Hello, world!"
      check result1.fromInput == "Hello, world!"

      check result2.kind      == failure
      check result2.error     == "[1:1] Expected successful negative lookahead"
      check result2.tail      == ""
      check result2.fromInput == ""

  test "atLeast":
    let 
      parser  = s("Hello ").atLeast(3)
      result1 = parser.parse("Hello Hello Hello ")
      result2 = parser.parse("Hello Hello Hello Hello ")
      result3 = parser.parse("Hello Hello ")

    check result1.kind      == success
    check result1.value     == @["Hello ","Hello ","Hello "]
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == success
    check result2.value     == @["Hello ","Hello ","Hello ","Hello "]
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello Hello Hello "

    check result3.kind      == failure
    check result3.error     == "[1:13] Expected 'Hello '"
    check result3.tail      == ""
    check result3.fromInput == "Hello Hello "

  test "atMost":
    let 
      parser  = s("Hello ").atMost(3) << eof
      result1 = parser.parse("Hello Hello Hello ")
      result2 = parser.parse("Hello Hello ")
      result3 = parser.parse("Hello Hello Hello Hello ")

    check result1.kind      == success
    check result1.value     == @["Hello ","Hello ","Hello "]
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == success
    check result2.value     == @["Hello ","Hello "]
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello "

    check result3.kind      == failure
    check result3.error     == "[1:19] Expected EOF"
    check result3.tail      == "Hello "
    check result3.fromInput == "Hello Hello Hello Hello "

  test "many":
    let 
      parser  = s("Hello ").many()
      result1 = parser.parse("Hello Hello Hello ")
      result2 = parser.parse("Hello Hello ")
      result3 = parser.parse("")

    check result1.kind      == success
    check result1.value     == @["Hello ","Hello ","Hello "]
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == success
    check result2.value     == @["Hello ","Hello "]
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello "

    check result3.kind      == success
    check result3.value     == newSeq[string]()
    check result3.tail      == ""
    check result3.fromInput == ""

  test "optional":
    let 
      parser  = s("Hello").optional()
      result1 = parser.parse("Hello")
      result2 = parser.parse("")

    check result1.kind      == success
    check result1.value     == @["Hello"]
    check result1.tail      == ""
    check result1.fromInput == "Hello"

    check result2.kind      == success
    check result2.value     == newSeq[string]()
    check result2.tail      == ""
    check result2.fromInput == ""

  test "flatten":
    let
      parser  = (digit & digit & digit).atLeast(1).flatten().join
      result1 = parser.parse("127")
      result2 = parser.parse("Hello, world!")

    check result1.kind      == success
    check result1.value     == "127"
    check result1.tail      == ""
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected character from 0..9"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"

  test "join":
    let
      parser  = s("Hello ").times(3).join
      result1 = parser.parse("Hello Hello Hello ")
      result2 = parser.parse("Hello Hello ")

    check result1.kind      == success
    check result1.value     == "Hello Hello Hello "
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == failure
    check result2.error     == "[1:13] Expected 'Hello '"
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello "

  test "join delimited":
    let
      parser1 = (s("Hello") << whitespace).times(3).join(", ")
      parser2 = (s("Hello") << whitespace).times(3).join(',')
      result1 = parser1.parse("Hello Hello Hello ")
      result2 = parser2.parse("Hello Hello Hello ")
      result3 = parser1.parse("Hello Hello ")

    check result1.kind      == success
    check result1.value     == "Hello, Hello, Hello"
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == success
    check result2.value     == "Hello,Hello,Hello"
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello Hello "

    check result3.kind      == failure
    check result3.error     == "[1:13] Expected 'Hello'"
    check result3.tail      == ""
    check result3.fromInput == "Hello Hello "

suite "custom parsers":
  
  test "custom parser":
    let parser = createParser(string):
      if input.len < 10: return fail(input, @["at least 10 characters"], input)
      return succeed(input, input[0..9], input[10..^1])

    let 
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Alfalfa")

    check result1.kind      == success
    check result1.value     == "Hello, wor"
    check result1.tail      == "ld!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected at least 10 characters"
    check result2.tail      == "Alfalfa"
    check result2.fromInput == "Alfalfa"

  test "custom combinator":
    proc both[T](a,b: Parser[T]): Parser[(T,T)] = createParser((T,T)):
      {.push warnings: off.} # Hide the warnings from applyParser, since I know what I'm doing
      let 
        result1 = applyParser(a, input, (T,T))
        result2 = applyParser(b, input, (T,T))
        resLen  = max(result1.fromInput.len-result1.tail.len, result2.fromInput.len-result2.tail.len)
      {.pop.} 
      return succeed(input, (result1.value, result2.value), input[resLen..^1])

    let 
      parser  = both(c('H').asString, s("Hello"))
      result1 = parser.parse("Hello, world!")
      result2 = parser.parse("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == ("H", "Hello")
    check result1.tail      == ", world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "[1:1] Expected 'H'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

suite "integration examples":

  test "receipt parser":
    let 
      receiptEntry  = (regex(r"[\w\s]+") << s(": ")) & (digit.atLeast(1) << c('.')).join & digit.times(2).join << c('\n').optional()
      receiptParser = receiptEntry.map(x => (x[0], parseFloat("$1.$2" % x[1..^1]))).atLeast(1)
      testReceipt   = "Milk: 4.00\nEggs: 15.99\nCool robot: 69.99"
      result1       = receiptParser.parse(testReceipt)

    check result1.kind      == success
    check result1.value     == @[("Milk", 4.00), ("Eggs", 15.99), ("Cool robot", 69.99)]
    check result1.tail      == ""
    check result1.fromInput == testReceipt

  test "arithmetic expression evaluator":
    var expression = fwdcl[float]()
    let
      padding = regex(r"\s*")
      number  = (digit.atLeast(1) & (c('.') & digit.atLeast(1)).optional).flatten.join.map(parseFloat)
      parens  = c('(') >> padding >> expression << padding << c(')')
      operand = number | parens

    func processOp(input: seq[string]): float =
      if input.len < 3: return input[0].parseFloat
      case input[1]:
      of "+": return input[0].parseFloat + processOp(input[2..^1])
      of "-": return input[0].parseFloat - processOp(input[2..^1])
      of "*": return input[0].parseFloat * processOp(input[2..^1])
      of "/": return input[0].parseFloat / processOp(input[2..^1])
      of "%": return input[0].parseFloat.mod(processOp(input[2..^1]))
      of "^": return input[0].parseFloat.pow(processOp(input[2..^1]))

    var prevt: Parser[float] = operand
    template defineBinOp(parseOp: Parser[string]) =
      let right = ((padding >> parseOp << padding) & prevt.asString).many().flatten
      prevt = (prevt.asString & right).map(processOp)

    defineBinOp c("^")
    defineBinOp c("*/%")
    defineBinOp c("+-")

    expression.become(padding >> prevt << padding)

    let
      parser  = expression << eof.desc("valid expression")
      result1 = parser.parse(" 1 + (2) * (3 - 4) / 5 ^ 6 ")
      result2 = parser.parse("1 + ")

    check result1.kind      == success
    check result1.value     == 0.999872
    check result1.tail      == ""
    check result1.fromInput == " 1 + (2) * (3 - 4) / 5 ^ 6 "

    check result2.kind      == failure
    check result2.error     == "[1:3] Expected valid expression"
    check result2.tail      == "+ "
    check result2.fromInput == "1 + "
