
import unittest
import honeycomb
import strutils

suite "core parsers":
  
  test "literal string":
    let 
      parser  = s("Hello")
      result1 = parser("Hello, world!")
      result2 = parser("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == "Hello"
    check result1.tail      == ", world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "Expected 'Hello'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

  test "single character":
    let 
      parser  = c('H')
      result1 = parser("Hello, world!")
      result2 = parser("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "Expected 'H'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

  test "character of string":
    let 
      parser  = c("HG")
      result1 = parser("Hello, world!")
      result2 = parser("Greetings, peasants!")
      result3 = parser("Ahoy there!")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == success
    check result2.value     == 'G'
    check result2.tail      == "reetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

    check result3.kind      == failure
    check result3.error     == "Expected one of 'HG'"
    check result3.tail      == "Ahoy there!"
    check result3.fromInput == "Ahoy there!"

  test "regex":
    let 
      parser  = regex(r"H\w+")
      result1 = parser("Hello, world!")
      result2 = parser("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == "Hello"
    check result1.tail      == ", world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == r"Expected 'H\w+'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"

suite "predefined parsers":
  
  test "eof":
    let
      result1 = eof("")
      result2 = eof("Hello, world!")

    check result1.kind      == success
    check result1.value     == ""
    check result1.tail      == ""
    check result1.fromInput == ""

    check result2.kind      == failure
    check result2.error     == "Expected EOF"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"
  
  test "anyChar":
    let
      result1 = anyChar("Hello, world!")
      result2 = anyChar("")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "Expected any character, got EOF"
    check result2.tail      == ""
    check result2.fromInput == ""
  
  test "whitespace":
    let
      result1 = whitespace("  \t \n  \r  Hello, world!")
      result2 = whitespace("Hello, world!")

    check result1.kind      == success
    check result1.value     == "  \t \n  \r  "
    check result1.tail      == "Hello, world!"
    check result1.fromInput == "  \t \n  \r  Hello, world!"

    check result2.kind      == failure
    check result2.error     == r"Expected '\s+'"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"
  
  test "letter":
    let
      result1 = letter("Hello, world!")
      result2 = letter("127")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'"
    check result2.tail      == "127"
    check result2.fromInput == "127"
  
  test "digit":
    let
      result1 = digit("127")
      result2 = digit("Hello, world!")

    check result1.kind      == success
    check result1.value     == '1'
    check result1.tail      == "27"
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "Expected one of '0123456789'"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"
  
  test "alphanumeric":
    let
      result1 = alphanumeric("Hello, world!")
      result2 = alphanumeric("127")
      result3 = alphanumeric("!@%$!^#")

    check result1.kind      == success
    check result1.value     == 'H'
    check result1.tail      == "ello, world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == success
    check result2.value     == '1'
    check result2.tail      == "27"
    check result2.fromInput == "127"

    check result3.kind      == failure
    check result3.error     == "Expected one of '0123456789'"
    check result3.tail      == "!@%$!^#"
    check result3.fromInput == "!@%$!^#"

suite "general combinators":
  
  test "chain":
    let parsers = [
      s("Hello") & c(',') & whitespace & letter.atLeast(1).join & c('!'),
      chain(s("Hello"), c(','), whitespace, letter.atLeast(1).join, c('!'))
    ]

    for parser in parsers:
      let 
        result1 = parser("Hello, world!")
        result2 = parser("Greetings, peasants!")

      check result1.kind      == success
      check result1.value     == @["Hello", ",", " ", "world", "!"]
      check result1.tail      == ""
      check result1.fromInput == "Hello, world!"

      check result2.kind      == failure
      check result2.error     == "Expected 'Hello'"
      check result2.tail      == "Greetings, peasants!"
      check result2.fromInput == "Greetings, peasants!"

  test "oneOf":
    let parsers = [
      s("Hello") | s("Greetings"),
      oneOf(s("Hello"), s("Greetings"))
    ]

    for parser in parsers:
      let 
        result1 = parser("Hello, world!")
        result2 = parser("Greetings, peasants!")
        result3 = parser("Ahoy there!")

      check result1.kind      == success
      check result1.value     == "Hello"
      check result1.tail      == ", world!"
      check result1.fromInput == "Hello, world!"

      check result2.kind      == success
      check result2.value     == "Greetings"
      check result2.tail      == ", peasants!"
      check result2.fromInput == "Greetings, peasants!"

      check result3.kind      == failure
      check result3.error     == "Expected 'Greetings'"
      check result3.tail      == "Ahoy there!"
      check result3.fromInput == "Ahoy there!"

  test "then":
    let parsers = [
      s("Hello") >> c(',').toStringParser >> whitespace >> letter.atLeast(1).join,
      s("Hello").then(c(',')).then(whitespace).then(letter.atLeast(1).join)
    ]

    for parser in parsers:
      let 
        result1 = parser("Hello, world!")
        result2 = parser("Greetings, peasants!")

      check result1.kind      == success
      check result1.value     == "world"
      check result1.tail      == "!"
      check result1.fromInput == "Hello, world!"

      check result2.kind      == failure
      check result2.error     == "Expected 'Hello'"
      check result2.tail      == "Greetings, peasants!"
      check result2.fromInput == "Greetings, peasants!"

  test "skip":
    let parsers = [
      s("Hello") << c(',').toStringParser << whitespace << letter.atLeast(1).join,
      s("Hello").skip(c(',')).skip(whitespace).skip(letter.atLeast(1).join)
    ]

    for parser in parsers:
      let 
        result1 = parser("Hello, world!")
        result2 = parser("Greetings, peasants!")

      check result1.kind      == success
      check result1.value     == "Hello"
      check result1.tail      == "!"
      check result1.fromInput == "Hello, world!"

      check result2.kind      == failure
      check result2.error     == "Expected 'Hello'"
      check result2.tail      == "Greetings, peasants!"
      check result2.fromInput == "Greetings, peasants!"

  test "many":
    let 
      parser  = s("Hello ").many()
      result1 = parser("Hello Hello Hello ")
      result2 = parser("Hello Hello ")
      result3 = parser("")

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

  test "times":
    let parsers = [
      s("Hello ") * 3,
      s("Hello ").times(3)
    ]

    for parser in parsers:
      let 
        result1 = parser("Hello Hello Hello ")
        result2 = parser("Hello Hello ")

      check result1.kind      == success
      check result1.value     == @["Hello ","Hello ","Hello "]
      check result1.tail      == ""
      check result1.fromInput == "Hello Hello Hello "

      check result2.kind      == failure
      check result2.error     == "Expected 3 value(s)"
      check result2.tail      == "Hello Hello "
      check result2.fromInput == "Hello Hello "

  test "times range":
    let parsers = [
      s("Hello ") * (3..4),
      s("Hello ").times(3..4)
    ]

    for parser in parsers:
      let 
        result1 = parser("Hello Hello Hello ")
        result2 = parser("Hello Hello Hello Hello ")
        result3 = parser("Hello Hello ")
        result4 = parser("Hello Hello Hello Hello Hello ")

      check result1.kind      == success
      check result1.value     == @["Hello ","Hello ","Hello "]
      check result1.tail      == ""
      check result1.fromInput == "Hello Hello Hello "

      check result2.kind      == success
      check result2.value     == @["Hello ","Hello ","Hello ","Hello "]
      check result2.tail      == ""
      check result2.fromInput == "Hello Hello Hello Hello "

      check result3.kind      == failure
      check result3.error     == "Expected 3 to 4 value(s)"
      check result3.tail      == "Hello Hello "
      check result3.fromInput == "Hello Hello "

      check result4.kind      == failure
      check result4.error     == "Expected 3 to 4 value(s)"
      check result4.tail      == "Hello Hello Hello Hello Hello "
      check result4.fromInput == "Hello Hello Hello Hello Hello "

  test "atLeast":
    let 
      parser  = s("Hello ").atLeast(3)
      result1 = parser("Hello Hello Hello ")
      result2 = parser("Hello Hello Hello Hello ")
      result3 = parser("Hello Hello ")

    check result1.kind      == success
    check result1.value     == @["Hello ","Hello ","Hello "]
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == success
    check result2.value     == @["Hello ","Hello ","Hello ","Hello "]
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello Hello Hello "

    check result3.kind      == failure
    check result3.error     == "Expected 3 to 9223372036854775807 value(s)"
    check result3.tail      == "Hello Hello "
    check result3.fromInput == "Hello Hello "

  test "atMost":
    let 
      parser  = s("Hello ").atMost(3)
      result1 = parser("Hello Hello Hello ")
      result2 = parser("Hello Hello ")
      result3 = parser("Hello Hello Hello Hello ")

    check result1.kind      == success
    check result1.value     == @["Hello ","Hello ","Hello "]
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == success
    check result2.value     == @["Hello ","Hello "]
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello "

    check result3.kind      == failure
    check result3.error     == "Expected 0 to 3 value(s)"
    check result3.tail      == "Hello Hello Hello Hello "
    check result3.fromInput == "Hello Hello Hello Hello "

  test "map":
    let
      parser  = digit.atLeast(1).join.map(parseInt)
      result1 = parser("127")
      result2 = parser("Hello, world!")

    check result1.kind      == success
    check result1.value     == 127
    check result1.tail      == ""
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "Expected 1 to 9223372036854775807 value(s)"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"

  test "result":
    let
      parser  = digit.atLeast(1).result("Successfully parsed number")
      result1 = parser("127")
      result2 = parser("Hello, world!")

    check result1.kind      == success
    check result1.value     == "Successfully parsed number"
    check result1.tail      == ""
    check result1.fromInput == "127"

    check result2.kind      == failure
    check result2.error     == "Expected 1 to 9223372036854775807 value(s)"
    check result2.tail      == "Hello, world!"
    check result2.fromInput == "Hello, world!"

suite "type-specific combinators":

  test "seq[string]: join":
    let
      parser  = s("Hello ").times(3).join
      result1 = parser("Hello Hello Hello ")
      result2 = parser("Hello Hello ")

    check result1.kind      == success
    check result1.value     == "Hello Hello Hello "
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == failure
    check result2.error     == "Expected 3 value(s)"
    check result2.tail      == "Hello Hello "
    check result2.fromInput == "Hello Hello "

  test "seq[string]: join delimited":
    let
      parser1 = (s("Hello") << whitespace).times(3).join(", ")
      parser2 = (s("Hello") << whitespace).times(3).join(',')
      result1 = parser1("Hello Hello Hello ")
      result2 = parser2("Hello Hello Hello ")
      result3 = parser1("Hello Hello ")

    check result1.kind      == success
    check result1.value     == "Hello, Hello, Hello"
    check result1.tail      == ""
    check result1.fromInput == "Hello Hello Hello "

    check result2.kind      == success
    check result2.value     == "Hello,Hello,Hello"
    check result2.tail      == ""
    check result2.fromInput == "Hello Hello Hello "

    check result3.kind      == failure
    check result3.error     == "Expected 3 value(s)"
    check result3.tail      == "Hello Hello "
    check result3.fromInput == "Hello Hello "

suite "custom parsers":
  
  test "custom parser":
    let parser = createParser:
      if input.len < 10: return fail(input, "Expected at least 10 characters")
      return succeed(input, input[0..9], input[10..^1])

    let 
      result1 = parser("Hello, world!")
      result2 = parser("Alfalfa")

    check result1.kind      == success
    check result1.value     == "Hello, wor"
    check result1.tail      == "ld!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "Expected at least 10 characters"
    check result2.tail      == "Alfalfa"
    check result2.fromInput == "Alfalfa"

  test "custom combinator":
    proc both[T](a,b: Parser[T]): Parser[(T,T)] = createParserTo((T,T)):
      {.push warnings: off.} # Hide the warnings from applyParser, since I know what I'm doing
      let 
        result1 = applyParser(a, input, (T,T))
        result2 = applyParser(b, input, (T,T))
        resLen  = max(result1.fromInput.len-result1.tail.len, result2.fromInput.len-result2.tail.len)
      {.pop.} 
      return succeed(input, (result1.value, result2.value), input[resLen..^1])

    let 
      parser  = both(c('H').toStringParser, s("Hello"))
      result1 = parser("Hello, world!")
      result2 = parser("Greetings, peasants!")

    check result1.kind      == success
    check result1.value     == ("H", "Hello")
    check result1.tail      == ", world!"
    check result1.fromInput == "Hello, world!"

    check result2.kind      == failure
    check result2.error     == "Expected 'H'"
    check result2.tail      == "Greetings, peasants!"
    check result2.fromInput == "Greetings, peasants!"
