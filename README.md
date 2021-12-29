# 🍯 Honeycomb

[![tests](https://github.com/KatrinaKitten/honeycomb/actions/workflows/test.yml/badge.svg)](https://github.com/KatrinaKitten/honeycomb/actions/workflows/test.yml)
[![docs](https://github.com/KatrinaKitten/honeycomb/actions/workflows/gendocs.yml/badge.svg)](https://github.com/KatrinaKitten/honeycomb/actions/workflows/gendocs.yml)

Honeycomb is a parser combinator library written in pure Nim. It's designed to be simple, straightforward, and easy to expand, while relying on zero dependencies from outside of Nim's standard library.

Honeycomb was heavily inspired by the excellent Python library [parsy](https://github.com/python-parsy/parsy), as well as the existing but unmaintained [combparser](https://github.com/PMunch/combparser).

```nim
let 
  parser  = ((s("Hello") | s("Greetings")) << c(',') << whitespace) & (regex(r"\w+") << c("!."))
  result1 = parser.parse("Hello, world!")
  result2 = parser.parse("Greetings, peasants.")

assert result1.kind  == success
assert result1.value == @["Hello", "world"]

assert result2.kind  == success
assert result2.value == @["Greetings", "peasants"]
```

Honeycomb supports the following key features:

- Predefined parsers and parser constructors for numerous basic parsing needs
- An extensive library of combinators with which to combine them
- Support for manually defining custom parsers / combinators
- Forward-declared parsers to support mutually recursive parser definitions

## Installation

You can install Honeycomb via Nim's package manager, `nimble`.

```bash
nimble install honeycomb
```

Once you've installed Honeycomb, you can use it in your project by importing it.

```nim
import honeycomb
```

## Usage

You can find extensive documentation on using Honeycomb [here](https://katrinakitten.github.io/honeycomb).

For a more in-depth conceptual look at parser combinators in general, you can try these resources:

- [Antoine Leblanc: Parser Combinators Walkthrough](https://hasura.io/blog/parser-combinators-walkthrough/) (article, Haskell)<br>Gives a very thorough and in-depth explanation of the basics of parser combinators.
- [Stephen Gutekanst: Zig, Parser Combinators - and Why They're Awesome](https://serokell.io/blog/parser-combinators-in-elixir) (article, Zig)<br>A more detailed look at implementing some basic parser combinators.
- [Graham Hutton et al.: Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) (whitepaper, Haskell)<br>An analysis of some of the type theory behind parser combinators.
- [Scott Wlaschin: Understanding Parser Combinators](https://fsharpforfunandprofit.com/series/understanding-parser-combinators/) (article series, F#)<br>A multipart series starting from the basics and ending with the implementation of a full JSON parser.
- [Yassine Elouafi: Introduction to Parser Combinators](https://gist.github.com/yelouafi/556e5159e869952335e01f6b473c4ec1) (article, Javascript)<br>A straightforward look at implementing simple parser combinators.
- [Computerphile: Functional Parsing](https://www.youtube.com/watch?v=dDtZLm7HIJs) (video, Haskell)<br>A high-level overview of the basics of parser combinators in video format.
- [Li Haoyi: Easy Parsing with Parser Combinators](https://www.lihaoyi.com/post/EasyParsingwithParserCombinators.html) (article, Scala)<br>A detailed explanation of how to use parser combinators in some more complex contexts.

## Contributing

1. Fork this repository.
2. Clone the fork to your local machine.
```bash
git clone https://github.com/<your-github-username>/honeycomb.git
cd honeycomb
```
3. Make your changes.
4. Make sure to add or update documentation comments to reflect your changes.
5. Make sure to add or update tests in [`tests/test.nim`](./tests/test.nim) to verify your changes.
6. Create a pull request with your changes.

Honeycomb has an extensive and expanding suite of unit tests, which can be found in [`tests/test.nim`](./tests/test.nim). You can run the tests with:
```bash
nimble test
```

To generate a local copy of the documentation from Honeycomb's code, you can use the following command. Note that the `docs` folder is intentionally `.gitignore`d and should not be committed; when your changes are merged into the `master` branch, an automated process will regenerate the documentation on the `docs` branch.
```bash
nimble gendocs
```
