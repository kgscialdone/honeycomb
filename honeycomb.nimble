# Package

version       = "0.1.0"
author        = "Katrina Scialdone"
description   = "A dead simple, no-nonsense parser combinator library written in pure Nim."
license       = "GPL-3.0-or-later"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.0"


# Tasks

import strutils
import sequtils

task test, "Run honeycomb test cases":
  exec "nim r tests/test.nim " & commandLineParams[3..^1].mapIt("\"$1\"" % it).join(" ")

task gendocs, "Generate documentation":
  exec "nimble doc2 -o:docs/ src/honeycomb.nim"
