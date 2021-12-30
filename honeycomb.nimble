# Package

version       = "0.1.1"
author        = "Katrina Scialdone"
description   = "A dead simple, no-nonsense parser combinator library written in pure Nim."
license       = "MPL-2.0"


# Dependencies

requires "nim >= 1.6.0"


# Tasks

import strutils
import sequtils

task test, "Run honeycomb test cases":
  exec "nim r tests/test.nim " & commandLineParams[3..^1].mapIt("\"$1\"" % it).join(" ")

task gendocs, "Generate documentation":
  var commit = commandLineParams[3..^1].mapIt("\"$1\"" % it).join(" ")
  if commit == "": commit = "master"
  exec "nimble doc2 --git.url:https://github.com/katrinakitten/honeycomb --git.commit:$1 --index:on -o:docs/ --project honeycomb.nim" % commit
