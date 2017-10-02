import base64
import unittest, quickcheck, strutils, sequtils, future

suite "base":
  quicktest "encode" do(s: string(min = 0, max = 20), z: int):
    check(s.encode().decode() == s)

  quicktest "lower" do(s: string(alphabet=AAscii, trans=(a) => a.toLowerAscii())):
    check(s.toUpperAscii().toLowerAscii() == s)

  quicktest "boolean" do(b: bool):
    check((b and b) == (b and b))

  quicktest "mapIt" do (s: seq[int(min = 0, max = 20)]):
    check(s.mapIt(it * 822).mapIt(it div 822) == s)
