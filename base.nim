import base64
import unittest, quickcheck, strutils, sequtils, future

type
  A* = ref object
    b*: int
    c*: string

proc a*(it: A): int =
  it.b

suite "base":
  quicktest "encode" do(s: string(min = 0, max = 20), z: int):
    check(s.encode().decode() == s)

  quicktest "lower" do(s: string(alphabet=AAscii, trans=(a) => a.toLowerAscii())):
    check(s.toUpperAscii().toLowerAscii() == s)

  quicktest "boolean" do(b: bool):
    check((b and b) == (b and b))

  quicktest "mapIt" do (s: seq[int(min = 0, max = 20)]):
    check(s.mapIt(it * 822).mapIt(it div 822) == s)

  # quicktest "a" do (it: A(b: int(min = 0, max = 5))):
  #   check(a(it) == it.b)
  
  