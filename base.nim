import base64
import unittest, quicktest, strutils, sequtils, future

type
  MyObject* = ref object
    a*: string
    b*: int

proc name*(it: MyObject): string =
  "name_zz_$1" % it.a

proc `$`*(m: MyObject): string =
  $(m[])

suite "base":
  quicktest "encode" do(s: string(min = 0, max = 20), z: int):
    check(s.encode().decode() == s)

  quicktest "lower" do(s: string(alphabet=AAscii, trans=(a) => a.toLowerAscii())):
    check(s.toUpperAscii().toLowerAscii() == s)

  quicktest "boolean" do(b: bool):
    check((b and b) == (b and b))

  quicktest "mapIt" do (s: seq[int(min = 0, max = 20)]):
    check(s.mapIt(it * 822).mapIt(it div 822) == s)

  quicktest "object" do (a: string(alphabet=ALatin, max = 20)):
    check(a == a)

  quicktest "object" do (s: MyObject(a = string(alphabet=ALatin, max = 20))):
    check(s.name().split('_')[^1] == s.a)

