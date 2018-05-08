import base64
import unittest, quicktest, strutils, sequtils, future, sets

when defined(js):
  import js_lib

type
  MyObject* = ref object
    a*: string
    b*: int

proc name*(it: MyObject): string =
  "name_zz_$1" % it.a

proc `$`*(m: MyObject): string =
  $(m[])

suite "base":
  quicktest "encode", 2 do(s: string(range=0..20), z: int):
    check(s.encode().decode() == s)
 
  quicktest "lower" do(s: string(alphabet=AAscii, trans=(a) => a.toLowerAscii())):
    check(s.toUpperAscii().toLowerAscii() == s)

  # seq
  # quicktest "mapIt" do (s: seq[Int(min = 0, max = 20)]):
  #   check(s.mapIt(it * 822).mapIt(it div 822) == s)

  # quicktest "object" do (a: string(alphabet=ALatin, max = 20)):
  #   check(a == a)

  # quicktest "object" do (s: MyObject(a = string(alphabet=ALatin, max = 20))):
  #   check(s.name().split('_')[^1] == s.a)

  quicktest "int", 100 do (a: int(range=0..200)):
    check a + a == 2 * a
    if a > 50:
      check a == 10

  # quicktest "uint", 100_000 do (a: uint(range=0..20), b: uint(range=0..20)):
  #   check a * b == b * a

  # quicktest "int32", 200 do (a: uint64(range=0..20), b: uint64(range=0..20)):
  #   check a * b == b * a
    
  quicktest "example", 20 do (x0, x1, x2, x3, y0, y1, y2, y3: uint):
    check x0 in [x0]
