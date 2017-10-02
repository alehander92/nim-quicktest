import base64
import unittest, quickcheck, strutils

suite "base":
  quicktest "encode" do(s: string(min = 0, max = 20), z: int, x: !M):
    check(decode(encode(s)) == s)
