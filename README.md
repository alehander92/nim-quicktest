# nim-quickcheck

A quickcheck-related and hypothesis-inspired library extremely early phase.

```nim
quicktest "reverse" do(s: string(symbols={'a'..'z'}, min=0, max=20)):
  check(s.reverse().reverse() == s)
```



