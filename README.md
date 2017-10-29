# nim-quickcheck

A quickcheck-related and hypothesis-inspired library extremely early phase.

```nim
quicktest "reverse" do(s: string(symbols={'a'..'z'}, min=0, max=20)):
  check(s.reverse().reverse() == s)
```


```nim
  quicktest "mapIt" do (s: seq[int(min = 0, max = 20)]):
    check(s.mapIt(b(it)).mapIt(c(it)) == s)
```

```nim
  quicktest "object" do (s: MyObject(a = string(alphabet=ALatin, max = 20))):
	check(s.name().split('_')[^1] == s.a)

```

I am trying to balance the power of types with extreme flexibility in tweaking 
generation for them, combining the haskell quickcheck-like heavy type approach with 
the way more generator-based hypothesis one.

Basically most of the generation should be automatically composed based on compile time
type introspection, but you should be able to tweak options for each element of the type

Oh, and it kinda works in the node js backend too
