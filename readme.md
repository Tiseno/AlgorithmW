# THM
Down the rabbit hole that is lambda calculus.

An attempt to implement some sort of type checking based on hindley milner.

```
make build
./thm example.hm

Type error: tried to apply argument Num to function that takes a Str
Type error: tried to apply argument Str -> Str to non function Num
```
