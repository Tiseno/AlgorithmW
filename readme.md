# THM
Down the rabbit hole that is lambda calculus.

An attempt to implement some sort of type checking based on hindley milner.

```
make build
./thm examples/unification-error.hm

Type error: can not unify types Str and (('t3 -> 't3) -> 'n)
```

### Resources
https://martinsteffen.github.io/staticanalysis/typeinfalgos/

https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/milner-type-polymorphism.pdf

[Unification algorithm](https://www.youtube.com/watch?v=KNbRLTLniZI)

[Good lecture](https://www.youtube.com/watch?v=OyrByPkiX7s)

