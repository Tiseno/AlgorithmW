default:
	runhaskell Main.hs examples/unification-error.hm --debug

in:
	cat example.hm | runhaskell Main.hs example.hm --stdin-format

build:
	ghc Main.hs -o w
	rm *.o
	rm *.hi

