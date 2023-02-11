default:
	runhaskell Main.hs example.hm --debug

in:
	cat example.hm | runhaskell Main.hs example.hm --stdin-format

build:
	ghc Main.hs -o thmc
	rm *.o
	rm *.hi
