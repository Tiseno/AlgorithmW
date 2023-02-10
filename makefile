default:
	runhaskell Main.hs example.hm

in:
	cat example.hm | runhaskell Main.hs example.hm --stdin-format

build:
	ghc Main.hs -o thmc
