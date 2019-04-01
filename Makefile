all:
	ghc simplify-bkg.hs -o simplify-bkg

clean:
	rm -f simplify-bkg simplify-bkg.hi simplify-bkg.o
