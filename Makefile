all: Combinator

clean:
	rm *.hi *.o Combinator

Combinator: Combinator.hs
	ghc Combinator.hs -O2 -main-is Combinator.main -o Combinator
