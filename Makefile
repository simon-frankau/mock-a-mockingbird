all: Combinator Bluebird BirdsGalore

clean:
	rm *.hi *.o Combinator

Combinator: Combinator.hs
	ghc Combinator.hs -O2 -main-is Combinator.main -o Combinator

Bluebird: Bluebird.hs
	ghc Bluebird.hs -O2 -main-is Bluebird.main -o Bluebird

BirdsGalore: BirdsGalore.hs
	ghc BirdsGalore.hs -O2 -main-is BirdsGalore.main -o BirdsGalore
