FILE = sierpinsky

build: L-System Translator

.PHONY: run clean

run: L-System Translator Turtle.scm
	./L-System $(FILE).lsys
	./Translator $(FILE).semantics
	mred -f Turtle.scm

L-System: L-System.hs
	ghc --make L-System.hs

Translator: Translator.hs
	ghc --make Translator.hs

clean:
	rm -f *.o *.hi L-System Translator


