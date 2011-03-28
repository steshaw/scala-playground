
SCALAC := fsc

GHC := ghc --make -Wall -O99 -fno-warn-type-defaults -fforce-recomp -rtsopts

all: run

.PHONY:
compile: classes/FunctionalProgramming.class

classes/FunctionalProgramming.class: Makefile FunctionalProgramming.scala
	$(SCALAC) -d classes/ FunctionalProgramming.scala

FunctionalProgramming: Makefile FunctionalProgramming.hs
	$(GHC) FunctionalProgramming.hs

.PHONY:
run: classes/FunctionalProgramming.class FunctionalProgramming
	time scala -cp classes/ FunctionalProgramming
	time ./FunctionalProgramming
