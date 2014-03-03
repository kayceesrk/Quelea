all: test1 test3

test1: test1.hs
	ghc -cpp test1.hs ../tracer/Tracer.hs

test2: test2.hs
	ghc -cpp test2.hs ../tracer/Tracer.hs

test3: test3.hs ../tracer/Tracer.hs
	ghc -cpp test3.hs ../tracer/Tracer.hs


clean:
	rm -rf test1 test2 test3 *.o *~ *.hi op
