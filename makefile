run: build
	out/main

build:
	ghc -o out/main main.hs

clean:
	rm *.hi
	rm *.o
