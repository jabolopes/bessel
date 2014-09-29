HS_FLAGS = -Wall -package-db \
	   .cabal-sandbox/x86_64-linux-ghc-7.6.2-packages.conf.d \
	   -idist/hs \
	   -outputdir dist/obj

DIST_DIRS = dist/bin \
	    dist/obj \
	    dist/hs  \
	    dist/log

all: build

$(DIST_DIRS):
	mkdir -p $@

dist/hs/Parser.hs: Parser.y dist/hs/Lexer.hs | dist/log
	happy $< -o $@ -idist/log/Parser.log

dist/hs/Lexer.hs: Lexer.x | dist/hs
	alex $< -o $@

.PHONY: dist/bin/bsl
dist/bin/bsl: dist/hs/Parser.hs dist/hs/Lexer.hs | dist/bin dist/hs dist/obj
	ghc ${HS_FLAGS} --make Main.hs -o dist/bin/bsl

.PHONY: dist/bin/test
dist/bin/test: dist/hs/Parser.hs dist/hs/Lexer.hs | dist/bin dist/hs dist/obj
	ghc ${HS_FLAGS} --make Test.hs -o dist/bin/test

build: dist/bin/bsl

run: dist/bin/bsl | dist/bin
	dist/bin/bsl

test: dist/bin/test | dist/bin
	dist/bin/test

clean:
	rm -rf dist
