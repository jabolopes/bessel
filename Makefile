HS_FLAGS = -Wall -package-db \
	   .cabal-sandbox/x86_64-linux-ghc-8.4.3-packages.conf.d \
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
	.cabal-sandbox/bin/happy $< -o $@ -idist/log/Parser.log

#dist/hs/Parser.hs: Parser.y dist/hs/Lexer.hs | dist/log
#	happy -a -d $< -o $@ -idist/log/Parser.log

dist/hs/Lexer.hs: Lexer.x | dist/hs
	.cabal-sandbox/bin/alex $< -o $@

.PHONY: dist/bin/bsl
dist/bin/bsl: dist/hs/Parser.hs dist/hs/Lexer.hs | dist/bin dist/hs dist/obj
	ghc ${HS_FLAGS} --make Main.hs -o dist/bin/bsl

.PHONY: dist/bin/test
dist/bin/test: dist/hs/Parser.hs dist/hs/Lexer.hs | dist/bin dist/hs dist/obj
	ghc ${HS_FLAGS} --make Test.hs -o dist/bin/test

.PHONY: build
build: dist/bin/bsl

.PHONY: run
run: dist/bin/bsl | dist/bin
	dist/bin/bsl

.PHONY: test
test: dist/bin/test | dist/bin
	dist/bin/test

.PHONY: ghci
ghci: dist/hs/Parser.hs dist/hs/Lexer.hs | dist/hs dist/obj
	ghci ${HS_FLAGS}

.PHONY: clean
clean:
	rm -rf dist
