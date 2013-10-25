DIST_DIRS = dist/bin \
	    dist/obj \
	    dist/hs  \
	    dist/log

.PHONY: all
all: dist/hs/Parser.hs dist/hs/Lexer.hs | dist/bin dist/obj dist/hs
	ghc -Wall --make Main.hs -idist/hs -o dist/bin/bsl -outputdir dist/obj

$(DIST_DIRS):
	mkdir -p $@

dist/hs/Parser.hs: Parser.y dist/hs/Lexer.hs | dist/log
	happy $< -o $@ -idist/log/Parser.log

dist/hs/Lexer.hs: Lexer.x | dist/hs
	alex $< -o $@

clean:
	rm -rf dist
