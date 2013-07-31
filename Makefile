.PHONY: all
all: dist/hs/Parser.hs dist/hs/Lexer.hs | dist/bin dist/obj dist/hs
	ghc --make Main.hs -idist/hs -o dist/bin/bsl -outputdir dist/obj

dist/bin:
	mkdir -p $@

dist/obj:
	mkdir -p $@

dist/hs:
	mkdir -p $@

dist/log:
	mkdir -p $@

dist/hs/Parser.hs: Parser.y dist/hs/Lexer.hs | dist/log
	happy $< -o $@ -idist/log/Parser.log

dist/hs/Lexer.hs: Lexer.x | dist/hs
	alex $< -o $@

clean:
	rm -rf dist
