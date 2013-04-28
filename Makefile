all: dist/bin dist/obj dist/hs dist/hs/Parser.hs dist/hs/Lexer.hs
	ghc --make Main.hs -idist/hs -o dist/bin/bsl -outputdir dist/obj

dist/bin:
	mkdir -p dist/bin

dist/obj:
	mkdir -p dist/obj

dist/hs:
	mkdir -p dist/hs

dist/hs/Parser.hs: Parser.y
	happy Parser.y -o dist/hs/Parser.hs

dist/hs/Lexer.hs: Lexer.x dist/hs/Parser.hs
	alex Lexer.x -o dist/hs/Lexer.hs

clean:
	rm -rf dist
