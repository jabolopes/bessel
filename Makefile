all: dist/bin dist/obj Parser.hs Lexer.hs
	ghc --make Main.hs -o dist/bin/fl -outputdir dist/obj

dist/bin:
	mkdir -p dist/bin

dist/obj:
	mkdir -p dist/obj

Parser.hs: Parser.y
	happy Parser.y

Lexer.hs: Lexer.x
	alex Lexer.x

clean:
	rm -rf dist