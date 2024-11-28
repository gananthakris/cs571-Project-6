default : test_ast

test_ast : ast.hs test_ast.hs
	ghc ast.hs test_ast.hs

clean :
	rm -rf test_ast test_ast.o test_ast.hi ast.hi ast.o