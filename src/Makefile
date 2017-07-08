all: grammar parse-test

parse-test: Test/Grammar.hs grammar
	ghc --make Test/Grammar.hs -o parse-test

grammar:
	bnfc -d grammar.cf
	happy -gca$(GRAM_DEBUG) --info=Grammar/LRDoc.txt Grammar/Par.y
	alex -g$(GRAM_DEBUG) Grammar/Lex.x

clean:
	-rm -f Grammar/*.log Grammar/*.aux Grammar/*.hi Grammar/*.o Grammar/*.dvi

distclean: clean
	-rm -f Grammar/Doc.* Grammar/Lex.* Grammar/Par.* Grammar/Layout.* Grammar/Skel.* Grammar/Print.* Grammar/Test.* Grammar/Abs.* Grammar/Test Grammar/ErrM.* Grammar/SharedString.* Grammar/ComposOp.* Grammar/grammar.dtd Grammar/XML.* Makefile*
		-rmdir -p Grammar/

