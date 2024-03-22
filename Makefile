.PHONY: all clean

all:
	dune build analyzer @install

clean:
	dune clean
	rm -f *~ */*~ .*.swp */.*.swp *.tar.gz

install:
	dune install --prefix=.

compress: clean
	tar -czvf ../tp-semantics.tar.gz --exclude=".git*" ../tp-semantics
	mv ../tp-semantics.tar.gz .

