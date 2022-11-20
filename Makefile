.PHONY: clean install test

steelcut: src/*.lisp
	./build.sh

test:
	./test.sh

clean:
	rm -rf steelcut

install: steelcut
	test -n "$(BINDIR)"  # $$BINDIR
	cp steelcut ${BINDIR}
