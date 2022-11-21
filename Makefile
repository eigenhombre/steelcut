.PHONY: clean install test docker

steelcut: src/*.lisp
	./build.sh

test:
	./test.sh

clean:
	rm -rf steelcut

docker:
	docker build -t steelcut .

install: steelcut
	test -n "$(BINDIR)"  # $$BINDIR
	cp steelcut ${BINDIR}
