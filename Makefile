
build:
	jbuilder build @install --dev

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build

.PHONY: build test install uninstall clean test

