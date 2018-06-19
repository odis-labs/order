
build:
	jbuilder build @install --dev
.PHONY: build

test:
	jbuilder runtest
.PHONY: test

install:
	jbuilder install
.PHONY: install

uninstall:
	jbuilder uninstall
.PHONY: uninstall

clean:
	rm -rf _build
.PHONY: clean
