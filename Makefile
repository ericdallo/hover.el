EMACS := emacs
EMACS_VER = docker run --rm -t \
	-v $(PWD):/work \
	-w /work \
	silex/emacs:$1 \
	emacs

test-ci: test test-26 test-25 test-24

test-ert test-26 test-25 test-24:
	$(EMACS) -Q -batch -l ert -L . -f ert-run-tests-batch-and-exit

test-26: EMACS := $(call EMACS_VER,26.2)

test-25: EMACS := $(call EMACS_VER,25.3)

test-24: EMACS := $(call EMACS_VER,24.5)

test: test-check-doc test-compile test-lint test-ert

test-check-doc:
	nix-build --no-out-link --quiet -A checkdoc package-checker.nix

test-compile:
	nix-build --no-out-link --quiet -A byte-compile package-checker.nix

test-lint:
	nix-shell --pure --quiet -A package-lint package-checker.nix

.PHONY: test test-ci test-default test-26 test-25 test-24
