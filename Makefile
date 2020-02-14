EMACS := emacs
EMACS_VER = docker run --rm -t \
	-v $(PWD):/work \
	-w /work \
	silex/emacs:$1 \
	emacs

test: test-default

test-ci: test-26 test-25 test-24

test-default test-26 test-25 test-24:
	$(EMACS) -Q -batch -l ert -L . -f ert-run-tests-batch-and-exit

test-26: EMACS := $(call EMACS_VER,26.2)

test-25: EMACS := $(call EMACS_VER,25.3)

test-24: EMACS := $(call EMACS_VER,24.5)

.PHONY: test test-ci test-default test-26 test-25 test-24
