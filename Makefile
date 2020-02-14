test-ci: test
test-ci: NIX_ARGS = --arg emacs "(import <emacs-ci>).${EMACS_VERSION}"

test: test-check-doc test-compile test-lint

test-check-doc:
	nix-build $(NIX_ARGS) --no-out-link --quiet -A checkdoc package-checker.nix

test-compile:
	nix-build --no-out-link --quiet -A byte-compile package-checker.nix

test-lint:
	nix-shell --pure --quiet -A package-lint package-checker.nix

.PHONY: test test-ci test-default test-26 test-25 test-24
