EMACS ?= emacs
COVERAGE_DIR ?= coverage
COVERAGE_MIN ?= 0
PACKAGE_INIT = (progn (setq load-prefer-newer t) (require 'package) (package-initialize) (add-to-list 'load-path default-directory))
EMACS_BATCH = $(EMACS) -Q --batch -L . --eval "$(PACKAGE_INIT)"
MODULE_ELC_FILES = \
	overleaf-project-log.elc \
	overleaf-project-core.elc \
	overleaf-project-http.elc \
	overleaf-project-sync.elc \
	overleaf-project-firefox.elc \
	overleaf-project-auth.elc
ELC_FILES = $(MODULE_ELC_FILES) overleaf-project.elc overleaf-project-magit.elc
TEST_FILES = \
	test/overleaf-project-test.el \
	test/overleaf-project-git-test.el \
	test/overleaf-project-sync-tree-test.el \
	test/overleaf-project-command-test.el \
	test/overleaf-project-http-auth-test.el \
	test/overleaf-project-async-test.el \
	test/overleaf-project-magit-test.el

.PHONY: all compile test coverage help clean

all: compile

compile: $(ELC_FILES)

test:
	$(EMACS_BATCH) $(foreach file,$(TEST_FILES),-l $(file)) -f ert-run-tests-batch-and-exit

coverage:
	$(EMACS_BATCH) \
		--eval "(setq overleaf-project-coverage-directory \"$(COVERAGE_DIR)\" overleaf-project-coverage-min $(COVERAGE_MIN))" \
		-l test/coverage.el

overleaf-project-log.elc: overleaf-project-log.el
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-log.el

overleaf-project-core.elc: overleaf-project-core.el overleaf-project-log.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-core.el

overleaf-project-http.elc: overleaf-project-http.el overleaf-project-core.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-http.el

overleaf-project-sync.elc: overleaf-project-sync.el overleaf-project-core.elc overleaf-project-http.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-sync.el

overleaf-project-firefox.elc: overleaf-project-firefox.el overleaf-project-core.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-firefox.el

overleaf-project-auth.elc: overleaf-project-auth.el overleaf-project-core.elc overleaf-project-http.elc overleaf-project-firefox.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-auth.el

overleaf-project.elc: overleaf-project.el $(MODULE_ELC_FILES)
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project.el

overleaf-project-magit.elc: overleaf-project-magit.el overleaf-project.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-magit.el

help:
	@printf '%s\n' \
		'Targets:' \
		'  make        Byte-compile the package files.' \
		'  make test   Run the ERT test suite.' \
		'  make coverage   Run ERT under built-in testcover.' \
		'  make help   Show this help message.' \
		'  make clean  Remove generated .elc files.' \
		'' \
		'Variables:' \
		'  EMACS=/path/to/emacs   Emacs executable to use (default: emacs).' \
		'  COVERAGE_DIR=coverage  Directory for coverage reports.' \
		'  COVERAGE_MIN=0         Minimum coverage percent required.'

clean:
	rm -f $(ELC_FILES)
	rm -rf $(COVERAGE_DIR)
