EMACS ?= emacs
PACKAGE_INIT = (progn (require 'package) (package-initialize))
EMACS_BATCH = $(EMACS) -Q --batch -L . --eval "$(PACKAGE_INIT)"
MODULE_ELC_FILES = \
	overleaf-project-log.elc \
	overleaf-project-core.elc \
	overleaf-project-http.elc \
	overleaf-project-sync.elc \
	overleaf-project-auth.elc \
	overleaf-project-commands.elc
ELC_FILES = $(MODULE_ELC_FILES) overleaf-project.elc overleaf-project-magit.elc

.PHONY: all compile help clean

all: compile

compile: $(ELC_FILES)

overleaf-project-log.elc: overleaf-project-log.el
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-log.el

overleaf-project-core.elc: overleaf-project-core.el overleaf-project-log.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-core.el

overleaf-project-http.elc: overleaf-project-http.el overleaf-project-core.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-http.el

overleaf-project-sync.elc: overleaf-project-sync.el overleaf-project-core.elc overleaf-project-http.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-sync.el

overleaf-project-auth.elc: overleaf-project-auth.el overleaf-project-core.elc overleaf-project-http.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-auth.el

overleaf-project-commands.elc: overleaf-project-commands.el overleaf-project-core.elc overleaf-project-http.elc overleaf-project-sync.elc overleaf-project-auth.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-commands.el

overleaf-project.elc: overleaf-project.el $(MODULE_ELC_FILES)
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project.el

overleaf-project-magit.elc: overleaf-project-magit.el overleaf-project.elc
	$(EMACS_BATCH) -f batch-byte-compile overleaf-project-magit.el

help:
	@printf '%s\n' \
		'Targets:' \
		'  make        Byte-compile the package files.' \
		'  make help   Show this help message.' \
		'  make clean  Remove generated .elc files.' \
		'' \
		'Variables:' \
		'  EMACS=/path/to/emacs   Emacs executable to use (default: emacs).'

clean:
	rm -f $(ELC_FILES)
