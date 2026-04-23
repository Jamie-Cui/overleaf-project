EMACS ?= emacs
PACKAGE_INIT = (progn (require 'package) (package-initialize))
EMACS_BATCH = $(EMACS) -Q --batch -L . --eval "$(PACKAGE_INIT)"
ELC_FILES = overleaf-project.elc overleaf-project-magit.elc

.PHONY: all compile help clean

all: compile

compile: $(ELC_FILES)

overleaf-project.elc: overleaf-project.el
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
