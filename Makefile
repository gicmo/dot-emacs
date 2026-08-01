
EMACS=$(shell command -v emacs 2> /dev/null)

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
endif

FILES = elisp/ck-dashboard.elc elisp/ck-modeline.elc elisp/ck-env.elc

%.elc: %.el
	emacs -l "$(HOME)/.emacs.d/init.el" -L . -batch -f batch-byte-compile $<

compile: $(FILES)

clean:
	rm -f *.elc

profile:
	@echo $(EMACS)
	$(EMACS) -Q --eval '(progn (profiler-start (quote cpu)) (load "$(HOME)/.emacs.d/init.el") (profiler-stop) (profiler-report))'

.PHONY: profile
