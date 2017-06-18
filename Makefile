
EMACS=$(shell command -v emacs 2> /dev/null)

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
endif

FILES = elisp/ck-dashboard.elc elisp/ck-modeline.elc

%.elc: %.el
	emacs -l "$(HOME)/.emacs.d/init.el" -L . -batch -f batch-byte-compile $<

compile: $(FILES)

clean:
	rm -f *.elc

profile-dotemacs.el:
	curl -O http://www.randomsample.de/profile-dotemacs.el

profile: profile-dotemacs.el
	@echo $(EMACS)
	$(EMACS) -Q -l "$(HOME)/.emacs.d/profile-dotemacs.el" \
	         --eval '(setq profile-dotemacs-file "~/.emacs.d/init.el")' \
		 -f profile-dotemacs

.PHONY: profile
