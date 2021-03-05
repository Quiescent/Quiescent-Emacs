EMACS_D_DIR ?=~/.emacs.d

EMACS ?=emacs
EMACS_FLAGS=-batch --no-site-file --no-site-lisp
EMACS_BYTE_COMPILE_FLAGS=-l bytecomp -f batch-byte-compile

all: clean get_external_dependencies build_external_dependencies build_wiki start_emacs

# Temporarily use my staging repo to check out how it's working
get_external_dependencies:
	@echo "> Fetching too-long-lines-mode"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/too-long-lines-mode/
	@echo "> Fetching calctex"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/calctex/
	@echo "> Fetching composable"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/composable.el
	@echo "> Fetching project-command-at-interval"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/project-command-at-interval
	@echo "> Fetching pyenv"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/pyvenv
	@echo "> Fetching ado-mode"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/ado-mode
	@echo "> Fetching org-brain-export"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/org-brain-export
	@echo "> Fetching helm-rg"
	cd $(EMACS_D_DIR) && git submodule update --init lisp/helm-rg

build_external_dependencies:
	@echo "> Building too-long-lines-mode"
	cd $(EMACS_D_DIR)/lisp/too-long-lines-mode/ && $(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "too-long-lines-mode.el"
	@echo "> Building calctex"
	cd $(EMACS_D_DIR)/lisp/calctex/ && $(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "calctex.el"
	@echo "> Building project-command-at-interval"
	cd $(EMACS_D_DIR)/lisp/project-command-at-interval && make
	@echo "> Building pyenv"
	cd $(EMACS_D_DIR)/lisp/pyvenv/ && make compile
	@echo "> IGNORING Building ado-mode -- there appear to be errors compiling it"
#	cd $(EMACS_D_DIR)/lisp/ado-mode/lisp && rm -rf *.elc
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/ado-mode/lisp/ado-clip.el"
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/ado-mode/lisp/ado-cus.el"
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/ado-mode/lisp/ado-font-lock.el"
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/ado-mode/lisp/ado-font.el"
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/ado-mode/lisp/ado-mode.el"
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/ado-mode/lisp/ado-stata-info.el"
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/ado-mode/lisp/ado-to-stata.el"
	@echo "> IGNORING Building org-brain-export -- it needs org-brain, which comes from startup.org(!)"
#	cd $(EMACS_D_DIR)/lisp/org-brain-export && rm -rf *.elc
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/org-brain-export/org-brain-export.el"
	@echo "> IGNORING Building helm-rg -- it needs helm, which comes from startup.org(!)"
#	cd $(EMACS_D_DIR)/lisp/helm-rg && rm -rf *.elc
#	$(EMACS) $(EMACS_FLAGS) $(EMACS_BYTE_COMPILE_FLAGS) "$(EMACS_D_DIR)/lisp/helm-rg/helm-rg.el"

build_wiki:
	@echo "==========Building Wiki Libraries=========="
	cd $(EMACS_D_DIR)/lisp/wiki && make all

bootstrap: get_external_dependencies build_external_dependencies

start_emacs:
	@echo "==========Starting Emacs to Download Any Extras=========="
	if [ -x "xinit" ]; then xinit; else $(EMACS) --debug-init --eval '(save-buffers-kill-terminal)'; fi
	@echo "==========COMPLETED SUCCESSFULLY! :D=========="

clean:
	@echo "==========Cleaning Old Installation=========="
	rm -f $(EMACS_D_DIR)/lisp/wiki/*.elc
	rm -rf $(EMACS_D_DIR)/straight

# TODO: Compile the whole of realgud when done:
#  find . -name "*.el" -exec ~/wip/emacs/src/emacs-27.0.50.1 --batch --eval '(progn (package-initialize) (load-file "~/.emacs.d/elpa/realgud-20190504.1238/realgud.elc") (byte-compile-file "{}"))' \;
