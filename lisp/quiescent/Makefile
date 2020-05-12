EMACS_D_DIR ?=~/.emacs.d

EMACS ?=emacs
EMACS_SERVER=--no-init --daemon
EMACS_CLIENT ?=emacsclient
EMACS_FLAGS=
EMACS_BARE_INIT=--eval '(load-file "bare-init.el")'
EMACS_STOP=--eval '(kill-emacs)'

TARGETS=buffer-disjunction.elc \
	buffer-set-difference.elc \
	code-explorer.elc \
	cross-link-mode.elc \
	drawing-interface.elc \
	editing.elc \
	emacs-lisp.elc \
	eshell.elc \
	git.elc \
	haskell.elc \
	hippie-expand-function.elc \
	javascript.elc \
	maven.elc \
	other-window.elc \
	sql.elc

compile: init_server $(TARGETS) compile_log

start_server:
	@echo "==========Starting Compilation Server=========="
	$(EMACS) $(EMACS_FLAGS) $(EMACS_SERVER)

init_server:
	@echo "==========Init Server=========="
	$(EMACS_CLIENT) $(EMACS_FLAGS) $(EMACS_BARE_INIT)

stop_server:
	@echo "==========Stopping Compilation Server=========="
	$(EMACS_CLIENT) $(EMACS_STOP)

all: clean start_server bootstrap build_wiki compile stop_server start_emacs

all_windows: clean bootstrap compile

# Temporarily use my staging repo to check out how it's working
get_external_dependencies:
	@echo "> Fetching too-long-lines-mode"
	cd ~/.emacs.d/ && git submodule update --init lisp/too-long-lines-mode/
	@echo "> Fetching calctex"
	cd ~/.emacs.d/ && git submodule update --init lisp/calctex/
	@echo "> Fetching composable"
	cd ~/.emacs.d/ && git submodule update --init lisp/composable.el
	@echo "> Fetching project-command-at-interval"
	cd ~/.emacs.d/ && git submodule update --init lisp/project-command-at-interval
	@echo "> Fetching pyenv"
	cd ~/.emacs.d/ && git submodule update --init lisp/pyvenv
	@echo "> Fetching ado-mode"
	cd ~/.emacs.d/ && git submodule update --init lisp/ado-mode

build_external_dependencies:
	@echo "> Building too-long-lines-mode"
	cd ~/.emacs.d/lisp/too-long-lines-mode/ && $(EMACS_CLIENT) --eval "(byte-compile-file \"too-long-lines-mode.el\")"
	@echo "> Building calctex"
	cd ~/.emacs.d/lisp/calctex/ && $(EMACS_CLIENT) --eval "(byte-compile-file \"calctex.el\")"
	@echo "> Building composable"
# cd ~/.emacs.d/lisp/composable.el/ && cask install && (make || echo 0) && $(EMACS_CLIENT) --eval "(byte-compile-file \"composable.el\")" && $(EMACS_CLIENT) --eval "(byte-compile-file \"composable-mark.el\")"
	@echo "> Building project-command-at-interval"
	cd ~/.emacs.d/lisp/project-command-at-interval && make
	@echo "> Building pyenv"
	cd ~/.emacs.d/lisp/pyvenv/ && make compile
	@echo "> Building ado-mode"
	cd ~/.emacs.d/lisp/ado-mode/lisp && rm -rf *.elc
	$(EMACS_CLIENT) --eval "(byte-compile-file \"~/.emacs.d/lisp/ado-mode/lisp/ado-clip.el\")"
	$(EMACS_CLIENT) --eval "(byte-compile-file \"~/.emacs.d/lisp/ado-mode/lisp/ado-cus.el\")"
	$(EMACS_CLIENT) --eval "(byte-compile-file \"~/.emacs.d/lisp/ado-mode/lisp/ado-font-lock.el\")"
	$(EMACS_CLIENT) --eval "(byte-compile-file \"~/.emacs.d/lisp/ado-mode/lisp/ado-font.el\")"
	$(EMACS_CLIENT) --eval "(byte-compile-file \"~/.emacs.d/lisp/ado-mode/lisp/ado-mode.el\")"
	$(EMACS_CLIENT) --eval "(byte-compile-file \"~/.emacs.d/lisp/ado-mode/lisp/ado-stata-info.el\")"
	$(EMACS_CLIENT) --eval "(byte-compile-file \"~/.emacs.d/lisp/ado-mode/lisp/ado-to-stata.el\")"

build_wiki:
	@echo "==========Building Wiki Libraries=========="
	cd $(EMACS_D_DIR)/lisp/wiki && make all

bootstrap: get_external_dependencies build_external_dependencies

%.elc: %.el
	@echo "    $^"
	@$(EMACS_CLIENT) --eval "(byte-compile-file \"$^\")" > aux-log.out

compile_log:
	@echo "==========Compile Log=========="
	$(EMACS_CLIENT) --eval '(progn (switch-to-buffer "*Compile-Log*") (write-file (concat "$(EMACS_D_DIR)/lisp/quiescent/Compile-Log.out"))))'
	@cat Compile-Log.out

start_emacs:
	@echo "==========Starting Emacs to Download Any Extras=========="
	if [ -x "xinit" ]; then xinit; else $(EMACS) --debug-init --eval '(save-buffers-kill-terminal)'; fi
	@echo "==========COMPLETED SUCCESSFULLY! :D=========="

clean:
	@echo "==========Cleaning Old Installation=========="
	rm -f $(EMACS_D_DIR)/lisp/quiescent/*.elc
	rm -f $(EMACS_D_DIR)/lisp/wiki/*.elc
	rm -rf $(EMACS_D_DIR)/elpa

# TODO: Compile the whole of realgud when done:
#  find . -name "*.el" -exec ~/wip/emacs/src/emacs-27.0.50.1 --batch --eval '(progn (package-initialize) (load-file "~/.emacs.d/elpa/realgud-20190504.1238/realgud.elc") (byte-compile-file "{}"))' \;
