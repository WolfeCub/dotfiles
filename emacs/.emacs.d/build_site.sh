#!/bin/bash

emacs --version
emacs --batch -f org-version --kill
emacs --daemon
emacsclient -e '(kill-emacs)'
emacs --batch -f org-version --kill
cd emacs/.emacs.d/
emacs --script generate-html.el
cd ../../
mkdir deploy
mv emacs/.emacs.d/README.html deploy/index.html
