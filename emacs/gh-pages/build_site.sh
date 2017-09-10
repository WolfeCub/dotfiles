#!/bin/bash

emacs --version
emacs --batch -f org-version --kill
cd emacs/gh-pages
emacs --script install.el
emacs --script generate-html.el
emacs --batch -f org-version --kill
cd ../../
mkdir -p deploy
mv TODO.html deploy/todo.html
mv emacs/.emacs.d/README.html deploy/index.html
