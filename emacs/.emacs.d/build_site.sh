#!/bin/bash

emacs --version
emacs --batch -f org-version --kill
cd emacs/.emacs.d/
emacs --script generate-html.el
emacs --batch -f org-version --kill
cd ../../
mkdir deploy
mv emacs/.emacs.d/README.html deploy/index.html
