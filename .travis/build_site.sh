#!/bin/bash

emacs --version
emacs --batch -f org-version --kill
cd .travis
emacs --script install.el
emacs --script generate-html.el
emacs --batch -f org-version --kill
cd ../
mkdir -p deploy
mv emacs/.emacs.d/README.html deploy/index.html
mv .travis/readtheorg.css deploy/
