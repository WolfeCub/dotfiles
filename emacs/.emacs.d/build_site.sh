#!/bin/bash
cp emacs/.emacs.d/* .
emacs --script generate-html.el
mkdir deploy
mv README.html deploy/index.html
