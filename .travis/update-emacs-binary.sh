#!/bin/bash
P=$(which emacs)
rm $P
ln -s $(which emacs-snapshot) $P
