#!/bin/bash

AGREEN="\e[92m"
ARED="\e[91m"
ABLUE="\e[34m"
ARESET="\e[0m"

clear
echo ""
echo -e "$AGREEN▄   ▄     ▗▄▖    ▄▄        ▄               ▗▖            ▄▄   █  ▗▄▖"
echo -e "$AGREEN█   █     ▝▜▌   ▐▛▀        █               ▐▌      ▐▌   ▐▛▀   ▀  ▝▜▌"
echo -e "$AGREEN▜▖█▗▛ ▟█▙  ▐▌  ▐███  ▟█▙   ▀  ▗▟██▖      ▟█▟▌ ▟█▙ ▐███ ▐███  ██   ▐▌   ▟█▙ ▗▟██▖"
echo -e "$AGREEN▐▌█▐▌▐▛ ▜▌ ▐▌   ▐▌  ▐▙▄▟▌     ▐▙▄▖▘     ▐▛ ▜▌▐▛ ▜▌ ▐▌   ▐▌    █   ▐▌  ▐▙▄▟▌▐▙▄▖▘"
echo -e "$AGREEN▐█▀█▌▐▌ ▐▌ ▐▌   ▐▌  ▐▛▀▀▘      ▀▀█▖     ▐▌ ▐▌▐▌ ▐▌ ▐▌   ▐▌    █   ▐▌  ▐▛▀▀▘ ▀▀█▖"
echo -e "$AGREEN▐█ █▌▝█▄█▘ ▐▙▄  ▐▌  ▝█▄▄▌     ▐▄▄▟▌     ▝█▄█▌▝█▄█▘ ▐▙▄  ▐▌  ▗▄█▄▖ ▐▙▄ ▝█▄▄▌▐▄▄▟▌"
echo -e "$AGREEN▝▀ ▀▘ ▝▀▘   ▀▀  ▝▘   ▝▀▀       ▀▀▀       ▝▀▝▘ ▝▀▘   ▀▀  ▝▘  ▝▀▀▀▘  ▀▀  ▝▀▀  ▀▀▀"
echo -e $ARESET

if [[ $1 = "-g" ]] || [[ $1 = "--git" ]]; then
    git clone https://github.com/WolfeCub/dotfiles.git
    cd dotfiles
    ./intstall.sh
elif [[ $1 = "-u" ]] || [[ $1 = "--uninstall" ]]; then
    for ITEM in `ls -d */`;
    do
        ( stow -D $ITEM )
    done
elif [[ $1 = "-h" ]] || [[ $1 = "--help" ]]; then
    echo -en "Wolfe's dotfile installer (inspired by anthonytam)\n\n"
    echo -en "$ABLUE""USAGE$ARESET:\n"
    echo -en "\t${0} [FLAGS]\n\n"
    echo -en "$ABLUE""FLAGS$ARESET:\n"
    echo -en "\t-u, --uninstall\tRemoves all dotfile symlinks\n"
    echo -en "\t-h, --help\tDisplay this message\n\n"
    echo -en "Report bugs/problems by creating an issue on github\n"
    echo -en "https://github.com/anthonytam/DotFiles\n"
else
    for ITEM in `ls -d */`;
    do
        TEMP=`echo $ITEM | sed s'/.$//'`
        echo -en "Setup config for $ARED$TEMP$ARESET? (y/n) "
        read USRINPUT
        case "$USRINPUT" in
            y)
                ( stow $ITEM )
                ;;
            *)
                printf "Skipping $TEMP\n"
                ;;
        esac
    done
fi
