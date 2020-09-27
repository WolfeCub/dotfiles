#! @shell@

set -e

sw="/nix/var/nix/profiles/system/sw/bin"
systemPath=`${sw}/readlink -f /nix/var/nix/profiles/system`

# Needs root to work
if [[ $EUID -ne 0 ]]; then
    echo "[ERROR] Requires root! :( Make sure the WSL default user is set to root"
    exit 1
fi

if [ ! -e "/run/current-system" ]; then
    ${sw}/ln -sfn "$(${sw}/readlink -f "$systemPath")" /run/current-system
fi

if [ ! -e "/run/systemd.pid" ]; then
    PATH=/run/current-system/systemd/lib/systemd:@fsPackagesPath@ \
        LOCALE_ARCHIVE=/run/current-system/sw/lib/locale/locale-archive \
        @daemonize@/bin/daemonize /run/current-system/sw/bin/unshare -fp --mount-proc systemd
    /run/current-system/sw/bin/pgrep -xf systemd > /run/systemd.pid
fi

usedShell=$($sw/getent passwd @defaultUser@ | $sw/cut -d: -f7)

# While bootstraping the image, we need to execute command inside without having a default user.
if [[ "@defaultUser@" == "root" ]]; then
    usedShell="/bin/sh"
fi

# Entering the namespace where systemd is PID1
exec $sw/nsenter -t $(< /run/systemd.pid) -p -m --wd="$PWD" -- \
    @wrapperDir@/su -s $usedShell @defaultUser@ "$@"
