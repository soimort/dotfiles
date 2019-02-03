#!/usr/bin/env zsh

rand() {
    head /dev/urandom | tr -dc A-Za-z0-9 | head -c10
}

urldec() {
    if [[ -z "$1" ]]; then
        echo 'Usage: urldec STRING'
        return 1
    fi

    python -c 'import sys, urllib.parse; print(urllib.parse.unquote(sys.argv[1]))' "$1"
}

doin() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo 'Usage: doin DIRECTORY COMMAND...'
        return 1
    fi

    cd $1 && shift && $@ && cd -
}

mc() {
    if [[ -z "$1" ]]; then
        echo 'Usage: mc DIRECTORY'
        return 1
    fi

    mkdir -p $1 && cd $1
}

48go() {
    if [[ -z "$ARCHIVE_PATH" ]]; then
        echo 'Please set $ARCHIVE_PATH first.'
        return 1
    fi

    if [[ $1 =~ "http" ]]; then
        echo 'Use "48get" instead. (Are you drunk?)'
        return 1
    fi

    if [[ -z "$1" ]]; then
        cd $ARCHIVE_PATH && ls "$_"
    else
        mkdir -p $ARCHIVE_PATH/$1 && cd "$_"
    fi
}

48get() {
    local WGOT_FLAG=0
    for url in "$@"; do
        if [[ $url =~ "7gogo\.jp" ]]; then
            Get-755 $url || break
        elif [[ $url =~ "instagram\.com" && ! ($url =~ "cdninstagram\.com") ]]; then
            Get-Insta $url || break
        elif [[ $url =~ "twitter\.com" ]]; then
            Get-Tweet $url || break
        elif [[ $url =~ "plus\.google\.com" ]]; then
            gugutasu $url || break
        else
            WGOT_FLAG=1
            wget $url || break
        fi
    done
    if [[ $WGOT_FLAG = 1 ]]; then
        fix-ext *
    fi
}

waifu() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo "Usage: waifu SCALE_RATIO FILENAME"
        return 1
    fi

    waifu2x-converter-cpp --scale_ratio $1 -i $2 -o $2_$1x.png
}

# Set custom icon / emblems of a file via GIO.
# see also: https://developer.gnome.org/gio/stable/gio.html
set-icon() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo 'Usage: set-icon DIRECTORY ICON'
        return 1
    fi

    gio set "$1" metadata::custom-icon "file://$2"
}
unset-icon() {
    if [[ -z "$1" ]]; then
        echo 'Usage: unset-icon DIRECTORY'
        return 1
    fi

    gio set -t unset "$1" metadata::custom-icon
}
set-emblem() {
    if [[ -z "$1" ]]; then
        echo 'Usage: set-emblem DIRECTORY [EMBLEMS]'
        return 1
    fi

    if [[ -z "$2" ]]; then
        gio set -t stringv "$1" metadata::emblems default
    else
        TMP=$1
        shift
        gio set -t stringv "$TMP" metadata::emblems $@
    fi
}
unset-emblem() {
    if [[ -z "$1" ]]; then
        echo 'Usage: unset-emblem DIRECTORY'
        return 1
    fi

    gio set -t unset "$1" metadata::emblems
}
