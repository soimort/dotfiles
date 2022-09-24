#!/usr/bin/env zsh

enable-magic-functions() {
    autoload -Uz bracketed-paste-magic
    zle -N bracketed-paste bracketed-paste-magic
    autoload -Uz url-quote-magic
    zle -N self-insert url-quote-magic
}

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

    TMP=$1
    if [ -d ${TMP%/*} ]; then
        mkdir -p $TMP && cd $TMP
    else
        echo "Path '${TMP%/*}' does not exist!"
        return 1
    fi
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
    for url in "$@"; do
        if [[ $url =~ "7gogo\.jp" ]]; then
            Get-755 $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "instagram\.com" && ! ($url =~ "cdninstagram\.com") ]]; then
            Get-Insta $url || log.e "failed to get \"$url\""
            #you-get -c "$HOME/instagram.com_cookies.txt" $url || log.e "failed to get images from \"$url\""
        elif [[ $url =~ "twitter\.com" ]]; then
            Get-Tweet $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "plus\.google\.com" ]]; then
            gugutasu $url || log.e "failed to get \"$url\""
        else
            local filename=${url##*/}
            filename=${filename%%\?*}
            # skip downloads that would overwrite existing files
            wget -nc -O $filename $url || log.e "failed to get \"$url\""
        fi
    done
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
        echo 'Usage: unset-icon DIRECTORY...'
        return 1
    fi

    for i in "$@"; do
        gio set -t unset "$i" metadata::custom-icon
    done
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
        echo 'Usage: unset-emblem DIRECTORY...'
        return 1
    fi

    for i in "$@"; do
        gio set -t unset "$i" metadata::emblems
    done
}
fav() {
    if [[ -z "$1" ]]; then
        echo 'Usage: fav DIRECTORY'
        return 1
    fi

    for i in "$@"; do
        gio set -t stringv "$i" metadata::emblems favorite
    done
}


# Set the color of folder(s).
# Available icons: ~/Pictures/icons/Adwaita/places
fcolor() {
    if [[ -z "$1" ]]; then
        # no param -- show help message
        echo 'Usage: fcolor COLOR DIRECTORY...'
        echo '       fcolor DIRECTORY...'
        echo
        echo 'Available colors:'
        for i in $HOME/Pictures/icons/Adwaita/places/folder_*.png; do
            local j=${i#*folder_}
            j=${j%.png}
            echo "\t"$j
        done
        return 0
    fi

    if [[ ! -z "$2" ]]; then
        # has at least 2 params
        local COLOR=$1
        if [[ -r "$HOME/Pictures/icons/Adwaita/places/folder_$COLOR.png" ]]; then
            # first param is an actual color
            shift
            for i in "$@"; do
                gio set "$i" metadata::custom-icon "file:///home/soimort/Pictures/icons/Adwaita/places/folder_$COLOR.png"
            done
            return 0
        fi
    fi

    for i in "$@"; do
        gio set "$i" metadata::custom-icon "file:///home/soimort/Pictures/icons/Adwaita/places/folder.png"
    done
    return 0
}


# Set/unset $SOCKS5_PROXY.
set-socks() {
    export SOCKS5_PROXY=127.0.0.1:1080
}
unset-socks() {
    export SOCKS5_PROXY=
}


get() {
    if [[ -z "$1" ]]; then
        # no param -- show help message
        echo 'Usage: get PREFIX URL...'
        echo '       get URL...'
        return 0
    fi

    if [[ ! -z $SOCKS5_PROXY ]]; then
        local XYC=proxychains4
        log.d "proxy set:  $SOCKS5_PROXY"
    fi

    if [[ ! -z "$2" ]]; then
        # has at least 2 params
        if [[ ! $1 =~ "https?://" ]]; then
            # first param is not an HTTP(S) URL
            local PREFIX="[$1] "
            shift
            log.d "prefix set: $PREFIX"
        fi
    fi

    for url in "$@"; do
        if [[ $url =~ "7gogo\.jp" && ! ($url =~ "stat\.7gogo\.jp") ]]; then
            $XYC Get-755 $url || log.e "failed to get \"$url\""

        elif [[ $url =~ "instagram\.com" && ! ($url =~ "cdninstagram\.com") ]]; then
            $XYC Get-Insta $url || log.e "failed to get \"$url\""

        elif [[ $url =~ "twitter\.com" ]]; then
            $XYC Get-Tweet $url || log.e "failed to get \"$url\""

        else
            local FILENAME=${url##*/}
            FILENAME=${FILENAME%%\?*}
            FILENAME=$PREFIX$FILENAME
            # TODO: fix ext
            # TODO: set $UA?
            if [ -f "$FILENAME" ]; then
                log.w "file \"$FILENAME\" already exists!"
                continue
            else
                $XYC wget -q --show-progress --no-check-certificate -U "$UA" -O "$FILENAME" "$url" ||
                    log.e "failed to get \"$url\""
            fi
        fi
    done
    return 0
}

pre() {
    if [[ -z "$1" ]]; then
        # no param -- show help message
        echo 'Usage: pre PREFIX FILE...'
        return 0
    fi

    local PREFIX="[$1] "
    shift
    log.d "prefix set: $PREFIX"

    for FILENAME in "$@"; do
        if [ ! -f "$FILENAME" ]; then
            log.w "file \"$FILENAME\" does not exist!"
            continue
        else
            NEW_FILENAME=`dirname $FILENAME`/$PREFIX`basename $FILENAME`
            mv $FILENAME $NEW_FILENAME
        fi
    done
    return 0
}

prefix() {
    if [[ -z "$1" ]]; then
        # no param -- show help message
        echo 'Usage: prefix PREFIX FILE...'
        return 0
    fi

    local PREFIX="$1"
    shift
    log.d "prefix set: $PREFIX"

    for FILENAME in "$@"; do
        if [ ! -f "$FILENAME" ]; then
            log.w "file \"$FILENAME\" does not exist!"
            continue
        else
            NEW_FILENAME=`dirname $FILENAME`/$PREFIX`basename $FILENAME`
            mv $FILENAME $NEW_FILENAME
        fi
    done
    return 0
}
