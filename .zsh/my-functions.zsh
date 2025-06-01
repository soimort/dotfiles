#!/usr/bin/env zsh

enable-magic-functions() {
    autoload -Uz bracketed-paste-magic
    zle -N bracketed-paste bracketed-paste-magic
    autoload -Uz url-quote-magic
    zle -N self-insert url-quote-magic
}

# Search for strings (patterns actually) in the command history. (AND-matches)
# [FIXME] when a string contains "/", "&", or "'"
# <https://unix.stackexchange.com/questions/55359/how-to-run-grep-with-multiple-and-patterns>
his() {
    if [ -z "$1" ]; then
        echo 'Usage: his STRING...'
        return 1
    fi

    local str pat="awk '/" ##grep
    for str in "$@"; do
        pat=$pat"/ && /"$str
        ##pat=$pat" -e \"$str\""
    done
    pat=$pat"/'"
    history | eval $pat | tac | less
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

# TODO: deprecated in favor of mcer?
doin() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo 'Usage: doin DIRECTORY COMMAND...'
        return 1
    fi

    cd $1 && shift && $@ && cd -
}

mc() {
    # mkdir and chdir
    if [[ -z "$1" ]]; then
        log.w 'Usage: mc DIRECTORY'
        return 1
    fi

    TMP=$1
    if [ -d ${TMP%/*} ]; then
        mkdir -p $TMP && cd $TMP
    else
        log.e "Path '${TMP%/*}' does not exist!"
        return 1
    fi
}

mcer() {
    # mkdir, chdir, execute and return
    if [[ -z "$1" ]]; then
        log.w 'Usage: mcer DIRECTORY'
        return 1
    fi

    local OLD_PWD=$PWD
    local OLD_OLDPWD=$OLDPWD
    local TMP=$1
    if [ -d ${TMP%/*} ]; then
        mkdir -p $TMP
        if [ ! "$?" -eq 0 ]; then
            return
        fi

        cd $TMP
        log.i "Executing command under: $TMP"
        shift 1
        $@
        popd -0 >/dev/null
        cd $OLD_OLDPWD && cd $OLD_PWD  # make sure 'cd -' returns to previous dir
    else
        log.e "Path '${TMP%/*}' does not exist!"
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
        elif [[ $url =~ "twitter\.com" || $url =~ "x\.com" ]]; then
            Get-Tweet $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "plus\.google\.com" ]]; then
            gugutasu $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "bltweb\.jp" ]]; then
            get-blt $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "bubkaweb\.com" || $url =~ "idol-culture\.jp" ]]; then
            get-bubka $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "smart-flash\.jp" ]]; then
            get-flash $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "mdpr\.jp" ]]; then
            get-mdpr $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "music-culture\.info" ]]; then
            get-music-culture $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "official-goods-store\.jp" || $url =~ "shop\.akb48\.co\.jp" ]]; then
            get-namashashin $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "popnroll\.tv" ]]; then
            get-popnroll $url || log.e "failed to get \"$url\""
        elif [[ $url =~ "thetv\.jp" ]]; then
            get-thetv $url || log.e "failed to get \"$url\""
        else
            local filename=${url##*/}
            filename=${filename%%\?*}
            # skip downloads that would overwrite existing files
            wget -nc -O $filename -- $url || log.e "failed to get \"$url\""
        fi
    done
}

waifu() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo "Usage: waifu SCALE_RATIO FILENAME"
        return 1
    fi

    waifu2x-converter-cpp --scale-ratio $1 -i $2 -o $2_$1x.png 2>/dev/null ||
    waifu2x-converter-cpp --scale_ratio $1 -i $2 -o $2_$1x.png 2>/dev/null
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

mark() {
    if [[ -z "$1" || -z "$2" || -z "$3" ]]; then
        echo 'Usage: mark ICON_THEME TYPE FILE...'
        return 1
    fi

    local FOLDER_ICON="$HOME/.icons/$1/512x512/places/folder-$2.png"
    local EMBLEM_TYPE="$2"
    local EMBLEM_ICON="$HOME/.icons/$1/512x512/emblems/emblem-$2.png"
    shift 2
    for FILENAME in "$@"; do
        if [ -d "$FILENAME" ]; then
            log.i "Setting custom icon for folder: $FILENAME"
            gio set "$FILENAME" metadata::custom-icon "file://$FOLDER_ICON"
        else
            log.i "Setting custom emblem for file: $FILENAME"
            gio set -t stringv "$FILENAME" metadata::emblems "$EMBLEM_TYPE"
        fi
    done
}
unmark() {
    if [[ -z "$1" ]]; then
        echo 'Usage: unmark FILE...'
        return 1
    fi

    for FILENAME in "$@"; do
        if [ -d "$FILENAME" ]; then
            log.i "Unsetting custom icon for folder: $FILENAME"
            gio set -t unset "$FILENAME" metadata::custom-icon
        else
            log.i "Unsetting custom emblem for file: $FILENAME"
            gio set -t unset "$FILENAME" metadata::emblems
        fi
    done
}

has-marked() {
    if [[ -z "$1" ]]; then
        echo 'Usage: has-marked FILE...'
        return 1
    fi

    for FILENAME in "$@"; do
        if [ -d "$FILENAME" ]; then
            # is a directory
            gio info -a metadata::custom-icon "$FILENAME" | grep metadata::custom-icon >/dev/null
            if [ $? -eq 0 ]; then echo $FILENAME; fi
            gio info -a metadata::emblems "$FILENAME" | grep metadata::emblems >/dev/null
            if [ $? -eq 0 ]; then echo $FILENAME; fi
            for SUBFILENAME in $FILENAME/* $FILENAME/**/*; do
                if [ -d "$SUBFILENAME" ]; then
                    # is a directory
                    gio info -a metadata::custom-icon "$SUBFILENAME" | grep metadata::custom-icon >/dev/null
                    if [ $? -eq 0 ]; then echo $SUBFILENAME; fi
                    gio info -a metadata::emblems "$SUBFILENAME" | grep metadata::emblems >/dev/null
                    if [ $? -eq 0 ]; then echo $SUBFILENAME; fi
                else
                    # is a file
                    gio info -a metadata::emblems "$SUBFILENAME" | grep metadata::emblems >/dev/null
                    if [ $? -eq 0 ]; then echo $SUBFILENAME; fi
                fi
            done
        else
            # is a known file
            gio info -a metadata::emblems "$FILENAME" | grep metadata::emblems
        fi
    done
}

# [TODO] deprecated in favor of mark/unmark
fav() {
    if [[ -z "$1" ]]; then
        echo 'Usage: fav DIRECTORY'
        return 1
    fi

    for i in "$@"; do
        gio set -t stringv "$i" metadata::emblems favorite
    done
}

# [TODO] deprecated in favor of mark/unmark
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
                $XYC wget -q --show-progress --no-check-certificate -U Googlebot -O "$FILENAME" "$url" ||
                    log.e "failed to get \"$url\""
            fi
        fi
    done
    return 0
}

pre() {
    if [[ -z "$1" ]]; then
        # no param -- show help message
        log.w 'Usage: pre PREFIX FILE...'
        return 0
    fi

    local PREFIX="[$1] "
    shift
    log.d "prefix set: $PREFIX"

    for FILENAME in "$@"; do
        if [[ ! -f "$FILENAME" && ! -d "$FILENAME" ]]; then
            log.w "file or directory \"$FILENAME\" does not exist!"
            continue
        else
            NEW_FILENAME=`dirname $FILENAME`/$PREFIX`basename $FILENAME`
            log.i "$FILENAME => $NEW_FILENAME"
            mv -- "$FILENAME" "$NEW_FILENAME"
        fi
    done
    return 0
}

unpre() {
    if [[ -z "$1" ]]; then
        # no param -- show help message
        log.w 'Usage: unpre FILE...'
        return 0
    fi

    for FILENAME in "$@"; do
        if [[ ! -f "$FILENAME" && ! -d "$FILENAME" ]]; then
            log.w "file or directory \"$FILENAME\" does not exist!"
            continue
        else
            NEW_FILENAME=`basename $FILENAME`
            NEW_FILENAME=`dirname $FILENAME`/${NEW_FILENAME#*] }  # double ## will remove all prefixes
            log.i "$FILENAME => $NEW_FILENAME"
            mv -- "$FILENAME" "$NEW_FILENAME"
        fi
    done
    return 0
}

prefix() {
    if [[ -z "$1" ]]; then
        # no param -- show help message
        log.w 'Usage: prefix PREFIX FILE...'
        return 0
    fi

    local PREFIX="$1"
    shift
    log.d "prefix set: $PREFIX"

    for FILENAME in "$@"; do
        if [[ ! -f "$FILENAME" && ! -d "$FILENAME" ]]; then
            log.w "file or directory \"$FILENAME\" does not exist!"
            continue
        else
            NEW_FILENAME=`dirname $FILENAME`/$PREFIX`basename $FILENAME`
            log.i "$FILENAME => $NEW_FILENAME"
            mv -- "$FILENAME" "$NEW_FILENAME"
        fi
    done
    return 0
}
