#!/usr/bin/env zsh

rand() {
    head /dev/urandom | tr -dc A-Za-z0-9 | head -c10
}

mc() {
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
    for url in "$@"; do
        if [[ $url =~ "7gogo" ]]; then
            Get-755 $url || break
        elif [[ $url =~ "instagram" ]]; then
            Get-Insta $url || break
        elif [[ $url =~ "twitter" ]]; then
            Get-Tweet $url || break
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
