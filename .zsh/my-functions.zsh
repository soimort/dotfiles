#!/usr/bin/env zsh

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
            Get-755 $url
        elif [[ $url =~ "instagram" ]]; then
            Get-Insta $url
        elif [[ $url =~ "twitter" ]]; then
            Get-Tweet $url
        fi
    done
}
