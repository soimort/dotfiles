export proxy_default='http://127.0.0.1:8087/'
export dev_port_default='8000'
export backup_media='Titania'

fork() { (setsid "$@" &); }

read_password() {
    echo -ne 'Password: '
    read -s password && echo
}

server() {
    ruby -run -e httpd . -p "$@"
}
alias serv="server $dev_port_default"

py_server() {
    python -m http.server "$@"
}
alias pyserv="py_server $dev_port_default"

assignProxy() {
    local -a PROXY_ENV
    PROXY_ENV=('http_proxy' 'HTTP_PROXY' 'https_proxy' 'HTTPS_PROXY' 'ftp_proxy' 'FTP_PROXY' 'rsync_proxy' 'RSYNC_PROXY' 'all_proxy' 'ALL_PROXY')
    for envVar in $PROXY_ENV; do
        export $envVar=$1
    done
}

setProxy() {
    assignProxy $proxy_default
    export no_proxy='localhost,127.0.0.1'
    export NO_PROXY=$no_proxy
    echo -e "Proxy environment variable set."
}

unsetProxy() {
    assignProxy ''
    echo -e "Proxy environment variable unset."
}

fixUnsavedHTML() {
    i=
    for i in .org.chromium.*; do
        echo -ne "$i : "
        f=`grep -o '"./[^"]*_files/' $i | uniq 2>/dev/null`
        if [ -n "$f" ]; then
            j=${f:3:-7}.html
            mv -f $i $j
            echo "moved to \"$j\""
        else
            rm -f $i
            echo "removed"
        fi
    done 2>/dev/null
}

rsyncp() {
    rsync -r -t -v --progress --delete -s $@
}

backup() {
    if [ -n "$1" ]; then
        local src="/home/$USER/$1"
        local dest="/run/media/$USER/$backup_media/$1"
        local destp="/run/media/$USER/$backup_media/$1/.."
        if [[ -r $src && -d $dest ]]; then
            src=$(cd $src; pwd)
            dest=$(cd $dest; pwd)
            echo "Backup data $src => $dest"
            echo -ne "Destination will be overwritten. Continue? [Y/n] "
            local choice
            read choice
            if [ "$choice" = "Y" ]; then
                if [ ${src##*/} != ${dest##*/} ]; then
                    echo "Source and dest do not share a common name. Sync prevented."
                else
                    rsync -r -t -v --progress --delete -s $src $destp
                fi
            fi
        fi
    fi
}

connect() {
    COOKIE=`mktemp`
    curl -c $COOKIE --data 'username=mort.yao%40gmail.com&password=qwer4321' -k https://service.thecloud.net/service-platform/login/
    curl -b $COOKIE -k https://service.thecloud.net/service-platform/connect/
    rm -f $COOKIE
}
