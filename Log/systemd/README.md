# My Dumb Logging

Log `dmesg` to a permanent place.

    # cp -v *.service /etc/systemd/system/
    # systemctl enable mylog

Log files shall be available at:

* `/var/log/my/dmesg.log`
