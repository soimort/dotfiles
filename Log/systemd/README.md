# My Dumb Logging

Log `dmesg` and GPU error state to a permanent place - so that in case of a kernel hardlock (or GPU hang) and hard reset becomes unavoidable, I would be able to see these logs after rebooting.

    # cp -v *.service /etc/systemd/system/
    # systemctl enable mylog
    # systemctl enable mylog-gpu

Log files shall be available at:

* `/var/log/my/dmesg.log`
* `/var/log/my/gpu.log`
