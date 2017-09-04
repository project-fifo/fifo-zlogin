#!/usr/bin/bash

. /usbkey/config

echo "[00] The guesswork starts."
conf_admin_mac=$(echo "$admin_nic" | sed 's/00/0/g')
echo "[01] The conf_admin_mac is '$conf_admin_mac'."

case "$conf_admin_mac" in
    aggr*)
        conf_admin_nic="$conf_admin_mac"
        echo "[02] Looks like a aggr, so nic == mac"
        ;;
    *)
        conf_admin_nic=$(dladm show-phys -m -o LINK,ADDRESS | grep "$conf_admin_mac" | awk '{print $1}')
        echo "[02] Looks like a normal nic, so '$conf_admin_mac' => '$conf_admin_nic'"
        ;;
esac
conf_admin_ip=$(ipadm show-addr -o ADDROBJ,ADDR  | grep "^$conf_admin_nic" | awk '{print $2}' | awk -F/ '{print $1}')
echo "[03] The IP of '$conf_admin_nic' should be '$conf_admin_ip'."

conf_fifo_nic=fifo0
echo "[04] Lets see if there is a FiFo nic: '$conf_fifo_nic'."
if ipadm show-addr -o ADDROBJ | grep "^$conf_fifo_nic" > /dev/null
then

    conf_fifo_ip=$(ipadm show-addr -o ADDROBJ,ADDR  | grep "^$conf_fifo_nic" | awk '{print $2}' | awk -F/ '{print $1}')
    echo "[05] We found a IP for '$conf_fifo_nic' => '$conf_fifo_ip' so we'll use this"
    conf_admin_ip=$conf_fifo_ip
else
    echo "[05] No IP found for '$conf_fifo_nic', we'll stick with the admin IP."
fi
