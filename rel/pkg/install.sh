#!/usr/bin/bash

. /usbkey/config

if [ -z "$DST" ]
then
    DST="/opt"
fi

#IFACE=`dladm show-phys -m | grep $admin_nic | awk '{print $1}'`
#IP=`ifconfig $IFACE | grep inet | awk '{print $2}'`

DIR=$(dirname "$0");
if [[ "$DIR" = "." ]]
then
    DIR=$(pwd)
fi
BASE=$(basename "$0");

FORCE=false
while getopts ":f" opt; do
    case $opt in
        f)
            FORCE=true
            ;;
    esac
done

(cd "$DST"; uudecode -p "$DIR/$BASE"| tar xf -)
mkdir -p /var/log/fifo_zlogin


## Generate all the needed values
conf_admin_mac=$(echo "$admin_nic" | sed 's/00/0/g')
case "$conf_admin_mac" in
    aggr*)
        conf_admin_nic="$conf_admin_mac"
        ;;
    *)
        conf_admin_nic=$(dladm show-phys -m -o LINK,ADDRESS | grep "$conf_admin_mac" | awk '{print $1}')
        ;;
esac
conf_admin_ip=$(ipadm show-addr -o ADDROBJ,ADDR  | grep "^$conf_admin_nic" | awk '{print $2}' | awk -F/ '{print $1}')

conf_fifo_nic=fifo0
if ipadm show-addr -o ADDROBJ | grep "^$conf_fifo_nic" > /dev/null
then
    conf_fifo_ip=$(ipadm show-addr -o ADDROBJ,ADDR  | grep "^$conf_fifo_nic" | awk '{print $2}' | awk -F/ '{print $1}')
    conf_admin_ip=$conf_fifo_ip
fi

# CONFFILE="${DST}/fifo_zlogin/etc/fifo_zlogin.conf"
# if [ ! -f $CONFFILE ]
# then
#     echo "Creating new configuration from example file."
#     if [[ "$conf_admin_ip" = "" ]]
#     then
#         cp ${CONFFILE}.example ${CONFFILE}
#     else
#         sed "s/^ip = 127.0.0.1:4200/ip=$conf_admin_ip:4200/" ${CONFFILE}.example > ${CONFFILE}
#     fi
# else
#     echo "Please make sure you update your config according to the update manual!"
#     #/opt/local/fifo-sniffle/share/update_config.sh ${CONFFILE}.example ${CONFFILE} > ${CONFFILE}.new &&
#     #    mv ${CONFFILE} ${CONFFILE}.old &&
#     #    mv ${CONFFILE}.new ${CONFFILE}

# fi

mkdir -p "$DST/custom/smf"
cp "$DST/fifo_zlogin/share/epmd.xml" "$DST/custom/smf"
cp "$DST/fifo_zlogin/share/fifo_zlogin.xml" "$DST/custom/smf"

svccfg import "$DST/custom/smf/epmd.xml"
svccfg import "$DST/custom/smf/fifo_zlogin.xml"

exit 0;
