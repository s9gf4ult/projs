#!/bin/bash
MY=192.168.0.9
ACCEPTED_FOR_PROXY=( "192.168.0.48" )

protect () {
    eval $@ && return 0 ||
    (stop ; echo failed: $@ && return 1)
}



start () {
    protect iptables -P INPUT DROP
    protect iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
    for addr in ${ACCEPTED_FOR_PROXY};do
        protect iptables -A INPUT -s ${addr} -d ${MY} -p tcp --dport 3128 -j ACCEPT
    done
    protect iptables -A INPUT -i lo -s 127.0.0.1 -j ACCEPT
    protect iptables -A INPUT -i lo -s ${MY} -d ${MY} -j ACCEPT
    protect iptables -A INPUT -d ${MY} -p tcp --dport 22 -j ACCEPT
}

stop () {
    iptables -t filter -F
}

case $1 in
    ("start")
        start;;
    ("stop")
        stop;;
esac
