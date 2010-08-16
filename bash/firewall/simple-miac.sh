#!/bin/bash
MY=192.168.0.9
ACCEPTED_FOR_PROXY=( "192.168.0.48" )
PUBLIC_PORTS=( "22" "139" "631" "445" )

protect () {
    eval $@ && return 0 ||
    (stop ; echo failed: $@ ; exit 1)
}



start () {
    iptables -P INPUT DROP
    iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
    for addr in ${ACCEPTED_FOR_PROXY[*]};do
        iptables -A INPUT -s ${addr} -d ${MY} -p tcp --dport 3128 -j ACCEPT
    done
    iptables -A INPUT -i lo -s 127.0.0.1 -j ACCEPT
    iptables -A INPUT -i lo -s ${MY} -d ${MY} -j ACCEPT
    for ourport in ${PUBLIC_PORTS[*]};do
        iptables -A INPUT -d ${MY} -p tcp --dport ${ourport} -j ACCEPT
    done
}

stop () {
    iptables -t filter -F
    iptables -P INPUT ACCEPT
}

case $1 in
    ("start")
        start;;
    ("stop")
        stop;;
    ("restart")
        stop
        start;;
esac
