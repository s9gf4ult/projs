#!/bin/bash
MY=192.168.0.9


start () {
    iptables -P INPUT DROP
    iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
    iptables -A INPUT -s 192.168.0.48 -d ${MY} -p tcp --dport 3128 -j ACCEPT
    iptables -A INPUT -i lo -s 127.0.0.1 -j ACCEPT
    iptables -A INPUT -i lo -s ${MY} -o ${MY} -j ACCEPT
    iptables -A INPUT -d ${MY} -p tcp --dport 22 -j ACCPET
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
