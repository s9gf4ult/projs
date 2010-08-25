#!/bin/bash
MY=( "192.168.0.9" "192.168.0.10" )
ACCEPTED_FOR_PROXY=( "192.168.0.48" "192.168.0.213" "192.168.0.10" )
APT_PROXY_LIST=(" 192.168.0.48" "192.168.0.9" )
PUBLIC_PORTS=( "22" "139" "631" "445" "873" )

protect () {
    eval $@ && return 0 ||
    (stop ; echo failed: $@ ; exit 1)
}



start () {
	for myip in ${MY[*]};do
		iptables -P INPUT DROP
		iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
		iptables -A INPUT -p icmp -d ${myip} -j ACCEPT 
		for addr in ${ACCEPTED_FOR_PROXY[*]};do
			iptables -A INPUT -s ${addr} -d ${myip} -p tcp --dport 3128 -j ACCEPT
		done
		for addr in ${APT_PROXY_LIST[*]};do
			iptables -A INPUT -s ${addr} -d ${myip} -p tcp --dport 3142 -j ACCEPT
		done
		iptables -A INPUT -i lo -s 127.0.0.1 -j ACCEPT
		iptables -A INPUT -i lo -s ${myip} -d ${myip} -j ACCEPT
		for ourport in ${PUBLIC_PORTS[*]};do
			iptables -A INPUT -d ${myip} -p tcp --dport ${ourport} -j ACCEPT
		done
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
