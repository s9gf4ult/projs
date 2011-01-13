#!/bin/bash
MY_IP=192.168.0.9
PROXY_IP=172.16.10.151
PORT=3128
die(){
	echo $1
	exit 1
}
check_ip_string(){
	if echo $1 | grep -e "^[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\(\/[0-9]\{1,2\}\)\{0,1\}$" > /dev/null;then
		[ $(echo $1 | sed -e "s/^\([0-9]\{1,3\}\)\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\(\/[0-9]\{1,2\}\)\{0,1\}$/\1/") -ge 256 ] && return 1
		[ $(echo $1 | sed -e "s/^[0-9]\{1,3\}\.\([0-9]\{1,3\}\)\.[0-9]\{1,3\}\.[0-9]\{1,3\}\(\/[0-9]\{1,2\}\)\{0,1\}$/\1/") -ge 256 ] && return 1
		[ $(echo $1 | sed -e "s/^[0-9]\{1,3\}\.[0-9]\{1,3\}\.\([0-9]\{1,3\}\)\.[0-9]\{1,3\}\(\/[0-9]\{1,2\}\)\{0,1\}$/\1/") -ge 256 ] && return 1
		[ $(echo $1 | sed -e "s/^[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.\([0-9]\{1,3\}\)\(\/[0-9]\{1,2\}\)\{0,1\}$/\1/") -ge 256 ] && return 1
		return 0
	else
		echo "Строка не соответствует формату"
		return 1
	fi
}

open(){
	while true;do
		check_ip_string $1 || return 1
		iptables -t nat -A PREROUTING --src $1 --dst $MY_IP -p tcp --dport $PORT -j DNAT --to-destination $PROXY_IP || return 1
		iptables -t nat -A POSTROUTING --src $1 --dst $PROXY_IP -p tcp --dport $PORT -j SNAT --to-source $MY_IP || return 1
		shift 
		[[ $# -eq 0 ]] && break
	done
	return 0
}
close(){
	while true;do
		check_ip_string $1 || return 1
		iptables -t nat -D PREROUTING --src $1 --dst $MY_IP -p tcp --dport $PORT -j DNAT --to-destination $PROXY_IP || return 1
		iptables -t nat -D POSTROUTING --src $1 --dst $PROXY_IP -p tcp --dport $PORT -j SNAT --to-source $MY_IP || return 1
		shift
		[[ $# -eq 0 ]] && break
	done
	return 0
}
do_help(){
	echo 'call ::
	proxy open [host address] [network address]
	proxy close [host address] [network address]
	proxy close all
	proxy show'
}

if [ $1 = "--help" ];then
	do_help
elif [ $1 = "open" ];then
	shift
	open $@ || die "Open was not passed"
elif [ $1 = "close" ];then
	if [ $2 = "all" ];then
		all=$(iptables -t nat -L PREROUTING | grep -e "^DNAT\ *tcp\ *--\ *[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\ *$MY_IP\ *tcp\ *dpt:$PORT\ *to:$PROXY_IP" | sed -e "s/^DNAT\ *tcp\ *--\ *\([0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\).*/\1/" | sed -e "/^$/d")
		if [[ -n $all ]];then
			close $all
		fi
	else
		shift 
		close $@ || die "Close was not passed"
	fi
elif [ $1 = "show" ];then
	iptables -t nat -L PREROUTING | grep -e "^DNAT\ *tcp\ *--\ *[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\(\/[0-9]\{1,2\}\)\{0,1\}\ *$MY_IP\ *tcp\ *dpt:$PORT\ *to:$PROXY_IP" | sed -e "s/^DNAT\ *tcp\ *--\ *\([0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\(\/[0-9]\{1,2\}\)\{0,1\}\).*/\1/"
else
	echo 'param1 is not equal to "close" or "open"'
	exit 1
fi
