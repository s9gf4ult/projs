#!/bin/bash
start () {
modprobe vboxdrv 
modprobe vboxnetadp
modprobe vboxnetflt
}

stop(){
	rmmod vboxnetflt
	rmmod vboxnetadp
	rmmod vboxdrv
}
case $1 in
	start)
	start ;;
	stop)
	stop ;;
esac
