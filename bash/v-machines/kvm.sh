start () {
	modprobe kvm_intel || return 1
	sleep 1
	chown root:kvm /dev/kvm || return 2
	chmod 0660 /dev/kmv
}
stop () {
	rmmod kvm_intel
	rmmod kvm
}
case $1 in
start)
start ;;
stop)
stop ;;
esac
