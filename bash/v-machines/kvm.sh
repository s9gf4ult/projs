start () {
	modprobe kvm_intel
	chown root:kvm /dev/kvm
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
