#! /bin/zsh

/etc/init.d/cupsd stop
apt-get remove --force-yes foo2zjs
apt-get autoclean
rmmod usblp
echo "blacklist usblp" >>/etc/modprobe.d/blacklist.conf
/etc/init.d/cupsd start
echo "А теперь быстра перезагружай компьютер, и пробуй печатать яаа сказаааал быстрааааа !!!"