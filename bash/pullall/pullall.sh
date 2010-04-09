#!/bin/bash
doit () {
	cd ~
	ssh-add -D
	ssh-add $1
	cd $2
	git $3 origin master:master
}


case $1 in
	"pull")
	doit ~/.ssh/emacs emacs/ pull
	doit ~/.ssh/org org/ pull
	doit ~/.ssh/proj projs/ pull
	doit ~/.ssh/configs configs/ pull ;;

	"push")
	doit ~/.ssh/emacs emacs/ push
	doit ~/.ssh/org org/ push
	doit ~/.ssh/proj projs/ push
	doit ~/.ssh/configs configs/ push ;;
esac



