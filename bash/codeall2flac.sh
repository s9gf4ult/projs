#!/bin/zsh
if [ $# -ne 2 ];then
    echo "it must be just 2 args"
    exit 1
fi
if [ ! -d "$1" ];then
    echo "first aurg must be dir"
    exit 2
fi
if [ ! -d "$2" ];then
    echo "second arg must be dir"
    exit 3
fi
RECODECOMD=flac
FROMSUFF=.wav
TOSUFF=.flac
recodeopts="--best --replay-gain"
fromdir="$1"
todir="$2"
(
    cd "$fromdir"
    for fname in **/*.wav;do
	subdirname=$(dirname "$fname")
	pointdirname="${todir}/${subdirname}"
	mkdir -p "$pointdirname"
	pointname=$(basename "$fname" ${FROMSUFF})${TOSUFF}
	if [ -e "${pointdirname}/${pointname}" ];then
	    counter=0
	    while true;do
		if [ ! -e "${pointdirname}/${counter}_${pointname}" ];then
		    export counter
		    break
		fi
		counter=$(($counter + 1))
	    done
	    export pointname="${counter}_${pointname}"
	fi
	echo ":::::::coding $fname >> ${pointdirname}/${pointname}"
	${RECODECOMD} --best --replay-gain "$fname" -o "${pointdirname}/$pointname"
    done
    )


