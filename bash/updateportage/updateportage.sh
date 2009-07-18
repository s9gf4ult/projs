#!/bin/bash
############################################################
# for this script is necessary you specify PORTDIR in your #
# make.conf						   #
############################################################
curpath=$(pwd)/$(dirname $0)
portdir=$(egrep "^\ *PORTDIR\ *=\ *" /etc/make.conf | sed -e 's/^\ *PORTDIR\ *=\ *"*\([^"]*\)"*/\1/')
echo $portdir
(
    cd $portdir
    git checkout -f gentoo.org
    git pull origin gentoo.org
)

for patchname in *.patch;do
    echo "processing: $patchname"
    esubd=$(dirname $(egrep "^---\ a\/" $patchname|sed -e "s/^---\ a\/\(.*\)$/\1/"))
    efname=$(basename $(egrep "^---\ a\/" $patchname|sed -e "s/^---\ a\/\(.*\)$/\1/"))
    echo "esubd is $esubd"
    echo "efname is $efname"
    cd $portdir
    patch -p1 <"$curpath/$patchname"
    cd $esubd
    ebuild $efname manifest
done

	