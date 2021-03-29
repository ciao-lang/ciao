#!/bin/bash

server="ciao-lang.org"
if [ `hostname` != $server ]; then
    # Execute on server and exit
    exec ssh ciao-lang.org 'bash -s' < $0
fi

# TODO: Changed! See other location at website.hooks.pl
# pushd ~ciao/srv/website/download_db > /dev/null

pushd ~ciao/srv/download_cgi-data/download_db > /dev/null
printf "Download stats\n"
all=`cat *.pl | wc -l`
t=0
for i in dmg rpm deb exe; do
    n=`grep '\.'$i *.pl | wc -l`
    printf "%s = %s %s%%\n" $i $n `perl -e "print $n * 100 / $all"`
    t=$(($t + $n))
done
n=$(($all - $t))
printf "%s = %s %s%%\n" "tar.gz" $n `perl -e "print $n * 100 / $all"`
popd > /dev/null

