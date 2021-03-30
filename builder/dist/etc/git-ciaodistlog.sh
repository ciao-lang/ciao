#!/bin/bash

# Show a git log with statistics about public/private files based on
# NODISTRIBUTE directory marks.
#
# Usage:
#
#   Go to your git repo and execute. As optional argument you may use
#   the number of commits to skip.
#
# Jose F. Morales

gitdir=.git
tmpd=/tmp/tmp-ciaodistlog-$$
f_allcommits=$tmpd/allcommits.txt
f_all=$tmpd/allfiles.txt
f_allpub=$tmpd/allfilespub.txt
f_nodist=$tmpd/nodist.txt
f_delta=$tmpd/deltafiles.txt
f_deltapub=$tmpd/deltafilespub.txt

mkdir -p "$tmpd"
trap 'rm -rf "$tmpd"' EXIT

git --git-dir=$gitdir log --format=%H > $f_allcommits
total=`cat $f_allcommits | wc -l`

skip=0
last=$total
if [ $# -ge 1 ]; then skip=$1; fi
if [ $# -ge 2 ]; then last=$2; fi

# Use 'tail -r' (mac) or 'tac' (linux) for reverse
if command -v tac > /dev/null 2>&1; then
    revtail=tac
else
    revtail="tail -r"
fi

cat <<EOF
<html>
<head>
<meta charset="utf-8" /> 
<style>
@import url(https://fonts.googleapis.com/css?family=Lato:300italic,700italic,300,700);

body {
  padding:50px;
  font:14px/1.5 Lato, "Helvetica Neue", Helvetica, Arial, sans-serif;
  color:#777;
  font-weight:300;
}
table {
  font:14px/1.5 Lato, "Helvetica Neue", Helvetica, Arial, sans-serif;
}

.hideextra { white-space: nowrap; overflow: hidden; text-overflow:ellipsis; width:400px; }
</style>
</head>
<body>
<h1>List of public and private changes</h1>
<p>Repository: `git config --get remote.origin.url`</p>
<table>
<tr><th></th><th>Commit</th><th>Public</th><th>Private</th><th>Log</th></tr>
EOF

i=0
prevh=
$revtail $f_allcommits | \
    while read h; do
        i=$((i+1))
        if [ $i -le $skip ]; then continue; fi
        if [ $i -gt $last ]; then break; fi
        git --git-dir=$gitdir ls-tree --name-only -r $h > $f_all
        n_all=`cat $f_all | wc -l`
        cat $f_all | grep -e '/NODISTRIBUTE$' | while read p; do printf "^%s\\(/\\|$\\)\n" `dirname $p`; done > $f_nodist
        cat $f_all | grep -v -f $f_nodist > $f_allpub
        n_allpub=`cat $f_allpub | wc -l`
        git --git-dir=$gitdir diff-tree --no-commit-id --name-only -r $h $prevh > $f_delta
        n_delta=`cat $f_delta | wc -l`
        cat $f_delta | grep -v -f $f_nodist > $f_deltapub
        n_deltapub=`cat $f_deltapub | wc -l`
        n_allpriv=$(($n_all - $n_allpub))
        n_deltapriv=$(($n_delta - $n_deltapub))
        log=`git --no-pager --git-dir=$gitdir show -s --format="(%an) %s" $h`
        if [ $n_deltapriv -eq 0 ]; then privstyle=; else privstyle=" style=\"color:red\""; fi
        printf "<tr><td>%s/%s</td><td><a href=\"http://phabricator.ciao-lang.org/rC%s\">%s</a></td><td>%s (Δ%s)</td><td %s>%s (Δ%s)</td><td><div class=\"hideextra\">%s</div></td></tr>\n" $i $total $h $h $n_allpub $n_deltapub "$privstyle" $n_allpriv $n_deltapriv "$log"
        prevh=$h
    done

cat <<EOF
</table>
</body>
</html>
EOF
