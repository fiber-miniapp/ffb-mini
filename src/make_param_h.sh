#!/bin/sh

file_in=$1

nline=`wc -l $file_in | awk '{print $1}'`

cat << EOF
integer, parameter :: NLINE = $nline
integer, parameter :: LINE_LEN = 60
character(LINE_LEN) :: param_lines(NLINE) = [ character(LINE_LEN) :: &
EOF

cat $file_in | awk '\
{
        if (NR > 1) printf ", &\n";
        printf "\"%s\"", $0;
}
END { print " ]" }'
