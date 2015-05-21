#!/bin/sh

PROG=../bin/ffb_mini

mpiexec -np 8 $PROG 2 2 2 46 | tee les3x.log.P0001 \
 && ./check.py master/les3x.log les3x.log
