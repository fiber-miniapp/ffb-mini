#!/bin/sh
#
#  pjsub --interact ./go_fx10.sh
#
#PJM -L "node=8"
#PJM -L "elapse=00:05:00"
#PJM -S

if [ "$SHELL" == "/bin/zsh" ]; then
  module () { eval `/usr/local/share/Modules/bin/modulecmd zsh $*`; }
fi
module load METIS

PROG=../bin/ffb_mini
export PARALLEL=16

mpiexec -np 8 ${PROG} 2 2 2 46 color_partsize=2000 reorder_ndiv=10 unroll=on \
 | tee les3x.log.P0001 \
 && ./check.py master/les3x.log les3x.log
