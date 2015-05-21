#!/bin/sh
#
#  pjsub --interact ./go_k.sh
#
#PJM -L "node=8"
#PJM -L "elapse=00:05:00"
#PJM -S

. /work/system/Env_base
export LD_LIBRARY_PATH=/opt/klocal/lib:${LD_LIBRARY_PATH}

PROG=../bin/ffb_mini
export PARALLEL=8

mpiexec -np 8 ${PROG} 2 2 2 46 color_partsize=2000 reorder_ndiv=10 unroll=on \
 | tee les3x.log.P0001 \
 && ./check.py master/les3x.log les3x.log
