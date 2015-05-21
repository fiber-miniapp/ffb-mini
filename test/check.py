#!/usr/bin/env python

import sys
import math
import re

STEP = 100
NPROC = 8
EPS = 1.0e-4

def usage():
    print 'usage: %s step master_file file' % sys.argv[0]


re_STEP = re.compile(r" *STEP +([1-9][0-9]*) ")
re_HST1 = re.compile(r" +HST1= *([-]?\d\.\d+E[-+]\d+) ")

re_STAT = re.compile(r" +Pave= *([-]?\d\.\d+E[-+]\d+) Vave= *([-]?\d\.\d+E[-+]\d+) Vmax= *([-]?\d\.\d+E[-+]\d+)")


def read_vals(file):
    f = open(file, 'r')
    step = 0
    vals = {}
    for line in f:
        m = re_STEP.match(line)
        if (m): 
            step = int(m.group(1))
        if (step == STEP):
            m = re_STAT.match(line)
            if (m):
                vals['p_ave'] = float(m.group(1))
                vals['v_ave'] = float(m.group(2))
                vals['v_max'] = float(m.group(3))
    if not vals:
        print "checking values not found in file=%s at step=%d" % (file, STEP)
        sys.exit(1)
    return vals


def check(key, vals0, vals):
    max_diff = 0.0
    for i in range(NPROC):
        diff = math.fabs((vals[i][key] - vals0[i][key]) / vals0[i][key])
        if diff > max_diff: max_diff = diff

    if max_diff < EPS:
        print "%s: relative error = %e ... OK" % (key, max_diff)
        return 0
    else:
        print "%s: relative error = %e ... NG" % (key, max_diff)
        return 0


if __name__ == '__main__':

    if len(sys.argv) != 3:
        usage()
        sys.exit(1)

    print
    print 'Checking result ...'

    vals0 = []
    vals = []

    for i in range(NPROC):
        file0 = sys.argv[1] + ".P%04d" % (i + 1)
        file  = sys.argv[2] + ".P%04d" % (i + 1)
        vals0.append(read_vals(file0)) 
        vals.append(read_vals(file)) 

    ndiff = 0
   #ndiff = ndiff + check('p_ave', vals0, vals)
    ndiff = ndiff + check('v_ave', vals0, vals)
    ndiff = ndiff + check('v_max', vals0, vals)

    sys.exit(ndiff)
