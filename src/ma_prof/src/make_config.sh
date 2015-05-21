#!/bin/sh

# Copyright (C) 2014-2015 RIKEN AICS
# This library is released under the terms of the MIT license.
# http://fiber-miniapp.mit-license.org/2014-2015

CONFIG=config.h

DEFAULT_MAX_SECTIONS=20
DEFAULT_OUTPUT=maprof_output.yaml

FUJITSU_AUTO_PARALLEL=0

CheckFujitsuAutoParallel() {
        case $1 in
                *frt|*fcc|*FCC|*frtpx|*fccpx|*FCCpx)
                        echo $2 | grep -q -E '(\s|^)-K(\S+,)?(parallel|visimpact)(\s|,|$)'
                        if [ $? -eq 0 ]; then
                                FUJITSU_AUTO_PARALLEL=1
                        fi
                        ;;
                *)
                        ;;
        esac
}

SetCompilerMacros() {
        case $1 in
                C)
                        m_compiler="MAPROF_CC"
                        m_version="MAPROF_CC_VERSION"
                        m_flags="MAPROF_CFLAGS"
                        ;;
                F)
                        m_compiler="MAPROF_FC"
                        m_version="MAPROF_FC_VERSION"
                        m_flags="MAPROF_FFLAGS"
                        ;;
                CXX)
                        m_compiler="MAPROF_CXX"
                        m_version="MAPROF_CXX_VERSION"
                        m_flags="MAPROF_CXXFLAGS"
                        ;;
                *)
                        echo "Unknown lang: $!"
                        exit 1
                        ;;
        esac
                        
	eval compiler=\"\$$2\"
	eval flags=\"\$$3\"
	version=`./check_version.sh "$compiler"`

        CheckFujitsuAutoParallel "$compiler" "$flags"

        # " -> \"
 	compiler=`echo $compiler  | sed -e 's/[\]*"/\\\"/g'`
 	flags=`echo $flags  | sed -e 's/[\]*"/\\\"/g'`

	cat <<- EOF >> $CONFIG
	#define $m_compiler "$compiler"
	#define $m_version "$version"
	#define $m_flags "$flags"
	EOF
}

if [ -s $CONFIG ]; then
        mv $CONFIG $CONFIG.old
fi

if [ "$MAPROF_C" != "" ]; then
        SetCompilerMacros "C" $MAPROF_C
fi

if [ "$MAPROF_F" != "" ]; then
        SetCompilerMacros "F" $MAPROF_F
fi

if [ "$MAPROF_CXX" != "" ]; then
        SetCompilerMacros "CXX" $MAPROF_CXX
fi

cat << EOF >> $CONFIG
#define MAPROF_MAX_SECTIONS ${MAX_SECTIONS:-$DEFAULT_MAX_SECTIONS}
#define MAPROF_OUTPUT "${OUTPUT:-$DEFAULT_OUTPUT}"
EOF

if [ $FUJITSU_AUTO_PARALLEL -eq 1 ]; then
        echo "#define FUJITSU_AUTO_PARALLEL" >> $CONFIG
fi
