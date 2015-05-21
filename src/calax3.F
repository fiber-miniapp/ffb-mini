C=======================================================================
      SUBROUTINE CALAX3(A, S, AS, NP, NE, NCRS, IPCRS, NPP,
     *                  N,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  RX,RY,IUT0,IERR,
     *                  JUNROL,NPPMAX,NCRS2,TS,TA,ITPCRS)
C=======================================================================
#include "timing.h"
      IMPLICIT NONE
C
      INTEGER NP, NE, NCRS
      REAL*4 A(NCRS), S(NP), AS(NP)
      INTEGER IPCRS(NCRS), NPP(NP)
      INTEGER IP, K, ICRS, IP2
      REAL*4  BUF
C
      INTEGER MAXBUF,IDUM
      INTEGER N,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,IUT0,IERR
      REAL*4  RX,RY
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM)
      DIMENSION IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      DIMENSION RX(0:N,ME),RY(0:N,ME)
C
C     [FULL UNROOL]
      INTEGER*4 JUNROL
      INTEGER*4 NPPMAX,NCRS2,ITPCRS(NCRS2)
      REAL*4    TS(0:NP),TA(NCRS2)
C      
C     [IN:MID NODE COLORING]
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE CALAXC: FATAL      ERROR REPORT   ; RETURNED'/
C
C
C     CALCULATE THE PRODUCT OF MATRIX A AND VECTOR X IN PRESSURE EQUATION 
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                              2009.12.01 RIST
C
C          OPERATION COUNTS:   FLOP /ELEMENT
C          DATA LOADINGS   :   WORDS/ELEMENT
C                           (  WORDS CONTIGUOUSLY,
C                              WORDS BY 4-WORD STRIDE, AND
C                              WORDS BY LIST )
C
C     ARGUMENT LISTINGS
C
C       (1) INPUT
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C
C          NCRS        ; NUMBER OF NONZERO ELEMENTS IN MATRIX
C                        OF CRS FORMAT
C          A     (ICRS); NODE-WISE COEFFICIENT MATRIX IN CRS FORMAT
C          S       (IP); GLOBAL FORCE VECTOR
C          IPCRS (ICRS); NODE NO. TABLE BASED ON CRS FORMAT
C          NPP     (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C
C          IPART       ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C          LDOM  (IDOM); NEIBERING SUB-DOMAIN NUMBER
C          NBPDOM(IDOM); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                       SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                       LDOM(IDOM)
C          NDOM        ; NUMBER OF THE NERIBERING SUB-DOMAINS
C          IPSLF (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                           NEIBERING SUB-DOMAIN, LDOM(IDOM)
C          IPSND (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                           TASK'S RESIDUALS.
C          MBPDOM      ; THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                       BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURRENCE
C
C       (2) OUTPUT
C          AS      (IP); GLOBAL SOLUTION VECTOR (PROVIDE INITIAL GUESS)
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C
C       (3) WORK
C          RX    (I,IE); USED IN DDCOMX 
C          RY    (I,IE); USED IN DDCOMX
C
C
      TIME_START(TM_CALAX3_CAL)
      IERR=0
C
      MAXBUF = NE*(N+1)
C
      DO 10 IP=1,NP
         AS(IP)=0.0E0
 10   CONTINUE   
C
      IF (JUNROL.EQ.0) GOTO 500
C
CC
CC    FULL UNROL
CC
      TS(0)=0.0E0
      DO 100 IP=1,NP
         TS (IP)=S(IP)
 100  CONTINUE
C
      IF (NPPMAX.GT.30) GOTO 500
C
CC
CC    FULL UNROL : NPPMAX <= 30
CC
#ifndef __AIX__
!ocl CACHE_SECTOR_SIZE(1,11)
!ocl CACHE_SUBSECTOR_ASSIGN(TS)
!!ocl UNROLL(0)
#endif
      DO 210 IP=1,NP
         BUF=0.0E0
         ICRS=(IP-1)*30
         BUF=BUF+  TA(ICRS+ 1)*TS(ITPCRS(ICRS+ 1))
     &            +TA(ICRS+ 2)*TS(ITPCRS(ICRS+ 2))
     &            +TA(ICRS+ 3)*TS(ITPCRS(ICRS+ 3))
     &            +TA(ICRS+ 4)*TS(ITPCRS(ICRS+ 4))
     &            +TA(ICRS+ 5)*TS(ITPCRS(ICRS+ 5))
     &            +TA(ICRS+ 6)*TS(ITPCRS(ICRS+ 6))
     &            +TA(ICRS+ 7)*TS(ITPCRS(ICRS+ 7))
     &            +TA(ICRS+ 8)*TS(ITPCRS(ICRS+ 8))
     &            +TA(ICRS+ 9)*TS(ITPCRS(ICRS+ 9))
     &            +TA(ICRS+10)*TS(ITPCRS(ICRS+10))
     &            +TA(ICRS+11)*TS(ITPCRS(ICRS+11))
     &            +TA(ICRS+12)*TS(ITPCRS(ICRS+12))
     &            +TA(ICRS+13)*TS(ITPCRS(ICRS+13))
     &            +TA(ICRS+14)*TS(ITPCRS(ICRS+14))
     &            +TA(ICRS+15)*TS(ITPCRS(ICRS+15))
     &            +TA(ICRS+16)*TS(ITPCRS(ICRS+16))
     &            +TA(ICRS+17)*TS(ITPCRS(ICRS+17))
     &            +TA(ICRS+18)*TS(ITPCRS(ICRS+18))
     &            +TA(ICRS+19)*TS(ITPCRS(ICRS+19))
     &            +TA(ICRS+20)*TS(ITPCRS(ICRS+20))
     &            +TA(ICRS+21)*TS(ITPCRS(ICRS+21))
     &            +TA(ICRS+22)*TS(ITPCRS(ICRS+22))
     &            +TA(ICRS+23)*TS(ITPCRS(ICRS+23))
     &            +TA(ICRS+24)*TS(ITPCRS(ICRS+24))
     &            +TA(ICRS+25)*TS(ITPCRS(ICRS+25))
     &            +TA(ICRS+26)*TS(ITPCRS(ICRS+26))
     &            +TA(ICRS+27)*TS(ITPCRS(ICRS+27))
     &            +TA(ICRS+28)*TS(ITPCRS(ICRS+28))
     &            +TA(ICRS+29)*TS(ITPCRS(ICRS+29))
     &            +TA(ICRS+30)*TS(ITPCRS(ICRS+30))
         AS(IP)=AS(IP)+BUF
 210  CONTINUE
#ifndef __AIX__
!ocl END_CACHE_SUBSECTOR
!ocl END_CACHE_SECTOR_SIZE
#endif
C
      GOTO 900
CC
CC    ORIGINAL
CC
 500  CONTINUE
      ICRS=0
      DO 510 IP=1,NP
         BUF=0.0E0
         DO 520 K=1,NPP(IP)
            ICRS=ICRS+1
            IP2=IPCRS(ICRS)
            BUF=BUF+A(ICRS)*S(IP2)
 520     CONTINUE   
         AS(IP)=AS(IP)+BUF
 510  CONTINUE
C
 900  CONTINUE
      TIME_STOP(TM_CALAX3_CAL)
C
      TIME_START(TM_CALAX3_COM)
      IDUM = 1
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            AS,AS,AS,NP,IUT0,IERR,RX,RY,MAXBUF)
      TIME_STOP(TM_CALAX3_COM)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
      RETURN
      END
