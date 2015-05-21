C=======================================================================
      SUBROUTINE CLRCRS(A,NP,NCRS,IPCRS,NPP,
     *                  LFIX3D,NUMIP,WEIGHT)
C=======================================================================
      IMPLICIT NONE
      INTEGER*4 NP,NCRS
      REAL*4  A(NCRS),WEIGHT(NP)
      INTEGER*4 IPCRS(NCRS),NPP(NP),NUMIP(NP)
      INTEGER*4 IP,K,ICRS,IP2,ICRDIG
      INTEGER*4 LFIX3D(NP)
C
C
CCC   1. SET WEIGHTING FUNCTION FOR COMPUTING AN INNER PRODUCT
C
C
      DO 1100 IP = 1 , NP
          WEIGHT(IP) = 1.E0/(FLOAT(NUMIP(IP))+1.E0)
 1100 CONTINUE
C
      ICRS=0
      DO 2000 IP=1,NP
          DO 2100 K=1,NPP(IP)
              ICRS=ICRS+1
              IP2 =IPCRS(ICRS)
C
              IF(LFIX3D(IP).EQ.1.AND.IP.NE.IP2) A(ICRS)=0.0E0
              IF(LFIX3D(IP).EQ.1.AND.IP.EQ.IP2) A(ICRS)=WEIGHT(IP)
C
              ICRDIG=0
              IF(LFIX3D(IP).NE.1.AND.IP.EQ.IP2) THEN
                  ICRDIG = ICRS
              ENDIF
 2100     CONTINUE
 2000 CONTINUE   
C
      RETURN
      END
