C=======================================================================
      SUBROUTINE MKCRS(NP,MPP,NCRS,NPP,IPNP,NPP2,IPCRS,IERR)
C=======================================================================
      IMPLICIT NONE
      INTEGER NP,  MPP, NCRS, IERR
      INTEGER NPP(NP), IPNP(MPP,NP), NPP2(NP), IPCRS(NCRS)
      INTEGER IP, ICRS, K
C
C     CREATE ARRAYS FOR COMPRESSED ROW STRAGE (CRS) FORMAT
C                                              2009.12.01 RIST
C
C     ARGUMENT LISTINGS
C
C       (1) INPUT
C          NP          ; NUMBER OF TOTAL    NODES
C          MPP         ; THE FIRST DIMENSION OF ARRAY IPNP
C          NPP     (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C          NCRS    ; NUMBER OF NONZERO ELEMENTS IN MATRIX OF CRS FORMAT
C          IPNP(IPP,IP); ADJACENT NODE    NUMBER TO NODE    IP
C
C       (2) OUTPUT
C          NPP2    (IP); BASE INDEX OF CRS FORMAT FOR EACH ROW
C          IPCRS (ICRS); NODE NO. TABLE BASED ON CRS FORMAT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C
C       (3) WORK
C
C
      IERR=0
C
      NPP2(1)=0
      DO 225 IP = 2 , NP
          NPP2(IP)=NPP2(IP-1)+NPP(IP-1)
 225  CONTINUE   
CC    NCRS=NPP2(NP)+NPP(NP)
C
      ICRS=0
      DO 226 IP = 1 , NP
          DO 227 K = 1 , NPP(IP)
              ICRS=ICRS+1
              IPCRS(ICRS)=IPNP(K,IP)
 227      CONTINUE   
 226  CONTINUE   
C
      RETURN
      END
