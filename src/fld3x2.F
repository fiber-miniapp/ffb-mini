      SUBROUTINE FLD3X2(IMODE,ME,NE,NP,NEX,N1,
     *                  FXYZ,FE,NODE,DNXI,DNYI,DNZI, 
     *                  IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION NEX(8)
      DIMENSION FXYZ(3,NP),FE(NE)
      DIMENSION NODE(N1,NE),
     1          DNXI(N1,ME), DNYI(N1,ME), DNZI(N1,ME)
C
C
C      CALCULATE VELOCITY FIELD CHARACTERISTICS
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'FIELD3'
C                                              2009.01.13 Y.YAMADE
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE TYPE OF FIELD DATA AS FOLLOWS
C                   1 --- ELEMENT DIVERGENT
C                   NOTE THAT CURRENTLLY ONLY IMODE=1 IS SUPPORTED. 
C
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          NEX(I)      ; INCLUDES NUMBER OF ELEMENTS AND NUMBER OF LOCAL NODES
C                        AS FOLOOWS
C          NEX(1)      ; NUMBER OF TET.    ELEMENTS
C          NEX(2)      ; NUMBER OF PYRAMID ELEMENTS
C          NEX(3)      ; NUMBER OF WEGDE   ELEMENTS
C          NEX(4)      ; NUMBER OF HEX.    ELEMENTS
C          NEX(5)      ; NUMBER OF LOCAL NODES IN A TET.    ELEMENT (=4)
C          NEX(6)      ; NUMBER OF LOCAL NODES IN A PYRAMID ELEMENT (=5)
C          NEX(7)      ; NUMBER OF LOCAL NODES IN A WEGDE   ELEMENT (=6)
C          NEX(8)      ; NUMBER OF LOCAL NODES IN A HEX.    ELEMENT (=8)
C
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C
C          NODE(I,IE)  ; NODE NO. TABLE BASED ON ELEMENT
C          DNXI(I,IE)  ; ELEMENT CENTER VALUE OF NX
C          DNYI(I,IE)  ; ELEMENT CENTER VALUE OF NY
C          DNZI(I,IE)  ; ELEMENT CENTER VALUE OF NZ
C
C      (2) OUTPUT
C          FE    (IE)  ; FIELD DATA
C
C
      IF(IMODE.NE.1) THEN
          WRITE(IUT0,*) 'INVALID VALUE FOR IMODE: ERROR'
          IERR=1
          RETURN
      ENDIF
C
      NEHEX=NEX(4)
C
C
C     (1) CALCULATE DIVERGENT
C
CC==== HEX. ====     
C
C     OPERATION COUNTS:  47 FLOP /ELEMENT
C     DATA LOADINGS   :  48 WORDS/ELEMENT
C                      (  0 WORDS CONTIGUOUSLY,
C                        24 WORDS BY STRIDE, AND
C                        24 WORDS BY LIST )
C
      DO 140 IE = 1, NEHEX
         FE(IE) = DNXI(1,IE)*FXYZ(1,(NODE(1,IE)))
     &           +DNXI(2,IE)*FXYZ(1,(NODE(2,IE)))
     &           +DNXI(3,IE)*FXYZ(1,(NODE(3,IE)))
     &           +DNXI(4,IE)*FXYZ(1,(NODE(4,IE)))
     &           +DNXI(5,IE)*FXYZ(1,(NODE(5,IE)))
     &           +DNXI(6,IE)*FXYZ(1,(NODE(6,IE)))
     &           +DNXI(7,IE)*FXYZ(1,(NODE(7,IE)))
     &           +DNXI(8,IE)*FXYZ(1,(NODE(8,IE)))
C
     &           +DNYI(1,IE)*FXYZ(2,(NODE(1,IE)))
     &           +DNYI(2,IE)*FXYZ(2,(NODE(2,IE)))
     &           +DNYI(3,IE)*FXYZ(2,(NODE(3,IE)))
     &           +DNYI(4,IE)*FXYZ(2,(NODE(4,IE)))
     &           +DNYI(5,IE)*FXYZ(2,(NODE(5,IE)))
     &           +DNYI(6,IE)*FXYZ(2,(NODE(6,IE)))
     &           +DNYI(7,IE)*FXYZ(2,(NODE(7,IE)))
     &           +DNYI(8,IE)*FXYZ(2,(NODE(8,IE)))
C
     &           +DNZI(1,IE)*FXYZ(3,(NODE(1,IE)))
     &           +DNZI(2,IE)*FXYZ(3,(NODE(2,IE)))
     &           +DNZI(3,IE)*FXYZ(3,(NODE(3,IE)))
     &           +DNZI(4,IE)*FXYZ(3,(NODE(4,IE)))
     &           +DNZI(5,IE)*FXYZ(3,(NODE(5,IE)))
     &           +DNZI(6,IE)*FXYZ(3,(NODE(6,IE)))
     &           +DNZI(7,IE)*FXYZ(3,(NODE(7,IE)))
     &           +DNZI(8,IE)*FXYZ(3,(NODE(8,IE)))
  140 CONTINUE
C
      RETURN
      END
