C=======================================================================
      SUBROUTINE E2PMATR(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                   M8,M9,NE,NCRS,AE2,AP,LTAB2,IERR)
C=======================================================================
      IMPLICIT NONE
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
      INTEGER M8, M9, NE, NCRS, IERR
      REAL*4  AE2(M9,M8,NE)
      INTEGER LTAB2(M9,M8,NE)
      REAL*4  AP(NCRS)
C
      INTEGER ICRS, J1, J2, IE, IES, IEE, ICOLOR,ICPART
C
C     CALCULATE NODE-BASE MATRIX COEFFICIENT FOR COMPRESSED ROW STRAGE
C     (CRS) FORMAT FROM ELEMENT-BASE MATRIX COEFFICIENT
C                                              2009.12.01 RIST
C
C     ARGUMENT LISTINGS
C
C       (1) INPUT
C          M8     ; THE SECOND DIMENSION OF ARRAY AE2,LTAB2
C          M9     ; THE FIRST DIMENSION OF ARRAY AE2,LTAB2
C          NE     ; NUMBER OF TOTAL ELEMENTS
C          NCRS   ; NUMBER OF NONZERO ELEMENTS IN MATRIX OF
C                        CRS FORMAT
C          AE2  (J1,J2,IE); ELEMENT-BASE MATRIX COEFFICIENT
C          LTAB2(J1,J2,IE); CRS INDEX TABLE FOR NODE-BASE MATRIX
C                           COEFFICIENT
C
C       (2) OUTPUT
C          AP  (ICRS); NODE-BASE MATRIX COEFFICIENT
C          IERR      ; RETURN CODE TO REPORT ERROR OCCURENCE
C
C       (3) WORK
C
C
      IERR=0
C
C-----------------------------------------------------------------------
C::   [1] INITILIZE
C
      DO 1000 ICRS=1,NCRS
              AP(ICRS)=0.0E0
 1000 CONTINUE
C
C::   [2] MAKE MATRIX
C
C-----------------------------------------------------------------------
!CDIR NODEP
*POPTION INDEP(AP)
      DO 2400 ICOLOR=1,NCOLOR(4)
!ocl norecurrence(AP)
      DO 2410 ICPART=1,NCPART(ICOLOR,4)
          IES=LLOOP(ICPART  ,ICOLOR,4)
          IEE=LLOOP(ICPART+1,ICOLOR,4)-1
!ocl nosimd
!ocl noswp
          DO 2420 IE=IES,IEE
              DO 2430 J1=1,8
                  DO 2440 J2=1,8
                      ICRS=LTAB2(J1,J2,IE)
                      AP(ICRS)=AP(ICRS)+AE2(J2,J1,IE)
 2440             CONTINUE
 2430         CONTINUE
 2420     CONTINUE
 2410 CONTINUE
 2400 CONTINUE
C-----------------------------------------------------------------------
 9000 CONTINUE
      RETURN    
      END
