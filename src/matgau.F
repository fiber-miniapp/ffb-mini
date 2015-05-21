C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : MATGAU                                                C
C                                                                      C
C                                       WRITTEN BY Y.GUO               C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE MATGAU(MDIM,NDIM,A,B,XX,IPVT,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
C
      DIMENSION A(MDIM,MDIM),B(MDIM),XX(MDIM),IPVT(MDIM)
C
      DATA EPS / 1.0E-30 /
C
C      SOLVING LINEAR EQUATIONS BY DIRECT METHOD (GAUSS METHOD)
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          MDIM        ; SPECIFIES THE MAXIMUM DIMENSION OF THE MATRIX
C          NDIM        ; SPECIFIES THE DIMENSION OF THE MATRIX
C          A      (I,J); THE MATRIX
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          B        (I); THE RIGHT-HAND SIDE VECTOR
C                       ALSO HOLDS THE SOLUTION VECTOR
C
C       (4) WORK
C          XX       (I); STORE THE SOLUTION VECTOR TEMPORARILY
C          IPVT     (I); STORE THE POSITION OF MATRIX COEFFIENT THAT
C                       HAS THE MAXIMUM ABSOLUTE VALUE TEMPORARILY
C
      IERR = 0
C
      DO 100 J = 1 , NDIM
          IPVT(J) = J
  100 CONTINUE
C
      DO 600 I = 1 , NDIM
C
          AIIMAX = 0.E0
          MAXI   = I
          DO 200 I2 = I , NDIM
              AII = ABS(A(I,IPVT(I2)))
              IF(AIIMAX.LE.AII) AIIMAX = AII
              IF(AIIMAX.EQ.AII) MAXI = I2
  200     CONTINUE
C
          IBUF = IPVT(I)
          IPVT(I) = IPVT(MAXI)
          IPVT(MAXI) = IBUF
C
          IPVTI = IPVT(I)
          AII = A(I,IPVTI)
C
          IF(ABS(AII).GE.EPS) THEN
              RAII = 1.E0/AII
              DO 300 J = I , NDIM
                  A(J,IPVTI) = A(J,IPVTI)*RAII
  300         CONTINUE
              B(IPVTI) = B(IPVTI)*RAII
              DO 500 I2 = I+1 , NDIM
                  IPVTI2 = IPVT(I2)
                  FACT = A(I,IPVTI2)
                  DO 400 J=I,NDIM
                      A(J,IPVTI2) = A(J,IPVTI2)-A(J,IPVTI)*FACT
  400             CONTINUE
                  B(IPVTI2) = B(IPVTI2)-B(IPVTI)*FACT
  500         CONTINUE
          ELSE
              WRITE(IUT0,*)
              WRITE(IUT0,*)' MATGAU: FATAL ERROR OCCURED         '
              WRITE(IUT0,*)'       : DIAGONAL TERM BECAME ZERO   '
              WRITE(IUT0,*)'       : WHEN SOLVING MATRIX EQUATION' 
              WRITE(IUT0,*)'       : WITH GAUSS METHOD           ' 
              IERR = 1
              RETURN
          ENDIF
C
  600 CONTINUE
C
      XX(NDIM) = B(IPVT(NDIM))
      DO 800 I = NDIM-1 , 1 , -1
          IPVTI = IPVT(I)
          BI = B(IPVTI)
          DO 700 J2 = I+1 , NDIM
              BI = BI-A(J2,IPVTI)*XX(J2)
  700     CONTINUE
          XX(I) = BI
  800 CONTINUE
C
      DO 900 I = 1 , NDIM
          B(I) = XX(I)
  900 CONTINUE
C
C
      RETURN
      END
