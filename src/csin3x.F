C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : CSIN3X                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE CSIN3X(LOCAL,X,Y,Z,NODE,NE,NP,N1,NS,NSP,
     *                  LEB,NB,XNB,YNB,ZNB)
      IMPLICIT NONE
C
      INTEGER*4 LOCAL,NODE,NE,NP,N1,NS,NSP,LEB,NB
      REAL*8    X,Y,Z
      REAL*4    XNB,YNB,ZNB
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N1,NE),
     1          LEB(2,NB),XNB(NB),YNB(NB),ZNB(NB)
C
      DIMENSION LOCAL(NSP,NS,4)
C
      INTEGER*4 IB,IE,IS,IETYPE,I,IA0,IA1,IB0,IB1,NNPE
      INTEGER*4 IP1,IP2,IP3,IP4
      REAL*8 AX,AY,AZ,BX,BY,BZ,CX,CY,CZ,CABS,DOT
C
C
C      CALCULATE ELEMENT'S SURFACE INWARD NORMAL VECTOR WITH 
C     UNIT LENGTH AND SURFACE AREA
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'COSIN3'
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LOCAL (I,IS); NODE NUMBER TABLE  DEFINING ELEMENT'S SURFACES
C          X      (IP) ; X-DIR. COORDINATE         OF NODE
C          Y      (IP) ; Y-DIR. COORDINATE         OF NODE
C          Z      (IP) ; Y-DIR. COORDINATE         OF NODE
C          NODE (I,IE) ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          LEB  (I,IB) ; ELEMENT(1, & SURFACE(2, NUMBER SPECIFYING LIST
C          NB          ; NUMBER OF SURFACES
C
C       (2) OUTPUT
C          XNB    (IB) ; X COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          YNB    (IB) ; Y COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          ZNB    (IB) ; X COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C
C
C    CAL. SURFACE AREA AND NORMAL UNIT VECTOR
C
      DO 100 IB = 1 , NB
         IE = LEB(1,IB)
         IS = LEB(2,IB)
         IF(     NODE(8,IE).GE.1) THEN ! HEX
            IETYPE = 4
         ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
            IETYPE = 3
         ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
            IETYPE = 2
         ELSE                          ! TET
            IETYPE = 1
         ENDIF   
         IF(LOCAL(4,IS,IETYPE).GE.1) THEN      ! QUADRILATERAL
            IA1 = NODE(LOCAL(3,IS,IETYPE),IE)
            IA0 = NODE(LOCAL(1,IS,IETYPE),IE)
            IB1 = NODE(LOCAL(4,IS,IETYPE),IE)
            IB0 = NODE(LOCAL(2,IS,IETYPE),IE)
         ELSE                                  ! TRIANGLE
            IA1 = NODE(LOCAL(2,IS,IETYPE),IE)
            IA0 = NODE(LOCAL(1,IS,IETYPE),IE)
            IB1 = NODE(LOCAL(3,IS,IETYPE),IE)
            IB0 = NODE(LOCAL(1,IS,IETYPE),IE)
         ENDIF   
C
          AX  = X(IA1) - X(IA0)
          AY  = Y(IA1) - Y(IA0)
          AZ  = Z(IA1) - Z(IA0)
C
          BX  = X(IB1) - X(IB0)
          BY  = Y(IB1) - Y(IB0)
          BZ  = Z(IB1) - Z(IB0)
C
          CX = AY*BZ-AZ*BY
          CY = AZ*BX-AX*BZ
          CZ = AX*BY-AY*BX 
          CABS = SQRT(CX*CX+CY*CY+CZ*CZ)
C
          XNB (IB) = REAL(-CX/CABS  )
          YNB (IB) = REAL(-CY/CABS  )
          ZNB (IB) = REAL(-CZ/CABS  )
  100 CONTINUE
C
C
C    CHECK NORMAL VECTOR DIRECTION
C    CHANGE IF THE VECTOR DIRECT OUTWARD
C
C
      DO 200 IB = 1 , NB
          IE = LEB(1,IB)
          IS = LEB(2,IB)
          IF(     NODE(8,IE).GE.1) THEN ! HEX
             IETYPE = 4
             NNPE   = 8
          ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
             IETYPE = 3
             NNPE   = 6
          ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
             IETYPE = 2
             NNPE   = 5
          ELSE                          ! TET
             IETYPE = 1
             NNPE   = 4
          ENDIF   
          AX=0.0D0
          AY=0.0D0
          AZ=0.0D0
          DO 201 I=1,NNPE
             AX = AX + X(NODE(I,IE))
             AY = AY + Y(NODE(I,IE))
             AZ = AZ + Z(NODE(I,IE))
 201      CONTINUE
          AX = AX/DBLE(NNPE)
          AY = AY/DBLE(NNPE)
          AZ = AZ/DBLE(NNPE)
C
          IP1=NODE(LOCAL(1,IS,IETYPE),IE)
          IP2=NODE(LOCAL(2,IS,IETYPE),IE)
          IP3=NODE(LOCAL(3,IS,IETYPE),IE)
          IF((IETYPE.EQ.1            ).OR. ! TRI
     *       (IETYPE.EQ.2.AND.IS.LE.4).OR.
     *       (IETYPE.EQ.3.AND.IS.LE.2))THEN
             BX=(X(IP1)+X(IP2)+X(IP3))/3.0E0
             BY=(Y(IP1)+Y(IP2)+Y(IP3))/3.0E0
             BZ=(Z(IP1)+Z(IP2)+Z(IP3))/3.0E0
          ELSE ! QUAD
             IP4=NODE(LOCAL(4,IS,IETYPE),IE)
             BX=(X(IP1)+X(IP2)+X(IP3)+X(IP4))/4.0E0
             BY=(Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4))/4.0E0
             BZ=(Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4))/4.0E0
          ENDIF
          CX  = AX - BX
          CY  = AY - BY
          CZ  = AZ - BZ
          CABS = SQRT(CX*CX+CY*CY+CZ*CZ)
C
          CX = CX/CABS
          CY = CY/CABS
          CZ = CZ/CABS
          DOT=CX*DBLE(XNB(IB))+CY*DBLE(YNB(IB))+CZ*DBLE(ZNB(IB))
C         AX  = AX - X(NODE(LOCAL(1,IS,IETYPE),IE))
C         AY  = AY - Y(NODE(LOCAL(1,IS,IETYPE),IE))
C         AZ  = AZ - Z(NODE(LOCAL(1,IS,IETYPE),IE))
C
C         DOT=AX*DBLE(XNB(IB))+AY*DBLE(YNB(IB))+AZ*DBLE(ZNB(IB))
          IF(DOT.LE.0.0E0)THEN
              XNB(IB)=-XNB(IB)
              YNB(IB)=-YNB(IB)
              ZNB(IB)=-ZNB(IB)
CC            STOP
          ENDIF   
  200 CONTINUE
C
      RETURN
      END
