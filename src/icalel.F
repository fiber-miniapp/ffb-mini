      SUBROUTINE ICALEL(N1,NE,NP,NODE,U,V,W,UE,VE,WE)
C
      IMPLICIT REAL*4(A-H,O-Z)
C
      DIMENSION U(NP),V(NP),W(NP),
     *          NODE(N1,NE),
     *          UE(NE),VE(NE),WE(NE)
C
C   PREPARE ELEMENT TABLE
C
      DO 100 IE=1,NE
         IF(NODE(8,IE).GE.1) THEN ! HEX
             NNPE = 8
         ELSE IF(NODE(6,IE).GE.1) THEN ! WED
            NNPE = 6
         ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
            NNPE = 5
         ELSE                   ! TET
            NNPE = 4
         ENDIF
         UE(IE)=0.0E0
         VE(IE)=0.0E0
         WE(IE)=0.0E0
         DO 101 I=1,NNPE
             UE(IE) = UE(IE) + U(NODE(I,IE))/FLOAT(NNPE)
             VE(IE) = VE(IE) + V(NODE(I,IE))/FLOAT(NNPE)
             WE(IE) = WE(IE) + W(NODE(I,IE))/FLOAT(NNPE)
 101     CONTINUE
 100  CONTINUE
C     
      RETURN
      END
