      SUBROUTINE CALUEL(N1,NE,NP,NEX,NODE,
     *                  U,V,W,UE,VE,WE,IERR)
      IMPLICIT NONE
C
      INTEGER*4 N1,NE,NP,NEX(12)
      INTEGER*4 NODE(N1,NE)
      INTEGER*4 IERR
      REAL*4    U(NP),V(NP),W(NP),UE(NE),VE(NE),WE(NE)
C               
      INTEGER*4 NEHEX, IE,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      REAL*4    COEF
C
      IERR=0
C
C   == HEX. ==  
C
      NEHEX=NEX(4)
C
      COEF=1.0E0/8.0E0
      DO 240 IE = 1 ,NEHEX
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          UE(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)+U(IP6)+U(IP7)+U(IP8))
          VE(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)+V(IP6)+V(IP7)+V(IP8))
          WE(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)+W(IP6)+W(IP7)+W(IP8))
 240  CONTINUE
C
      RETURN
      END
