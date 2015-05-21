      SUBROUTINE LUMPEX(N1,NE,NP,NEX,NODE,MELM,E,CM)
      IMPLICIT NONE
      
***** DEFINE ARGUMENTS *****
      INTEGER N1,NE,NP,NEX(12),NODE,MELM
      REAL E, CM
      DIMENSION NODE(N1,NE)
      DIMENSION E(MELM)
      DIMENSION CM(NP)
      
***** OBJECTS *****
      INTEGER IP,IN, JN, NN, IE
      INTEGER NEHEX, NHEX, NSKIP4
      INTEGER IELM, IELM0
C
       NEHEX=NEX(4)
        NHEX=NEX(8)
      NSKIP4=NEX(12)

      DO 100 IP=1,NP
          CM(IP)=0.0
  100 CONTINUE     

      IELM0=1
********************
***** HEX LOOP *****
********************
      DO IE=1,NEHEX
         DO IN=1,NHEX
            NN=NODE(IN,IE)
            DO JN=1,NHEX
               IELM=IELM0+(IN-1)+NHEX*(JN-1)   
               CM(NN) =CM(NN)+E(IELM)
            ENDDO
         ENDDO
         IELM0=IELM0+NSKIP4
      ENDDO
      
      RETURN
      END
