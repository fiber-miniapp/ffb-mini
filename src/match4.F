C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : MATCH4                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
        SUBROUTINE MATCH4(LIST1,LIST2,IMATCH)
        INTEGER LIST1(4), LIST2(4)
        INTEGER IMATCH 
C       [INPUT]  LIST1,LIST2 :VECTORS TO BE CHECK 
C       [OUTPUT] IMATCH      :0 NOT MATCH ,1 MATCH     
C 
       IMATCH = 0
C
       DO 100 I=1,4 
           IBUF=1 	 
           DO 200 J=1,4 
               IBUF=IBUF*(LIST1(I)-LIST2(J))
 200       CONTINUE
           IF(IBUF.NE.0)RETURN
 100   CONTINUE
C
        IMATCH = 1
        RETURN
        END
