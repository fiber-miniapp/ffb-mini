C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : RFNAME                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE RFNAME(FILE1,FILE2,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      CHARACTER*60 FILE1,FILE2
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE RFNAME: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP2
     & / ' LENGTH OF THE PASSED FILE NAME EXCEEDED THE LIMIT OF 58   ' /
C
C
CC    CHARACTER* 6 INDEX
C
C
C      MAKE A FILE NAME FOR A SUB-DOMAIN
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          FILE1       ; FILE NAME (IN)
C           NOTES ; THE LENGTH OF THE GLOBAL-DOMAN FILE NAME MUST BE
C                  58 OR SHORTER, TO ALLOW SPACE FOR THE SUB-DOMAIN
C                  INDEX TO BE GENERATED AND ADDED TO THE ORIGINAL
C                  FILE NAME.
C
C       (2) OUTPUT
C          FILE2       ; FILE NAME (OUT)
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C
      IERR = 0
C
C
C DETERMINE LENGTH OF THE GLOBAL FILE NAME
C
C    
      DO 10 ICHAR = 1 , 58
          IF(FILE1(ICHAR:ICHAR).EQ.' ') THEN
              NCHAR = ICHAR-1
              GO TO 20
          ENDIF
   10 CONTINUE
      WRITE(IUT0,*) ERMSGB
      WRITE(IUT0,*) EREXP2
      IERR = 1
      RETURN
C
   20 CONTINUE
C
C
C ADD GENERATED SUB-DOMAIN INDEX
C
C
      FILE2 = FILE1(1:NCHAR)//".R"
C
C     NOTE: '//' ARE THE STARTING CHARACTERS OF A COMMENT STATEMENT
C    IN C++ LANGUAGE. A C-PREPROCESSOR IN SOME PLATFORM STRIPS OUT
C    ALL (NAMELY, BOTH C AND C++) COMMENT STATEMENTS. THUS, IF YOU
C    USE A C-PREPROCESSOR IN SUCH A PLATFORM, BE SURE TO AVOID THIS
C    HAPPENING. THIS CAN BE GENERALLY DONE BY ADDING '-C' OPTION WHEN
C    CALLING YOUR C-PREPROCESSOR.
C
C                                        APRIL 12TH, 2001, CHISACHI KATO
C
C
      RETURN
      END
