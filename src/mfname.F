C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    MFNAME                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE MFNAME(FILEGL,FILEPT,IPART,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      CHARACTER*60 FILEGL,FILEPT
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE MFNAME: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' AN ILLEGAL SUB-DOMAIN NUMBER HAS BEEN SPECIFIED           ' /
      CHARACTER*60 EREXP2
     & / ' LENGTH OF THE PASSED FILE NAME EXCEEDED THE LIMIT OF 54   ' /
C
C
      CHARACTER* 6 INDEX
      CHARACTER* 7 INDEX2
C
C
C      MAKE A FILE NAME FOR A SUB-DOMAIN
C
C
C     NOTES; THIS SUBROUTINE GENERATES AND RETURNS A FILE NAME FOR
C           A SUB-DOMAIN, BASED ON THE GIVEN FILE NAME SPECIFYING THE
C           GLOBAL DOMAIN. THE SUB-DOMAIN FILE NAME TO BE GENERATED
C           WILL BE 'GLOBAL-DOMAIN-FILENAME'.PXXXX, WHERE XXXX IS A
C           4-DIGIT INTEGER NUMBER INDICATING THE SUB-DOMAIN NUMBER
C           (STARTING WITH 0). AN ERROR WILL BE REPORTED IF THE PASSED
C           SUB-DOMAIN NUMBER IS LESS THAN 0 OR GREATER THAN 9999.
C
C            AN EXAMPLE OF THE FILE NAME CONVENTION IS GIVEN BELOW:
C               FILE NAME OF GLOBAL DOMAIN = 'DOMAIN'
C               FILE NAME OF SUB-DOMAIN 24 = 'DOMAIN.P0024'
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          FILEGL      ; FILE NAME INDICATING THE GLOBAL-DOMAIN
C           NOTES ; THE LENGTH OF THE GLOBAL-DOMAN FILE NAME MUST BE
C                  54 OR SHORTER, TO ALLOW SPACE FOR THE SUB-DOMAIN
C                  INDEX TO BE GENERATED AND ADDED TO THE ORIGINAL
C                  FILE NAME.
C
C          IPART       ; SUB-DOMAIN NUMBER ( FROM 0 TO 9999 )
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C
C       (2) OUTPUT
C          FILEPT      ; FILE NAME INDICATING THE SUB-DOMAIN
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C
      IERR = 0
C
C
C CHECK PASSED PARAMETER
C
C
      IF(IPART.LT.0 .OR. IPART.GT.9999) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
C
C DETERMINE LENGTH OF THE GLOBAL FILE NAME
C
C    
CC    DO 10 ICHAR = 1 , 54
      DO 10 ICHAR = 1 , 53
          IF(FILEGL(ICHAR:ICHAR).EQ.' ') THEN
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
C GENERATE SUB-DOMAIN INDEX
C
C    
      IF(IPART.LE.9)    THEN
          WRITE(INDEX,'(5H.P000,I1)') IPART
          GO TO 30
      ENDIF
C
      IF(IPART.LE.99)   THEN
          WRITE(INDEX,'(4H.P00, I2)') IPART
          GO TO 30
      ENDIF
C
      IF(IPART.LE.999)  THEN
          WRITE(INDEX,'(3H.P0,  I3)') IPART
          GO TO 30
      ENDIF
C
      IF(IPART.LE.9999) THEN
          WRITE(INDEX,'(2H.P,   I4)') IPART
          GO TO 30
      ENDIF
C
      IF(IPART.LE.99999) THEN
          WRITE(INDEX2,'(2H.P,   I5)') IPART
          GO TO 40
      ENDIF
C

   30 CONTINUE
C
C
C ADD GENERATED SUB-DOMAIN INDEX
C
C
      FILEPT = FILEGL(1:NCHAR)//INDEX
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
C
   40 CONTINUE
C
C
C ADD GENERATED SUB-DOMAIN INDEX
C
C
      FILEPT = FILEGL(1:NCHAR)//INDEX2
      RETURN
      END
