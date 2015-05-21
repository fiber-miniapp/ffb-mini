C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : SRFEXX                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE SRFEXX(MESURF,MLST,NE,NP,N1,NS,NSP,N2D,
     *                  LPNT,NPNT,LOCAL,NODE,
     *                  LESURF,NESURF,MWRK,IWRK,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LPNT(NPNT),LOCAL(NSP,NS,4),NODE(N1,NE),
     1          LESURF(MLST,MESURF),IWRK(MWRK)
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE SRFEXX: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & /' THE FIRST DIMENSION OF OUTPUT LIST PASSED IS NOT SUFFICIENT'/
      CHARACTER*60 EREXP2
     & /' THE PASSED NODAL POINT LIST CONTAINS ILLEGAL NODE NUMBER   '/
      CHARACTER*60 EREXP3
     & /' NUMBER OF SURFACE ELEMENTS EXCEEDED LIMIT OF               '/
C
      DIMENSION LIST1(4),LIST2(4)
C
C
C      EXTRACT ALL BOUNDARY SURFACES / ALL SURFACES COMPOSED OF A GIVEN
C     GROUP OF NODAL POINTS
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'SURFEX'
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LPNT  (IPNT); NODE NUMBERS    FOR SURFACE EXTRACTION 
C          NPNT        ; NUMBER OF NODES FOR SURFACE EXTRACTION 
C          LOCAL (I,IS); NODE NUMBER TABLE DEFINING ELEMENT SURFACES
C          NODE  (I,IE); NODE NUMBER TABLE BASED ON ELEMENT
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          MESURF      ; THE MAXIMUM NUMBER  OF SURFACE ELEMENTS
C          MLST        ; THE FIRST DIMENSION OF ARRAY LESURF 
C                        MLST SHOULD BE 2 IN THIS VERSION   
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C       (2) OUTPUT
C          LESURF(I,IESURF); THE EXTRACTED SURFACES ARE REPRESENTED
C               BY ELEMENT NUMBER FACING ON THE SURFACE, CONTAINED IN
C               ARGUMENT LESURF(1,IESURF), AND THE LOCAL SURFACE NUMBER
C               IN THE ELEMENT, CONTAINED IN ARGUMENT LESURF(2,IESURF).
C               THE LOCAL SURFACE NUMBER IS THE SAME AS THE PASSED
C               LOCAL SURFACE NODE LIST ARGUMENT 'LOCAL'.
C
C          NESURF      ; NUMBER OF SURFACE ELEMENTS EXTRACTED
C
C
      IERR = 0
C
C CHECK PASSED PARAMETERS
C
      IF(MLST.NE.2) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
      DO 100 IPNT = 1 , NPNT
          IF(LPNT(IPNT).LT.1 .OR. LPNT(IPNT).GT.NP) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2
              WRITE(IUT0,*) 
     &        ' *** POINT NO. = ',IPNT,' *** NODE NO. = ',LPNT(IPNT)
              IERR = 1
          ENDIF
  100 CONTINUE
      IF(IERR.EQ.1) RETURN
C
      NESURF = 0
C
C EXTRACT ALL THE SURFACES COMPOSED OF GIVEN SET OF NODES
C
C
C EXTRACT ALL THE SURFACES COMPOSED OF GIVEN SET OF NODES
C
      DO 200 IP=1,NP
          IWRK(IP)=0
  200 CONTINUE
C
      DO 300 IPNT=1,NPNT
          IP = LPNT(IPNT)
          IWRK(IP)=1
  300 CONTINUE
C
      DO 400 IE = 1 , NE
C
         IF(NESURF+6.GT.MESURF) THEN
            WRITE(IUT0,*) ERMSGB
            WRITE(IUT0,*) EREXP3, MESURF
            IERR   = 1
            RETURN
         ENDIF
C
         IF(     NODE(8,IE).GE.1) THEN ! HEX
            IETYPE = 4
            NSURF  = 6
         ELSE IF(NODE(6,IE).GE.1) THEN ! PRS  
            IETYPE = 3
            NSURF  = 5
         ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
            IETYPE = 2
            NSURF  = 5
         ELSE                          ! TET
            IETYPE = 1
            NSURF  = 4
         ENDIF   
C
         DO 401 IS=1,NSURF
            DO 402 I=1,N2D
               II = LOCAL(I,IS,IETYPE)
               IF(II.GE.1) THEN
                  IPF = NODE(II,IE)
                  IF(IWRK(IPF).EQ.0) GOTO 401
               ENDIF
 402        CONTINUE
            NESURF = NESURF +1
            LESURF(1,NESURF) = IE
            LESURF(2,NESURF) = IS
 401     CONTINUE   
C
 400  CONTINUE
C
C REMOVE INNER SURFACE
C
      DO 500 J=1,NESURF
          IWRK(J)=0
  500 CONTINUE
C
      DO 610 J1=1,NESURF-1
         IF(IWRK(J1).EQ.1) GOTO 610
         IE1=LESURF(1,J1)
         IS1=LESURF(2,J1)
         IF(     NODE(8,IE1).GE.1) THEN ! HEX
            IETYPE1 = 4
         ELSE IF(NODE(6,IE1).GE.1) THEN ! PRS
            IETYPE1 = 3
         ELSE IF(NODE(5,IE1).GE.1) THEN ! PYR
            IETYPE1 = 2
         ELSE                           ! TET
            IETYPE1 = 1
         ENDIF   
         LIST1(1) = NODE(LOCAL(1,IS1,IETYPE1),IE1)
         LIST1(2) = NODE(LOCAL(2,IS1,IETYPE1),IE1)
         LIST1(3) = NODE(LOCAL(3,IS1,IETYPE1),IE1)
         IF(LOCAL(4,IS1,IETYPE1).GE.1) THEN
            LIST1(4) = NODE(LOCAL(4,IS1,IETYPE1),IE1)
         ELSE
            LIST1(4) = 0
         ENDIF   
         DO 600 J2=J1+1,NESURF
            IF(IWRK(J2).EQ.1) GOTO 600
            IE2=LESURF(1,J2)
            IS2=LESURF(2,J2)
            IF(     NODE(8,IE2).GE.1) THEN ! HEX
               IETYPE2 = 4
            ELSE IF(NODE(6,IE2).GE.1) THEN ! PRS
               IETYPE2 = 3
            ELSE IF(NODE(5,IE2).GE.1) THEN ! PYR
               IETYPE2 = 2
            ELSE
               IETYPE2 = 1                 ! TET
            ENDIF
            LIST2(1) = NODE(LOCAL(1,IS2,IETYPE2),IE2)
            LIST2(2) = NODE(LOCAL(2,IS2,IETYPE2),IE2)
            LIST2(3) = NODE(LOCAL(3,IS2,IETYPE2),IE2)
            IF(LOCAL(4,IS2,IETYPE2).GE.1) THEN
               LIST2(4) = NODE(LOCAL(4,IS2,IETYPE2),IE2)
            ELSE
               LIST2(4) = 0
            ENDIF   
C
            IMATCH=0
            CALL MATCH4(LIST1,LIST2,IMATCH)
            IF(IMATCH.EQ.1) THEN
               IWRK(J1)=1
               IWRK(J2)=1
               GOTO 610
            ENDIF   
 600     CONTINUE
 610  CONTINUE   
C
      NBUF=NESURF
      NESURF=0
      DO 700 J=1,NBUF
          IF(IWRK(J).EQ.1)GOTO 700
          NESURF=NESURF+1
          LESURF(1,NESURF)=LESURF(1,J)
          LESURF(2,NESURF)=LESURF(2,J)
  700 CONTINUE
C   
      RETURN
      END
