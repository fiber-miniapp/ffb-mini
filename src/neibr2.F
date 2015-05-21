C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : NEIBR2                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE NEIBR2(NODE,NE,NP,
     *  N0,N1,
     *  MAXE,MAXP,MAXEP,MAXPP,
     *  IENP,JENP,NEP,IPNP,NPP,NPPMAX,IUT0,IERR)
      IMPLICIT NONE
      INTEGER NE,NP,N0,N1,MAXE,MAXP,MAXEP,MAXPP,IUT0,IERR
      INTEGER NODE(N1,NE)
      INTEGER IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP)
      INTEGER IPNP(MAXPP,NP),NPP(NP)
      INTEGER NPPMAX
      INTEGER IE,I,IP,IEP,IPP,IPR,INCERT,N
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE NEIBR2: FATAL      ERROR OCCURENCE; RETURNED'/
      CHARACTER*60 EREXP1
     & / ' NUMBER OF ELEMENTS ADJACENT TO 1 NODE    EXCEEDED LIMIT OF' /
      CHARACTER*60 EREXP2
     & / ' NUMBER OF NODES    ADJACENT TO 1 NODE    EXCEEDED LIMIT OF' /
      CHARACTER*60 EREXP3
     & / ' NUMBER OF ELEMENTS ADJACENT TO 1 ELEMENT EXCEEDED LIMIT OF' /
C
C
C      MAKE NEIBERING RELATION SPECIFYING LIST VECTOR
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1; THIS SUBROUTINE IS APPLICABLE TO MIXED-ELEMENT MESH
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE = 002
C                  AS TO THE FIRST  FIGURE,
C                   0 --- IPNP , NPP WILL NOT BE MADE
C                   1 --- IPNP , NPP WILL     BE MADE
C                   2 --- IPNP , NPP WILL     BE MADE ( ORDER OF IP )
C                  AS TO THE SECOND FIGURE,
C                   0 --- IENE , NEE WILL NOT BE MADE
C                   1 --- IENE , NEE WILL     BE MADE
C                  AS TO THE THIRD  FIGURE,
C                   0 --- LIST       WILL NOT BE MADE
C                   1 --- LIST       WILL     BE MADE
C                 NOTE 1 ; IENP , JENP , AND NEP WILL ALWAYS BE MADE
C                 NOTE 2 ; IPNP MUST BE MADE IN ORDER TO MAKE LIST
C                 NOTE 3 ; AS AN UNAUTHORIZED OPTION, -IMODE IS
C                         SUPPORTED. IF A NEGATIVE VALUE OF IMODE IS
C                         SPECIFIED, THE PROCESS OF MAKING IENP,JENP AND
C                         NEP WILL BE SKIPPED AND THE LISTS CORRESPON-
C                         DING TO /IMODE/ WILL BE MADE.
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          (ARGUMENT N0 AND N1 ADDED BY RIST 2009.12.01)
C          N0           ; THE FIRST AND SECOND DIMENSION OF ARRAY LIST
C          N1           ; THE FIRST DIMENSION OF ARRAY NODE
C          MAXE        ; THE MAXIMUM NUMBER OF ELEMENTS
C          MAXP        ; THE MAXIMUM NUMBER OF    NODES
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          MAXPP       ; THE FIRST DIMENSION OF ARRAY IPNP
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MAXEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MAXEP,IP) WILL BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,MAXE.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          IPNP(IPP,IP); ADJACENT NODE    NUMBER TO NODE    IP
C                      ( IF NPP(IP).LT.MAXPP , THEN IPNP(NPP(IP)+1,IP),
C                       IPNP(MAXPP,IP) WILL BE SET TO AN IMAGINARY
C                       NODE    NO. BETWEEN NP+1,MAXP.)
C          NPP     (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C          LIST(J,I,IE); THE POSITION IN IPNP(IPP,NODE(I,IE))
C                       OF NODE(J,IE) I.E. IPNP(LIST(J,I,IE),NODE(I,IE))
C                       IS EQUAL TO NODE(J,IE)
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  INODE=2 version for MINIAPP  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IERR = 0
C
      N = N0
C
C      (1) MAKE IENP(IEP,IP) , NEP(IP)
C
C
C INITIAL
C
      DO 110 IP = 1 , NP
          NEP(IP) = 0
          IF(MAXE.GT.NE) THEN
          DO 100 IEP = 1 , MAXEP
              IENP(IEP,IP) = MOD(IEP-1+MAXEP*(IP-1),MAXE-NE)+1+NE
              JENP(IEP,IP) = MOD(IEP-1             ,N      )+1
  100     CONTINUE
          ENDIF
  110 CONTINUE
C
C ELEMENT LOOP
C
      DO 130 IE = 1 , NE
          DO 120 I = 1 , N
              IP = NODE(I,IE)
              IF(IP.EQ.0) GO TO 120
              NEP(IP) = NEP(IP)+1
              IF(NEP(IP).GT.MAXEP) THEN
                  WRITE(IUT0,*) ERMSGB
                  WRITE(IUT0,*) EREXP1, MAXEP
                  IERR = 1
                  RETURN
              ENDIF
              IENP(NEP(IP),IP) = IE
              JENP(NEP(IP),IP) = I
  120     CONTINUE
  130 CONTINUE
C
C
C      (2) MAKE IPNP(IPP,IP) , NPP(IP)
C
C
C INITIAL
C
      DO 210 IP = 1 , NP
          NPP(IP) = 0
          IF(MAXP.GT.NP) THEN
          DO 200 IPP = 1 , MAXPP
              IPNP(IPP,IP) = MOD(IPP-1+MAXPP*(IP-1),MAXP-NP)+1+NP
  200     CONTINUE
          ENDIF
  210 CONTINUE
C
C
      DO 360 IP = 1 , NP
          DO 350 IEP = 1 , NEP(IP)
              IE = IENP(IEP,IP)
              DO 340 I = 1 , N
                  IPR    = NODE(I,IE)
                  IF(IPR.EQ.0) GO TO 340
                  INCERT = NPP(IP)+1
                  DO 310 IPP = 1 , NPP(IP)
                      IF(IPNP(IPP,IP).EQ.IPR) GO TO 340
                      IF(IPNP(IPP,IP).GT.IPR) THEN
                          INCERT = IPP
                          GO TO 320
                      ENDIF
  310             CONTINUE
  320             CONTINUE
                  NPP(IP) = NPP(IP)+1
                  IF(NPP(IP).GT.MAXPP) THEN
                      WRITE(IUT0,*) ERMSGB
                      WRITE(IUT0,*) EREXP2, MAXPP
                      IERR = 1
                      RETURN
                  ENDIF
                  DO 330 IPP = NPP(IP) , INCERT+1 , -1
                      IPNP(IPP,IP) = IPNP(IPP-1,IP)
  330             CONTINUE
                  IPNP(INCERT,IP) = IPR
  340         CONTINUE
  350     CONTINUE
  360 CONTINUE
C
      NPPMAX=0
      DO 600 IP=1,NP
         NPPMAX=MAX(NPPMAX,NPP(IP))
 600  CONTINUE
C
      RETURN
      END
