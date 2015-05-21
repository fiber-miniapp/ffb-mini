      SUBROUTINE LRFNMS(IRFNFF,IRFN,NGRID,
     *                  ME,MP,N1,NE,NP,X,Y,Z,U,V,W,PN,P,NODE,LEACNV,
     *                  MPWALL,NPWALL,LPWALL,UWALL,VWALL,WWALL,
     *                  MPINLT,NPINLT,LPINLT,UINLT,VINLT,WINLT,
     *                  MPFREE,NPFREE,LPFREE,
     *                  MPSYMT,NPSYMT,LPSYMT,
     *                  MPBODY,NPBODY,LPBODY,
     *                  MPINT ,NPINT ,LPINT1,LPINT2,LPINT3,
     *                  IPART ,
     *                  MDOM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  UFIX,VFIX,WFIX,LPFRM,
     *                  CRD,LWRK01,LWRK02,WRK04,NDRFN,NDORG,NODEBK,
     *                  RX,RY,NPB0, 
     *                  NPB1,LPB1,XPB1,YPB1,ZPB1,
     *                  NPB2,LPB2,XPB2,YPB2,ZPB2,
     *                  ITYPOR,ITYPRF,IUT6,IUT0,IERR)
C
      IMPLICIT NONE
C
      INCLUDE "rcapRefiner.inc"
C
      INTEGER*4 IRFNFF,IRFN,NGRID
      INTEGER*4 ME,MP,N1,NE,NP
      REAL*8    X(MP),Y(MP),Z(MP)
      REAL*4    U(MP),V(MP),W(MP),PN(MP),P(ME)
      INTEGER*4 NODE(N1,ME)
      INTEGER*4 IUT6,IUT0,IERR,IERRA
C
      INTEGER*4 MPWALL,NPWALL,LPWALL(MPWALL),
     *          MPINLT,NPINLT,LPINLT(MPINLT),
     *          MPFREE,NPFREE,LPFREE(MPFREE),
     *          MPSYMT,NPSYMT,LPSYMT(MPSYMT),
     *          MPBODY,NPBODY,LPBODY(MPBODY)
      REAL*4    UWALL(MPWALL),VWALL(MPWALL),WWALL(MPWALL),
     *          UINLT(MPINLT),VINLT(MPINLT),WINLT(MPINLT)
C
      INTEGER*4 IPART,MDOM,
     *          MPINT,NPINT,LPINT1(MPINT),LPINT2(MPINT),LPINT3(MPINT),
     *          NDOM,MBPDOM,LDOM(MDOM),NBPDOM(MDOM),
     *          IPSLF(MBPDOM,MDOM),IPSND(MBPDOM,MDOM)
C
      INTEGER*4 NPB0(MDOM),NPB1(MDOM),NPB2(MDOM),
     *          LPB1(MBPDOM,MDOM),LPB2(MBPDOM,MDOM)
      REAL*4    XPB1(MBPDOM,MDOM),XPB2(MBPDOM,MDOM),
     *          YPB1(MBPDOM,MDOM),YPB2(MBPDOM,MDOM),
     *          ZPB1(MBPDOM,MDOM),ZPB2(MBPDOM,MDOM)
C
      INTEGER(KIND=1) :: IETYPE
C
      INTEGER*4 KSEG,KQUD,KHEX
      DATA KSEG / 0/
      DATA KQUD / 4/
      DATA KHEX /12/
C
      INTEGER*4 NBOUN
      PARAMETER(NBOUN=7) 
      INTEGER*4 LBPRTY(NBOUN)
      DATA      LBPRTY( 1) /  4 /   ! INLET
      DATA      LBPRTY( 2) /  6 /   ! MOVING-WALL
      DATA      LBPRTY( 3) /  5 /   ! WALL
      DATA      LBPRTY( 4) /  2 /   ! SYMMETRIC
      DATA      LBPRTY( 5) /  3 /   ! FREE
      DATA      LBPRTY( 6) /  1 /   ! OVERSET
      DATA      LBPRTY( 7) /  7 /   ! BODY
      CHARACTER*20 BGNAME(3)
      DATA         BGNAME( 1) / "GROUP1" /
      DATA         BGNAME( 2) / "GROUP2" /
      DATA         BGNAME( 3) / "GROUP3" /
      CHARACTER*10 CDOM
      CHARACTER*50 CBUF
C
C     [WORK]
      REAL*4    UFIX(MP),VFIX(MP),WFIX(MP)
      INTEGER*4 LPFRM(MP)
      REAL*8    CRD(MP*3)
      REAL*4    RX(ME*8),RY(ME*8)
      INTEGER*4 NODED(8),LEACNV(ME)
      INTEGER*4 LWRK01(ME),LWRK02(MP)
      INTEGER*4 WRK04(MP),NDRFN(ME*8),NDORG(NE*8),NODEBK(8,NE)
      INTEGER(KIND=1) ITYPOR(ME),ITYPRF(ME) 
C
C     [PARAMETER]       
      INTEGER*4 NOFSTP,NOFSTE
      DATA NOFSTP /1/
      DATA NOFSTE /1/
      REAL*4 EPS
      DATA EPS /1.0E-6/
C
C     [WORK]
      INTEGER*4 MAXBUF,IDIM,NPB
      REAL*4    X1,Y1,Z1,DX,DY,DZ,RR
C
C     [COUNTER]
      INTEGER*4 IE,IP,IB,IB1,IB2,NB,IDOM,I,J,NER,NPOLD,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,
     *          NE1,NE2
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE LRFNMS: ERROR OCCURED             ; RETURNED' /
CC      CHARACTER*60 ERMSG1
CC     & /' ## SUBROUTINE LRFNMS: LIST ERROR AT DDCOM0      ; RETURNED' /
CC    CHARACTER*60 ERMSG2
CC   & /' ## SUBROUTINE RFNMSH: CANNOT OBTAIN PARENT NODE ; RETURNED' /
      CHARACTER*60 ERMSG3
     & /' ## SUBROUTINE LRFNMS: INSUFFICIENT MEMORY       ; RETURNED' /
      CHARACTER*60 ERMSG4
     & /' ## SUBROUTINE LRFNMS: INVARIT NODE TABLE        ; RETURNED' /
      CHARACTER*60 ERMSG5
     & /' ## SUBROUTINE LRFNMS: INVARIT ELEMENT TYPE      ; RETURNED' /
      CHARACTER*60 ERMSG6
     & /' ## SUBROUTINE LRFNMS: INVARIT O.S. FRANE NUMBER ; RETURNED' /
C
C
C     ARGUMENT LISTINGS
C     ME     (IN    ); MAX. NUMBER OF TOTAL ELEMENTS 
C     MP     (IN    ); MAX. NUMBER OF TOTAL    NODES 
C     NE     (IN/OUT);      NUMBER OF TOTAL ELEMENTS
C     NP     (IN/OUT);      NUMBER OF TOTAL    NODES
C     N1     (IN    ); DIMENSION OF 'NODE'
C     NODE   (IN/OUT); NODE NO. TABLE BASED ON ELEMENT
C     X      (IN/OUT); X-COORDINATES OF NODES
C     Y      (IN/OUT); Y-COORDINATES OF NODES
C     Z      (IN/OUT); Y-COORDINATES OF NODES
C
C   BOUNDARY CONDITIONS  
C
C    B. WALL BOUNDARY
C     MPWALL (IN    ); MAX. NUMBER OF WALL BOUNDARY NODES
C     NPWALL (IN/OUT); NUMBER OF WALL BOUNDARY NODES
C     LPWALL (IN/OUT); WALL BOUNDARY NODES
C     UWALL  (IN/OUT); WALL BOUNDARY U-VELOCITIES
C     VWALL  (IN/OUT); WALL BOUNDARY V-VELOCITIES
C     WWALL  (IN/OUT); WALL BOUNDARY W-VELOCITIES
C
C    C. MOVING WALL BOUNDARY
C     MPINLT (IN    ); MAX. NUMBER OF INLET BOUNDARY NODES
C     NPINLT (IN/OUT); NUMBER OF INLET BOUNDARY NODES
C     LPINLT (IN/OUT); INLET BOUNDARY NODES
C     UINLT  (IN/OUT); INLET BOUNDARY U-VELOCITIES
C     VINLT  (IN/OUT); INLET BOUNDARY V-VELOCITIES
C     WINLT  (IN/OUT); INLET BOUNDARY W-VELOCITIES
C
C    D. FREE BOUNDARY
C     MPFREE (IN    ); MAX. NUMBER OF FREE BOUNDARY NODES
C     NPFREE (IN/OUT); NUMBER OF FREE BOUNDARY NODES
C     LPFREE (IN/OUT); FREE BOUNDARY NODES
C
C    E. SYMMETRIC BOUNDARY
C     MPSYMT (IN    ); MAX. NUMBER OF SYMMETRIC BOUNDARY NODES
C     NPSYMT (IN/OUT); NUMBER OF SYMMETRIC BOUNDARY NODES
C     LPSYMT (IN/OUT); SYMMETRIC BOUNDARY NODES
C
C    F. BODY BOUNDARY
C     MPBODY (IN    ); MAX. NUMBER OF BODY BOUNDARY NODES
C     NPBODY (IN/OUT); NUMBER OF BODY BOUNDARY NODES
C     LPBODY (IN/OUT); BODY BOUNDARY NODES
C
C    G. INTER-CONNECT BOUNDARY
C     IPART   (IN    ); SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C      LDOM   (IN    ); NEIBERING SUB-DOMAIN NUMBER
C      NBPDOM (IN/OUT); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                       SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                       LDOM(IDOM)
C      NDOM   (IN    ); NUMBER OF THE NERIBERING SUB-DOMAINS
C      IPSLF  (WORK  ); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                       CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                       NEIBERING SUB-DOMAIN, LDOM(IDOM)
C      IPSND  (WORK  ); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                       SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                       TASK'S RESIDUALS.
C      MBPDOM (IN    ); THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                       BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C
C
      IERR=0
      MAXBUF=N1*ME
CC
CCHY [1] MAKE NEIGHBORING DOMAIN LIST
CC
      IF(IPART.GE.1)THEN
         WRITE(IUT6,*)
         WRITE(IUT6,*) ' **LRFNMS** : MAKE NEIGHBORING DOMAIN LIST'
         CALL DDCOM0(LPINT1,LPINT2,LPINT3,NPINT,MDOM,MBPDOM,
     *               LDOM,NBPDOM,NDOM,IPSLF,IPSND,IUT0,IERR)
         CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
         IF(IERRA.NE.0) THEN
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSGC
            IERR=IERRA
            RETURN
         ENDIF
C
         IDIM = 0
         CALL DDCOMX(IPART,IDIM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *               WRK04,WRK04,WRK04,NP,IUT0,IERR,RX,RY,MAXBUF)
         CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
         IF(IERRA.NE.0) THEN
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSGC
            IERR=IERRA
            RETURN
         ENDIF
      ENDIF
C     
CC
CCHY [2] EXTRACT ELEMENTS TO BE REFINED
CC
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **LRFNMS** : EXTRACT ELEMENTS TO BE REFINED'
      CALL EXTRFN(NGRID,ME,N1,NE,NP,NODE,NPBODY,LPBODY,
     *            NPINLT,LPINLT,
     *            LWRK01,LWRK02,LEACNV,NE1,NE2,NDORG,NODEBK,
     *            WRK04,IPART,NDOM,MBPDOM,LDOM,NBPDOM,
     *            IPSLF,IPSND,RX,RY,MAXBUF,IUT0,IERR)
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         IERR=IERRA
         RETURN
      ENDIF
      WRITE(IUT6,*) ' **LRFNMS** : NUM. OF ELEM.     TO BE REFINED',NE1
      WRITE(IUT6,*) ' **LRFNMS** : NUM. OF ELEM. NOT TO BE REFINED',NE2
C
CC
CCHY [3] PRE-SET U,V,W,PN AT B.C. NODES
CC
      DO 1000 IP=1,NP
         UFIX(IP)=0.0
         VFIX(IP)=0.0
         WFIX(IP)=0.0
         LPFRM(IP)=0
 1000 CONTINUE   
C
      DO 1100 IB=1,NPINLT
         IP=LPINLT(IB)
         UFIX(IP)=UINLT(IB)
         VFIX(IP)=VINLT(IB)
         WFIX(IP)=WINLT(IB)
 1100 CONTINUE   
C
      DO 1200 IB=1,NPWALL
         IP=LPWALL(IB)
         UFIX(IP)=UWALL(IB)
         VFIX(IP)=VWALL(IB)
         WFIX(IP)=WWALL(IB)
 1200 CONTINUE   
C
CC
CCHY [4] INITIALIZE OF REFINER
CC
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **LRFNMS** : REFINER INITIALIZATION'
      CALL RCAPINITREFINER(NOFSTP,NOFSTE)
CC
CCHY [5] SET COORDINATES TO REFINER 
CC
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **LRFNMS** : SET COORDINATES TO REFINER'
      DO 2000 IP=1,NP
         CRD(IP*3-2)=X(IP)
         CRD(IP*3-1)=Y(IP)
         CRD(IP*3-0)=Z(IP)
 2000 CONTINUE
      CALL RCAPSETNODE64(NP,CRD,-1,0)
CC
CCHY [6] SET B.C. NODES TO REFINER 
CC
CC
CCHY [6.1] B.C. GROUP-1
CC
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **LRFNMS** : SET B.C. NODES TO REFINER'
C
      NB=0
      DO 3000 IB = 1 , NPINLT
         NB=NB+1
         LWRK01(NB) = LPINLT(IB)
         LWRK02(NB) = LBPRTY(1)
 3000 CONTINUE  
C
      DO 3010 IB = 1 , NPWALL
         NB=NB+1
         LWRK01(NB) = LPWALL(IB)
         LWRK02(NB) = LBPRTY(3)
 3010 CONTINUE  
C
      DO 3020 IB = 1 , NPSYMT
         NB=NB+1
         LWRK01(NB) = LPSYMT(IB)
         LWRK02(NB) = LBPRTY(4)
 3020 CONTINUE  
C
      DO 3040 IB = 1 , NPFREE
         NB=NB+1
         LWRK01(NB) = LPFREE(IB)
         LWRK02(NB) = LBPRTY(5)
 3040 CONTINUE  
C
      CBUF = BGNAME(1)//CHAR(0)
      CALL RCAPAPPENDBNODEVARINT(CBUF,NB,LWRK01,LWRK02)
      WRITE(IUT6,*) ' **LRFNMS** : SET B.C. NODES (1)',NB
CC
CCHY [6.2] B.C. GROUP-2
CC
      DO 3100 IB = 1 , NPBODY
         LWRK01(IB) = LPBODY(IB)
         LWRK02(IB) = LBPRTY(7)
 3100 CONTINUE  
C
      CBUF = BGNAME(2)//CHAR(0)
      CALL RCAPAPPENDBNODEVARINT(CBUF,NPBODY,LWRK01,LWRK02)
      WRITE(IUT6,*) ' **LRFNMS** : SET B.C. NODES (2)',NPBODY
CC
CCHY [6.3] B.C. GROUP-3
CC
      IF(IPART.GE.1)THEN
         NB=0
         DO 3200 IDOM=1,NDOM
            WRITE(CDOM,'(I8.8)') IDOM
            CBUF = BGNAME(3)//CDOM//CHAR(0)
            DO 3210 IB = 1 , NBPDOM(IDOM)
               NB=NB+1
               LWRK01(IB)  = IPSLF(IB,IDOM)
 3210       CONTINUE
C
            CALL RCAPAPPENDBNODEGROUP(CBUF,NBPDOM(IDOM),LWRK01)
 3200    CONTINUE
         WRITE(IUT6,*) ' **LRFNMS** : SET B.C. NODES (3)',NB
      ENDIF    
CC
CCHY [7] SET NODE TABLE TO REFINER 
CC
      DO 4000 IE=1,NE
         DO 4010 I=1,8
            NDORG((IE-1)*8+I)=0
 4010    CONTINUE
 4000 CONTINUE
C
      J=0
      DO 4100 IE=1,NE1
         IF(NODE(8,IE).NE.0) THEN
            IETYPE=RCAP_HEXAHEDRON
            NDORG(J+1)=NODE(1,IE)
            NDORG(J+2)=NODE(2,IE)
            NDORG(J+3)=NODE(3,IE)
            NDORG(J+4)=NODE(4,IE)
            NDORG(J+5)=NODE(5,IE)
            NDORG(J+6)=NODE(6,IE)
            NDORG(J+7)=NODE(7,IE)
            NDORG(J+8)=NODE(8,IE)
            J=J+8
         ELSE IF(NODE(6,IE).NE.0) THEN
            IETYPE=RCAP_WEDGE
            NDORG(J+1)=NODE(1,IE)
            NDORG(J+2)=NODE(2,IE)
            NDORG(J+3)=NODE(3,IE)
            NDORG(J+4)=NODE(4,IE)
            NDORG(J+5)=NODE(5,IE)
            NDORG(J+6)=NODE(6,IE)
            J=J+6
         ELSE IF(NODE(5,IE).NE.0) THEN
            IETYPE=RCAP_PYRAMID
CC
CC NOTE THAT DEFINITION OF PYRMID NODE TABLE IS DIFFERENT
CC IN FFB AND REVOCAP_REFINER        
CC
            NDORG(J+1)=NODE(5,IE)
            NDORG(J+2)=NODE(1,IE)
            NDORG(J+3)=NODE(2,IE)
            NDORG(J+4)=NODE(3,IE)
            NDORG(J+5)=NODE(4,IE)
            J=J+5
         ELSE IF(NODE(4,IE).NE.0) THEN
            IETYPE=RCAP_TETRAHEDRON
            NDORG(J+1)=NODE(1,IE)
            NDORG(J+2)=NODE(2,IE)
            NDORG(J+3)=NODE(3,IE)
            NDORG(J+4)=NODE(4,IE)
            J=J+4
         ELSE
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSG4
            IERR=1
            RETURN
         ENDIF  
C
         ITYPOR(IE)=IETYPE
 4100 CONTINUE
CC
CCHY [8] REFINE ELEMENTS
CC
CC    COUNT NUMBER OF ARRAY
      NER=0
      ITYPRF(1) = -1
      NDRFN (1) = -1
      IE=RCAPREFINEELEMENTMULTI(NE1,ITYPOR,NDORG,NER,ITYPRF,NDRFN)
      IF(NE2+NER.GT.ME) THEN
          WRITE(IUT0,*) ERMSG3
          IERR=1
      ENDIF
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
      ITYPRF(1) = 0
      NDRFN (1) = 0
      IE= RCAPREFINEELEMENTMULTI(NE1,ITYPOR,NDORG,NER,ITYPRF,NDRFN)
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **LRFNMS** : NUMBER OF ELEM. AFTER REFINE',NER+NE2

CC
CCHY [9] COMMIT REFINEMENT
CC
      CALL RCAPCOMMIT()
CC
CCHY [10] SET NEW NODE TABLE
CC
      J=0
      DO 5000 IE=1,NER
         IETYPE=ITYPRF(IE)
C
         IF(IETYPE.EQ.RCAP_HEXAHEDRON) THEN
            NODE(1,IE)=NDRFN(J+1)
            NODE(2,IE)=NDRFN(J+2)
            NODE(3,IE)=NDRFN(J+3)
            NODE(4,IE)=NDRFN(J+4)
            NODE(5,IE)=NDRFN(J+5)
            NODE(6,IE)=NDRFN(J+6)
            NODE(7,IE)=NDRFN(J+7)
            NODE(8,IE)=NDRFN(J+8)
            J=J+8
         ELSE IF(IETYPE.EQ.RCAP_WEDGE) THEN
            NODE(1,IE)=NDRFN(J+1)
            NODE(2,IE)=NDRFN(J+2)
            NODE(3,IE)=NDRFN(J+3)
            NODE(4,IE)=NDRFN(J+4)
            NODE(5,IE)=NDRFN(J+5)
            NODE(6,IE)=NDRFN(J+6)
            NODE(7,IE)=0
            NODE(8,IE)=0
            J=J+6
         ELSE IF(IETYPE.EQ.RCAP_PYRAMID) THEN
CC
CC NOTE THAT DEFINITION OF PYRMID NODE TABLE IS DIFFERENT
CC IN FFB AND REVOCAP_REFINER        
CC
            NODE(1,IE)=NDRFN(J+2)
            NODE(2,IE)=NDRFN(J+3)
            NODE(3,IE)=NDRFN(J+4)
            NODE(4,IE)=NDRFN(J+5)
            NODE(5,IE)=NDRFN(J+1)
            NODE(6,IE)=0
            NODE(7,IE)=0
            NODE(8,IE)=0
            J=J+5
         ELSE IF(IETYPE.EQ.RCAP_TETRAHEDRON) THEN
            NODE(1,IE)=NDRFN(J+1)
            NODE(2,IE)=NDRFN(J+2)
            NODE(3,IE)=NDRFN(J+3)
            NODE(4,IE)=NDRFN(J+4)
            NODE(5,IE)=0
            NODE(6,IE)=0
            NODE(7,IE)=0
            NODE(8,IE)=0
            J=J+4
         ELSE
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSG5
            IERR=1
            RETURN
         ENDIF  
C
 5000 CONTINUE
CC
CCHY [11] SET NEW COORDINATE
CC
      NPOLD=NP
CC    NP=0
      DO 5100 IE=1,NER
         DO 5200 J=1,N1
            IF (NODE(J,IE).GT.NP) NP=NODE(J,IE)
 5200    CONTINUE
 5100 CONTINUE
      WRITE(IUT6,*) ' **LRFNMS** : NUMBER OF NODES AFTER REFINE',NP
C
      IF(NP.GT.MP) THEN
         WRITE(IUT0,*) ERMSG3
         IERR=1
      ENDIF
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF
C
      CALL RCAPGETNODESEQ64(NP, 1,CRD)
C
      DO 5300 IP=1,NP
         X(IP)=CRD(IP*3-2)
         Y(IP)=CRD(IP*3-1)
         Z(IP)=CRD(IP*3-0)
 5300 CONTINUE
CC
CCHY [12] SET NEW B.C. NODE
CC
C
CC
CCHY [12.1] B.C. GROUP-1
CC
      CBUF = BGNAME(1)//CHAR(0)
      NB   = RCAPGETBNODEVARINTCOUNT(CBUF)
      IF(NB.GT.MP) THEN
         WRITE(IUT0,*) ERMSG3
         IERR=1
      ENDIF
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         IERR=IERRA
         RETURN
      ENDIF
C
      CBUF = BGNAME(1)//CHAR(0)
      CALL RCAPGETBNODEVARINT(CBUF,NB,LWRK01,LWRK02)
C
      DO 6000 IB=1,NB
         IP=LWRK01(IB)
         J =LWRK02(IB)
         IF (IP.LE.NPOLD) GOTO 6000
C
         IF(J.EQ.LBPRTY(1)) THEN
            NPINLT=NPINLT+1
            IF(NPINLT.GT.MPINLT) THEN
               WRITE(IUT0,*) ERMSG3
               IERR=1
               GOTO 6050
            ELSE
               LPINLT(NPINLT)=IP 
            ENDIF
         ENDIF
C
         IF(J.EQ.LBPRTY(3)) THEN
            NPWALL=NPWALL+1
            IF(NPWALL.GT.MPWALL) THEN
               WRITE(IUT0,*) ERMSG3
               IERR=1
               GOTO 6050
            ELSE
               LPWALL(NPWALL)=IP 
            ENDIF
         ENDIF
C
         IF(J.EQ.LBPRTY(4)) THEN
            NPSYMT=NPSYMT+1
            IF(NPSYMT.GT.MPSYMT) THEN
               WRITE(IUT0,*) ERMSG3
               IERR=1
               GOTO 6050
            ELSE
               LPSYMT(NPSYMT)=IP 
            ENDIF
         ENDIF
C
         IF(J.EQ.LBPRTY(5)) THEN
            NPFREE=NPFREE+1
            IF(NPFREE.GT.MPFREE) THEN
               WRITE(IUT0,*) ERMSG3
               IERR=1
               GOTO 6050
            ELSE
               LPFREE(NPFREE)=IP 
            ENDIF
         ENDIF
C
 6000 CONTINUE
C
 6050 CONTINUE
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         IERR=IERRA
         RETURN
      ENDIF
C
      WRITE(IUT6,*)
      IF(NB.GT.0)
     *WRITE(IUT6,*) ' **LRFNMS** : TOTAL NUMBER OF  B.C. NODES:',NB
      IF(NPINLT.GT.0)
     *WRITE(IUT6,*) ' **LRFNMS** : NUMBER OF INLET       NODES:',NPINLT
      IF(NPWALL.GT.0)
     *WRITE(IUT6,*) ' **LRFNMS** : NUMBER OF WALL        NODES:',NPWALL
      IF(NPSYMT.GT.0)
     *WRITE(IUT6,*) ' **LRFNMS** : NUMBER OF SYMMETRIC   NODES:',NPSYMT
      IF(NPFREE.GT.0)
     *WRITE(IUT6,*) ' **LRFNMS** : NUMBER OF FREE        NODES:',NPFREE
CC
CCHY [12.2] B.C. GROUP-2
CC
      CBUF = BGNAME(2)//CHAR(0)
      NB = RCAPGETBNODEVARINTCOUNT(CBUF)
      IF(NB.GT.MPBODY) THEN
         WRITE(IUT0,*) ERMSG3
         IERR=1
      ENDIF
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         IERR=IERRA
         RETURN
      ENDIF
      CBUF = BGNAME(2)//CHAR(0)
      CALL RCAPGETBNODEVARINT(CBUF,NB,LWRK01,LWRK02)
C
      DO 6100 IB=1,NB
         IP=LWRK01(IB)
         IF (IP.LE.NPOLD) GOTO 6100
         NPBODY=NPBODY+1
         IF (NPBODY.GT.MPBODY) THEN
            WRITE(IUT0,*) ERMSG3
            IERR=1
            GOTO 6150
         ENDIF
         LPBODY(NPBODY)=IP
 6100 CONTINUE
C
 6150 CONTINUE
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         IERR=IERRA
         RETURN
      ENDIF
C
      IF(NB.GT.0)
     *WRITE(IUT6,*)
     *' **LRFNMS** : NUMBER OF BODY        NODES:',NPBODY
CC
CCHY [12.3] B.C. GROUP-3
CC
      IF(IPART.GE.1) THEN
C     
         DO 6200 IDOM=1,NDOM
C
            IF (IRFN.EQ.1) THEN
               NPB1(IDOM)=0
               DO 6300 IB=1,NBPDOM(IDOM)
                  NPB1(IDOM)=NPB1(IDOM)+1
                  IP=IPSLF(IB,IDOM)               
                  LPB1(NPB1(IDOM),IDOM)=IP
                  XPB1(NPB1(IDOM),IDOM)=X(IP)
                  YPB1(NPB1(IDOM),IDOM)=Y(IP)
                  ZPB1(NPB1(IDOM),IDOM)=Z(IP)              
 6300          CONTINUE
            ENDIF
C
            WRITE(CDOM,'(I8.8)') IDOM
            CBUF = BGNAME(3)//CDOM//CHAR(0)
            NPB = RCAPGETBNODEGROUPCOUNT(CBUF)
            IF(NPB.GT.MBPDOM) THEN
               WRITE(IUT0,*) ERMSG3
               IERR=1
               GOTO 6500
            ENDIF
C
            CALL RCAPGETBNODEGROUP(CBUF,NPB,IPSLF(1,IDOM))
C
            DO 6400 IB=1,NPB
               IP=IPSLF(IB,IDOM)
               IF (IP.LE.NPOLD) GOTO 6400
               NPB1(IDOM)=NPB1(IDOM)+1
               LPB1(NPB1(IDOM),IDOM)=IP
               XPB1(NPB1(IDOM),IDOM)=REAL(X(IP))
               YPB1(NPB1(IDOM),IDOM)=REAL(Y(IP))
               ZPB1(NPB1(IDOM),IDOM)=REAL(Z(IP))
 6400       CONTINUE
 6200    CONTINUE
C
 6500    CONTINUE
         CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
         IF(IERRA.NE.0) THEN
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSGC
            IERR=IERRA
            RETURN
         ENDIF
C
         CALL DDCOM4(NDOM,LDOM,MBPDOM,
     *               NPB1,LPB1,XPB1,YPB1,ZPB1,
     *               NPB2,LPB2,XPB2,YPB2,ZPB2,
     *               MAXBUF,RX,RY,IUT0,IERR)
C
CC
CC    MAKE INTER-CONNECT NODES
CC
         NPINT=0
         DO 6600 IDOM=1,NDOM
C
            DO 6700 IB2=1,NPB2(IDOM)
               LWRK01(IB2)=0
 6700       CONTINUE      
C
            NPB0(IDOM)=0
            DO 6800 IB1=1,NPB1(IDOM)
               X1=XPB1(IB1,IDOM)
               Y1=YPB1(IB1,IDOM)
               Z1=ZPB1(IB1,IDOM)
C
               DO 6850 IB2=1,NPB2(IDOM)
C
                  IF(LWRK01(IB2).EQ.1) GOTO 6850
C
                  DX=X1-XPB2(IB2,IDOM)
                  DY=Y1-YPB2(IB2,IDOM)
                  DZ=Z1-ZPB2(IB2,IDOM)
                  RR=DX*DX+DY*DY+DZ*DZ
C
                  IF(RR.LE.EPS*EPS) THEN
                     NPB0(IDOM)=NPB0(IDOM)+1
                     NPINT =NPINT +1
                     IF(NPINT.GT.MPINT) THEN
                        WRITE(IUT0,*) ERMSG3
                        IERR=1
                        GOTO 6900
                     ENDIF
C
                     LPINT1(NPINT)=LPB1(IB1,IDOM)
                     LPINT2(NPINT)=LDOM(IDOM)
                     LPINT3(NPINT)=LPB2(IB2,IDOM)
                     LWRK01(IB2)=1
                     GOTO 6800
                  ENDIF
C
 6850          CONTINUE
 6800       CONTINUE
 6600    CONTINUE
C
 6900    CONTINUE
         CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
         IF(IERRA.NE.0) THEN
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSGC
            IERR=IERRA
            RETURN
         ENDIF
C
         WRITE(IUT6,*)
         WRITE(IUT6,*)
     *' **LRFNMS** : NUMBER OF INTER-CONNECT NODES:',NPINT
C
         CALL DDCOM4(NDOM,LDOM,MBPDOM,
     *               NPB0,LPB1,XPB1,YPB1,ZPB1,
     *               NPB2,LPB2,XPB2,YPB2,ZPB2,
     *               MAXBUF,RX,RY,IUT0,IERR)
C
         DO 6950 IDOM=1,NDOM
            IF(NPB0(IDOM).NE.NPB2(IDOM)) IERR=1
 6950    CONTINUE
         CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
         IF(IERRA.NE.0) THEN
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSGC
            IERR=IERRA
            RETURN
         ENDIF
C
      ENDIF
CC
CCHY [13] SET U,V,W,PN AT NEW NODES
CC
      DO 7000 IP=NPOLD+1,NP
         J = RCAPGETORIGINAL(IP,NODED)
C
         IF(J.EQ.KSEG) THEN
            IP1=NODED(1)  
            IP2=NODED(2)  
            UFIX(IP)=0.5E0*(UFIX(IP1)+UFIX(IP2))
            VFIX(IP)=0.5E0*(VFIX(IP1)+VFIX(IP2))
            WFIX(IP)=0.5E0*(WFIX(IP1)+WFIX(IP2))
            LPFRM(IP)=LPFRM(IP1)
            IF(LPFRM(IP).LT.LPFRM(IP2)) LPFRM(IP)=LPFRM(IP2)
            IF(IRFNFF.NE.1) GOTO 7000
            U (IP)=0.5E0*(U (IP1)+U (IP2))
            V (IP)=0.5E0*(V (IP1)+V (IP2))
            W (IP)=0.5E0*(W (IP1)+W (IP2))
            PN(IP)=0.5E0*(PN(IP1)+PN(IP2))
         ELSE IF(J.EQ.KQUD) THEN
            IP1=NODED(1)  
            IP2=NODED(2)  
            IP3=NODED(3)  
            IP4=NODED(4)  
            UFIX(IP)=0.25E0*(UFIX(IP1)+UFIX(IP2)+UFIX(IP3)+UFIX(IP4))
            VFIX(IP)=0.25E0*(VFIX(IP1)+VFIX(IP2)+VFIX(IP3)+VFIX(IP4))
            WFIX(IP)=0.25E0*(WFIX(IP1)+WFIX(IP2)+WFIX(IP3)+WFIX(IP4))
            LPFRM(IP)=LPFRM(IP1)
            IF(LPFRM(IP).LT.LPFRM(IP2)) LPFRM(IP)=LPFRM(IP2)
            IF(LPFRM(IP).LT.LPFRM(IP3)) LPFRM(IP)=LPFRM(IP3)
            IF(LPFRM(IP).LT.LPFRM(IP4)) LPFRM(IP)=LPFRM(IP4)
            IF(IRFNFF.NE.1) GOTO 7000
            U (IP)=0.25E0*(U (IP1)+U (IP2)+U (IP3)+U (IP4))
            V (IP)=0.25E0*(V (IP1)+V (IP2)+V (IP3)+V (IP4))
            W (IP)=0.25E0*(W (IP1)+W (IP2)+W (IP3)+W (IP4))
            PN(IP)=0.25E0*(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4))
         ELSE IF(J.EQ.KHEX) THEN
            IP1=NODED(1)  
            IP2=NODED(2)  
            IP3=NODED(3)  
            IP4=NODED(4)  
            IP5=NODED(5)  
            IP6=NODED(6)  
            IP7=NODED(7)  
            IP8=NODED(8)  
            IF(IRFNFF.NE.1) GOTO 7000
            U (IP)=0.125E0*( U (IP1)+U (IP2)+U (IP3)+U (IP4)
     *            +U (IP5)+U (IP6)+U (IP7)+U (IP8))
            V (IP)=0.125E0*( V (IP1)+V (IP2)+V (IP3)+V (IP4)
     *            +V (IP5)+V (IP6)+V (IP7)+V (IP8))
            W (IP)=0.125E0*( W (IP1)+W (IP2)+W (IP3)+W (IP4)
     *            +W (IP5)+W (IP6)+W (IP7)+W (IP8))
            PN(IP)=0.125E0*( PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4)
     *            +PN(IP5)+PN(IP6)+PN(IP7)+PN(IP8))
CC
CC            NOTE THAT
CC            NEW NODES FROM ELEMENT CENTER NEVER BECOME B.C. NODES 
CC
         ELSE 
            WRITE(IUT0,*) 'J=',J
            WRITE(IUT0,*) ERMSG5
            IERR=1
            RETURN
         ENDIF
 7000 CONTINUE
C
      DO 7100 IB=1,NPINLT
         IP=LPINLT(IB)
         UINLT(IB)=UFIX(IP)
         VINLT(IB)=VFIX(IP)
         WINLT(IB)=WFIX(IP)
 7100 CONTINUE   
C
      DO 7200 IB=1,NPWALL
         IP=LPWALL(IB)
         UWALL(IB)=UFIX(IP)
         VWALL(IB)=VFIX(IP)
         WWALL(IB)=WFIX(IP)
 7200 CONTINUE   
C
CC
CCHY [14] CLEAR 
CC
      CALL rcapClearRefiner()
CC
CCHY [15] TERMINATE REFINER
CC
      CALL rcapTermRefiner()
CC
CCHY [16] ADD NODE TABLE NOT REFINED
CCC
      DO 8000 IE=1,NE2
         DO 8100 I=1,8
            NODE(I,NER+IE)=NODEBK(I,IE)
 8100    CONTINUE
 8000 CONTINUE
      NE=NER+NE2
C
CC
CCHY [17] INTERPORATE NODE PRESSURE TO ELEMENTS
CCC
      IF (IRFNFF.EQ.0) RETURN
C
      DO 8200 IE = 1 , NE
C
         IF(NODE(8,IE).NE.0) THEN
            P(IE) = ( PN(NODE(1,IE))+ PN(NODE(2,IE))
     *               +PN(NODE(3,IE))+ PN(NODE(4,IE))
     *               +PN(NODE(5,IE))+ PN(NODE(6,IE))
     *               +PN(NODE(7,IE))+ PN(NODE(8,IE)))/8.0E0
         ELSE IF(NODE(6,IE).NE.0) THEN
            P(IE) = ( PN(NODE(1,IE))+ PN(NODE(2,IE))
     *               +PN(NODE(3,IE))+ PN(NODE(4,IE))
     *               +PN(NODE(5,IE))+ PN(NODE(6,IE)))/6.0E0
         ELSE IF(NODE(5,IE).NE.0) THEN
            P(IE) = ( PN(NODE(1,IE))+ PN(NODE(2,IE))
     *               +PN(NODE(3,IE))+ PN(NODE(4,IE))
     *               +PN(NODE(5,IE))                )/5.0E0
         ELSE IF(NODE(4,IE).NE.0) THEN
            P(IE) = ( PN(NODE(1,IE))+ PN(NODE(2,IE))
     *               +PN(NODE(3,IE))+ PN(NODE(4,IE)))/4.0E0
         ELSE
            IERR=1
            RETURN
         ENDIF
C
 8200 CONTINUE
C
      RETURN
      END
