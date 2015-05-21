      SUBROUTINE REORDR
     *   ( JSORT,JCOLOR,NDIVX,NDIVY,NDIVZ,NEIP,
     *     MP,ME,MWRK,NP,NE,N1,NODE,
     *     MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *     LPBTOA,LPATOB,LEBTOA,LEATOB,
     *     NETET,NEPRD,NEWED,NEHEX,
     *     NPINLT,NPWALL,NPSYMT,NPFREE,NPCCL ,NPBODY,
     *     NPINT ,
     *     LPINLT,LPWALL,LPSYMT,LPFREE,LPCCL1,LPCCL2,
     *     LPBODY,LPINT1,
     *     X,Y,Z,XD,YD,ZD,U,V,W,PN,P,
     *     WRK01,IERR,IUT0,IUT6 )
C
      IMPLICIT NONE
C
      INTEGER*4 JSORT,JCOLOR
      INTEGER*4 NDIVX,NDIVY,NDIVZ
      INTEGER*4 MP,ME,MWRK,NP,NE,N1
      INTEGER*4 NODE(N1,ME)
C
C     PART INFO
      INTEGER*4 NEIP(4)
C
C     COLOR INFO
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
C
C     TABLE
      INTEGER*4 LPBTOA(MP),LPATOB(MP),LEBTOA(ME),LEATOB(ME)
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX
C
C     LIST DATA
      INTEGER*4 NPINLT,NPWALL,NPSYMT,NPFREE,NPCCL ,NPBODY,
     *          NPINT
      INTEGER*4 LPINLT(NPINLT),LPWALL(NPWALL),LPSYMT(NPSYMT),
     *          LPFREE(NPFREE),LPCCL1(NPCCL ),LPCCL2(NPCCL ),
     *          LPBODY(NPBODY),LPINT1(NPINT )
C
C     FLOW FILED DATA
      REAL*4    X(MP),Y(MP),Z(MP),U(NP),V(NP),W(NP),PN(NP),P(NE)
      REAL*8    XD(MP),YD(MP),ZD(MP)
C
C     WORK
      INTEGER*4,ALLOCATABLE :: REFCNT(:),REFELM(:,:)
      INTEGER*4 MAXREF
      REAL*4    WRK01(MWRK)
      INTEGER*4 TYPE,NPART
      INTEGER*4 IE,IP,I,IEBUF
      INTEGER*4 IERR,IUT0,IUT6
CCHY_DEBUG
      INTEGER IUT80
      DATA IUT80 /80/
CCHY_DEBUG
C
      IERR = 0
C
      ! allocate and init
      ! before to after table and
      ! after to before table
      DO 100 IP=1,MP
         LPBTOA(IP) = IP
         LPATOB(IP) = IP
 100  CONTINUE
      DO 110 IE=1,ME
         LEBTOA(IE) = IE
         LEATOB(IE) = IE
 110  CONTINUE
C
      ! define element type
      DO 120 IE=1,NE
         TYPE=0
         DO 130 I=1,N1
            IF (NODE(I,IE).NE.0) TYPE=TYPE+1
 130     CONTINUE
         if (TYPE .ne. 8) then
           write(IUT0,*)
     &       'ELEMENT(S) OF OTHER TYPE THAN HEXAHEDRON FOUND'
           IERR = 1
           return
         endif
 120  CONTINUE
C
      NETET=0
      NEPRD=0
      NEWED=0
      NEHEX=NE
C
      NCOLOR(1) = 1
      NCOLOR(2) = 1
      NCOLOR(3) = 1
      NCOLOR(4) = 1
C
      NCPART(1,1) = 1
      NCPART(1,2) = 1
      NCPART(1,3) = 1
      NCPART(1,4) = 1
C
      LLOOP(1,1,1) = 1
      LLOOP(2,1,1) = 1
      LLOOP(1,1,2) = 1
      LLOOP(2,1,2) = 1
      LLOOP(1,1,3) = 1
      LLOOP(2,1,3) = 1
      LLOOP(1,1,4) = 1
      LLOOP(2,1,4) = NE + 1
C
      IF (JSORT.NE.1) GOTO 290
C
      !-----------------------------------------------------
      ! reordering nodes by inoue method
      !-----------------------------------------------------
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **REORDR** : SORTING NODE'
C
      ! reordering (make node reordering table)
      CALL REORD_NODE(LPBTOA, MP, NP,
     *                NDIVX, NDIVY, NDIVZ,
     *                X, Y, Z,
     *                IUT6)

       ! making after to before table
      DO 200 IP=1,NP
         LPATOB(LPBTOA(IP))=IP
 200  CONTINUE
C
      ! update re-ordered node
      DO 210 IE=1,NE
         DO 220 I=1,8
            IP=NODE(I,IE)
            IF (IP.EQ.0) GOTO 220
            NODE(I,IE) = LPBTOA(IP)
 220     CONTINUE
 210  CONTINUE
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **REORDR** : DONE'
C
 290  CONTINUE
      IF (JCOLOR.NE.1) GOTO 390
C
      !---------------------------------
      ! make node connection list
      !---------------------------------
      MAXREF = 8  ! HEX
      allocate(REFCNT(MP))
      allocate(REFELM(MAXREF,MP))
      CALL MAKENODECONNECT(MP, NP, ME, NE, N1, NODE, MAXREF,
     *                     REFCNT, REFELM, IUT6)
C
      !---------------------------------
      ! define number of parts
      !---------------------------------
C      
      ! define number of parts so that each part will have
      ! NEIP elements
C
      NPART=1+NE/NEIP(4)
C
      ! check
      IF (NPART.GT.NE) THEN
         WRITE(IUT6,*)
         WRITE(IUT6,"('HEXA')")
         WRITE(IUT6,1000) 
         WRITE(IUT6,2000) NPART,NE
         NPART=NE
      ENDIF
C
      WRITE(IUT6,*)
      WRITE(IUT6,"('NPART(HEXA   ) = ',I9)") NPART
C
      !---------------------------------
      ! make part(subspace)
      !---------------------------------
      WRITE(IUT6,*)
      WRITE(IUT6,*)
     *' **REORDR** : MAKING PART AND COLORG FOR EACH TYPE OF ELEMENTS'
C
      NCOLOR = 0     
C
C      WRITE(IUT80, "('========')")
C      WRITE(IUT80, "('HEXA')")
C      WRITE(IUT80, "('========')")
      CALL MAKEPARTCOLOR(MP, NP, ME, NE, N1, NODE, MAXREF, REFCNT,
     *                   REFELM, 1, NE, NPART, NCOLOR(4),
     *                   MCOLOR, NCPART(:,4), MCPART, LLOOP(:,:,4),
     *                   LEBTOA, IUT6)
C
      WRITE(IUT6,*)
      WRITE(IUT6,"('HEXA    COLOR = ', I4)") NCOLOR(4)
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' **REORDR** : DONE'
C
      deallocate(REFCNT)
      deallocate(REFELM)
C
 390  CONTINUE
C
C     MAKING CONVERISON TEBLE
      DO 400 IE=1,NE
         IEBUF        =LEBTOA(IE  )
         LEATOB(IEBUF)=IE
 400  CONTINUE
C
      CALL DATCNV(NP,NE,MWRK,
     *            NPINLT,NPWALL,NPSYMT,NPFREE,NPCCL ,NPBODY,
     *            NPINT ,
     *            LPINLT,LPWALL,LPSYMT,LPFREE,LPCCL1,LPCCL2,
     *            LPBODY,LPINT1,
     *            X,Y,Z,XD,YD,ZD,U,V,W,PN,P,
     *            LPBTOA,LEBTOA,WRK01,IERR,IUT0)
C
 1000 FORMAT('SPECIFIED PART WAS MORE THAN PROCESSED ELEMENTS')
 2000 FORMAT('NUMBER OF PART HAS BEEN CHANGED ',I9,' TO ',I9)
 3000 FORMAT('  COLOR ',I4,' CONTAINS ',I4,' PARTS')
C
      RETURN
      END
C=====================================================================
C makenodeconnect
C Make element list refferencing to each node
C
C Essential Input:
C TYPE(ME)    Element type
C NODE(N1,ME) Element connectivety
C
C Output:
C REFCNT(MP)   Number of owner element refferencing to each node
C REFELM(:,MP) Owner element list for each node
C      
C 2012/06/04 Kuma(RIKEN) Fix
C=====================================================================
      subroutine makenodeconnect(MP, NP, ME, NE, N1, NODE,
     *                           MAXREF, REFCNT, REFELM, IUT6)
      implicit none

      ! argument
      integer(4),intent(in)  :: MP
      integer(4),intent(in)  :: NP
      integer(4),intent(in)  :: ME
      integer(4),intent(in)  :: NE
      integer(4),intent(in)  :: N1
      integer(4),intent(in)  :: NODE(N1,ME)
      integer(4),intent(in)  :: MAXREF
      integer(4),intent(out) :: REFCNT(MP)       
      integer(4),intent(out) :: REFELM(MAXREF,MP)
      integer(4),intent(in)  :: IUT6

      ! local
      integer(4) :: I, J, K

C      write(IUT6,*)
C      write(IUT6,"('makenodeconnect')")

      ! list-up elements that reffer the node.I
      REFCNT = 0
      REFELM = 0
      do I=1, NE
         do J=1, 8
            K = NODE(J,I)
            REFCNT(K) = REFCNT(K) + 1
            REFELM(REFCNT(K),K) = I
         enddo
      enddo
      
#if 0
      do I=1,NP
         write(IUT6,"(I7,'(',I3,') ',$)") I,REFCNT(I)
         do J=1,REFCNT(I)
            write(IUT6,"(I7,' ',$)") REFELM(J,I)
         enddo
         write(IUT6,*)
      enddo
      stop
#endif      

      end subroutine
C=====================================================================
C makepartcolor
C Divide mesh into parts, and separate parts into colors so that
C adjacent parts will be belong different color
C
C Essential Input:
C REFCNT(MP)   Number of owner element refferencing to each node
C REFELM(:,MP) Owner element list for each node
C
C Output:
C TYPE(ME)    Element type (sorted)
C NODE(N1,ME) Element connectivety (sorted)
C NCOLOR      Number of resultant color
C NCPART(:)   Number of parts in each color
C LSTART(:,:) Start index of a part in array NODE. For example
C             LSTART(2,3)=100 means that 2nd part of 3rd color
C             starts in index 100 of array NODE
C
C 2012/06/04 Kuma(RIKEN) Fix
C=====================================================================
      subroutine makepartcolor(MP, NP, ME, NE, N1, NODE, MAXREF,
     *                         REFCNT, REFELM, IES, IEE, NPART, NCOLOR,
     *                         NMAXCOL, NCPART, NMAXPRT, LSTART, 
     *                         LEBTOA, IUT6)
      implicit none

      ! argument (entire mesh info)
      integer(4),intent(in)    :: MP
      integer(4),intent(in)    :: NP                ! (NODE) number of nodes
      integer(4),intent(in)    :: ME
      integer(4),intent(in)    :: NE                ! (ELEM) number of elements
      integer(4),intent(in)    :: N1
      integer(4),intent(inout) :: NODE(N1,ME)        ! (ELEM)vertices list
      integer(4),intent(in)    :: MAXREF            ! (NODE)maxinum adjacent number
      integer(4),intent(in)    :: REFCNT(ME)        ! (NODE)adjacent number of each node
      integer(4),intent(in)    :: REFELM(MAXREF,ME) ! (NODE)adjacent node list
      integer(4),intent(in)    :: NPART             ! Number of partitioned parts

      integer(4),intent(in) :: IES ! first elem index in array NODE
      integer(4),intent(in) :: IEE ! last elem index in array NODE

      integer(4),intent(out) :: NCOLOR  ! number of resultant color
      integer(4),intent(in)  :: NMAXCOL ! maxinum number of color
      integer(4),intent(out) :: NCPART(NMAXCOL) ! number of part of each color
      integer(4),intent(in)  :: NMAXPRT ! maxinum number of parts of each color
      integer(4),intent(out) :: LSTART(NMAXPRT, NMAXCOL)
      integer(4),intent(out) :: LEBTOA(ME)
      integer(4),intent(in)  :: IUT6

      ! local
      integer(4) :: NPE
      integer(4) :: I, J, K, L, M, N, P
      integer(4),allocatable :: XADJE(:)
      integer(4),allocatable :: YADJE(:)
      integer(4),allocatable :: WORKE(:)
      integer(4) :: DUPFLG
      integer(4) :: NADJ
      integer(4) :: MAX, MIN
      integer(4) :: MEADJ

      ! local
      integer(4),allocatable :: WORKP(:)
      integer(4),allocatable :: WORKC(:)
      integer(4),allocatable :: XPELE(:)
      integer(4),allocatable :: YPELE(:)
      integer(4),allocatable :: XADJP(:)
      integer(4),allocatable :: YADJP(:)

      ! local
      integer(4),allocatable :: PARTC(:)     ! (PART)
      integer(4),allocatable :: ADJFLG(:)    ! (PART)
      integer(4),allocatable :: WK_NODE(:,:) ! (ELEM)
      
      ! local part number contained each color
      integer(4),allocatable :: XCOLP(:)
      integer(4),allocatable :: YCOLP(:)

      ! number of processed elements
      NPE = IEE - IES + 1
      if(NPE .eq. 0) return

C      write(IUT6,*)
C      write(IUT6,"('makepartcolor')")

      !---------------------------------
      ! make element adjacent list
      !---------------------------------
      MEADJ = 26 * NPE ! HEX
      allocate(XADJE(NPE+1))
      allocate(YADJE(MEADJ+1))
      call makeelemadj(MP, ME, N1, NODE, MAXREF, REFCNT,
     *                 REFELM, IES, IEE, MEADJ, XADJE, YADJE, IUT6)
#if 0
      write(IUT6,*)
      write(IUT6,"('ELEMENT RELATIONSHIP')")
      do I=1,NPE
         write(IUT6,"(I7,'(',I7,')',$)") I, XADJE(I+1)-XADJE(I)
         do J=XADJE(I), XADJE(I+1)-1
            write(IUT6,"(I7, ' ',$)") YADJE(J)
         enddo
         write(IUT6,*)
      enddo
      stop
#endif      
      
      !---------------------------------
      ! call metis 
      !---------------------------------
      allocate(WORKE(NPE))
      WORKE = 1
      if(NPART .ne. 1) then
#ifndef NO_METIS
         call metis_wrapper(NPE, XADJE, YADJE, NPART, WORKE)
#endif
      endif
#if 0
      do I=1,NPE
         write(IUT6,"('ELEM 'I8,' IS BELOING TO PART ',I8)")
     *        I+(NPE-1), WORKE(I)
      enddo
      stop
#endif      

      !-----------------------
      ! write partitioned part info
      !-----------------------
      allocate(WORKP(NPART))
      WORKP = 0
#if 1
      do I=1, NPE
         J = WORKE(I)
         WORKP(J) = WORKP(J) + 1
      enddo
      write(IUT6,"('NUM PART = ', I8)") NPART
!!!   write(IUT6,"('PART NO.,   ELEMS')")
      MAX = 0
      MIN = WORKP(1)
      do I=1, NPART
!!!      write(IUT6, "(I8,' ',I8)") I, WORKP(I)
         if(WORKP(I) .gt. MAX) MAX = WORKP(I)
         if(WORKP(I) .lt. MIN) MIN = WORKP(I)
      enddo
      write(IUT6,"('MAX = ', I8)") MAX
      write(IUT6,"('MIN = ', I8)") MIN
      write(IUT6,"('AVE = ', I8)") NPE / NPART
#endif

      !-----------------------------------------
      ! make element list contained in each part
      !-----------------------------------------
      allocate(XPELE(NPART+1))
      allocate(YPELE(NPE))
      XPELE = 0
      YPELE = 0

      ! count elements belonging to each part
      do I=1, NPE
         J = WORKE(I)
         XPELE(J) = XPELE(J) + 1
      enddo
#if 0
      do I=1, NPART
         write(IUT6,"('PART ',I4,' HAS ',I4,' ELEMS')") I, XPELE(I)
      enddo
      stop
#endif      

      ! adjust XPELE into CSR format
      J = 0
      do I=1, NPART+1
         K = XPELE(I)
         XPELE(I) = J + 1
         J = J + K
      enddo

      ! substitute elems into part
      WORKP = 0
      do I=1, NPE
         J = WORKE(I)
         YPELE(XPELE(J) + WORKP(J)) = I
         WORKP(J) = WORKP(J) + 1
      enddo
#if 0
      write(IUT6,*)
      write(IUT6,"('PART LIST')")
      do I=1,NPART
         write(IUT6,"(I4,'(',I4,')',$)") I, XPELE(I+1)-XPELE(I)
         do J=XPELE(I), XPELE(I+1)-1
            write(IUT6,"(I7, ' ',$)") YPELE(J)
            
            K=YPELE(J)
            if(I .ne. WORKE(K)) then
               write(IUT6,"('WRONG')")
            endif
         enddo
         write(IUT6,*)
      enddo
#endif
      
      !-----------------------
      ! make part relationship
      !-----------------------
      allocate(XADJP(NPART+1))
      allocate(YADJP(NPART*NPART)) !! optimal array size is under considering 05/16
      XADJP  = 1
      YADJP  = 0
      NADJ   = 1
      do I=1, NPART
         do J=XPELE(I), XPELE(I+1)-1
            K = YPELE(J) ! element

            ! search element adjacent list
            do L=XADJE(K), XADJE(K+1)-1
               M = YADJE(L) ! one of adjacent element of element K
               N = WORKE(M) ! belonging part of element K
#ifdef _DEBUG
               write(IUT6,"(' PART ',I2,$)") I
               write(IUT6,"(' ELM ',I2,'(',I2,')',$)") K,WORKE(K)
               write(IUT6,"(' ELM ',I2,'(',I2,')',$)") M,WORKE(M)
#endif
               if(I .ne. N) then
#ifdef _DEBUG
                  write(IUT6,"('  !')")
#endif
                  DUPFLG = 0
                  do P=XADJP(I),NADJ
                     if(N .eq. YADJP(P)) then
                        DUPFLG = 1
                        continue
                     endif
                  enddo

                  if(DUPFLG .ne. 1) then
                     YADJP(NADJ) = N
                     NADJ = NADJ + 1
                  endif
#ifdef _DEBUG                  
               else
                  write(IUT6,*)
#endif                  
               endif
            enddo
         enddo
         XADJP(I+1) = NADJ
      enddo
#if 0
      write(IUT6,*)
      write(IUT6,"('PART RELATIONSHIP')")
      do I=1,NPART
         write(IUT6,"(I3,'(',I3,')',$)") I, XADJP(I+1)-XADJP(I)
         do J=XADJP(I), XADJP(I+1)-1
            write(IUT6,"(I3, ' ',$)") YADJP(J)
         enddo
         write(IUT6,*)
      enddo
#endif      

      !-----------------------
      ! do coloring
      !-----------------------
      allocate(PARTC(NPART))
      allocate(ADJFLG(NPART))
      PARTC  = 0
#if 0
      write(IUT6,*)
      write(IUT6,"('PART ADJACENT MATRIX')")
      do I=1, NPART
         ADJFLG = 0
        do J=XADJP(I),XADJP(I+1)-1
            K = YADJP(J)
            ADJFLG(K) = ADJFLG(K) + 1
         enddo

         do J=1, NPART
            write(IUT6,"(I2,' ',$)") ADJFLG(J)
         enddo
         write(IUT6,*)
      enddo
      write(IUT6,*)
#endif      

      NCOLOR = 0
      do I=1,NPART
         if(PARTC(I) .ne. 0) goto 100
         NCOLOR = NCOLOR + 1
         PARTC(I) = NCOLOR

         ADJFLG = 0
         do J=XADJP(I),XADJP(I+1)-1
            K = YADJP(J)
            ADJFLG(K) = ADJFLG(K) + 1
         enddo
         
         do J=1,NPART
            if(J .eq. I)         goto 200
            if(PARTC(J) .ne. 0)  goto 200
            if(ADJFLG(J) .ne. 0) goto 200
            PARTC(J) = NCOLOR
            
            ! superimpose part.J's adjacent info
            do K=XADJP(J),XADJP(J+1)-1
               L = YADJP(K)
               ADJFLG(L) = ADJFLG(L) + 1
            enddo
 200     enddo
 100  enddo

!      write(IUT6,*)
!      write(IUT6,"('COLOR INFO')")
!      do I=1, NPART
!         write(IUT6,"('PART ',I3,' BELONGS COLOR ',I3)") I,PARTC(I)
!      enddo
      
      !-----------------------
      ! store part into color
      !-----------------------
      allocate(XCOLP(NCOLOR+1))
      allocate(YCOLP(NPART))
      allocate(WORKC(NCOLOR))
      XCOLP = 0
      YCOLP = 0

      ! count parts beloinging to each color
      do I=1, NPART
         J = PARTC(I)
         XCOLP(J) = XCOLP(J) + 1
      enddo

      ! adjust XCOLP into CSR format
      J = 0
      do I=1, NCOLOR+1
         K = XCOLP(I)
         XCOLP(I) = J + 1
         J = J + K
      enddo

      ! substitute parts into color
      WORKC = 0
      do I=1, NPART
         J = PARTC(I)
         YCOLP(XCOLP(J) + WORKC(J)) = I
         WORKC(J) = WORKC(J) + 1
      enddo

C      write(IUT6,*)
C      write(IUT6,"('RESULTANT COLOR =',I3,' COLOR')") NCOLOR
C      write(IUT6,"('COLOR INFO (CONTAINED PARTS)')")
      do I=1,NCOLOR
         NCPART(I) = XCOLP(I+1)-XCOLP(I)
         
C         write(IUT6,"(I3,'(',I3,'): ',$)") I, NCPART(I)
C         do J=XCOLP(I),XCOLP(I+1)-1
C            write(IUT6,"(I3,' ',$)") YCOLP(J)
C         enddo
C         write(IUT6,*)
      enddo

      !---------------------------------
      ! sort elems by part and color
      !---------------------------------
      WORKE = 0
      N     = 1
      do I=1,NCOLOR                         ! iterate color (color.I)
         do J=XCOLP(I),XCOLP(I+1)-1         ! iterate part in the color (part.K)
            K=YCOLP(J)
            LSTART(J-XCOLP(I)+1,I) = N+(IES-1)
            
!            write(IUT6,"('|',5I7)") I, J-XCOLP(I)+1, J, K,
!     *           LSTART(J-XCOLP(I)+1,I)
            
            do L=XPELE(K),XPELE(K+1)-1      ! iterate elem in the part
               WORKE(N) = YPELE(L)+(IES-1)
               N = N + 1
            enddo
         enddo
         LSTART(J-XCOLP(I)+1,I) = N+(IES-1)
!         write(IUT6,"('*',5I7)") I,J-XCOLP(I)+1,J,K,
!     *        LSTART(J-XCOLP(I)+1,I)
      enddo
      
!      do I=1,NPE
!         write(IUT6,"(I8,' ->',I8,'(',I1,')',$)") I,WORKE(I),TYPE(WORKE(I))
!         if(WORKE(I) .lt. IES .or. WORKE(I) .gt. IEE) then
!            write(IUT6,"('  WRONG')")
!         else
!            write(IUT6,*)
!         endif
!      enddo
!      stop

      ! sort elems into local array
      allocate(WK_NODE(8,NE))
      WK_NODE = 0
      do I=1,NPE
         J = WORKE(I)
         do K=1,8
            WK_NODE(K,I) = NODE(K,J)
         enddo
      enddo

      ! superimpose sorted elems in array NODE
!      write(IUT6,"('substitute')")
      do I=1,NPE
         K = I+IES-1
         do J=1,8
            NODE(J,K) = WK_NODE(J,I)
         enddo
!         write(IUT6,"('IES=',I0,' IEE=',I0,' LOCAL=',I0,' GLOBAL=',I0)")
!     *        IES, IEE, I, I+IES-1
      enddo

C     ADDED BY MIZUHO
      DO I=1,NPE
         J=WORKE(I)
         LEBTOA(J)=I+IES-1
      ENDDO
C
C      write(IUT6,*)
C      write(IUT6,"('size(XADJE)  = ',I12)") size(XADJE)
C      write(IUT6,"('size(YADJE)  = ',I12)") size(YADJE)
C      write(IUT6,"('size(WORKE)  = ',I12)") size(WORKE)
C      write(IUT6,"('size(WORKP)  = ',I12)") size(WORKP)
C      write(IUT6,"('size(WORKC)  = ',I12)") size(WORKC)
C      write(IUT6,"('size(XPELE)  = ',I12)") size(XPELE)
C      write(IUT6,"('size(YPELE)  = ',I12)") size(YPELE)
C      write(IUT6,"('size(XADJP)  = ',I12)") size(XADJP)
C      write(IUT6,"('size(YADJP)  = ',I12)") size(YADJP)
C      write(IUT6,"('size(PARTC)  = ',I12)") size(PARTC)
C      write(IUT6,"('size(ADJFLG) = ',I12)") size(ADJFLG)
C      write(IUT6,"('size(XCOLP)  = ',I12)") size(XCOLP)
C      write(IUT6,"('size(YCOLP)  = ',I12)") size(YCOLP)
C      write(IUT6,"('TOTAL        = ',I12)") size(XADJE) + size(YADJE) +
C     *     size(WORKE) + size(WORKP) + size(XPELE) + size(YPELE) +
C     *     size(XADJP) + size(YADJP) + size(PARTC) + size(ADJFLG) +
C     *     size(XCOLP) + size(YCOLP) + size(WORKC)

      deallocate(XADJE)
      deallocate(YADJE)
      deallocate(WORKE)
      deallocate(WORKP)
      deallocate(XPELE)
      deallocate(YPELE)
      deallocate(XADJP)
      deallocate(YADJP)
      deallocate(PARTC)
      deallocate(ADJFLG)
      deallocate(XCOLP)
      deallocate(YCOLP)
      deallocate(WORKC)
      deallocate(WK_NODE)

      end subroutine makepartcolor
C=====================================================================
C makeelemadj
C Make element adjacent list      
C
C Essential Input:
C REFCNT(MP)   Number of owner element refferencing to each node
C REFELM(:,MP) Owner element list for each node
C
C Output:
C XADJE, YADJE  Element adjacent list as CSR
C      
C 2012/06/04 Kuma(RIKEN) Fix
C=====================================================================
      subroutine makeelemadj(MP, ME, N1, NODE, MAXREF,
     *                       REFCNT, REFELM, IES, IEE, MEADJ, XADJE,
     *                       YADJE, IUT6)
      implicit none

      ! argument (entire mesh info)
      integer(4),intent(in) :: MP
      integer(4),intent(in) :: ME
      integer(4),intent(in) :: N1
      integer(4),intent(in) :: NODE(N1,ME)        ! (ELEM)vertices list
      integer(4),intent(in) :: MAXREF            ! (NODE)maxinum adjacent number
      integer(4),intent(in) :: REFCNT(MP)        ! (NODE)adjacent number of each node
      integer(4),intent(in) :: REFELM(MAXREF,MP) ! (NODE)adjacent node list
      
      ! argument (processing element range)
      integer(4),intent(in) :: IES ! start elem index in array NODE
      integer(4),intent(in) :: IEE ! end   elem index in array NODE

      ! argument (element adjacent list in CSR)
      integer(4),intent(in)  :: MEADJ
      integer(4),intent(out) :: XADJE(IEE-IES+2)
      integer(4),intent(out) :: YADJE(MEADJ+1)

      integer(4),intent(in) :: IUT6

      ! local
      integer(4) :: I, J, K, L, M, N
      integer(4) :: DUPFLG
      integer(4) :: NUMADJ
      integer(4) :: NADJ
      integer(4) :: ADJELM(26)  ! HEX
      integer(4) :: NPE ! number of processed elements

C      write(IUT6,*)
C      write(IUT6,"('makeelemadj')")

      NPE = IEE - IES + 1
      XADJE(:)  = 1
      YADJE(:)  = 0
      ADJELM(:) = 0
C      write(IUT6,"('IES = ', I8)") IES
C      write(IUT6,"('IEE = ', I8)") IEE
C      write(IUT6,"('NPE = ', I8)") NPE
C      write(IUT6,"('size(XADJE) =',I12)") size(XADJE)
C      write(IUT6,"('size(YADJE) =',I12)") size(YADJE)
C      write(IUT6,"('size(ADJELM)=',I12)") size(ADJELM)

      NUMADJ = 1
      do I=IES, IEE
C        if(MOD(I-IES+1,100000) .eq. 0) then
C            write(IUT6, "('I=',I7,' /'I7)") I-IES+1,NPE
C        endif

         NADJ = 0
         do J=1, 8
            K = NODE(J,I)
            do L=1, REFCNT(K)
               N = REFELM(L,K)
               
               ! check duplex
               DUPFLG = 0
               do M=1, NADJ
                  if(N .eq. ADJELM(M)) then
                     DUPFLG = 1
                     goto 100
                  endif
 100           enddo

               ! add adjacent elem into array ADJELM
               if((DUPFLG .ne. 1) .and. (I   .ne. N) .and.
     *            (IES    .le. N) .and. (IEE .ge. N)) then
                  NADJ = NADJ + 1
                  if (NADJ.gt.26) stop 'NADJ.gt.26'  ! HEX
                  ADJELM(NADJ) = N
               endif
            enddo
         enddo

         ! convert into csr
         do J=1, NADJ
            if(ADJELM(J) .ne. I) then
               YADJE(NUMADJ) = ADJELM(J)
               NUMADJ = NUMADJ + 1
            endif
         enddo
         XADJE((I-IES+1)+1) = NUMADJ
      enddo
#if 0      
      open(100, file="csr.txt", form='formatted')
      write(100,"(8I8)") (XADJE(J), J=1, NE+1)
      write(100,"(8I8)") (ADJNCY(J), J=1, size(ADJNCY))
      close(100)
      stop
#endif

#if 0
      !---------------------------------
      ! check
      !---------------------------------
      do I=1,NPE
!         write(IUT6,"(I8,'('I3,')',$)") I,XADJE(I+1)-XADJE(I)
         do J=XADJE(I), XADJE(I+1)-1
            K = YADJE(J)
!            write(IUT6,"(I7, ' ',$)") K
            if(K .lt. IES .or. K .gt. IEE) write(IUT6,"('WRONG')")
         enddo
!         write(IUT6,*)
      enddo
#endif      

      !---------------------------------
      ! convert golbal index to local index
      ! local index starts IES
      !---------------------------------
      do I=1,NPE
         do J=XADJE(I),XADJE(I+1)-1
            YADJE(J) = YADJE(J) - IES + 1
         enddo
      enddo

      end subroutine makeelemadj
C=====================================================================
C reord_node
C Reordering node number
C      
C Essential Input:
C x(mp),y(mp), z(mp)  coordinates
C numx,numy,numz      divide number of xyz axis      
C       
C Output:
C num_reorder(mp)     table from original to reordered
C=====================================================================
      subroutine reord_node(num_reorder,mp, np, numx, numy, numz,
     *                      x, y, z, IUT6)
      implicit real*8 (a-h,o-z)  
      implicit integer*4 (i-n)

      ! arguments
      integer(4),intent(out) :: num_reorder(mp)
      integer(4),intent( in) :: mp
      integer(4),intent( in) :: np
      integer(4),intent( in) :: IUT6
      integer(4),intent( in) :: numx, numy, numz
      real(4),   intent( in) :: x(mp), y(mp), z(mp)

      ! local
      real*8,dimension(:,:),allocatable :: pos
      integer*4,dimension(:,:,:),allocatable :: icount_in 
      integer*4,dimension(:,:,:),allocatable :: icount_out 
      integer*4,dimension(:,:,:,:),allocatable :: num_new_in 
      integer*4,dimension(:,:,:,:),allocatable :: num_new_out
      integer*4 :: iout,iin

      !! param
      parameter(xl_ratio=0.1,yl_ratio=0.1,zl_ratio=0.1)
      
      ! allocate and initialize
      num = np
      allocate(pos(3,num))
      allocate(icount_in (numx,numy,numz))
      allocate(icount_out(numx,numy,numz))
      do i = 1, num
         pos(1,i) = x(i)
         pos(2,i) = y(i)
         pos(3,i) = z(i)
      enddo

      write(IUT6, "(/,'DIVISION NUMBER')")
      write(IUT6, "('X: ', I0)") numx
      write(IUT6, "('Y: ', I0)") numy
      write(IUT6, "('Z: ', I0)") numz

      write(IUT6, "(/,'BOUNDARY RATIO')")
      write(IUT6, "('X: ', F24.18)") xl_ratio
      write(IUT6, "('Y: ', F24.18)") yl_ratio
      write(IUT6, "('Z: ', F24.18)") zl_ratio
      
      ! search max,min position for x-y-z direction
      posx_max = 0.0
      posy_max = 0.0
      posz_max = 0.0
      posx_min = 1.0e+20
      posy_min = 1.0e+20
      posz_min = 1.0e+20
      do i = 1,num
         posx = pos(1,i) 
         posy = pos(2,i)  
         posz = pos(3,i)
         if(posx < posx_min ) then
            posx_min = posx
         endif 
         if(posy < posy_min ) then
            posy_min = posy
         endif 
         if(posz < posz_min ) then
            posz_min = posz
         endif 
         if(posx_max < posx ) then
            posx_max = posx
         endif 
         if(posy_max < posy ) then
            posy_max = posy
         endif 
         if(posz_max < posz ) then
            posz_max = posz
         endif 
      end do
      write(IUT6, "(/,'MINIMUM AND MAXIMUM COORDINATE')")
      write(IUT6, "('X:', F10.7, ' to', F10.7)") posx_min, posx_max
      write(IUT6, "('Y:', F10.7, ' to', F10.7)") posy_min, posy_max
      write(IUT6, "('Z:', F10.7, ' to', F10.7)") posz_min, posz_max
      
      !! calculation x-y-z unit
      xunit = (posx_max - posx_min) / numx
      yunit = (posy_max - posy_min) / numy
      zunit = (posz_max - posz_min) / numz
C      write(IUT6, "(/,'DIVISION LENGTH')")
C      write(IUT6, "('X:', F10.7)") xunit
C      write(IUT6, "('Y:', F10.7)") yunit
C      write(IUT6, "('Z:', F10.7)") zunit

      ! count number of nodes belonging to the subdivision
      icount_in = 0 
      do i = 1,num
         posx = pos(1,i) - posx_min 
         posy = pos(2,i) - posy_min  
         posz = pos(3,i) - posz_min
         
         if(posx .eq. 0.0) then 
            numx_pos = 1
         else
            numx_pos = int((posx - 1.0e-10) / xunit) +1
         end if
         if(numx_pos < 0 ) then
            write(6,*) 'minus x_number o!!urs i=',i
         endif
         
         if(posy .eq. 0.0) then 
            numy_pos = 1
         else
            numy_pos = int((posy - 1.0e-10) / yunit) + 1
         end if
         if(numy_pos < 0 ) then
            write(6,*) 'minus y_number o!!urs i=',i
         endif
         
         if(posz .eq. 0.0) then 
            numz_pos = 1
         else
            numz_pos = int((posz - 1.0e-10) / zunit) + 1
         end if
         if(numz_pos < 0 ) then
            write(6,*) 'minus z_number o!!urs i=',i
         endif
         
         icount_in(numx_pos,numy_pos,numz_pos) =
     *   icount_in(numx_pos,numy_pos,numz_pos)+1
      end do

      ! search max number of box points
      max_num_box = 0
      do kk=1,numz 
      do jj=1,numy 
      do ii=1,numx 
         ic=icount_in(ii,jj,kk)
         if(max_num_box < ic) then
            max_num_box = ic
         end if
      end do
      end do
      end do

      ! allocate section
      allocate(num_new_in(max_num_box,numx,numy,numz))
      allocate(num_new_out(max_num_box,numx,numy,numz))

      ! make the lists containing node amount in inner region and
      ! outer region of a subudivision
      icount_in   = 0
      icount_out  = 0
      num_new_in  = 0 
      num_new_out = 0
      
      iin  = 0
      iout = 0
      do i = 1,num
         posx = pos(1,i) - posx_min
         posy = pos(2,i) - posy_min 
         posz = pos(3,i) - posz_min
         
         if(posx .eq. 0.0) then 
            numx_pos = 1
            postx    = posx
         else
            numx_pos = int((posx - 1.0E-10 ) / xunit) + 1
            postx    = posx - (numx_pos - 1) * xunit
         end if
         
         if(posy .eq. 0.0) then 
            numy_pos = 1
            posty    = posy
         else
            numy_pos = int((posy - 1.0e-10) / yunit) + 1
            posty    = posy - (numy_pos - 1) * yunit
         end if
         
         if(posz .eq. 0.0) then 
            numz_pos = 1
            postz    = posz
         else
            numz_pos = int((posz - 1.0e-10) / zunit) + 1
            postz    = posz - (numz_pos - 1) * zunit
         end if

         ! check outer or inner
         if(xunit*xl_ratio .le. postx                .and.      
     &      postx          .le. xunit*(1.0-xl_ratio) .and.  
     &      yunit*yl_ratio .le. posty                .and. 
     &      posty          .le. yunit*(1.0-yl_ratio) .and. 
     &      zunit*zl_ratio .le. postz                .and.       
     &      postz          .le. zunit*(1.0-zl_ratio)) then

            ! inner
            icount_in(numx_pos,numy_pos,numz_pos) =
     &      icount_in(numx_pos,numy_pos,numz_pos)+1
            
            num_new_in(icount_in(numx_pos,numy_pos,numz_pos),
     &                           numx_pos,numy_pos,numz_pos)=i
            iin=iin+1
         else
            ! outer
            icount_out(numx_pos,numy_pos,numz_pos) =
     &      icount_out(numx_pos,numy_pos,numz_pos)+1
            num_new_out(icount_out(numx_pos,numy_pos,numz_pos),
     &                             numx_pos,numy_pos,numz_pos)=i
            iout=iout+1
         end if
      end do
C      write(IUT6, "(/,'CONTAINING NODES IN EACH DIVISION')")
      ITOTAL = 0
      do kk=1,numz 
      do jj=1,numy
      do ii=1,numx
C         write(IUT6, 1000) ii,jj,kk,
C     *                     icount_in(ii,jj,kk),
C     *                     icount_out(ii,jj,kk)
 1000    format('(',I2,',',I2,',',I2,') -> Inner(',I3,'), Outer(',I3')')
         ITOTAL = ITOTAL + icount_in(ii,jj,kk) +
     *                     icount_out(ii,jj,kk)
      enddo
      enddo
      enddo
C      write(IUT6, "('TOTAL: ',I0, ' NODES')") ITOTAL
      
      ! make renumber list 
      icc = 0
      do kk=1,numz 
      do jj=1,numy 
      do ii=1,numx
         
         ic=icount_in(ii,jj,kk)
         do ll=1,ic 
            iold = num_new_in(ll,ii,jj,kk)
            icc  = icc + 1
            num_reorder(iold)=icc 
         end do
         
         ic=icount_out(ii,jj,kk)
         do ll=1,ic 
            iold = num_new_out(ll,ii,jj,kk)
            icc  = icc + 1
            num_reorder(iold)=icc 
         end do
      end do
      end do
      end do
      deallocate(pos)
      deallocate(icount_in)
      deallocate(icount_out)
      deallocate(num_new_in)
      deallocate(num_new_out)

      return
      end subroutine reord_node
