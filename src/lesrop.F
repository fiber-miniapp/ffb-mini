C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : LESROP                                                C
C                                                                      C
C                                       WRITTEN BY Y.GUO               C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE LESROP(IUT0,IUT5,IUT6,
     *                  ISOLP,
     *                  NMAXB,
     *                  BTDCOE,DIVESC,
     *                  JSORT,JCOLOR,JUNROL,NDIVX,NDIVY,NDIVZ,NEIP,
     *                  NRFN,IRFNMW,IRFNFF,NGRID,
     *                  MER,MPR,MEPR,MPPR,MBR,MBPR,MDOMR,
     *                  JPRESS,IVELIN,VELIN0,
     *                  U000,
     *                  EPSP,EPSREP,
     *                  IWRITE,JGRID, 
     *                  IERR)
      IMPLICIT REAL*4(A-H,O-Z)
C
      REAL*4    U000
C
      DIMENSION VELIN0(3)
C
      DIMENSION BTDCOE(4)
C
      INTEGER*4 NEIP(4)
C
      PARAMETER ( MKEYWD = 85 )
      CHARACTER*8 CKEYWD(MKEYWD)
      DATA CKEYWD( 1) / '#OPTIONS' /
      DATA CKEYWD( 2) / '#OPTIONE' /
      DATA CKEYWD( 3) / '#GRDFILT' /
      DATA CKEYWD( 4) / '#OVERROP' /
      DATA CKEYWD( 5) / '#BC_FREE' /
      DATA CKEYWD( 6) / '#BC_NRBC' /
      DATA CKEYWD( 7) / '#BC_NRIO' /
      DATA CKEYWD( 8) / '#BC_EPSM' /
      DATA CKEYWD( 9) / '#BC_MICX' /
      DATA CKEYWD(10) / '#CALHEAD' /
      DATA CKEYWD(11) / '#SOL_TMP' /
      DATA CKEYWD(12) / '#SOL_PRS' /
      DATA CKEYWD(13) / '#ITR_RCM' /
      DATA CKEYWD(14) / '#ITR_NRX' /
      DATA CKEYWD(15) / '#MOD_DES' /
      DATA CKEYWD(16) / '#LIM_DSM' /
      DATA CKEYWD(17) / '#BC_PREF' /
      DATA CKEYWD(18) / '#CAVPREF' /
      DATA CKEYWD(19) / '#VT_INIT' /
      DATA CKEYWD(20) / '#ITRT_IN' /
      DATA CKEYWD(21) / '#ITRP_IN' /
      DATA CKEYWD(22) / '#COE_BTD' /
      DATA CKEYWD(23) / '#RO_SORT' /
      DATA CKEYWD(24) / '#RO_COLR' /
      DATA CKEYWD(25) / '#MLT_OMG' /
      DATA CKEYWD(26) / '#RCAP_RF' /
      DATA CKEYWD(27) / '#RF_MESH' /
      DATA CKEYWD(28) / '#RF_FLOW' /
      DATA CKEYWD(29) / '#RCAP_CP' /
      DATA CKEYWD(30) / '#SZ_GRID' /
      DATA CKEYWD(31) / '#SZ_CNCT' /
      DATA CKEYWD(32) / '#SZ_BOUN' /
      DATA CKEYWD(33) / '#MONITOR' /
      DATA CKEYWD(34) / '#DT_AUTO' /
      DATA CKEYWD(35) / '#DT_LOCL' /
      DATA CKEYWD(36) / '#CFL_VAL' /
      DATA CKEYWD(37) / '#RELAX_P' /
      DATA CKEYWD(38) / '#RELAX_V' /
      DATA CKEYWD(39) / '#FFOBJCT' /
      DATA CKEYWD(40) / '#FR_CONT' /
      DATA CKEYWD(41) / '#RELAX_Q' /
      DATA CKEYWD(42) / '#DIAGVEL' /
      DATA CKEYWD(43) / '#EFFE_DT' /
      DATA CKEYWD(44) / '#LIMIT_P' /
      DATA CKEYWD(45) / '#FS_PRES' /
      DATA CKEYWD(46) / '#PRS_ELM' /
      DATA CKEYWD(47) / '#INLT__K' /
      DATA CKEYWD(48) / '#INLT_EP' /
      DATA CKEYWD(49) / '#RELAX_K' /
      DATA CKEYWD(50) / '#RELAX_E' /      
      DATA CKEYWD(51) / '#SETTURB' /      
      DATA CKEYWD(52) / '#FIXFLOW' /
      DATA CKEYWD(53) / '#GRAVITY' /
      DATA CKEYWD(54) / '#INLTVEL' /
      DATA CKEYWD(55) / '#PRO_TBL' /
      DATA CKEYWD(56) / '#EPSIL_K' /      
      DATA CKEYWD(57) / '#EPSIL_E' /      
      DATA CKEYWD(58) / '#EPSRE_V' /
      DATA CKEYWD(59) / '#EPSRE_P' /
      DATA CKEYWD(60) / '#EPSRE_K' /
      DATA CKEYWD(61) / '#EPSRE_E' /
      DATA CKEYWD(62) / '#RELAX_T' /
      DATA CKEYWD(63) / '#EPSIL_T' /
      DATA CKEYWD(64) / '#EPSRE_T' /
      DATA CKEYWD(65) / '#IDR__NS' /
      DATA CKEYWD(66) / '#IDR__NL' /
      DATA CKEYWD(67) / '#USE_ALE' /
      DATA CKEYWD(68) / '#MRESALE' /
      DATA CKEYWD(69) / '#CL_TIME' /
      DATA CKEYWD(70) / '#ALE_DBG' /
      DATA CKEYWD(71) / '#ALELYNG' /
      DATA CKEYWD(72) / '#SPWRITE' /
      DATA CKEYWD(73) / '#GFWRITE' /
      DATA CKEYWD(74) / '#EPS_MID' /
      DATA CKEYWD(75) / '#AXUNROL' /
      DATA CKEYWD(76) / '#DBLGRID' /
      DATA CKEYWD(77) / '#SOL_VEL' /
      DATA CKEYWD(78) / '#DIV_ESC' /
      DATA CKEYWD(79) / '#MULTI_R' /
      DATA CKEYWD(80) / '#DSM_OLD' /
      DATA CKEYWD(81) / '#BLK_NUM' /
      DATA CKEYWD(82) / '#WRTOVST' /
      DATA CKEYWD(83) / '#MLPHASE' /
      DATA CKEYWD(84) / '#HSTATIC' /
      DATA CKEYWD(85) / '#FIX_VOF' /
C
      CHARACTER*60 CBUF,CBUF2
C
      CHARACTER*60 ERMSG1
     & / ' ## THIS PARAMETER DOES NOT WORK IN THIS VERSION (FFB8.1)  ' /
C
      CHARACTER*60 ERMSG2
     & / ' ## THIS PARAMETER DOES NOT WORK IN MINIAPP                ' /
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE LESROP: FATAL      ERROR OCCURENCE; RETURNED' /
C
      CHARACTER*60 EREXP1
     & / ' AN ILLEGAL VALUE WAS SPECIFIED FOR CONTROL PARAMETER      ' /
C
      CHARACTER*5 CONOFF(0:1)
      DATA CONOFF(0) /'OFF  '/
      DATA CONOFF(1) /'ON   '/
C
C
C
C      READ CONTROL PARAMETERS FOR OPTIONAL FUNCTIONS
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C          IUT5        ; FILE NUMBER TO READ PARAMETERS AND FILE NAMES
C          IUT6        ; FILE NUMBER TO WRITE CALCULATION SEQUENCE
C
C
C       (2) OUTPUT
C
C          ISOLP       ; MATRIX SOLVER FOR PRESSURE EQUATION
C                   1 --- Bi-CGSTAB
C                   2 --- RESIDUAL CUTTING METHOD (RCM)
C                   3 --- IDR
C
C          NMAXB       ; THE NUMBER OF INNER ITERATIONS OF RCM
C
C          JSORT       ;RENUMBERING FLAG FOR SORTING      (0:OFF, 1:ON)
C          JCOLOR      ;RENUMBERING FLAG FOR COLORING     (0:OFF, 1:ON)
C          JUNROL      ;FULL UNROLL FLAG FOR AX OPERATION (0:OFF, 1:ON)
C
C          NRFN        ;NUMBER OF REFINE  (DEFAULT=0)
C                       IT MUST BE ZERO OR ONE IN THIS VERSION 
C          IRFNMW      ;REFINED MESH AND BOUN FILE WILL BE OUTPUT
C                       WHEN THIS PARAMETER IS ONE (DEFAULT=0)      
C          IRFNFF      ;READ IN FLOW DATA (U,V,W,P) WILL BE REFINED
C                       WHEN THIS PARAMETER IS ONE (DEFAULT=0)      
C          NGRID       ;
C                       
      IERR = 0
C
      EPSREP = -1.0
C
   10 READ(IUT5,'(A60)',END=100) CBUF
      IF(CBUF(1:8).EQ.CKEYWD(1)) THEN
          WRITE(IUT6,*) ' LESROP: START READING OPTIONAL PARAMETERS '
      ELSE
          GO TO 10
      ENDIF
C
 1000 CONTINUE
C
      READ(IUT5,'(A60)') CBUF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(3)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(3), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(4)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(4), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(5)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(5), '" IS DETECTED.'
          WRITE(IUT0,'(A60)') ERMSG2
          GO TO 999
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(6)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(6), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(7)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(7), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(8)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(8), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(9)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(9), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(10)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(10), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(11)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(11), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(12)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(12), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ISOLP
          IF(ISOLP.LT.1 .OR. ISOLP.GT.3)THEN
              WRITE(IUT0,'(A60)') ISOLP
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          IF(ISOLP.EQ.1) THEN
              WRITE(IUT6,*)
     &        ' LESROP: BI-CGSTAB WIL BE APPLIED FOR PRS-E.Q.'
          ELSE IF(ISOLP.EQ.2) THEN
              WRITE(IUT6,*)
     *        ' LESROP: RCM WILL BE APPLIED FOR PRS.-E.Q.'
          ELSE IF(ISOLP.EQ.3) THEN
              WRITE(IUT6,*)
     *        ' LESROP: IDR WILL BE APPLIED FOR PRS.-E.Q.'
              WRITE(IUT6,*) ERMSG2
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(13)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(13), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NMAXB
          IF(NMAXB.LT.1)THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*)
     &    ' LESROP: NUMBER OF RCM INNER ITERATIONS =',NMAXB
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(14)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(14), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(15)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(15), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(16)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(16), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(17)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(17), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(18)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(18), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(19)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(19), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(20)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(20), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(21)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(21), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(22)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(22), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) BTDCOE(1),BTDCOE(2),BTDCOE(3),BTDCOE(4)
          IF(BTDCOE(1).LT.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: COEFFICIENT OF BTD TERM '
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR TET = ', BTDCOE(1)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR PYR = ', BTDCOE(2)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR WED = ', BTDCOE(3)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR HEX = ', BTDCOE(4)
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(23)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(23), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) JSORT,NDIVX,NDIVY,NDIVZ
          IF(JSORT.NE.0 .AND. JSORT.NE.1) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: REORDERING '
          WRITE(IUT6,*) ' LESROP: SORT       :',CONOFF(JSORT )
          WRITE(IUT6,*) ' LESROP: NDIVX      =',NDIVX
          WRITE(IUT6,*) ' LESROP: NDIVY      =',NDIVY
          WRITE(IUT6,*) ' LESROP: NDIVZ      =',NDIVZ
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(24)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(24), '" IS DETECTED.'
#ifdef NO_METIS
          WRITE(IUT6,*)
     *    ' LESROP: THIS OPTION IS NOT AVALABLE WITH THIS SOLVER.'
          GO TO 999
#endif
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) JCOLOR,NEIP(1),NEIP(2),NEIP(3),NEIP(4)
          IF(JCOLOR.NE.0 .AND. JCOLOR.NE.1) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: REORDERING '
          WRITE(IUT6,*) ' LESROP: COLORING   :',CONOFF(JCOLOR)
          WRITE(IUT6,*) ' LESROP: NEIP(TET)  =',NEIP(1)
          WRITE(IUT6,*) ' LESROP: NEIP(PRD)  =',NEIP(2)
          WRITE(IUT6,*) ' LESROP: NEIP(WED)  =',NEIP(3)
          WRITE(IUT6,*) ' LESROP: NEIP(HEX)  =',NEIP(4)
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(75)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(75),'" IS DETECTED.'
          JUNROL=1
C
          WRITE(IUT6,*) ' LESROP: AX OPERATION '
          WRITE(IUT6,*) ' LESROP: FULL UNROOL:',CONOFF(JUNROL)
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(76)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(76), '" IS DETECTED.'
C
          JGRID = 2
C
          WRITE(IUT6,*) 
     *    ' LESROP: DOUBLE PRECISION GRID DATA WILL BE READ'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(25)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(25), '" IS DETECTED.'
          WRITE(IUT0,'(A60)') EREXP1
      ENDIF
C
C     * #RCAP_RF *
      IF(CBUF(1:8).EQ.CKEYWD(26)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(26),'" IS DETECTED.'
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NRFN,NGRID
C
          IF(NRFN.LT.0 .AND. NRFN.GT.5 ) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              WRITE(IUT0,'(A60)') 'NUM. OF REFINE MUST BE 0, 1 .. 5'
              GO TO 999
          ENDIF
C
          IF (NGRID.LT.-1) THEN
             WRITE(IUT0,'(A60)')
             WRITE(IUT0,'(A60)') EREXP1
             WRITE(IUT0,'(A60)') 'NUM. OF NGRID MUST BE LATGER THAN 0'
             GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: NUMBER OF REFINE = ',NRFN
          IF(NGRID.EQ.-1) THEN
              WRITE(IUT6,*) ' LESROP: GLOBAL REFINE   '
          ELSE
              WRITE(IUT6,*) ' LESROP: NUMBER OF NGRID  = ',NGRID
          ENDIF
C
#ifdef NO_REFINER
          if (NRFN.gt.0) then
             WRITE(IUT6,*)
     &          ' LESROP: REVOCAP_Refiner MUST BE NEEDED FOR NRFN>0.'
             GO TO 999
          endif
#endif
C
       ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(27)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(27), '" IS DETECTED.'
C
          IRFNMW = 1
C
          WRITE(IUT6,*) 
     *    ' LESROP: REFINED MESH AND B.C FILE WILL BE WRITTEN'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(28)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(28), '" IS DETECTED.'
C
          IRFNFF = 1
C
          WRITE(IUT6,*) 
     *    ' LESROP: FLOW DATA READ IN WILL BE REFINED           '
      ENDIF
C
C      * #RCAP_CP *
      IF(CBUF(1:8).EQ.CKEYWD(29)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(29),'" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(30)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(30), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) MER,MPR
C
          WRITE(IUT6,*) ' LESROP: MAX. NUMBER FOR GRID' 
          WRITE(IUT6,*) ' LESROP: MER  =',MER
          WRITE(IUT6,*) ' LESROP: MPR  =',MPR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(31)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(31), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) MEPR,MPPR
C
          WRITE(IUT6,*) ' LESROP: MAX. NUMBER FOR CNNECTIVITY DATA' 
          WRITE(IUT6,*) ' LESROP: MEPR =',MEPR
          WRITE(IUT6,*) ' LESROP: MPPR =',MPPR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(32)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(32), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) MBR,MBPR,MDOMR
C
          WRITE(IUT6,*) ' LESROP: MAX. NUMBER FOR B.C. DATA' 
          WRITE(IUT6,*) ' LESROP: MBR  =',MBR
          WRITE(IUT6,*) ' LESROP: MBPR =',MBPR
          WRITE(IUT6,*) ' LESROP: MDOMR=',MDOMR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(33)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(33), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(34)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(34), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(35)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(35), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(36)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(36), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(37)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(37), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(38)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(38), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(39)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(39), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(40)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(40), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(41)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(41), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(42)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(42), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(43)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(43), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(44)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(44), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(45)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(45), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(46)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(46), '" IS DETECTED.'
C
          JPRESS=2
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(47)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(47), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(48)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(48), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C 
      IF(CBUF(1:8).EQ.CKEYWD(49)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(49), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(50)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(50), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(51)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(51), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(52)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(52), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(53)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(53), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(54)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(54), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) VELIN0(1),VELIN0(2),VELIN0(3)
          IVELIN=1
C
          WRITE(IUT6,*) ' LESROP: INLET VEL. IS SET AS FOLLOWS; '
          WRITE(IUT6,*) ' LESROP: U                  =',VELIN0(1)
          WRITE(IUT6,*) ' LESROP: V                  =',VELIN0(2)
          WRITE(IUT6,*) ' LESROP: Z                  =',VELIN0(3)
C
          VELIN0(1)=VELIN0(1)/U000
          VELIN0(2)=VELIN0(2)/U000
          VELIN0(3)=VELIN0(3)/U000
      ENDIF 
C
      IF(CBUF(1:8).EQ.CKEYWD(55)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(55), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(56)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(56), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(57)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(57), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(58)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(58), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(59)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(59), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSREP
C
          IF(EPSREP.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(60)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(60), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(61)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(61), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(62)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(62), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(63)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(63), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(64)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(64), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(65)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(65), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(66)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(66), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(67)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(67), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(68)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(68), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(69)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(69), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(70)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(70), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(71)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(71), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(72)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(72), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(73)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(73), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IWRITE
C
          WRITE(IUT6,*) ' LESROP: GF-WRITE-MODE IWRITE =',IWRITE
          IF(IWRITE.EQ.1) THEN
              WRITE(IUT6,*) ' LESROP: ASCII-MODE' 
          ELSE IF(IWRITE.EQ.2) THEN
              WRITE(IUT6,*) ' LESROP: UNFORMATED-MODE'
          ELSE IF(IWRITE.EQ.3) THEN
              WRITE(IUT6,*) ' LESROP: BINARY MODE'
          ELSE 
              IWRITE=2
              WRITE(IUT6,*) ' LESROP: UNFORMATED-MODE'
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(74)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(74), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(77)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(77), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ISOLV
          IF(ISOLV.NE.1 .AND. ISOLV.NE.2) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(78)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(78), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) DIVESC
C
          IF(DIVESC.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: DIVESC = ', DIVESC
C
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(79)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(79), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(80)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(80), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(81)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(81), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG2
          GO TO 999
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(83)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(83), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(84)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(84), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(85)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(85), '" IS DETECTED.'
         WRITE(IUT6,*) ERMSG2
         GO TO 999
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(2)) THEN
          WRITE(IUT6,*) ' LESROP: END   READING OPTIONAL PARAMETERS'
      ELSE
          GO TO 1000
      ENDIF
C
C     DEFAULT VALUES
C
      IF(EPSREP.LT.0.0) EPSREP = EPSP
C
      RETURN
C
  999 CONTINUE
      WRITE(IUT0,'(A60)') ERMSGB
      IERR = 1
      RETURN
C
  100 WRITE(IUT6,*) ' LESROP: NO OPTIONAL PARAMETERS'
      RETURN
C
      END
