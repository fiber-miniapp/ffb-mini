#ifdef PROF_MAPROF
      use mod_maprof

#define  TM_MAIN_LOOP              0
#define    TM_VEL3D1               1
#define      TM_VEL3D1_OP1         2
#define      TM_VEL3D1_OP2         3
#define      TM_VEL3D1_OP3         4
#define      TM_VEL3D1_COM         5
#define      TM_CALUEL             6
#define      TM_E2PMATR            7
#define      TM_DGNSCL             8
#define      TM_CLRCRS             9
#define      TM_CRSCVA             10
#define      TM_BCGS3X             11
#define        TM_BCGS3X_COM       12
#define        TM_CALAX3           13
#define          TM_CALAX3_CAL     14
#define          TM_CALAX3_COM     15
#define    TM_PRES3E               16
#define      TM_BCGSXE             17
#define        TM_PRES3E_COM       18
#define        TM_CALLAP           19
#define          TM_GRAD3X         20
#define            TM_GRAD3X_OP0   21
#define            TM_GRAD3X_OP1   22
#define            TM_GRAD3X_OP2   23
#define            TM_GRAD3X_OP3   24
#define            TM_GRAD3X_COM   25
#define          TM_FLD3X2         26

#define TIME_START(id) call maprof_time_start(id)
#define TIME_STOP(id)  call maprof_time_stop(id)

#elif defined PROF_FAPP

#define TIME_START(id) call fapp_start(#id, 0, 0)
#define TIME_STOP(id)  call fapp_stop(#id, 0, 0)

#else

#define TIME_START(id)
#define TIME_STOP(id)

#endif
