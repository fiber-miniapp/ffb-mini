/* INCLUDE FILE FOR GENERAL FILE VER-2.1 */

/* new keywords (79-95) are added by Y.Yamade    2010.10.21 */
/* new keywords (96-97) are added by Y.Yamade    2010.11.08 */
/* new keywords (98-99) are added by H.Yoshimura 2012.10.22 */
/* new keywords (100-107) are added by Y.Yamade  2013.02.07 */

#define LOGLENGTH 90
char static MESSAGE[LOGLENGTH];

#define FILEMAX 1024
static FILE *fp[FILEMAX];
static int  IRREV[FILEMAX];

static int   ZERO  =  0 ;
static int   ONE   =  1 ;
static int   TWO   =  2 ;
static int   THREE =  3 ;

static const int   ANY    = -1 ;
static const int   INQUIR =  0 ;
static const int   OPEN   =  1 ;
static const int   ASCII  =  1 ;
static const int   UNFMT  =  2 ;
static const int   BINARY =  3 ;
static const int   NEWSET =  4 ;
static const int   CLOSE  =  5 ;

static const int   INTARY =  6 ; 
static const int   FLTARY =  7 ; 
static const int   DBLARY =  8 ; 

static const int   IENDIAN[2] = {1,16777216};

#define MAXMOD  3  
static const char  *FLMODE[MAXMOD+1] = { "DUMMY   "
                                        ,"ASCII   "
                                        ,"UNFORMAT"
                                        ,"BINARY  " 
                                        } ; 

#define MAXTYP  8  
static const char  *HEADER[MAXTYP+1] = { "DUMMY   "
                                        ,"#A_GF_V1"
                                        ,"#U_GF_V1"  
                                        ,"#B_GF_V1"  
                                        ,"#NEW_SET"  
                                        ,"#ENDFILE"  
                                        ,"#INT_ARY"  
                                        ,"#FLT_ARY"  
                                        ,"#DBL_ARY"  
                                         };
static const int   ARY0D = 0 ; 
static const int   ARY1D = 1 ; 
static const int   ARY2D = 2 ; 

static const int   UNKNWN = 0 ; 
static const int   HEAD   = 1 ; 
static const int   DATA   = 2 ; 
static const int   END    = 3 ; 

static const int   NONE    = 0 ; 
static const int   SELECT  = 1 ; 
static const int   ALL     = 2 ; 
static const int   GIVE    = 3 ; 

static const int   EXIT    = 999 ; 

static const char  *FMTAS = "%8s  \n";
static const char  *FMTAN = "%30s \n";
static const char  *FMTAM = "%60s \n";
static const char  *FMTI1 = "%12d ";
static const char  *FMTI2 = "%12d %12d \n";
static const char  *FMTI6 = "%12d %12d %12d %12d %12d %12d \n";
static const char  *FMTF1 = "%14.6E \n";
static const char  *FMTF6 = "%14.6E 14.6E 14.6E 14.6E 14.6E \n";

typedef struct _TABLE
{
char   *name     ;
int    keynum    ;
int    itype     ;
int    *max2,*max;
int    *num2,*num;
int    *i0,*i1,*i2,*i3,*in;
float  *f0,*f1,*f2,*f3,*fn;
} TABLE;

#define  UNDEF     -1 
#define  TYPEI0     0 
#define  TYPEI1     1 
#define  TYPEI2     2 
#define  TYPEI3     3 
#define  TYPEIN     4 
#define  TYPEF0     5 
#define  TYPEF1     6 
#define  TYPEF2     7 
#define  TYPEF3     8 
#define  TYPEFN     9 

#define MAXKEY  107
static const char  *KEYWD0[MAXKEY+1] = 
{  "DUMMY   ",
   "*GRID_2D" ,"*GRID_3D" ,"*NODE_2D" ,"*NODE_3D" ,"*NODE_XD" ,
   "*TEMP_2D" ,"*TEMP_3D" ,"*TEMP_2E" ,"*TEMP_3E" ,"*PRES_2D" ,
   "*PRES_3D" ,"*PRES_2E" ,"*PRES_3E" ,"*DENS_2D" ,"*DENS_3D" ,
   "*DENS_2E" ,"*DENS_3E" ,"*TURK_2D" ,"*TURK_3D" ,"*TURK_2E" ,
   "*TURK_3E" ,"*TURE_2D" ,"*TURE_3D" ,"*TURE_2E" ,"*TURE_3E" ,
   "*SCAL_2D" ,"*SCAL_3D" ,"*SCAL_2E" ,"*SCAL_3E" ,"*VELO_2D" ,
   "*VELO_3D" ,"*VELO_2E" ,"*VELO_3E" ,"*VECT_2D" ,"*VECT_3D" ,
   "*VECT_2E" ,"*VECT_3E" ,"*STIJ_2D" ,"*STIJ_3D" ,"*STIJ_2E" ,
   "*STIJ_3E" ,"*TTIJ_2D" ,"*TTIJ_3D" ,"*TTIJ_2E" ,"*TTIJ_3E" ,
   "*TRAN_2D" ,"*TRAN_3D" ,"*TRAN_2E" ,"*TRAN_3E" ,"*BC_INLT" ,
   "*BC_WALL" ,"*BC_MWAL" ,"*BC_SYMT" ,"*BC_CYCL" ,"*BC_FREE" ,
   "*TIME_PS" ,"*STEP_PS" ,"*BC_IV2D" ,"*BC_IV3D" ,"*BC_RELY" ,
   "*BC_DAMP" ,"*BC_FACT" ,"*BC_BODY" ,"*BC_WV2D" ,"*BC_WV3D" ,
   "*BC_INTR" ,"*BC_OLAP" ,"*BC_OC3D" ,"*BC_CNER" ,"*BC_PSET" ,
   "*BC_PGET" ,"*BC_ESET" ,"*BC_EGET" ,"*ELM_TYP" ,"*ELM_ATR" ,
   "*PT_NODE" ,"*PT_ELEM" ,"*HISTORY" ,"*ELM_MDA" ,"*ELM_PRO" ,
   "*BC_TMPN" ,"*BC_TMPV" ,"*BC_HFXN" ,"*BC_HFXV" ,"*BC_HTRN" ,
   "*BC_HTRV" ,"*BC_HSRN" ,"*BC_HSRV" ,"*BC_TMPE" ,"*BC_TPEV" ,
   "*BC_FORC" ,"*BC_FOIN" ,"*LIQD_3D" ,"*TURO_3D" ,"*HSTLIST" ,
   "*BC_INLE" ,"*BC_VELE" ,"*MOV1_3D" ,"*MOV0_3D" ,"*VFRC_3E" ,
   "*FREQ_FR" ,"*STEP_FR" ,"*SRC3D_T" ,"*SRC3D_F" ,"*APS3D_F" ,
   "*APW3D_F" ,"*AVL3D_F"                                     };

static const int   TYPE[MAXKEY+1] =  
{    UNDEF,
     TYPEF2 ,  TYPEF3 ,  TYPEIN ,  TYPEIN ,  TYPEIN ,
     TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,
     TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,
     TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,
     TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,
     TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF1 ,  TYPEF2 ,
     TYPEF3 ,  TYPEF2 ,  TYPEF3 ,  TYPEF2 ,  TYPEF3 ,
     TYPEF2 ,  TYPEF3 ,  TYPEFN ,  TYPEFN ,  TYPEFN ,
     TYPEFN ,  TYPEFN ,  TYPEFN ,  TYPEFN ,  TYPEFN ,
     TYPEFN ,  TYPEFN ,  TYPEFN ,  TYPEFN ,  TYPEI1 ,
     TYPEI1 ,  TYPEI1 ,  TYPEI1 ,  TYPEI2 ,  TYPEI1 ,
     TYPEF0 ,  TYPEI0 ,  TYPEF2 ,  TYPEF3 ,  TYPEI2 ,
     TYPEI1 ,  TYPEF1 ,  TYPEI1 ,  TYPEF2 ,  TYPEF3 ,
     TYPEI3 ,  TYPEI2 ,  TYPEF3 ,  TYPEI1 ,  TYPEI3 ,
     TYPEF3 ,  TYPEI3 ,  TYPEF3 ,  TYPEI1 ,  TYPEI1 ,
     TYPEI1 ,  TYPEI1 ,  TYPEFN ,  TYPEI1 ,  TYPEI1 ,
     TYPEI1 ,  TYPEF1 ,  TYPEI1 ,  TYPEF1 ,  TYPEI1 ,
     TYPEF1 ,  TYPEI1 ,  TYPEF1 ,  TYPEI1 ,  TYPEF1 ,
     TYPEI2 ,  TYPEI2 ,  TYPEF1 ,  TYPEF1 ,  TYPEI1 ,
     TYPEI1 ,  TYPEF3 ,  TYPEF3 ,  TYPEF3 ,  TYPEF1 ,
     TYPEF0 ,  TYPEI0 ,  TYPEF1 ,  TYPEFN ,  TYPEFN ,
     TYPEF1 ,  TYPEFN                                };

static const char   *NAME0[MAXKEY+1] =
{  "DUMMY   ",
   " GRID COORDINATES        (2-D)" ," GRID COORDINATES        (3-D)" ," NODE TABLE              (2-D)" ,  /*  1, 2, 3 */ 
   " NODE TABLE              (3-D)" ," GENERIC NODE TABLE           " ," TEMPERATURE AT NODES    (2-D)" ,  /*  4, 5, 6 */ 
   " TEMPERATURE AT NODES    (3-D)" ," TEMPERATURE AT ELEMENTS (2-D)" ," TEMPERATURE AT ELEMENTS (3-D)" ,  /*  7, 8, 9 */ 
   " PRESSURE AT NODES       (2-D)" ," PRESSURE AT NODES       (3-D)" ," PRESSURE AT ELEMENTS    (2-D)" ,  /* 10,11,12 */ 
   " PRESSURE AT ELEMENTS    (3-D)" ," DENSITY AT NODES        (2-D)" ," DENSITY AT NODES        (3-D)" ,  /* 13,14,15 */ 
   " DENSITY AT ELEMENTS     (2-D)" ," DENSITY AT ELEMENTS     (3-D)" ," TURB ENERGY AT NODES    (2-D)" ,  /* 16,17,18 */ 
   " TURB ENERGY AT NODES    (3-D)" ," TURB ENERGY AT ELEMENTS (2-D)" ," TURB ENERGY AT ELEMENTS (3-D)" ,  /* 19,20,21 */ 
   " DISSIPATION RATE AT NOD (2-D)" ," DISSIPATION RATE AT NOD (3-D)" ," DISSIPATION RATE AT ELE (2-D)" ,  /* 22,23,24 */ 
   " DISSIPATION RATE AT ELE (3-D)" ," GENERIC SCALAR AT NODES (2-D)" ," GENERIC SCALAR AT NODES (3-D)" ,  /* 25,26,27 */ 
   " GENERIC SCALAR AT ELEMS (2-D)" ," GENERIC SCALAR AT ELEMS (3-D)" ," FLOW VELOCITY AT NODES  (2-D)" ,  /* 28,29,30 */ 
   " FLOW VELOCITY AT NODES  (3-D)" ," FLOW VELOCITY AT ELEMS  (2-D)" ," FLOW VELOCITY AT ELEMS  (3-D)" ,  /* 31,32,33 */ 
   " GENERIC VECTOR AT NODES (2-D)" ," GENERIC VECTOR AT NODES (3-D)" ," GENERIC VECTOR AT ELEMS (2-D)" ,  /* 34,35,36 */ 
   " GENERIC VECTOR AT ELEMS (3-D)" ," STRAIN  TENSOR AT NODES (2-D)" ," STRAIN  TENSOR AT NODES (3-D)" ,  /* 37,38,39 */ 
   " STRAIN  TENSOR AT ELEMS (2-D)" ," STRAIN  TENSOR AT ELEMS (3-D)" ," STRESS  TENSOR AT NODES (2-D)" ,  /* 40,41,42 */ 
   " STRESS  TENSOR AT NODES (3-D)" ," STRESS  TENSOR AT ELEMS (2-D)" ," STRESS  TENSOR AT ELEMS (3-D)" ,  /* 43,44,45 */ 
   " GENERIC TENSOR AT NODES (2-D)" ," GENERIC TENSOR AT NODES (3-D)" ," GENERIC TENSOR AT ELEMS (2-D)" ,  /* 46,47,48 */ 
   " GENERIC TENSOR AT ELEMS (3-D)" ," INLET BOUNDARY NODES         " ," WALL  BOUNDARY NODES         " ,  /* 49,50,51 */ 
   " MOVING WALL BOUNDARY NODES   " ," SYMMETRIC    BOUNDARY NODES  " ," CYCLIC       BOUNDARY NODES  " ,  /* 52,53,54 */ 
   " FREE  BOUNDARY NODES         " ," PRESENT TIME                 " ," PRESENT STEP                 " ,  /* 55,56,57 */ 
   " INLET BOUNDARY VELOCITIES 2-D" ," INLET BOUNDARY VELOCITIES 3-D" ," DEPENDING    BOUNDARY NODES  " ,  /* 58,59,60 */ 
   " EDDY VISCOSITY DAMP  ELEMENTS" ," EDDY VISCOSITY DAMP  COEFF   " ," BODY SURFACE BOUNDARY NODES  " ,  /* 61,62,63 */ 
   " MOVING WALL VELOCITIES    2-D" ," MOVING WALL VELOCITIES    3-D" ," INTER-CONNECT BOUNDARY NODES " ,  /* 64,65,66 */ 
   " OVERLAPPING   BOUNDARY NODES " ," OVERLAPPING COORDINATES   3-D" ," CORNER BOUNDARY NODES        " ,  /* 67,68,69 */ 
   " OVERSET BOUNDARY NODES       " ," OVERSET BOUNDARY COORDINATES " ," OVERSET BOUNDARY ELEMENTS    " ,  /* 70,71,72 */ 
   " OVERSET BOUNDARY COORDINATES " ," ELEMENT TYPE                 " ," ELEMENT FRAME ATTRIBUTES     " ,  /* 73,74,75 */ 
   " LOCAL-TO-GLOBAL NODE    LIST " ," LOCAL-TO-GLOBAL ELEMENT LIST " ," TIME HISTORY DATA            " ,  /* 76,77,78 */ 
   "ELEMENT MEDIA ATTRIBUTES      " ,"ELEMENT MATERIAL PROPERTY ID  " ,"TEMP-FIX BOUNDARY NODES       " ,  /* 79,80,81 */
   "TEMPERATURE AT BOUNDARY       " ,"HEAT-FLUX BOUNDARY NODES      " ,"HEAT FLUX AT BOUNDARY         " ,  /* 82,83,84 */
   "HEAT-TRANSFER BOUNDARY NODES  " ,"HEAT-TRANSFER  COEFFICIENT    " ,"HEAT-SOURCE ELEMENTS          " ,  /* 85,86,87 */
   "HEAT-SOURCE                   " ,"TEMP-FIX ELEMENT              " ,"TEMPRATURE AT ELEMENTS        " ,  /* 88,89,90 */
   "FLUID FORCE ELEMENT           " ,"INLET NODES OF F-FORCE REGION " ,"LIQUID FRACTION               " ,  /* 91,92,93 */
   "SPECIFIC DISSIPATION RATE     " ,"HISTORY DATA ID               " ,"VELOCITY-FIX ELEMENT          " ,  /* 94,95,96 */
   "VELOCITY AT ELEMENTS          " ,"MOVING VELOCITY  AT NODES(3-D)" ,"MOVING VELOCITY0 AT NODES(3-D)" ,  /* 97,98,99 */
   "VOLUME FRACTION AT ELEMENTS   " ," FREQUENCY                    " ," FREQUENCY NUMBER             " ,  /*100,101,102*/
   " SOUND SORCE (TIME)           " ," SOUND SORCE (FREQUENCY)      " ," SOUND PRESSURE               " ,  /*103,104,105*/
   " SOUND POWER                  " ," PRESSURE GRADIENT            "                                      /*106,107    */
   };

static const char  *CTYPE[10] = { 
                                    "TYPEI0","TYPEI1","TYPEI2","TYPEI3","TYPEIN"
                                   ,"TYPEF0","TYPEF1","TYPEF2","TYPEF3","TYPEFN"
                                 } ;  

/* END OF INCLUDE FILE FOR GENERAL FILE VER-1.2 */
