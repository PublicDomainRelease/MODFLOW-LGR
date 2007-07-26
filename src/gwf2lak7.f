! Time of File Save by ERB: 3/23/2004 1:57PM
Crgn  Made EVAP, PRECIP, SEEP, and SEEP3 double precision Nov. 6, 2006
Cdep  Converted from MODFLOW-2000 to MODFLOW-2005 May 2006, RGN and DEP
Cdep  Lake Package modified by DEP and RGN May 20 through June 29, 2006
Cdep  to compute lake outflow as a function of lake stage inside the
Cdep  FORMULATE MODULE. Lake outflow had been previously computed in the
Cdep  Streamflow-Routing Package. The Streamflow-Routing (sfr7) Package
Cdep  was also modified to remain compatible with the modifications in
Cdep  the Lake Package.
C     Modifications made February and March 21, 2004; DEP
C     Last change:  MLM & LFK  10 Oct 2003;  LFK 21 Jan 2004
C     Previous change:  ERB  13 Sep 2002    9:22 am
C
C
      MODULE GWFLAKMODULE
C------VERSION 7;   CREATED FOR MODFLOW-2005
        CHARACTER(LEN=64),PARAMETER ::Version_lak =
     +'$Id: gwf2lak7.f 3297 2007-05-03 17:48:43Z rsregan $'
        INTEGER,SAVE,POINTER   ::NLAKES,NLAKESAR,ILKCB,NSSITR
        INTEGER,SAVE,POINTER   ::MXLKND,LKNODE,ICMX,NCLS,LWRT,NDV,NTRB
        REAL,   SAVE,POINTER   ::THETA,SSCNCR
Crgn    Added budget variables for GSFLOW CSV file
        REAL,   SAVE,POINTER   ::TOTGWIN_LAK,TOTGWOT_LAK,TOTDELSTOR_LAK
        REAL,   SAVE,POINTER   ::TOTSTOR_LAK,TOTEVAP_LAK,TOTPPT_LAK
        REAL,   SAVE,POINTER   ::TOTRUNF_LAK,TOTWTHDRW_LAK,TOTSURFIN_LAK
        REAL,   SAVE,POINTER   ::TOTSURFOT_LAK
        INTEGER,SAVE, DIMENSION(:),  POINTER ::ICS, NCNCVR, LIMERR
        INTEGER,SAVE, DIMENSION(:,:),POINTER ::ILAKE,ITRB,IDIV,ISUB,IRK
        INTEGER,SAVE, DIMENSION(:,:,:),POINTER ::LKARR1
        REAL,   SAVE, DIMENSION(:),  POINTER ::STAGES
        DOUBLE PRECISION,SAVE,DIMENSION(:), POINTER ::STGNEW,STGOLD,
     +                                                STGITER
        REAL,   SAVE, DIMENSION(:),  POINTER ::VOL,FLOB,DSRFOT
        REAL,   SAVE, DIMENSION(:),  POINTER ::PRCPLK,EVAPLK,BEDLAK
        REAL,   SAVE, DIMENSION(:),  POINTER ::WTHDRW,RNF,CUMRNF
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMPPT,CUMEVP,CUMGWI
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMGWO,CUMSWI,CUMSWO
        REAL,   SAVE, DIMENSION(:),  POINTER ::CUMWDR,CUMFLX,CNDFCT
        REAL,   SAVE, DIMENSION(:),  POINTER ::VOLINIT,STGOLD2
        REAL,   SAVE, DIMENSION(:),  POINTER ::BOTTMS,BGAREA,SSMN,SSMX
crgn        REAL,   SAVE, DIMENSION(:),  POINTER ::EVAP,PRECIP,SEEP,SEEP3
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::EVAP,PRECIP
        DOUBLE PRECISION,   SAVE, DIMENSION(:),  POINTER ::SEEP,SEEP3
        REAL,   SAVE, DIMENSION(:),  POINTER ::SURFA,SURFIN,SURFOT
        REAL,   SAVE, DIMENSION(:),  POINTER ::SUMCNN,SUMCHN
        REAL,   SAVE, DIMENSION(:,:),POINTER ::CLAKE,CRNF,SILLVT
        REAL,   SAVE, DIMENSION(:,:),POINTER ::CAUG,CPPT,CLAKINIT
        REAL,   SAVE, DIMENSION(:,:,:),POINTER ::BDLKN1
Cdep  Added arrays for tracking lake budgets for dry lakes
        REAL,   SAVE, DIMENSION(:),  POINTER ::EVAPO,WITHDRW,FLWIN
        REAL,   SAVE, DIMENSION(:),  POINTER ::FLWITER,GWRATELIM
Cdep    Allocate arrays to add runoff from UZF Package
        REAL,   SAVE, DIMENSION(:),  POINTER ::OVRLNDRNF,CUMLNDRNF
Cdep    Allocate arrays for lake area and depth relations
        REAL,   SAVE, DIMENSION(:,:),  POINTER ::DEPTHTABLE,AREATABLE
Cdep    Allocate space for three dummy arrays used in GAGE Package
C         when Solute Transport is active
        REAL,   SAVE, DIMENSION(:,:),POINTER ::XLAKES,XLAKINIT,XLKOLD
Crsr    Allocate arrays in BD subroutine
        INTEGER,SAVE, DIMENSION(:),  POINTER ::LDRY,NCNT,NCNST,KSUB
        INTEGER,SAVE, DIMENSION(:),  POINTER ::MSUB1
        INTEGER,SAVE, DIMENSION(:,:),POINTER ::MSUB
        REAL,   SAVE, DIMENSION(:),  POINTER ::FLXINL,VOLOLD,GWIN,GWOUT
        REAL,   SAVE, DIMENSION(:),  POINTER ::DELH,TDELH,SVT,STGADJ
      TYPE GWFLAKTYPE
        INTEGER,      POINTER   ::NLAKES,NLAKESAR,ILKCB,NSSITR
        INTEGER,      POINTER   ::MXLKND,LKNODE,ICMX,NCLS,LWRT,NDV,NTRB
        REAL,         POINTER   ::THETA,SSCNCR
Crgn    Added budget variables for GSFLOW CSV file
        REAL,         POINTER   ::TOTGWIN_LAK,TOTGWOT_LAK,TOTDELSTOR_LAK
        REAL,         POINTER   ::TOTSTOR_LAK,TOTEVAP_LAK,TOTPPT_LAK
        REAL,         POINTER   ::TOTRUNF_LAK,TOTWTHDRW_LAK
        REAL,         POINTER   ::TOTSURFOT_LAK,TOTSURFIN_LAK
        INTEGER,      DIMENSION(:),  POINTER ::ICS, NCNCVR, LIMERR
        INTEGER,      DIMENSION(:,:),POINTER ::ILAKE,ITRB,IDIV,ISUB,IRK
        INTEGER,      DIMENSION(:,:,:),POINTER ::LKARR1
        REAL,         DIMENSION(:),  POINTER ::STAGES
        DOUBLE PRECISION,DIMENSION(:),POINTER ::STGNEW,STGOLD,STGITER
        REAL,         DIMENSION(:),  POINTER ::VOL,FLOB, DSRFOT
        REAL,         DIMENSION(:),  POINTER ::PRCPLK,EVAPLK,BEDLAK
        REAL,         DIMENSION(:),  POINTER ::WTHDRW,RNF,CUMRNF
        REAL,         DIMENSION(:),  POINTER ::CUMPPT,CUMEVP,CUMGWI
        REAL,         DIMENSION(:),  POINTER ::CUMGWO,CUMSWI,CUMSWO
        REAL,         DIMENSION(:),  POINTER ::CUMWDR,CUMFLX,CNDFCT
        REAL,         DIMENSION(:),  POINTER ::VOLINIT,STGOLD2
        REAL,         DIMENSION(:),  POINTER ::BOTTMS,BGAREA,SSMN,SSMX
Crgn        REAL,         DIMENSION(:),  POINTER ::EVAP,PRECIP,SEEP,SEEP3
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: EVAP,PRECIP
        DOUBLE PRECISION,DIMENSION(:),  POINTER :: SEEP,SEEP3
        REAL,         DIMENSION(:),  POINTER ::SURFA,SURFIN,SURFOT
        REAL,         DIMENSION(:),  POINTER ::SUMCNN,SUMCHN
        REAL,         DIMENSION(:,:),POINTER ::CLAKE,CRNF,SILLVT
        REAL,         DIMENSION(:,:),POINTER ::CAUG,CPPT,CLAKINIT
        REAL,         DIMENSION(:,:,:),POINTER ::BDLKN1
Cdep  Added arrays for tracking lake budgets for dry lakes
        REAL,         DIMENSION(:),  POINTER ::EVAPO,WITHDRW,FLWIN
        REAL,         DIMENSION(:),  POINTER ::FLWITER,GWRATELIM
Cdep    Allocate arrays to add runoff from UZF Package
        REAL,         DIMENSION(:),  POINTER ::OVRLNDRNF,CUMLNDRNF
Cdep    Allocate arrays for lake area and depth relations
        REAL,         DIMENSION(:,:),POINTER ::DEPTHTABLE,AREATABLE
Cdep    Allocate space for three dummy arrays used in GAGE Package
C         when Solute Transport is active
        REAL,         DIMENSION(:,:),POINTER ::XLAKES,XLAKINIT,XLKOLD
Crsr    Allocate arrays in BD subroutine
        INTEGER,      DIMENSION(:),  POINTER ::LDRY,NCNT,NCNST,KSUB
        INTEGER,      DIMENSION(:),  POINTER ::MSUB1
        INTEGER,      DIMENSION(:,:),POINTER ::MSUB
        REAL,         DIMENSION(:),  POINTER ::FLXINL,VOLOLD,GWIN,GWOUT
        REAL,         DIMENSION(:),  POINTER ::DELH,TDELH,SVT,STGADJ
      END TYPE
      TYPE(GWFLAKTYPE), SAVE:: GWFLAKDAT(10)
      END MODULE GWFLAKMODULE
C
      SUBROUTINE GWF2LAK7AR(IN,IUNITSFR,IUNITGWT,IUNITUZF,NSOL,IGRID)
C
C------USGS VERSION 7; JUNE 2006 GWF2LAK7AR 
C
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY SFR1 TO SUPPORT LAKE3 AND
C     GAGE PACKAGES AND THE GWT PROCESS
C     ******************************************************************
C
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: IOUT, NCOL, NROW, NLAY, IFREFM, ITRSS,
     +                        NODES
      USE GWFSFRMODULE, ONLY: NSS
C
C      ******************************************************************
C      ALLOCATE ARRAY STORAGE FOR LAKES
C      ******************************************************************
C
C      ------------------------------------------------------------------
C      SPECIFICATIONS:
C      ------------------------------------------------------------------
Crsr  Allocate lake variables used by SFR even if lakes not active so that
C       argument lists are defined     
      ALLOCATE (NLAKES, NLAKESAR,THETA)
      NLAKES = 0
      NLAKESAR = 1
      THETA = 0.0
      IF (IN.GT.0) THEN
        ALLOCATE (ILKCB, NSSITR, SSCNCR)
        ALLOCATE (MXLKND, LKNODE, ICMX, NCLS, LWRT, NDV, NTRB)
C
C1------IDENTIFY PACKAGE AND INITIALIZE LKNODE.
      WRITE(IOUT,1) IN
      LKNODE=0
Cdep  initialize number of iterations and closure criteria to zero.
      DUM = 0.0
      NSSITR = 0
      SSCNCR = 0.0
C
C2------READ NLAKES, ILKCB.
C
Cdep  Revised input statement to read THETA,NSSITR,SSCNCR for
Cdep  transient simulations when THETA is negative.
        IF(IFREFM.EQ.0) THEN
           READ(IN,'(2I10)')NLAKES,ILKCB
           IF (ITRSS.LE.0) THEN
              READ(IN,'(F10.2,I10,F10.2)') THETA,NSSITR,SSCNCR
           ELSE
              READ(IN,'(F10.2)') THETA
              IF (THETA.LT.0.0) BACKSPACE IN
           END IF
        ELSE
           READ(IN,*) NLAKES,ILKCB
           IF (ITRSS.LE.0) THEN
              READ(IN,*) THETA,NSSITR,SSCNCR
           ELSE
              READ(IN,*) THETA
              IF(THETA.LT.0.0) BACKSPACE IN
           END IF
        END IF
      
Cdep    Set default values for number of iterations and closure criteria
Cdep     for transient simulations when using original version of 
Cdep     LAKE Package.
        IF(THETA.GE.0.0.AND.NSSITR.EQ.0) THEN
          NSSITR=100
          SSCNCR=1.0E-04
        ELSE IF(THETA.LT.0.0)THEN
          THETA=ABS(THETA)
          IF(IFREFM.EQ.0) THEN
            READ(IN,'(F10.2,I10,F10.2)') DUM,NSSITR,SSCNCR
          ELSE
            READ(IN,*) DUM,NSSITR,SSCNCR
          END IF
        END IF
Cdep   Add check to reset THETA when > 1 or < 0.5.
        IF(THETA.GT.1.0) THEN
          THETA = 1.0
        ELSE IF(THETA.LT.0.5)THEN
          THETA = 0.5
        END IF
      END IF
C
C
C  SET NLAKES ARRAY VARIABLE TO NLAKES IF NLAKES GREATER THAN 0.
      IF (NLAKES.GT.0) NLAKESAR = NLAKES
      ALLOCATE (VOL(NLAKESAR), STGOLD(NLAKESAR), STGNEW(NLAKESAR))
      ALLOCATE (STGITER(NLAKESAR))
      STGNEW = 0.0D0
      STGOLD = 0.0D0
      STGITER = 0.0D0
      VOL = 0.0
      CALL SGWF2LAK7PSV1(IGRID)
      IF (IN.LT.1) RETURN
C
C Lakes are active
      ALLOCATE (STAGES(NLAKESAR), CLAKE(NLAKESAR,NSOL))
      STAGES = 0.0
      CLAKE = 0.0
C Budget variables for GSFLOW   
      ALLOCATE (TOTGWIN_LAK,TOTGWOT_LAK,TOTDELSTOR_LAK,TOTSTOR_LAK)  
      ALLOCATE (TOTEVAP_LAK,TOTPPT_LAK,TOTRUNF_LAK,TOTWTHDRW_LAK)
      ALLOCATE (TOTSURFIN_LAK,TOTSURFOT_LAK)
      TOTGWIN_LAK = 0.0
      TOTGWOT_LAK = 0.0
      TOTDELSTOR_LAK = 0.0
      TOTSTOR_LAK = 0.0
      TOTEVAP_LAK = 0.0
      TOTPPT_LAK = 0.0
      TOTRUNF_LAK = 0.0
      TOTWTHDRW_LAK = 0.0
      TOTSURFIN_LAK = 0.0
      TOTSURFOT_LAK = 0.0
C
C  VALUE OF MXLKND (NUMBER OF LAKE-AQUIFER INTERFACES) IS AN ESTIMATE.
C    TO SAVE MEMORY, REDUCE ITS SIZE IF APPROPRIATE.
C    IF MXLKND TOO SMALL, ERROR MESSAGE WILL BE PRINTED.
      MXLKND=NCOL*NROW*NLAY/2
      IF (NLAKES.LT.1) THEN
        WRITE(IOUT,2)
        IN=0
        NLAKES = 0
      ELSE
      WRITE(IOUT,5) MXLKND,NLAKES
      IF (ILKCB.GT.0) WRITE(IOUT,7) ILKCB
      IF (ILKCB.LE.0) WRITE(IOUT,9)
Cdep   Write THETA, NSSITR, SSCNCR
      IF (ITRSS.GT.0) THEN
        WRITE(IOUT,22) THETA
        WRITE(IOUT,10) NSSITR, SSCNCR
      ELSE
        WRITE(IOUT,11) THETA, NSSITR, SSCNCR
      END IF
Cdep   Changed default values for NSSITR and SSCNCR and revised
Cdep     print statements using format statement 10.   
Cdep      IF(ITRSS.LE.0.AND.NSSITR.EQ.0) NSSITR = 50
Cdep      IF(ITRSS.LE.0.AND.SSCNCR.EQ.0.0) SSCNCR = 0.01
Cdep      IF(ITRSS.EQ.0) WRITE(IOUT,23) NSSITR, SSCNCR
Cdep      IF(ITRSS.LT.0) WRITE(IOUT,24) NSSITR, SSCNCR
1     FORMAT(/1X,'LAK7 -- LAKE PACKAGE, VERSION 7, 6/28/2006',
     1' INPUT READ FROM UNIT',I3)
2       FORMAT(1X,' NUMBER OF LAKES=0, ',
     1              ' SO LAKE PACKAGE IS BEING TURNED OFF')
5     FORMAT(1X,'SPACE ALLOCATION FOR',I7,' GRID CELL FACES ADJACENT TO
     1LAKES'/1X,'MAXIMUM NUMBER OF LAKES IS',I3, ' FOR THIS SIMULATION')
7     FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I5)
9     FORMAT(1X,'CELL-BY-CELL SEEPAGES WILL NOT BE PRINTED OR SAVED')
Cdep added format statement when starting with transient simulation
  10  FORMAT(//1X,'LAKE PACKAGE HAS BEEN MODIFIED TO ITERATIVELY ',
     1 'SOLVE FOR LAKE STAGE DURING TRANSIENT STRESS PERIODS:',/1X,
     2 'MAXIMUM NUMBER OF ITERATIONS (NSSITR) = ',I5,/1X,
     3 'CLOSURE CRITERIA FOR LAKE STAGE (SSCNCR) = ',1PE12.6,/1X,
     4 'DEFAULT VALUES FOR TRANSIENT ONLY SIMULATIONS ARE: ',
     5 'NSSITR = 100 AND SSCNCR = 0.0001',/1X,'VALUES OTHER THAN ',
     6 'DEFAULT CAN BE READ BY SPECIFYING A THETA LESS THAN ZERO ',
     7 'THEN ADDING NSSITR AND SSCNCR PER ORIGINAL INSTRUCTIONS.',/1X,
     8 'NEGATIVE THETA MUST BE LESS THAN ZERO BUT NOT MORE THAN ',
     9 'ONE. THETA IS CONVERTED TO A POSITIVE VALUE.',/1X,
     * 'MINIMUM AND MAXIMUM LAKE STAGES FOR TRANSIENT ',
     * 'SIMULATIONS ARE SET TO BOTTOM AND TOP ELEVATIONS USED TO ',
     * 'COMPUTE LAKE VOLUME, RESPECTIVELY.',//)
Cdep added format statement for steady state only simulations.
  11  FORMAT(//1X,'NEWTON ITERATION METHOD FOR COMPUTING LAKE STAGE ',
     1 'DURING STEADY-STATE STRESS PERIODS HAS BEEN MODIFIED:',/1X,
     2 'SPECIFIED THETA OF ',F6.3,' WILL BE AUTOMATICALLY CHANGED TO ',
     3 '1.0 FOR ALL STEADY STATE STRESS PERIODS.',/1X,
     4 'MAXIMUM NUMBER OF STEADY-STATE ITERATIONS (NSSITR) = ',I5,/1X,
     5 'CLOSURE CRITERIA FOR STEADY-STATE LAKE STAGE (SSCNCR) = ',
     6  1PE12.6,//)
Cdep revised print statement to note that time weighting of theta can
Cdep  vary only between 0.5 and 1 for transient simulations
Cdep   22 FORMAT(/1X,'THETA = ',F10.2,'  METHOD FOR UPDATING LAKE STAGES IN
Cdep     1ITERATIONS OF THE SOLUTION FOR AQUIFER HEADS.'/20X,'0.0 IS EXPLICI
Cdep     2T, 0.5 IS CENTERED, AND 1.0 IS FULLY IMPLICIT.')
   22 FORMAT(/1X,'THETA = ',F6.3,/1X,'THETA IS THE TIME WEIGHTING ',
     1'FACTOR FOR COMPUTING LAKE STAGE DURING TRANSIENT MODFLOW ', 
     2'TIME STEPS AND ITS DEFINITION HAS BEEN MODIFIED.',/1X,'THETA ',
     3'CANNOT BE LESS THAN 0.5 NOR GREATER THAN 1.0 AND VALUES ',
     4'OUTSIDE THIS RANGE ARE AUTOMATICALLY RESET TO 0.5 AND 1.0, '
     5'RESPECTIVELY.',/1X,'A THETA OF 0.5 REPRESENTS THE AVERAGE LAKE ',
     6'STAGE DURING A TIME STEP.'/1X,'A THETA OF 1.0 REPRESENTS THE ',
     7'LAKE STAGE AT THE END OF THE TIME STEP.',//)
Cdep   23 FORMAT(/1X,'STEADY-STATE SOLUTION FOR LAKES.'
Cdep     2/1X,'MAXIMUM NUMBER OF ITERATIONS = ',I4,3X,
Cdep     1'CONVERGENCE CRITERION = ',1PE9.2)
Cdep   24 FORMAT(/1X,'COMBINED STEADY-STATE/TRANSIENT SOLUTION FOR LAKES.'
Cdep     2/1X,'MAXIMUM NUMBER OF ITERATIONS = ',I4,3X,
Cdep     1'CONVERGENCE CRITERION = ',1PE9.2)

        ALLOCATE (ILAKE(5,MXLKND), BEDLAK(MXLKND), CNDFCT(MXLKND))
        ALLOCATE (PRCPLK(NLAKES), EVAPLK(NLAKES), WTHDRW(NLAKES))
        ALLOCATE (RNF(NLAKES), CRNF(NLAKES,NSOL), CUMRNF(NLAKES))
        ALLOCATE (ISUB(NLAKES,NLAKES), SILLVT(NLAKES,NLAKES))
        ALLOCATE (IRK(2,NLAKES))
        ALLOCATE (CUMPPT(NLAKES), CUMEVP(NLAKES), CUMGWI(NLAKES))
        ALLOCATE (CUMGWO(NLAKES), CUMSWI(NLAKES), CUMSWO(NLAKES))
        ALLOCATE (CUMWDR(NLAKES), CUMFLX(NLAKES), STGOLD2(NLAKES))
        ALLOCATE (CAUG(NLAKES,NSOL), CPPT(NLAKES,NSOL))
        ALLOCATE (CLAKINIT(NLAKESAR,NSOL), VOLINIT(NLAKES))
        ALLOCATE (ICS(NLAKES),BOTTMS(NLAKES), BGAREA(NLAKES))
        ALLOCATE (SSMN(NLAKES), SSMX(NLAKES))
        ALLOCATE (LKARR1(NCOL,NROW,NLAY), BDLKN1(NCOL,NROW,NLAY))
        ALLOCATE (EVAP(NLAKES), PRECIP(NLAKES), SEEP(NLAKES),
     +            SEEP3(NLAKES))
        ALLOCATE (SURFA(NLAKES), SURFIN(NLAKES), SURFOT(NLAKES))
        ALLOCATE (SUMCNN(NLAKES), SUMCHN(NLAKES))
        ALLOCATE (NCNCVR(NLAKES), LIMERR(NLAKES), DSRFOT(NLAKES))
Cdep  Allocate arrays that track lake budgets for dry lakes
        ALLOCATE (EVAPO(NLAKES),WITHDRW(NLAKES),FLWIN(NLAKES))
        ALLOCATE (FLWITER(NLAKES),GWRATELIM(NLAKES))
        EVAPO = 0.0
        WITHDRW = 0.0
        FLWIN = 0.0
        FLWITER = 0.0
        GWRATLIM= 0.0
Cdep  Allocate space for three arrays used in GAGE Package 
C       when Solute Transport is active
        ALLOCATE (XLAKES(NLAKES,1), XLAKINIT(NLAKES,1))
        ALLOCATE (XLKOLD(NLAKES,1))
crsr  Allocate arrays for BD subroutine
        ALLOCATE (LDRY(NODES), FLXINL(NLAKES), VOLOLD(NLAKES))
        ALLOCATE (NCNT(NLAKES), NCNST(NLAKES))
        ALLOCATE (SVT(NLAKES), KSUB(NLAKES), STGADJ(NLAKES))
        ALLOCATE (MSUB(NLAKES,NLAKES), MSUB1(NLAKES))
        ALLOCATE (GWIN(NLAKES), GWOUT(NLAKES))
        ALLOCATE (DELH(NLAKES), TDELH(NLAKES))
      END IF
Cdep   ALLOCATE SPACE FOR CONNECTION WITH STREAMS
      IF (IUNITSFR.LE.0) THEN
        NSSAR = 1
      ELSE
        NSSAR = NSS
      END IF
Cdep   ALLOCATE SPACE FOR FLOB ARRAY WHEN TRANSPORT ACTIVE.   
      IF (IUNITGWT.LE.0) THEN
        MXLKAR = 1
      ELSE
        MXLKAR = MXLKND
      END IF
Cdep    ALLOCATE SPACE FOR OVERLAND FLOW WHEN UNSATURATED FLOW ACTIVE.
      IF (IUNITUZF.LE.0) THEN
        NUZFAR = 1
      ELSE
        NUZFAR = NLAKESAR
      END IF
      ALLOCATE (ITRB(NLAKES,NSSAR), IDIV(NLAKES,NSSAR))
      ALLOCATE (FLOB(MXLKAR))
      ALLOCATE (OVRLNDRNF(NUZFAR), CUMLNDRNF(NUZFAR))
Cdep    ALLOCATE SPACE FOR DEPTHTABLE, AND AREATABLE
      ALLOCATE (DEPTHTABLE(151,NLAKES), AREATABLE(151,NLAKES))
      ITRB = 0
      IDIV = 0
      FLOB = 0.0
      OVRLNDRNF = 0.0
      CUMLNDRNF = 0.0
      DEPTHTABLE = 0.0
      AREATABLE = 0.0
C-----SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2LAK7PSV(IGRID)
C
C11-----RETURN.
      RETURN
      END
C
      SUBROUTINE GWF2LAK7RP(IN,IUNITBCF,IUNITGWT,IUNITLPF,IUNITHUF,
     +                      IUNITSFR,IUNITUZF,KKPER,NSOL,IOUTS,IGRID)
C
C------USGS VERSION 7;  JUNE 2006 GWF2LAK7RP
C
C     ******************************************************************
C       READ INPUT DATA FOR THE LAKE PACKAGE.
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: IOUT, NCOL, NROW, NLAY, IFREFM, IBOUND,
     +                        LBOTM, BOTM, DELR, DELC, ISSFLG
C     USE GWFSFRMODULE, ONLY: NSS
      CHARACTER*24 ANAME(2)
C     CHARACTER*30 LFRMAT      !gsf
      DATA ANAME(1)/'           LAKE ID ARRAY'/
      DATA ANAME(2)/'  LAKEBED LEAKANCE ARRAY'/
C
C     ------------------------------------------------------------------
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
C
C1A-----IF MXLKND IS LESS THAN 1, THEN LAKE IS INACTIVE. RETURN.
      IF(MXLKND.LT.1) RETURN
C
C1A1----READ INITIAL CONDITIONS FOR ALL LAKES (ONLY READ ONCE)
      ISS = ISSFLG(KKPER)
      IF (KKPER.EQ.1) THEN
         WRITE (IOUT,19)
         IF(ISS.NE.0) WRITE (IOUT,20)
         IF(ISS.EQ.0) WRITE (IOUT,820)
         IF (IUNITGWT.EQ.0) THEN
            DO 30 LM=1,NLAKES
               IF (IFREFM.EQ.0) THEN
                  IF(ISS.NE.0) READ (IN,'(3F10.4)') STAGES(LM),SSMN(LM),
     1              SSMX(LM)
                  IF(ISS.EQ.0) READ (IN,'(3F10.4)') STAGES(LM)
               ELSE
                  IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),SSMX(LM)
                  IF(ISS.EQ.0) READ (IN,*) STAGES(LM)
               END IF
            IF(ISS.NE.0) WRITE (IOUT,22) LM,STAGES(LM),SSMN(LM),SSMX(LM)
            IF(ISS.EQ.0) WRITE (IOUT,22) LM,STAGES(LM)
 30         CONTINUE
         ELSE
Crgn stop program if solute transport is active.
            WRITE(iout,*)'GSFLOW-1.0 cannot simulate transport',
     1                    '--program stopping'
            WRITE(iout,*)'Please change input and restart program'
            CALL USTOP(' ')
C            WRITE (IOUTS,21) NSOL
C            WRITE (LFRMAT,23) NSOL
C            DO 35 LM=1,NLAKES
C               IF (IFREFM.EQ.0) THEN
C                 IF(ISS.NE.0) READ(IN,'(100F10.4)') STAGES(LM),SSMN(LM),
C     1              SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL)
C                 IF(ISS.EQ.0) READ (IN,'(100F10.4)') STAGES(LM),
C     1                        (CLAKE(LM,ISOL),ISOL=1,NSOL)
C               ELSE
C                 IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),SSMX(LM),
C     1                        (CLAKE(LM,ISOL),ISOL=1,NSOL)
C                 IF(ISS.EQ.0) READ (IN,*) STAGES(LM),
C     1                        (CLAKE(LM,ISOL),ISOL=1,NSOL)
C               END IF
C            IF(ISS.NE.0) WRITE (IOUT,22) LM,STAGES(LM),SSMN(LM),SSMX(LM)
C            IF(ISS.EQ.0) WRITE (IOUT,22) LM,STAGES(LM)
C 35           WRITE (IOUTS,LFRMAT) LM,(CLAKE(LM,ISOL),ISOL=1,NSOL)
cgage
C            CLAKINIT=CLAKE
         END IF
      END IF
C
      WRITE (IOUT,'(/)')
      WRITE(IOUT,822)
 19   FORMAT(//1X,'LAKE PACKAGE ACTIVE:  CALCULATED LAKE STAGE FOR EACH 
     1TIME STEP WILL BE STORED IN HNEW ARRAY.')
 20   FORMAT(///1X,'INITIAL LAKE STAGE:  LAKE    STAGE    SS MIN    SS M
     1AX'/)
C 21   FORMAT (//1X,'INITIAL LAKE CONCENTRATIONS:  LAKE   CONCENTRATION (   !rsr
C     1NSOL =',I3,')'/)                                                     !rsr
 22   FORMAT (22X,I3,3F10.3)
C 23   FORMAT ('(31X,I3,3X,1P',I3,'(E12.3))')                               !rsr
 820  FORMAT (/1X,'INITIAL LAKE STAGE:  LAKE    STAGE'/)
 822  FORMAT(//1X,'If any subsequent steady-state stress periods, min. a
     1nd max. stages for each lake will be read in Record 9a.'//)
C
C1B-----READ ITMP (FLAG TO REUSE LAKE-GEOMETRY DATA).
      IF(IFREFM.EQ.0) THEN
         READ(IN,'(3I10)') ITMP, ITMP1, LWRT
      ELSE
         READ(IN,*) ITMP, ITMP1, LWRT
      END IF
C
C2A-----IF ITMP < 0 THEN REUSE LAKE CONFIGURATION DATA FROM LAST STRESS
C       PERIOD.
      IF(ITMP.GE.0) GO TO 50
      WRITE (IOUT,'(/)')
      WRITE(IOUT,2)
    2 FORMAT(1H ,'REUSING LAKE CONFIGURATION DATA FROM LAST STRESS PERIO
     1D'/)
      GO TO 800
C
C4------IF THERE ARE NO LAKE NODES THEN RETURN.
   50 LKNODE = 0
      IF(ITMP.EQ.0) GOTO 900
C
C   INITIALIZE BGAREA
      DO 60 LK=1,NLAKES
      BGAREA(LK)=0.0
   60 CONTINUE
Cdep Initialize DEPTHTABLE AND AREA TABLE TO ZERO.
      DO JNODE=1,151
        DO ILK=1,NLAKES
          AREATABLE(JNODE,ILK)=0.0
          DEPTHTABLE(JNODE,ILK)=0.0
        END DO
      END DO   
C
C5------READ INTEGER ARRAYS THAT DEFINE THE POSITIONS OF ALL LAKES IN
C5A     EACH MODEL GRID LAYER.  THEN READ ARRAYS OF LAKEBED CONDUCTANCES
C5B     IN EACH LAYER.
C
C   READ ARRAY OF LAKE ID'S, LAYER BY LAYER
C   REVISED 11/30/2005 DEP
      DO 125 K=1,NLAY
      KK = K
      CALL U2DINT(LKARR1(:,:,KK),ANAME(1),NROW,NCOL,KK,IN,IOUT)
  125 CONTINUE
C
C   CHECK THAT ALL ENTRIES ARE VALID LAKE ID NUMBERS OR ZERO
C
      DO 130 K=1,NLAY
      DO 130 I=1,NCOL
      DO 130 J=1,NROW
      IF(LKARR1(I,J,K).GT.0.AND.LKARR1(I,J,K).LE.NLAKES) GO TO 130
      LKARR1(I,J,K)=0
  130 CONTINUE
C
C   CHECK IF LAKE CELLS HAVE VALUES OF IBOUND=0; WARN IF INCONSISTENT
C
      WRITE (IOUT,'(/)')
      DO 132 K=1,NLAY
      DO 132 I=1,NCOL
      DO 132 J=1,NROW
      IF(LKARR1(I,J,K).GT.0.AND.IBOUND(I,J,K).NE.0) THEN
         WRITE (IOUT,232) IBOUND(I,J,K),LKARR1(I,J,K),I,J,K
  232    FORMAT (7X,'*** WARNING: IBOUND = ',I2,
     1  ' & LKARR = ',I2,' at CELL I=',I3,
     2  ', J=',I3,', K=',I3,' ***')
      END IF
  132 CONTINUE
C
C   READ ARRAY OF BED LEAKANCES, LAYER BY LAYER
Cdep    REVISED 11/30/2005
      WRITE (IOUT,'(/)')
      DO 135 K=1,NLAY
      KK = K
      CALL U2DREL(BDLKN1(:,:,KK),ANAME(2),NROW,NCOL,KK,IN,IOUT)
  135 CONTINUE
C
        WRITE(IOUT,36)
        WRITE(IOUT,4)
36    FORMAT(/7X,'LOCATIONS, LAKE #, INTERFACE TYPE FOR GRID CELLS',
     1 ' ADJACENT TO LAKES:',5X,/
     3 5X,71('-'))
4     FORMAT(5X,'LAYER #',4X,'ROW #',4X,'COLUMN #',3X,'LAKE #',
     1       2X,'INTERFACE TYPE',2X,'LAKEBED LEAKANCE')
C
C   IDENTIFY LAKE BORDER CELLS, ASSIGN CELL TYPE ID'S, COMPUTE AND
C     ASSIGN LAKE-AQUIFER INTERFACE CONDUCTANCES.
C
      M = 0
      DO 180 I=1,NCOL
      DO 180 J=1,NROW
      K = 1
      IF(LKARR1(I,J,K).EQ.0) GO TO 150
      IF(NLAY.EQ.1) GO TO 145
C   Keep searching in vertical direction until non-lake cell is found, and define
C     interface there ("K" for interface is layer below bottom of lake)
      DO 140 K=2,NLAY
      IF(LKARR1(I,J,K).EQ.0) GO TO 145
  140 CONTINUE
C   Make sure that K=NLAY if lake extends to bottom cell of grid:
      K=NLAY
C      GO TO 145
C
C   VERTICAL LAKEBED INTERFACE (TYPE 0) DETECTED
C
  145 M = M + 1
      IF(M.LE.MXLKND) GO TO 147
      WRITE(IOUT,149) I,J,K
  149 FORMAT(/1X,'MAXIMUM NUMBER OF GRID CELLS ADJACENT TO LAKES HAS BEE
     1N EXCEEDED WITH CELL ',3I5,'  REDEFINE VARIABLE MXLKND TO A LARGER
     2 VALUE IN MODULE GWF2LAK7AR')
      CALL USTOP(' ')
  147 ILAKE(1,M) = K
      ILAKE(2,M) = J
      ILAKE(3,M) = I
      IF(K.GT.1.AND.LKARR1(I,J,K).EQ.0) LID = LKARR1(I,J,K-1)
      IF(LKARR1(I,J,K).NE.0) LID = LKARR1(I,J,K)
      ILAKE(4,M) = LID
      ILAKE(5,M) = 6
      BEDLAK(M) = BDLKN1(I,J,K-1)
      IF(K.EQ.NLAY.AND.LKARR1(I,J,K).NE.0) BEDLAK(M) = 0.0
      BGAREA(LID) = BGAREA(LID) + DELC(J)*DELR(I)
      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
5     FORMAT(5I10,10X,F10.5)
      IF(LKARR1(I,J,K).NE.0) GO TO 180
C
C   SEARCH FOR CELL(S) ADJACENT TO LAKE
C
  150 K2 = K
      DO 175 K1=K2,NLAY
cgzh fix for 2D-problems
      IF(NCOL.EQ.1) GO TO 165
      IF(I.NE.1) GO TO 1151
      IF(LKARR1(I+1,J,K1).EQ.0) GO TO 165
      GO TO 1153
 1151 IF(I.NE.NCOL) GO TO 1152
      IF(LKARR1(I-1,J,K1).EQ.0) GO TO 165
      GO TO 1153
 1152 IF(LKARR1(I+1,J,K1).EQ.0.AND.LKARR1(I-1,J,K1).EQ.0) GO TO 165
C
C   CELL(S) LATERALLY ADJACENT TO LAKE IN X-DIRECTION (TYPE 1) DETECTED
C
 1153 DO 160 N=1,2
      IF(N.EQ.2) GO TO 155
      IF(I.EQ.1) GO TO 160
      IF(LKARR1(I-1,J,K1).EQ.0) GO TO 160
      I2 = I-1
      IFACE=1
      GO TO 157
  155 IF(I.EQ.NCOL) GO TO 160
      IF(LKARR1(I+1,J,K1).EQ.0) GO TO 160
      I2 = I + 1
      IFACE=2
  157 M = M + 1
      IF(M.LE.MXLKND) GO TO 158
      WRITE(IOUT,149) I,J,K1
      CALL USTOP(' ')
  158 ILAKE(1,M) = K1
      ILAKE(2,M) = J
      ILAKE(3,M) = I
      ILAKE(4,M) = LKARR1(I2,J,K1)
      ILAKE(5,M) = IFACE
      BEDLAK(M) = BDLKN1(I,J,K1)
      K4 = K1 - 1
      DO 3158 K3=1,K4
      IF(LKARR1(I,J,K3).EQ.0) GO TO 3158
      GO TO 3162
 3158 CONTINUE
      BEDLAK(M) = BDLKN1(I,J,1)
 3162 CONTINUE
      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
  160 CONTINUE
cgzh fix for 2D-problems
  165 IF(NROW.EQ.1) GO TO 175
      IF(J.NE.1) GO TO 1161
      IF(LKARR1(I,J+1,K1).EQ.0) GO TO 175
      GO TO 1163
 1161 IF(J.NE.NROW) GO TO 1162
      IF(LKARR1(I,J-1,K1).EQ.0) GO TO 175
      GO TO 1163
 1162 IF(LKARR1(I,J+1,K1).EQ.0.AND.LKARR1(I,J-1,K1).EQ.0) GO TO 175
C
C   CELL(S) LATERALLY ADJACENT TO LAKE IN Y-DIRECTION (TYPE 2) DETECTED
C
 1163 DO 170 N=1,2
      IF(N.EQ.2) GO TO 172
      IF(J.EQ.1) GO TO 170
      IF(LKARR1(I,J-1,K1).EQ.0) GO TO 170
      J2 = J - 1
      IFACE=4
      GO TO 174
  172 IF(J.EQ.NROW) GO TO 170
      IF(LKARR1(I,J+1,K1).EQ.0) GO TO 170
      J2 = J + 1
      IFACE=3
  174 M = M + 1
      IF(M.LE.MXLKND) GO TO 176
      WRITE(IOUT,149) I,J,K1
      CALL USTOP(' ')
  176 ILAKE(1,M) = K1
      ILAKE(2,M) = J
      ILAKE(3,M) = I
      ILAKE(4,M) = LKARR1(I,J2,K1)
      ILAKE(5,M) = IFACE
      BEDLAK(M) = BDLKN1(I,J,K1)
      K4 = K1 - 1
      DO 4158 K3=1,K4
      IF(LKARR1(I,J,K3).EQ.0) GO TO 4158
      GO TO 4162
 4158 CONTINUE
      BEDLAK(M) = BDLKN1(I,J,1)
 4162 CONTINUE
      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
  170 CONTINUE
  175 CONTINUE
  180 CONTINUE
      WRITE(IOUT,195) M
  195 FORMAT(/5X,'NUMBER OF LAKE-AQUIFER CELL INTERFACES = ',I5)
      LKNODE = M
C
C   SET LAKE BOTTOM ELEVATIONS
      DO 295 LK=1,NLAKES
  295 BOTTMS(LK) = 999999
C
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      IF(NTYP.EQ.0) THEN
        LAKE = ILAKE(4,II)
        IF(K.GT.1) BOTLK = BOTM(I,J,LBOTM(K-1))
        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) BOTLK = BOTM(I,J,LBOTM(K))
        IF(BOTLK.LT.BOTTMS(LAKE)) BOTTMS(LAKE) = BOTLK
      END IF
  350 CONTINUE
C
C-- COMPUTE AND PRINT STAGE/VOLUME TABLES WHEN MORE THAN ONE LAYER
Cdep  revised print statement to include stage/area tables
C
      IF(NLAY.EQ.1) GO TO 1331
      DO 1330 L1=1,NLAKES
      WRITE(IOUT,1306) L1
Cdep  revised print statement to include area
 1306 FORMAT(//1X,'STAGE/VOLUME RELATION FOR LAKE',I3//6X,'STAGE',
     1        8X,'VOLUME',8X,'AREA'/)
      EVOL = 0.0
      GTSDPH = 0.0
      TOPMST = BOTTMS(L1)
      TBELV = BOTTMS(L1)
      DO 1340 I=1,NCOL
      DO 1340 J=1,NROW
      IF(LKARR1(I,J,1).NE.L1) GO TO 1340
      IF(BOTM(I,J,LBOTM(1)).GT.TOPMST) TOPMST = BOTM(I,J,LBOTM(1))
      DTHK = BOTM(I,J,LBOTM(1)) - BOTM(I,J,LBOTM(2))
      IF(DTHK.GT.GTSDPH) GTSDPH = DTHK
 1340 CONTINUE
      TOPMST = TOPMST + GTSDPH
      TBNC = (TOPMST-BOTTMS(L1))/100.0
Cdep Revised looping for computing lake stage, volume, and area
Cdep   WRITE(IOUT,1315) TBELV, EVOL
 1315 FORMAT(3(1X,1PE13.5))
Cdep  DO 1325 K=1,150         ! Increased increment by 1.
      DO 1325 K=1,151
      EVOL=0.0
Cdep     TBELV = TBELV + TBNC  ! moved end of loop
      DO 1320 I=1,NCOL
      DO 1320 J=1,NROW
      IF(LKARR1(I,J,1).NE.L1) GO TO 1320
      DO 1318 K2=1,NLAY
      IF(LKARR1(I,J,K2).EQ.0) GO TO 1319
 1318 CONTINUE
      BOTIJ = BOTM(I,J,LBOTM(NLAY))
      GO TO 1313
 1319 BOTIJ = BOTM(I,J,LBOTM(K2-1))
Cdep Add values to area and depth tables
      IF(K.EQ.1) THEN
        IF(TBELV+TBNC.GT.BOTIJ) THEN
          AREATABLE(K,L1)=AREATABLE(K,L1)+DELC(J)*DELR(I)
          DEPTHTABLE(K,L1)=TBELV
        END IF
      ELSE 
        IF(TBELV.GT.BOTIJ) THEN
          AREATABLE(K,L1)=AREATABLE(K,L1)+DELC(J)*DELR(I)
          DEPTHTABLE(K,L1)=TBELV
        END IF
      END IF  
 1313 IF(TBELV.LE.BOTIJ) GO TO 1320
      DV = (TBELV-BOTIJ)*DELC(J)*DELR(I)   
      EVOL = EVOL + DV
 1320 CONTINUE
Cdep Added printing of area for each lake depth
Cdep  WRITE(IOUT,1315) TBELV, EVOL
      WRITE(IOUT,1315) TBELV, EVOL, AREATABLE(K,L1)
      TBELV = TBELV + TBNC
 1325 CONTINUE
      WRITE(IOUT,1326)
 1326 FORMAT(120X)
Cdep  set minimum and maximum lake stages for transient simulations
      IF(ISS.EQ.0) THEN
        SSMN(L1)=BOTTMS(L1)
        SSMX(L1)=TBELV
      END IF
 1330 CONTINUE
 1331 CONTINUE
C
Cdep initialized arrays to zero where allocated
C
C-- INITIALIZE STREAM INFLOW SEGMENT ARRAY TO ZERO.
Cdep      DO 400 LNUM=1,NLAKES
Cdep         DO 400 LNP=1,NSS
Cdep400         ITRB(LNUM,LNP)=0
C
C-- INITIALIZE STREAM OUTFLOW SEGMENT ARRAY TO ZERO.
Cdep      DO 500 LNUM=1,NLAKES
Cdep         DO 500 LNP=1,NSS
Cdep500         IDIV(LNUM,LNP)=0
C
      IF(IUNITSFR.LE.0) THEN
         NDV=0
         NTRB=0
      END IF
C
C
C--  READ LINKAGE PARAMETERS FOR COALESCING LAKES
C
C    FOR EACH CONNECTED LAKE SYSTEM, READ LAKE NUMBERS OF CENTER LAKES
C    AND ADJOINING LAKES AND SILL ELEVATIONS.  ENTER CARD IMAGES
C    FOR SUBLAKE SYSTEMS EVEN IF LINKED TO MAIN LAKE SYSTEM.  SYSTEMS
C    MUST BE ORDERED HIERARCHICALLY.
C
      ICMX = 0
      NCLS=0
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(I5)') NSLMS
      ELSE
        READ(IN,*) NSLMS
      END IF
      WRITE(IOUT,680) NSLMS
  680 FORMAT(/1X,'NUMBER OF CONNECTED LAKE SYSTEMS IN SIMULATION IS ',I3
     1)
      IF(NSLMS.LE.0) GO TO 760
      DO 700 IS=1,NSLMS
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(16I5)',END=750) IC,(ISUB(IS,I),I=1,IC)
      ELSE
        READ(IN,*,END=750) IC,(ISUB(IS,I),I=1,IC)
      END IF
      IF(IC.LE.0) GO TO 750
      IF(IC.GT.ICMX) ICMX=IC
      ICS(IS)=IC
      IC1 = IC - 1
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(100F10.2)') (SILLVT(IS,I),I=1,IC1)
      ELSE
        READ(IN,*) (SILLVT(IS,I),I=1,IC1)
      END IF
      WRITE(IOUT,18) IS, ICS(IS), ISUB(IS,1)
   18 FORMAT(/10X,'SYSTEM',I3//2X,'NUMBER OF LAKES IN SYSTEM',I5,
     1  '  CENTER LAKE NUMBER',I5//1X,'SUBLAKE NUMBER',3X,
     2  'SILL ELEVATION'/)
      DO 715 JK=2,IC
  715 WRITE(IOUT,717) ISUB(IS,JK), SILLVT(IS,JK-1)
  717 FORMAT(8X,I2,8X,F10.2)
  700 CONTINUE
  750 CONTINUE
      NCLS=IS-1
      WRITE(IOUT,751) NCLS
  751 FORMAT(/1X,'READ DATA FOR',I5,' LAKE SYSTEMS'/)
  760 CONTINUE
C
C----- READ LAKE PRECIPITATION, EVAPORATION, RUNOFF, AND WITHDRAWAL RATES.
C      IF ITMP1 LT 0, SPECIFICATIONS FROM LAST STRESS PERIOD ARE USED.
C
  800 IF(ITMP1.GE.0) GO TO 801
      WRITE(IOUT,802)
  802 FORMAT(1H0,'REUSING RECH,ET,WITHDRAWAL RATES FROM LAST STRESS PERI
     1OD'/)
      GOTO 900
  801 IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,7)
7     FORMAT(/1X,'LAKE',7X,'PRECIP',5X,'EVAP',5X,'RUNOFF',
     2     3X,'WITHDRAW',3X,'BOTTOM',5X,'AREA',5X,'SS MIN',3X,'SS MAX'
     1/90('-'))
      IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,77)
   77 FORMAT(/1X,'LAKE',7X,'PRECIP',5X,'EVAP',5X,'RUNOFF',
     2     3X,'WITHDRAW',3X,'BOTTOM',5X,'AREA',5X,/70('-'))
      IF (IUNITGWT.GT.0) WRITE (IOUTS,8)
 8    FORMAT (//1X,'LAKE',4X,'SOLUTE',6X,'CPPT',6X,'CRNF',6X,'CAUG'/)
      DO 300 LM=1,NLAKES
      IF(IFREFM.EQ.0) THEN
        IF(ISS.NE.0.AND.KKPER.GT.1) READ(IN,'(6F10.4)') PRCPLK(LM),
     1   EVAPLK(LM),RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)
        IF(ISS.EQ.0.OR.KKPER.EQ.1) READ(IN,'(6F10.4)') PRCPLK(LM),
     1   EVAPLK(LM),RNF(LM),WTHDRW(LM)
      ELSE
        IF(ISS.NE.0.AND.KKPER.GT.1) READ(IN,*) PRCPLK(LM),EVAPLK(LM),
     1   RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)
        IF(ISS.EQ.0.OR.KKPER.EQ.1) READ(IN,*) PRCPLK(LM),EVAPLK(LM),
     1   RNF(LM),WTHDRW(LM)
      END IF
      IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,9) LM,PRCPLK(LM),EVAPLK(LM)
     1 ,RNF(LM),WTHDRW(LM),BOTTMS(LM),BGAREA(LM),SSMN(LM),SSMX(LM)
9     FORMAT(1X,I3,4X,1P,3E10.3,1X,5E10.3)
      IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,9) LM,PRCPLK(LM),EVAPLK(LM),
     1 RNF(LM),WTHDRW(LM),BOTTMS(LM),BGAREA(LM)
      IF(IUNITGWT.LE.0) GO TO 300
      DO 850 ISOL=1,NSOL
      IF(IFREFM.EQ.0) THEN
         IF(WTHDRW(LM).LT.0.0) THEN
            READ(IN,'(3F10.4)')CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)
         ELSE
            READ(IN,'(2F10.4)')CPPT(LM,ISOL),CRNF(LM,ISOL)
         END IF
      ELSE
         IF(WTHDRW(LM).LT.0.0) THEN
            READ(IN,*) CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)
         ELSE
            READ(IN,*) CPPT(LM,ISOL),CRNF(LM,ISOL)
         END IF
      END IF
      IF(WTHDRW(LM).LT.0.0)
     1WRITE(IOUTS,840) LM,ISOL,CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)
      IF(WTHDRW(LM).GE.0.0)
     1WRITE(IOUTS,841) LM,ISOL,CPPT(LM,ISOL),CRNF(LM,ISOL)
  840 FORMAT(1X,I3,6X,I3,4X,1P,3E10.2)
  841 FORMAT(1X,I3,6X,I3,4X,1P,2E10.2)
  850 CONTINUE
C      WRITE (IOUTS,'(/)')
  300 CONTINUE
      WRITE (IOUT,'(/)')
C
C--  Define Initial Lake Volume & Initialize Cumulative Budget Terms
      IF(KKPER.EQ.1) THEN
         DO 8400 LK=1,NLAKES
 8400    VOL(LK)=0.0
         DO 8450 LK=1,NLAKES
             CUMPPT(LK)=0.0
             CUMEVP(LK)=0.0
             CUMRNF(LK)=0.0
             CUMGWI(LK)=0.0
             CUMGWO(LK)=0.0
             CUMSWI(LK)=0.0
             CUMSWO(LK)=0.0
             CUMWDR(LK)=0.0
             CUMFLX(LK)=0.0
 8450    CONTINUE
            DO 8900 L=1,LKNODE
               IL=ILAKE(1,L)
               IR=ILAKE(2,L)
               IC=ILAKE(3,L)
               LAKE=ILAKE(4,L)
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
               ITYPE = (ILAKE(5,L)+1)/2
               IF(ITYPE.EQ.3) ITYPE=0
               IF(ITYPE.NE.0) GO TO 8900
               IF(IL.GT.1) BOTLK = BOTM(IC,IR,LBOTM(IL-1))
               IF(IL.EQ.NLAY.AND.LKARR1(IC,IR,IL).GT.0)
     1            BOTLK = BOTM(IC,IR,LBOTM(IL))
               IF(STAGES(LAKE).GT.BOTLK) THEN
                  AREA = DELR(IC)*DELC(IR)
                  VOL(LAKE)=VOL(LAKE)+(STAGES(LAKE)-BOTLK)*AREA
cgage
                  VOLINIT(LAKE)=VOL(LAKE)
               ENDIF
 8900       CONTINUE
      ENDIF

 900  IF (IUNITBCF.GT.0) THEN  ! rsr, moved if block from main
        CALL SGWF2LAK7BCF7RPS()
      ELSE IF (IUNITLPF.GT.0) THEN
        CALL SGWF2LAK7LPF7RPS()
      ELSE IF (IUNITHUF.GT.0) THEN
        STOP 'ERROR, HUF Package not implemented in GSFLOW'   !gsf
        CALL SGWF2LAK7HUF7RPS()
      ELSE
!gsf    WRITE (IOUT, *) 'LAK Package requires BCF, LPF, or HUF'
        WRITE (IOUT, *) 'LAK Package requires BCF or LPF'     !gsf
        CALL USTOP(' ')
      END IF
      IF (IUNITSFR.GT.0) CALL SGWF2LAK7SFR7RPS()
      
C
C7------RETURN
      RETURN
      END
C
      SUBROUTINE GWF2LAK7AD(KKPER,KKSTP,IUNITGWT,IGRID)
C
C------VERSION 7 JUNE 2006 GWF2LAK7AD
C
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP FOR TRANSIENT LAKE SIMULATION, AND COPY
C             INITIAL LAKE STAGES TO STGOLD FOR STEADY STATE.
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: NLAKES, LKNODE, STGOLD2, FLOB, STAGES,
     +                        STGNEW, STGOLD
C     ------------------------------------------------------------------
C
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
C
C1 --- COPY INITIAL LAKE STAGES TO STGOLD.
          DO 10 I=1,NLAKES
      IF(KKPER.EQ.1.AND.KKSTP.EQ.1) STGOLD(I)=STAGES(I)
      IF(KKPER.EQ.1.AND.KKSTP.EQ.1) STGOLD2(I)=STAGES(I)
10    IF(KKPER.EQ.1.AND.KKSTP.EQ.1) STGNEW(I)=STAGES(I)
C2 ----- IF NOT FIRST TIME STEP, OR FIRST STRESS PERIOD, UPDATE
C           STGOLD BY STGNEW.
      IF (KKPER.NE.1.OR.KKSTP.NE.1) THEN
            DO 30 K=1,NLAKES
               STGOLD(K)=STGNEW(K)
30             STGOLD2(K)=STGNEW(K)
      ENDIF
C
C-----Initialize FLOB array (stores cell by cell flux between lake and
C                            aquifer)
      IF (IUNITGWT.GT.0) THEN
        DO 50 LK=1,LKNODE
 50        FLOB(LK)=0.0
      END IF
C
C3------RETURN
      RETURN
      END
C
      SUBROUTINE GWF2LAK7ST(NFLG,IGRID)
C   ********************************************************************
C   SET IBOUND VALUES SO THAT RECHARGE AND EVAPOTRANSPIRATION (ET) WILL
C   BE ASSIGNED CORRECTLY UNDERNEATH DRYING LAKES (NFLG = 0), OR RESET
C   IBOUND AFTER RECHARGE AND ET ARE COMPUTED (NFLG = 1).
C   ********************************************************************
C
C   SPECIFICATIONS:
C
C-----------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, ILAKE, STGOLD
      USE GLOBAL,       ONLY: IBOUND, LBOTM, BOTM
C-----------------------------------------------------------------------
C
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)

      IF(LKNODE.EQ.0) RETURN
      DO 10 L=1,LKNODE
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      ITYPE = (ILAKE(5,L)+1)/2
      IF(ITYPE.EQ.3) ITYPE=0
C
C-------ONLY CHANGE IBOUND FOR VERTICALLY ADJACENT NODE FACES
      IF(ITYPE.NE.0) GO TO 10
      IL = ILAKE(1,L)
      IR = ILAKE(2,L)
      IC = ILAKE(3,L)
C
C-------RESET AFTER EXECUTING RECHARGE OR ET ROUTINES
      IF(NFLG.EQ.1) GO TO 8
C
C-------RESET BEFORE EXECUTING RECHARGE OR ET ROUTINES
      IBOUND(IC,IR,IL-1) = -7
C
C-------THIS IS THE CORRECT ASSIGNMENT IF PORTION OF LAKE IN COLUMN
C       IS WET.
      LAKE = ILAKE(4,L)
      IF(STGOLD(LAKE).GT.BOTM(IC,IR,LBOTM(IL)-1)) GO TO 10
C
C-------IF PORTION OF LAKE IN NODE IS DRY, LET RECHARGE AND ET BE
C       APPLIED TO THE AQUIFER NODE UNDERNEATH THE LAKE BY SETTING
C       IBOUND EQUAL TO 0.
    8 IBOUND(IC,IR,IL-1) = 0
   10 CONTINUE
C
C3------RETURN
      RETURN
      END
C
      SUBROUTINE GWF2LAK7FM(KITER,KKPER,IUNITSFR,IUNITUZF,IUZFBND,
     +                      FINF,VKS,IGRID)
C
C----- USGS VERSION 7; JUNE 2006 GWF2LAK7FM
Cdep  MODIFIED SUBROUTINE TO ITERATIVELY SOLVE FOR LAKE STAGE EVEN
C       DURING TRANSIENT STRESS PERIODS.
C     ******************************************************************
C     ADD LAKE TERMS TO RHS AND HCOF IF SEEPAGE OCCURS IN MODEL CELLS
C     ******************************************************************
C
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IBOUND, IOUT, ISSFLG, 
     +                        DELR, DELC, LBOTM, BOTM, HNEW, HCOF, RHS 
      USE GWFBASMODULE, ONLY: DELT
      USE GWFSFRMODULE, ONLY: STRIN, STROUT, FXLKOT, DLKSTAGE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
Cdep  Added functions for interpolating between areas, derivatives, 
Cdep     and outflow rates
C     ------------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
      DOUBLE PRECISION FINTERP, DERIVTERP, OUTFLWTERP
      EXTERNAL FINTERP, DERIVTERP, OUTFLWTERP
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER, INTENT(IN) :: KITER, KKPER, IUNITSFR, IUNITUZF, IGRID   
Cdep  added runoff and flobo3
      REAL :: RUNOFF
Cdep DIMENSIONED UZF ARRAYS (NEXT 2 LINES).
      REAL :: FINF(NCOL,NROW), VKS(NCOL,NROW)
      INTEGER :: IUZFBND(NCOL,NROW)
Cdep  added unsaturated flow beneath lakes flag as a local variable
      INTEGER iuzflg
      DOUBLE PRECISION BOTLK,BOTCL,CONDUC,H,FLOBOT,STGON,FLOBO2,
     1                 FLOBO1,FLOBO3,H1,THET1
Cdep  added double precision variables
      DOUBLE PRECISION RESID1, RESID2, DERIV, DSTAGE
      PARAMETER(CLOSEZERO = 1.0E-07)
C     ------------------------------------------------------------------
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)

C1------IF LKNODE<=0 THERE ARE NO LAKE NODES. RETURN.
      IF (LKNODE.LE.0) RETURN
      ISS = ISSFLG(KKPER)
C
C2------PROCESS EACH CELL IN THE ILAKE LIST.
Cdep   added STGITER, and STGNEW to INITIALIZATION.
      DO 100 LK=1,NLAKES
        IF(KITER.EQ.1)THEN
         STGITER(LK) = STGOLD(LK)
         STGNEW(LK) = STGOLD(LK)
        END IF
        NCNCVR(LK) = 0
        LIMERR(LK) = 0
        SURFIN(LK)=0.0
        DSRFOT(LK)=0.0
Cdep   added array for limiting ground water seepage to surface inflow
        GWRATELIM(LK) = 0.0
100     SURFOT(LK)=0.0
C
C2A --- SUM UP INFLOWS FROM INFLOWING STREAM REACHES.
Crsr  Added conditional if statement
      IF (IUNITSFR.GT.0) THEN
        DO 200 LK=1,NLAKES
        DO 200 ITRIB=1,NTRB
            INODE=ITRB(LK,ITRIB)
            IF (INODE.LE.0) GO TO 200
            SURFIN(LK)=SURFIN(LK)+STRIN(INODE)
200     CONTINUE
      END IF
Crsr    end of conditional if statement
Cdep  Compute flow into lake for surface sources
      DO LAKE = 1,NLAKES
       IF(RNF(LAKE).GE.0.0) RUNF = RNF(LAKE)
       IF(RNF(LAKE).LT.0.0) RUNF =-RNF(LAKE)*PRCPLK(LAKE)*BGAREA(LAKE)
       FLWIN(LAKE) = PRCPLK(LAKE)*AREATABLE(1,LAKE)+SURFIN(LAKE)
     +               +RUNF
       IF(WTHDRW(LAKE).LE.0.0) FLWIN(LAKE) = FLWIN(LAKE) - WTHDRW(LAKE)
      END DO
Cdep    commented the initialization of SURFOT; NO LONGER NEEDED.
C2B --- SUM UP OUTFLOWS FROM OUTFLOWING STREAM REACHES.
Cdep         DO 300 LK=1,NLAKES
Cdep         DO 300 IDV=1,NDV
Cdep            INODE=IDIV(LK,IDV)
Cdep            IF (INODE.LE.0) GO TO 300
Cdep            SURFOT(LK)=SURFOT(LK)+STROUT(INODE)
Cdep            IF(ISS.NE.0) DSRFOT(LK) = DSRFOT(LK) + DSTROT(INODE)
Cdep300      CONTINUE
Cdep   use newton method to compute transient lake change no matter
Cdep     if stress period is steady-state or transient.
         ISS1 = 1   
Cdep         ISS1 = ISS
  350    MTER = 1
Cdep   Note theta is a time weighted solution for lake stage. It is
Cdep    used in steady state as a relaxation parameter and is not as
Cdep    defined in the original documentation report by Merritt and 
Cdep    Konikow. Theta is set to 1 for any steady state simulation
Cdep    as relaxation is not necessary for the Newton solution.
Cdep         THET1 = THETA
         IF (ISS.NE.1) THEN
           THET1 = DBLE(THETA)
         ELSE
           THET1 = 1.0D0
         END IF
         IF(ISS1.NE.0) MTER = NSSITR
         DO 1001 L1=1,MTER
C2C ---- INITIALIZE SUMMATION PARAMETERS.
          DO 400 LK=1,NLAKES
             SUMCNN(LK) = 0.0
             SUMCHN(LK) = 0.0
             EVAP(LK)=0.0
             PRECIP(LK)=0.0
             SEEP(LK)=0.0
Cdep   Added SEEP3 to initialization
             SEEP3(LK)=0.0
400          SURFA(LK)=0.0
Cdep    Limit ET to surface flow into lake
         DO LL=1,NLAKES
           EVAPO(LL) = EVAPLK(LL)
           WITHDRW(LL) = WTHDRW(LL)
           FLWITER(LL) =FLWIN(LL)
           IF (STGNEW(LL)-BOTTMS(LL).LT.CLOSEZERO) THEN
             IF(WTHDRW(LL).GT.0.0) THEN
               IF(WTHDRW(LL).GE.FLWITER(LL)) THEN
                 WITHDRW(LL) = FLWITER(LL)
                 EVAPO(LL) = 0.0
                 FLWITER(LL) = 0.0
               ELSE
                 FLWITER(LL) = FLWITER(LL)-WTHDRW(LL)
                 IF(EVAPO(LL)*AREATABLE(1,LL).GE.FLWITER(LL)) THEN
                   EVAPO(LL) = FLWITER(LL)/AREATABLE(1,LL)
                   FLWITER(LL) = 0.0
                 ELSE
                   FLWITER(LL) = FLWITER(LL)-EVAPO(LL)*AREATABLE(1,LL)
                 END IF
               END IF
             ELSE
               IF(EVAPO(LL)*AREATABLE(1,LL).GE.FLWITER(LL)) THEN
                 EVAPO(LL) = FLWITER(LL)/AREATABLE(1,LL)
                 FLWITER(LL) = 0.0
               ELSE
                 FLWITER(LL) = FLWITER(LL)-EVAPO(LL)*AREATABLE(1,LL)
               END IF
             END IF
           END IF
           IF(FLWITER(LL).GT.0.0) THEN
             GWRATELIM(LL) = FLWITER(LL)/AREATABLE(1,LL)
           ELSE
             GWRATELIM(LL) = 0.0
           END IF
         END DO
Cdep   End of do for limiting ET to surface flow into lake
C
C   MASTER NODE LOOP -- COMPUTE LAKEBED SEEPAGE TERMS AND ADD TO RESIDUAL
C   AND MATRIX TERMS FOR SOLUTION.
C
      DO 900 L=1,LKNODE
         IL=ILAKE(1,L)
         IR=ILAKE(2,L)
         IC=ILAKE(3,L)
C
C4------DETERMINE LAKE AND NODAL LAYER,ROW,COLUMN NUMBER.
         LAKE=ILAKE(4,L)
Cdep    added flag for simulating unsaturated flow beneath lakes
         iuzflg = 0
         IF(ISS1.NE.0.AND.NCNCVR(LAKE).NE.0) GO TO 900
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
         ITYPE = (ILAKE(5,L)+1)/2
         IF(ITYPE.EQ.3) ITYPE=0
         AREA = DELR(IC)*DELC(IR)
         IF(IL.GT.1) BOTLK = BOTM(IC,IR,LBOTM(IL-1))
         BOTCL = BOTM(IC,IR,LBOTM(IL))
         IF(IL.EQ.NLAY.AND.CNDFCT(L).EQ.0.0) BOTLK = BOTCL
         RAIN = PRCPLK(LAKE)
         EV = EVAPLK(LAKE)
C
C5------CONDUCTANCE FACTOR NEEDED FOR SEEPAGE CALCULATIONS.
Cdep    Initialize flobot, flobo1, flobo2, and flobo3 to 0.0d0
         DLSTG = 0.000001
         FLOBOT = 0.0D0
         FLOBO1 = 0.0D0
         FLOBO2 = 0.0D0
         FLOBO3 = 0.0D0
         CONDUC=CNDFCT(L)
         IF(CONDUC.EQ.0.0) GO TO 600
         H=HNEW(IC,IR,IL)
         STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
C
C6------COMPUTE SEEPAGE INTO OR OUT OF A LAKE BED NODE, AND
C       SEEPAGE INTO OR OUT OF A LAKE WALL NODE, WHEN ITYPE=1 OR 2.
         IF (ITYPE.NE.0) GO TO 515
            IL1 = IL
            IF(IBOUND(IC,IR,IL).GT.0) GO TO 508
            DO 505 LI=IL,NLAY
            IF(IBOUND(IC,IR,LI).GT.0) GO TO 507
  505       CONTINUE
            WRITE(IOUT,506) L,IC,IR,IL
  506       FORMAT(1X,'ERROR - NO AQUIFER UNDER LAKE CELL ',4I5)
Cdep LI is nlay+1 if no active cells beneath lake!
  507       IF(LI.LE.NLAY) THEN
              IL1 = LI
            ELSE
              IL1 = NLAY
            END IF
            H = BOTLK
Cdep    Added replaced STGON with STGNEW and added Head check to 
C           IF statement (important check)
C  508      IF(STGON.LE.BOTLK.AND.H.LE.BOTLK) GO TO 547
 508      IF(STGNEW(LAKE).LE.BOTLK.AND.H.LE.BOTLK) GO TO 547
Cdep    added flobo3 for the purpose of using newton method
Cdep    revised if statements
             IF(H.GE.BOTLK)THEN
               FLOBO2=CONDUC*(STGNEW(LAKE)-H)
               FLOBO3=CONDUC*(STGNEW(LAKE)+DLSTG-H)
               FLOBO1=CONDUC*(STGOLD(LAKE)-H)
               FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
               FLOBO3 = THET1*FLOBO3 + (1.0D0-THET1)*FLOBO1
             ELSE
               FLOBO2=CONDUC*(STGNEW(LAKE)-BOTLK)
               FLOBO3=CONDUC*(STGNEW(LAKE)+DLSTG-BOTLK)
               FLOBO1=CONDUC*(STGOLD(LAKE)-BOTLK)
               FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
Cdep    Correct ground water inflow when STGNEW at lake bottom
             IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO)THEN
               IF(FLOBOT.LT.0.0) FLWITER(LAKE) = FLWITER(LAKE) - FLOBOT
             END IF
               FLOBO3 = THET1*FLOBO3 + (1.0D0-THET1)*FLOBO1
Cdep    restrict flow through bottom of cell to vertical saturated K
               IF(IUNITUZF.GT.0.AND.IUZFBND(IC,IR).GT.0)THEN
                 iuzflg = 1
                 IF (FLOBOT/AREA.GT.VKS(IC,IR))
     +             FLOBOT = VKS(IC,IR)*AREA
                 IF (FLOBO3/AREA.GT.VKS(IC,IR))
     +             FLOBO3 = VKS(IC,IR)*AREA
                 IF (FLOBOT/AREA.GT.VKS(IC,IR))
     +             FLOBOT = VKS(IC,IR)*AREA
                 IF (FLOBO3/AREA.GT.VKS(IC,IR))
     +             FLOBO3 = VKS(IC,IR)*AREA
                 FINF(IC,IR)=FLOBOT/AREA  
               END IF
             END IF
           GO TO 535
  515     IF (ITYPE.NE.1.AND.ITYPE.NE.2) GO TO 550
          IF(IBOUND(IC,IR,IL).LE.0) GO TO 900
Cdep    Added Head check to IF statement (important check)          
          IF(STGON.LE.BOTCL.AND.H.LE.BOTLK) GO TO 550
          HD = H
          IF(H.GT.BOTM(IC,IR,LBOTM(IL)-1))
     1                         HD = BOTM(IC,IR,LBOTM(IL)-1)
          THCK = HD - BOTCL
          IF(THCK.LE.0.0) THCK = 0.0
          CONDUC = CONDUC*THCK
Cdep    Added flobo3 for revised newton method
          FLOBO3 = CONDUC*(STGNEW(LAKE)+DLSTG-H)
          FLOBO2 = CONDUC*(STGNEW(LAKE)-H)
          FLOBO1 = CONDUC*(STGOLD(LAKE)-H)
          FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
Cdep    Correct ground water inflow when stgon at lake bottom
          IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO)THEN
            IF(FLOBOT.LT.0.0)FLWITER(LAKE)=FLWITER(LAKE)-FLOBOT
          END IF
          FLOBO3  = THET1*FLOBO3 + (1.0D0-THET1)*FLOBO1
  535     SUMCNN(LAKE) = SUMCNN(LAKE) + CONDUC
          H1 = H
          IF(ITYPE.EQ.0.AND.BOTLK.GT.H) H1 = BOTLK
          SUMCHN(LAKE) = SUMCHN(LAKE) + CONDUC*H1
          GO TO 550
Cdep      limit flobot if botlk equal to overall lake bottom
  547     IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) THEN
            FLOBOT = GWRATELIM(LAKE)*AREA
            FLWITER(LAKE) = FLWITER(LAKE) - FLOBOT
          ELSE
            IBOUND(IC,IR,IL-1) = 0
          END IF
  550     CONTINUE
C
C  ADD LAKE SEEPAGE RATES TO MATRIX AND RESIDUAL TERMS BEFORE SOLVING
C
       IF(ISS1.NE.0) GO TO 600
         IF (ITYPE.NE.0) GO TO 825
Cdep    Added FLOBOT check to IF statement  (important check)
           IF (STGON.LE.BOTLK.AND.ABS(FLOBOT).LT.CLOSEZERO) GO TO 600
           IF (H.LE.BOTLK) GO TO 870
           IF (H.GT.BOTLK) GO TO 850
825      IF (ITYPE.NE.1.AND.ITYPE.NE.2) GO TO 600
            IF (STGON.GT.BOTCL.AND.THCK.GT.0.0) GO TO 850
            GO TO 600
Cdep    Revised 850 statement to limit seepout out of lake to
Cdep       available water in lake
850      IF(STGNEW(LAKE)-BOTTMS(LAKE).GT.CLOSEZERO) THEN
           RHS(IC,IR,IL)=RHS(IC,IR,IL) - STGON*CONDUC
           HCOF(IC,IR,IL)=HCOF(IC,IR,IL) - CONDUC
         ELSE
           RHS(IC,IR,IL)=RHS(IC,IR,IL)-FLOBOT
         END IF
         GO TO 600
Crgn870      RHS(IC,IR,IL1)=RHS(IC,IR,IL1)-FLOBOT
Cdep  Only add flobot to right hand side when unsaturated flow beneath
Cdep      lakes is inactive otherwise leakage is added to unsaturated zone
870      IF(iuzflg.EQ.0. AND. IL1.LE.NLAY)THEN
           RHS(IC,IR,IL1)=RHS(IC,IR,IL1)-FLOBOT
         END IF
600      CONTINUE
C
C7------COMPUTE EVAPORATION FOR EACH LAKE NODE AND ADD IT TO THE
C          ACCUMULATIVE EVAPORATION DISCHARGE.
          IF (ITYPE.EQ.0) THEN
             EP=EV*AREA
Cdep revised computation of evaporation on basis of ground-water discharge
             IF (STGON.LT.BOTLK.AND.H.LT.BOTLK) EP=0.0
             EVAP(LAKE)=EVAP(LAKE)+EP
          ENDIF
C8-------COMPUTE RAINFALL RECHARGE FOR EACH LAKE NODE AND ADD IT TO
C         THE ACCUMULATIVE PRECIPITATION RECHARGE.
          IF (ITYPE.EQ.0) THEN
             R2 = RAIN*AREA
Cdep revised computation of precipitation on basis of ground-water discharge
             IF (STGON.LT.BOTLK.AND.H.LT.BOTLK) R2 = 0.0
             PRECIP(LAKE)=PRECIP(LAKE)+R2
          ENDIF
C9-------COMPUTE ACCUMULATIVE SEEPAGE THROUGH LAKEBED SEDIMENTS.
Cdep      Compute seepage for transient simulations. Added
C         variable SEEP3 to allow seepage to vary as a function 
C         of lake stage within the Newton iterations.
C             IF(ISS1.NE.0) SEEP(LAKE)=SEEP(LAKE)-FLOBOT
          SEEP(LAKE)=SEEP(LAKE)-FLOBOT
          SEEP3(LAKE)=SEEP3(LAKE)-FLOBO3        
C10------COMPUTE ACCUMULATIVE LAKE NODAL AREA, EXCLUDE WALL NODES.
Cdep    Revised computation of surface area.
Cdep       IF (ITYPE.EQ.0.AND.STGON.GE.BOTLK) THEN
Cdep          SURFA(LAKE)=SURFA(LAKE)+ AREA
              SURFA(LAKE)=FINTERP(STGITER(LAKE),LAKE)
Cdep       ENDIF
900      CONTINUE
C
C11------ONLY COMPUTE LAKE LEVEL AFTER SCANNING THRU ALL NODES OF A LAKE.
C
       DO 1000 LAKE=1,NLAKES
       IF(SURFA(LAKE).LE.0.0) GO TO 1000
       IF(RNF(LAKE).GE.0.0) RUNF = RNF(LAKE)
       IF(RNF(LAKE).LT.0.0) RUNF =-RNF(LAKE)*PRCPLK(LAKE)*BGAREA(LAKE)
Cdep  added runoff from overland flow when Unsaturated Flow is active
       IF (IUNITUZF.GT.0) THEN
         RUNOFF = OVRLNDRNF(LAKE)
        ELSE
         RUNOFF = 0.0
       END IF
Cdep revised following section to iteratively solve for lake stage for
Cdep    both steady state and transient stress periods.
Cdep       IF(ISS1.NE.0) GO TO 970
Cdep       IF(ISS.NE.0) GO TO 1000
Cdep       DDSF = DELT/SURFA(LAKE)
Cdep       STGNEW(LAKE)=(STGOLD(LAKE)+DDSF*(PRECIP(LAKE)
Cdep     1 -EVAP(LAKE)+RUNF+RUNOFF-WTHDRW(LAKE)+SURFIN(LAKE)-SURFOT(LAKE)-
Cdep     2 (1.0-THETA)*STGOLD(LAKE)*SUMCNN(LAKE)+
Cdep     3 SUMCHN(LAKE)))/(1.0+THETA*DDSF*SUMCNN(LAKE))
Cdep      GO TO 1000
      IF (ISS1.NE.0) THEN
Cdep  970   IF(NCNCVR(LAKE).NE.0) GO TO 1000
        IF(NCNCVR(LAKE).NE.0) GO TO 1000
          SSMN1 = SSMN(LAKE)
          SSMX1 = SSMX(LAKE)
Cdep moved comment 2B ---SUM UP OUTFLOWS FROM OUTFLOWING STREAM REACHES.
Cdep Revised following to end of subroutine.
         DSRFOT(LAKE) = 0.0D0
         SURFOT(LAKE) = 0.0D0
         STGITER(LAKE) = STGNEW(LAKE)
Cdep  Test for lake stage less than or equal to bottom elevation of lake.
        IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) THEN
          EVAPO(LAKE) = EVAPO(LAKE)*AREATABLE(1,LAKE)
          IF(EVAPO(LAKE).LT.EVAP(LAKE)) THEN
            IF(EVAP(LAKE).LT.FLWITER(LAKE)) THEN
              EVAPO(LAKE)=EVAP(LAKE)
              FLWITER(LAKE)=FLWITER(LAKE)-EVAPO(LAKE)
            ELSE
               EVAPO(LAKE)=EVAPO(LAKE)+FLWITER(LAKE)
               FLWITER(LAKE)= 0.0
            END IF
          END IF
          EVAP(LAKE)=EVAPO(LAKE)
          PRECIP(LAKE)=PRCPLK(LAKE)*AREATABLE(1,LAKE)
        END IF
        STGITER(LAKE) = STGNEW(LAKE)
Cdep Added conditional if to skip computation if no streams. 
        IF(IUNITSFR.GT.0) THEN
          DO 300 IDV=1,NDV
            INODE=IDIV(LAKE,IDV)
            IF (INODE.GT.0) THEN
              IF( DLKSTAGE(1,INODE).LT.BOTTMS(LAKE)) THEN
                WRITE(IOUT,972)LAKE,BOTTMS(LAKE),
     +                         DLKSTAGE(1,INODE),INODE
972             FORMAT(' BOTTOM ELEVATION OF LAKE ',I5,' IS ', F10.2,
     +                 ' AND IS ABOVE OUTLET ELEVATION OF ', F10.2,
     +                 ' FOR STREAM SEGMENT ',I5,/1X,
     +                 ' THIS WILL CAUSE PROBLEMS IN COMPUTING LAKE',
     +                 ' STAGE USING THE NEWTON METHOD. '/1X,
     +                 ' ELEVATION OF STREAM OUTLET MUST BE GREATER'
     +                 ' THAN OR EQUAL TO THE LOWEST ELEVATION OF THE',
     +                 ' LAKE.',/1X,'*****PROGRAM STOPPING'/)
                CALL USTOP(' ')
              END IF
              IF (FXLKOT(INODE).LE.0.0) THEN
                DSTAGE = STGITER(LAKE)
                DSRFOT(LAKE) = DSRFOT(LAKE) + DERIVTERP(DSTAGE,
     +                           INODE)
                STROUT(INODE) = OUTFLWTERP(DSTAGE,INODE)
              ELSE
                STROUT(INODE) = 0.0
              END IF
            SURFOT(LAKE)= SURFOT(LAKE) + STROUT(INODE)+ FXLKOT(INODE)
            IF(SURFOT(LAKE).LT.0.0)SURFOT(LAKE)=0.0
            END IF
300       CONTINUE
         END IF
Cdep  Replaced next five lines with new iterative newton method
Cdep      RESID = PRECIP(LAKE)-EVAP(LAKE)+RUNF-WTHDRW(LAKE)+
Cdep     1  SURFIN(LAKE)-SURFOT(LAKE)+SEEP(LAKE)
Cdep      STGO = STGOLD(LAKE)
Cdep      STGOLD(LAKE) = STGOLD(LAKE) + RESID/(DSRFOT(LAKE)+SUMCNN(LAKE))
Cdep      IF(STGOLD(LAKE).LE.SSMX1.AND.STGOLD(LAKE).GE.SSMN1) GO TO 975
Cdep  Test for transient or steady-state
         IF(ISS.EQ.0) THEN
           IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) THEN
             RESID1 = FLWITER(LAKE)*DELT/AREATABLE(1,LAKE)
           ELSE
             RESID1 = (PRECIP(LAKE)-EVAP(LAKE)+RUNF-WTHDRW(LAKE)+
     1                SURFIN(LAKE)-SURFOT(LAKE)+SEEP(LAKE))*
     2                DELT/FINTERP(STGITER(LAKE),LAKE)-(STGITER(LAKE)-
     3                STGOLD(LAKE))
           END IF
             OUTFLOW = SURFOT(LAKE)+ DSRFOT(LAKE)*DLSTG
             IF(OUTFLOW.LT.0.0)SURFOT(LAKE)=0.0
             RESID2 = (PRECIP(LAKE)-EVAP(LAKE)+RUNF-WTHDRW(LAKE)+
     1                SURFIN(LAKE)-OUTFLOW+SEEP3(LAKE))*
     2                DELT/FINTERP(STGITER(LAKE)+DLSTG,LAKE)-
     4                (STGITER(LAKE)+DLSTG-STGOLD(LAKE))
         ELSE
           IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) THEN
             RESID1 = FLWITER(LAKE)*DELT/AREATABLE(1,LAKE)
           ELSE
             RESID1 = (PRECIP(LAKE)-EVAP(LAKE)+RUNF-WTHDRW(LAKE)+
     1                SURFIN(LAKE)-SURFOT(LAKE)+SEEP(LAKE))*
     2                DELT/FINTERP(STGITER(LAKE),LAKE)
           END IF
             OUTFLOW = SURFOT(LAKE)+ DSRFOT(LAKE)*DLSTG
             RESID2 = (PRECIP(LAKE)-EVAP(LAKE)+RUNF-WTHDRW(LAKE)+
     1                SURFIN(LAKE)-OUTFLOW+SEEP3(LAKE))*
     2                DELT/FINTERP(STGITER(LAKE)+DLSTG,LAKE)
         END IF
           IF (DABS(RESID2-RESID1).GT.0.0D0) THEN
             DERIV = (RESID2-RESID1)/(DLSTG)
             STGNEW(LAKE) = STGITER(LAKE) - RESID1/DERIV
           ELSE
             STGNEW(LAKE) = STGITER(LAKE)- RESID1
           END IF
Cdep  Set STGNEW to lake bottom if STGNEW less than lake bottom.
        IF(STGNEW(LAKE).LT.BOTTMS(LAKE)) STGNEW(LAKE)=BOTTMS(LAKE)
        IF(ISS.NE.0)THEN
          IF(STGNEW(LAKE).LE.SSMX1.AND.STGNEW(LAKE).GE.SSMN1) GO TO 975
Cdep  Revised write statement to print STGNEW instead of STGOLD
Cdep      WRITE(IOUT,976) STGOLD(LAKE)
Cdep  976 FORMAT(/1X,'ERROR -- COMPUTED STEADY-STATE STAGE EXCEEDS SPECIFIED
Cdep     1 MAXIMUM OR MINIMUM ',F10.2)
          WRITE(IOUT,976) STGNEW(LAKE)
  976     FORMAT(/1X,'ERROR -- COMPUTED STAGE EXCEEDS SPECIFIED ',
     1     'MAXIMUM OR MINIMUM ',F10.2)
          LIMERR(LAKE) = LIMERR(LAKE) + 1
          IF(LIMERR(LAKE).LT.10) GO TO 977
          WRITE(IOUT,979) LAKE
  979     FORMAT(1X,'LAKE',I3,3X,'STAGE REPEATEDLY EXCEEDS ITS BOUNDS ',
     1         '--STOPPING EXECUTION')
          CALL USTOP(' ')
Cdep  Replaced STGOLD with STGNEW
Cdep  977 STGOLD(LAKE) = (SSMN1+SSMX1)/2.0
Cdep  975 STGNEW(LAKE) = STGOLD(LAKE)
  977     STGNEW(LAKE) = (SSMN1+SSMX1)/2.0
  975     CONTINUE
        END IF
Cdep  Replaced STGOLD with STGNEW
Cdep      DSTG = ABS(STGOLD(LAKE)-STGO)
        DSTG = ABS(STGNEW(LAKE)-STGITER(LAKE))
        IF(DSTG.LE.SSCNCR) NCNCVR(LAKE) = 1
Cdep  Moved end of IF (ISS1.NE.0) THEN to here
      END IF
 1000  CONTINUE
       IF(ISS1.EQ.0) RETURN
       NCNV = 0
       DO 1002 LAKE=1,NLAKES
       IF(NCNCVR(LAKE).EQ.0.AND.SURFA(LAKE).GT.0.0) NCNV = 1
 1002  CONTINUE
       IF(NCNV.EQ.0) GO TO 1005
 1001  CONTINUE
       DO 1003 LAKE=1,NLAKES
Cdep  Revised write statement to print STGNEW and STGITER
Cdep       IF(NCNCVR(LAKE).EQ.0) WRITE(IOUT,1004) KITER, LAKE, STGOLD(LAKE)
Cdep 1004  FORMAT(1X,'ITERATION ',I3,2X,'LAKE ',I3,2X,'STAGE ',F10.4,2X,
Cdep     1  'DID NOT CONVERGE TO STEADY-STATE')
       IF(NCNCVR(LAKE).EQ.0) WRITE(IOUT,1004) KITER, LAKE, 
     1   STGNEW(LAKE), STGITER(LAKE)
 1004  FORMAT(1X,'ITERATION ',I4,2X,'LAKE ',I4,2X,'NEW STAGE ',1PE14.8,
     1  '  DID NOT CONVERGE-- PREVIOUS INTERNAL ITERATION STAGE  ',
     2  1PE14.8,/)
 1003  CONTINUE
Cdep Added ISS1=0 to skip out of loop when one or more lakes do not 
Cdep   converge. Continued nonconvergence can lead to indefinite loop.
       ISS1 = 0
       GO TO 350
Cdep  Removed "STEADY-STATE" from write statement.
 1005  WRITE(IOUT,1006) L1
Cdep 1006  FORMAT(1X,'ALL LAKES CONVERGED TO STEADY-STATE AFTER ',I3,2X,
 1006  FORMAT(1X,'ALL LAKES CONVERGED AFTER ',I3,2X,'ITERATIONS')
Cdep  Removed printing of lake stages. Adds unnecessary output.
Cdep       WRITE(IOUT,1007) (STGNEW(LAKE),LAKE=1,NLAKES)
Cdep 1007  FORMAT(1X,'LAKE STAGES = ',10F11.3)
       ISS1 = 0
       GO TO 350
C
Cdep revised end statement to include SUBROUTINE GWF2LAK7FM
Cdep      END
      END SUBROUTINE GWF2LAK7FM
C
      SUBROUTINE GWF2LAK7BD(KSTP,KPER,IUNITGWT,IUNITGAGE,IUNITSFR,
     1                      IUNITUZF,NSOL,IUZFBND,FINF,VKS,IGRID)
C
C----- USGS VERSION 7; JUNE 2006 GWF2LAK7BD
C
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR LAKES
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, NODES, IBOUND, IOUT,
     +                        ISSFLG, DELR, DELC, LBOTM, BOTM, HNEW,
     +                        BUFF
     
      USE GWFBASMODULE, ONLY: MSUM, ICBCFL, IAUXSV, DELT, PERTIM, TOTIM,
     +                        HNOFLO, VBVL, VBNM
      USE GWFSFRMODULE, ONLY: STRIN, DLKSTAGE, SLKOTFLW
      CHARACTER*16 TEXT
Cdep  Added functions for interpolating between areas, derivatives,
Cdep     and outflow rates
C     ------------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
      DOUBLE PRECISION FINTERP, OUTFLWTERP
      EXTERNAL FINTERP, OUTFLWTERP
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      DOUBLE PRECISION BOTLK,BOTCL,CONDUC,H,FLOBOT,STGON,FLOBO2,
     1FLOBO1,H1,RATE,RATIN,RATOUT
Cdep  added double precision variable THET1
      DOUBLE PRECISION THET1
Cdep Add new double precision variable for storing lake seepage to Buff
      DOUBLE PRECISION FLOTOUZF               !rsr
Cdep DIMENSIONED UZF ARRAYS (NEXT 2 LINES).
      REAL FINF(NCOL,NROW), VKS(NCOL,NROW),vol2    !rsr
      INTEGER IUZFBND(NCOL,NROW)              !rsr
      DIMENSION JCLS(NCLS,ICMX)
      DIMENSION ILB(5),IRB(5),ICB(5)
      CHARACTER*16 LAKAUX(5)
      DIMENSION FACE(1)
      DATA TEXT /'   LAKE  SEEPAGE'/
      DATA LAKAUX(1)/'IFACE'/
Cdep  Defined closezero for conditional if calculations
      PARAMETER (CLOSEZERO=1.0E-7)
C     ------------------------------------------------------------------
C
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
      ISS = ISSFLG(KPER)
C
C1------SET IBD=1 IF BUDGET TERMS SHOULD BE SAVED ON DISK.
      ZERO = 0.0
      IBD=0
      KCNT = 0
      RATIN = 0.
      RATOUT =0.
C1A-----Set Lake budget terms for GSFLOW to zero.
      TOTGWIN_LAK = 0.0
      TOTGWOT_LAK = 0.0
      TOTDELSTOR_LAK = 0.0
      TOTSTOR_LAK = 0.0
      TOTEVAP_LAK = 0.0
      TOTPPT_LAK = 0.0
      TOTRUNF_LAK = 0.0
      TOTWTHDRW_LAK = 0.0
      TOTSURFIN_LAK = 0.0
      TOTSURFOT_LAK = 0.0
C
      DO 104 LDR = 1,NODES
  104 LDRY(LDR) = 0
      LDR = 0
C
C1B-----TEST TO SEE IF CELL-BY-CELL TERMS ARE NEEDED.
      IF(ILKCB.GT.0) IBD=ICBCFL
C  -----IF COMPACT BUDGET, WRITE LIST HEADER
      IF(IBD.EQ.2) THEN
         NAUX=0
         IF(IAUXSV.NE.0) NAUX=1
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,LAKAUX,ILKCB,NCOL,NROW,NLAY,
     1               LKNODE,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C1C------IF NO LAKE NODES, KEEP ZERO IN ACCUMULATORS.
      IF (LKNODE.EQ.0) GO TO 1200
C
C1D-----CLEAR CELL-BY-CELL BUFFER.
      DO 5 IL=1,NLAY
      DO 5 IR=1,NROW
      DO 5 IC=1,NCOL
5        BUFF(IC,IR,IL)=ZERO
C------
C2------PROCESS EACH CELL IN THE ILAKE LIST.
      DO 100 LK=1,NLAKES
        FLXINL(LK)=ZERO
        SURFIN(LK)=ZERO
Cdep   Initialize flow limiting and 3 dummy arrays to zero 
Cdep      for gage package
        GWRATELIM(LK) = ZERO
        XLAKES(lk,1) = ZERO
        XLKOLD(lk,1) = ZERO
        XLAKINIT(lk,1) = ZERO
100     CONTINUE
Cdep  Use streamoutflow computed from final MODFLOW iteration.
Cdep 100     SURFOT(LK)=ZERO
C
CMOC---If transport active, save previous value of lake volume.
      IF (IUNITGWT.GT.0.OR.IUNITGAGE.GT.0) THEN
        DO 150 LK=1,NLAKES
          VOLOLD(LK)=VOL(LK)
150     CONTINUE
      END IF
C
C2A --- SUM UP INFLOWS FROM INFLOWING STREAM REACHES.
Cdep   removed delta from computation of SURFIN
      DO 200 LK=1,NLAKES
         DO 200 ITRIB=1,NTRB
            INODE=ITRB(LK,ITRIB)
            IF (INODE.LE.0) GO TO 200
            SURFIN(LK)=SURFIN(LK)+STRIN(INODE)
200   CONTINUE
Cdep  Compute flow into lake for surface sources
      DO LAKE = 1,NLAKES
       IF(RNF(LAKE).GE.0.0) RUNF = RNF(LAKE)
       IF(RNF(LAKE).LT.0.0) RUNF =-RNF(LAKE)*PRCPLK(LAKE)*BGAREA(LAKE)
       FLWIN(LAKE) = PRCPLK(LAKE)*AREATABLE(1,LAKE)+SURFIN(LAKE)+RUNF
       IF(WTHDRW(LAKE).LE.0.0) FLWIN(LAKE) = FLWIN(LAKE) - WTHDRW(LAKE)
      END DO
Cdep  End do loop for flow into lake
C2B --- SUM UP OUTFLOWS FROM OUTFLOWING STREAM REACHES.
Cdep   revised theta for steady state. It is a time
Cdep   weighted variable that was used in steady state
Cdep   as a relaxation parameter.
         IF (ISS.NE.1) THEN
           THET1 = DBLE(THETA)
         ELSE
           THET1 = 1.0D0
         END IF
Cdep   Use outflowing streams from final MODFLOW iteration.
         DO 300 LK=1,NLAKES
Cdep         DO 300 IDV=1,NDV
Cdep            INODE=IDIV(LK,IDV)
Cdep            IF (INODE.LE.0) GO TO 300
Cdep            SURFOT(LK)=SURFOT(LK)+STROUT(INODE)*DELT
Cdep   SURFOT is a volumetric rate
          SURFOT(LK)=SURFOT(LK)
 300     CONTINUE
C2C ---- INITIALIZE SUMMATION PARAMETERS.
          DO 400 LK=1,NLAKES
             SUMCNN(LK) = ZERO
             SUMCHN(LK) = ZERO
             EVAP(LK)=ZERO
             PRECIP(LK)=ZERO
             SEEP(LK)=ZERO
             VOL(LK)=ZERO
400          SURFA(LK)=ZERO
        DO 425 NM=1,NLAKES
           GWIN(NM)=ZERO
425       GWOUT(NM)=ZERO
Cdep    Limit ET to surface flow into lake
         DO LL=1,NLAKES
           EVAPO(LL) = EVAPLK(LL)
           WITHDRW(LL) = WTHDRW(LL)    
           IF (STGNEW(LL).LE.BOTTMS(LL)) THEN
             IF(WTHDRW(LL).GT.0.0) THEN
               IF(WTHDRW(LL).GE.FLWIN(LL)) THEN
                 WITHDRW(LL) = FLWIN(LL)
                 EVAPO(LL) = 0.0
                 FLWIN(LL) = 0.0
               ELSE
                 FLWIN(LL) = FLWIN(LL)-WTHDRW(LL)
                 IF(EVAPO(LL)*AREATABLE(1,LL).GE.FLWIN(LL)) THEN
                   EVAPO(LL) = FLWIN(LL)/AREATABLE(1,LL)
                   FLWIN(LL) = 0.0
                 ELSE
                   FLWIN(LL) = FLWIN(LL)-EVAPO(LL)*AREATABLE(1,LL)
                 END IF
               END IF
             ELSE
               IF(EVAPO(LL)*AREATABLE(1,LL).GE.FLWIN(LL)) THEN
                 EVAPO(LL) = FLWIN(LL)/AREATABLE(1,LL)
                 FLWIN(LL) = 0.0
               ELSE
                 FLWIN(LL) = FLWIN(LL)-EVAPO(LL)*AREATABLE(1,LL)
               END IF
             END IF
           END IF
           IF(FLWIN(LL).GT.0) THEN
             GWRATELIM(LL) = FLWIN(LL)/AREATABLE(1,LL)
           ELSE
             GWRATELIM(LL) = 0.0
           END IF
         END DO
Cdep    End do loop for limiting ET to surface flow into lake
C
C   MASTER NODE LOOP -- COMPUTE LAKEBED SEEPAGE TERMS AND BUDGET TERMS
C
         IF (ILKCB.LT.0.AND.ICBCFL.NE.0) WRITE (IOUT,'(//)')
      DO 900 L=1,LKNODE
         IL=ILAKE(1,L)
         IR=ILAKE(2,L)
         IC=ILAKE(3,L)
         RATE=ZERO
C
C4------DETERMINE LAKE AND NODAL LAYER,ROW,COLUMN NUMBER.
         LAKE=ILAKE(4,L)
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
         ITYPE = (ILAKE(5,L)+1)/2
         IF(ITYPE.EQ.3) ITYPE=0
         AREA = DELR(IC)*DELC(IR)
         IF(IL.GT.1) BOTLK = BOTM(IC,IR,LBOTM(IL-1))
         BOTCL = BOTM(IC,IR,LBOTM(IL))
         IF(IL.EQ.NLAY.AND.CNDFCT(L).EQ.ZERO) BOTLK = BOTCL
         RAIN = PRCPLK(LAKE)
         EV = EVAPLK(LAKE)
C5------CONDUCTANCE FACTOR NEEDED FOR SEEPAGE CALCULATIONS.
Cdep Set FLOTOUZF TO ZERO for unsaturated flow.
         FLOTOUZF = ZERO         !rsr
         FLOBOT = ZERO
         CONDUC=CNDFCT(L)
         IF(CONDUC.EQ.ZERO) GO TO 550
         H=HNEW(IC,IR,IL)
Cdep      changed THETA to THET1
C         STGON = (1.0-THETA)*STGOLD(LAKE) + THETA*STGNEW(LAKE)
          STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
C
C6------COMPUTE SEEPAGE INTO OR OUT OF A LAKE BED NODE, AND
C       SEEPAGE INTO OR OUT OF A LAKE WALL NODE, WHEN ITYPE=1 OR 2.
         IF (ITYPE.NE.0) GO TO 515
            IF(IBOUND(IC,IR,IL).GT.0) GO TO 508
            DO 505 LI=IL,NLAY
            IF(IBOUND(IC,IR,LI).GT.0) GO TO 507
  505       CONTINUE
            WRITE(IOUT,506) L,IC,IR,IL
Cdep revised format statment
  506       FORMAT(1X,'ERROR - NO AQUIFER UNDER LAKE CELL ',I5,
     +             '; COLUMN ',I5,'; ROW ',I5,'; AND LAYER ',I5,
     +             '.'/,' PROGRAM STOPPING ')
            CALL USTOP(' ')
  507       H = BOTLK
Cdep  508      IF(STGON.LE.BOTLK.AND.H.LE.BOTLK) GO TO 550
  508      IF(STGNEW(LAKE).LE.BOTLK.AND.H.LE.BOTLK) GO TO 549
C           IF(H.GE.BOTLK) FLOBO2=CONDUC*(STGNEW(LAKE)-H)
C           IF(H.GE.BOTLK) FLOBO1=CONDUC*(STGOLD(LAKE)-H)
C           IF(H.LT.BOTLK) FLOBO2=CONDUC*(STGNEW(LAKE)-BOTLK)
C           IF(H.LT.BOTLK) FLOBO1=CONDUC*(STGOLD(LAKE)-BOTLK)
C           FLOBOT = THETA*FLOBO2 + (1.0-THETA)*FLOBO1
Cdep    revised if statements and changed THETA to THET1
           IF(H.GE.BOTLK)THEN
             FLOBO2=CONDUC*(STGNEW(LAKE)-H)
             FLOBO1=CONDUC*(STGOLD(LAKE)-H)
             FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
Crgn             FLOTOUZF = FLOBOT
           ELSE
             FLOBO2=CONDUC*(STGNEW(LAKE)-BOTLK)
             FLOBO1=CONDUC*(STGOLD(LAKE)-BOTLK)
             FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
             FLOTOUZF = FLOBOT
Cdep    restrict flow through bottom of cell to vertical saturated K
Crgn    remove option for simulating unsaturated flow beneath lakes
             IF(IUNITUZF.GT.0.AND.IUZFBND(IC,IR).GT.0)THEN
             IF (FLOBOT/AREA.GT.VKS(IC,IR))
     +          FLOBOT = VKS(IC,IR)*AREA
                FLOTOUZF = FLOBOT
                FLOBOT = 0.0D0
                CONDUC = FLOTOUZF/(STGNEW(LAKE)-BOTLK)
                FINF(IC,IR)=FLOBOT/AREA
             END IF
           END IF
Cdep    Correct ground water inflow when stgon at lake bottom
           IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO)THEN
             IF(FLOBOT.LT.0.0)FLWIN(LAKE)=FLWIN(LAKE)-FLOBOT
           END IF
           GO TO 535
  515     IF (ITYPE.NE.1.AND.ITYPE.NE.2) GO TO 550
          IF(IBOUND(IC,IR,IL).LE.0) GO TO 899
Cdep    Added head check to conditional if statement
          IF(STGON.LE.BOTCL.AND.H.LE.BOTLK) GO TO 550
          HD = H
          IF(H.GT.BOTM(IC,IR,LBOTM(IL)-1))
     1                      HD = BOTM(IC,IR,LBOTM(IL)-1)
          THCK = HD - BOTCL
          IF(THCK.LE.ZERO) THCK = ZERO
          CONDUC = CONDUC*THCK
          FLOBO2 = CONDUC*(STGNEW(LAKE)-H)
          FLOBO1 = CONDUC*(STGOLD(LAKE)-H)
C          FLOBOT = THETA*FLOBO2 + (1.0D0-THETA)*FLOBO1
Cdep       Changed THETA to THET1
          FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
Crgn          FLOTOUZF = FLOBOT
  535     SUMCNN(LAKE) = SUMCNN(LAKE) + CONDUC
          H1 = H
          IF(ITYPE.EQ.0.AND.BOTLK.GT.H) H1 = BOTLK
          SUMCHN(LAKE) = SUMCHN(LAKE) + CONDUC*H1
Cdep      limit flobot if botlk equal to overall lake bottom
          GO TO 550
  549     IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) THEN
            FLOBOT = GWRATELIM(LAKE)*AREA
            FLWIN(LAKE) = FLWIN(LAKE) - FLOBOT
          END IF
  550     CONTINUE
C
C7------COMPUTE EVAPORATION FOR EACH LAKE NODE AND ADD IT TO THE
C          ACCUMULATIVE EVAPORATION DISCHARGE.
          IF (ITYPE.EQ.0) THEN
             EP=EV*AREA
Cdep revised computation of evaporation on basis of ground-water discharge
             IF (STGON.LT.BOTLK.AND.H.LT.BOTLK) EP=ZERO
             EVAP(LAKE)=EVAP(LAKE)+EP*DELT
          ENDIF
C
C8-------COMPUTE RAINFALL RECHARGE FOR EACH LAKE NODE AND ADD IT TO
C         THE ACCUMULATIVE PRECIPITATION RECHARGE.
          IF (ITYPE.EQ.0) THEN
             R2 = RAIN*AREA
Cdep revised computation of precipitation on basis of ground-water discharge
             IF (STGON.LT.BOTLK.AND.H.LT.BOTLK) R2 = ZERO
             PRECIP(LAKE)=PRECIP(LAKE)+R2*DELT
          ENDIF
C
C9-------COMPUTE ACCUMULATIVE SEEPAGE THROUGH LAKEBED SEDIMENTS.
Cdep   Seep if a volumetric rate. Removed DELT from equation
          SEEP(LAKE)=SEEP(LAKE)-FLOBOT
C
C10------COMPUTE ACCUMULATIVE LAKE NODAL AREA, EXCLUDE WALL NODES.
Cdep    Revised computation of set STGNEW to BOTLK 
          IF (ITYPE.EQ.0.AND.STGON.GT.BOTLK) THEN
             SURFA(LAKE)=SURFA(LAKE)+AREA
             VOL(LAKE)=VOL(LAKE)+(STGON-BOTLK)*AREA
c          ELSE IF (ITYPE.EQ.0.AND.STGON.LE.BOTLK) THEN
c             VOL(LAKE)= 0.0
c             STGNEW(LAKE) = BOTLK
          ENDIF
Cdep  Added check to reset lake volume to zero if lake is dry
          IF (STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) THEN
            VOL(LAKE)=0.0
            SURFA(LAKE)=AREATABLE(1,LAKE)
          END IF
          RATE=FLOBOT
          
          IF (IUNITGWT.GT.0) FLOB(L)=FLOBOT
          IF (ILKCB.LT.0.AND.ICBCFL.NE.0) WRITE(IOUT,880)
     1          TEXT,KPER,KSTP,L,IL,IR,IC,RATE
880       FORMAT(1X,A,'   PERIOD',I3,'   STEP',I3,'   NODE',I4,
     1            '   LAYER',I3,'   ROW',I4,'   COL',I4,'   RATE',
     2            G15.7)
C
C------ ADD RATE TO BUFFER.
Crgn          BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+FLOTOUZF
              BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
C
C------ SEE IF FLOW IS INTO AQUIFER OR INTO LAKE.
Cdep            IF (RATE) 885,899,890
           IF(RATE.LT.0.0D0) THEN
C
C------ AQUIFER IS DISCHARGING TO LAKE SUBTRACT RATE FROM RATOUT.
Cdep 885         RATOUT=RATOUT-RATE
            RATOUT=RATOUT-RATE
            GWIN(LAKE)=GWIN(LAKE)-RATE           
Cdep         GO TO 899
           ELSE IF (RATE.GT.0.0D0) THEN
C
C------ AQUIFER IS RECHARGED FROM LAKE ADD RATE TO RATIN.
Cdep 890         RATIN=RATIN+RATE
            RATIN=RATIN+RATE
            GWOUT(LAKE)=GWOUT(LAKE)+RATE
           END IF
C------ IF SAVING COMPACT BUDGET, WRITE FLOW FOR ONE LAKE FACE
899      IF(IBD.EQ.2) THEN
            FACE(1)=ILAKE(5,L)
            R=RATE
            CALL UBDSVB(ILKCB,NCOL,NROW,IC,IR,IL,R,FACE(1),1,NAUX,
     1                 1,IBOUND,NLAY)
         END IF
900      CONTINUE
C
C11------ONLY COMPUTE LAKE LEVEL AFTER SCANNING THRU ALL NODES OF A LAKE.
C
Cdep    Adjust lake precipitation and evaporation for a flow limited lake
         DO LL = 1,NLAKES
           EVAPO(LL) = EVAPO(LL)*AREATABLE(1,LL)
           IF(STGNEW(LL)-BOTTMS(LL).LT.CLOSEZERO) THEN
             IF(EVAPO(LL).LT.EVAP(LL)/DELT) THEN
               IF(EVAP(LL).LT.FLWIN(LL)) THEN
                 EVAPO(LL)=EVAP(LL)/DELT
                 FLWIN(LL)=FLWIN(LL)-EVAPO(LL)
               ELSE
                 EVAPO(LL)=EVAPO(LL)+FLWIN(LL)
                 FLWIN(LL)= 0.0
               END IF
             END IF
             EVAP(LL)=EVAPO(LL)*DELT
             PRECIP(LL)=PRCPLK(LL)*AREATABLE(1,LL)*DELT
             STGNEW(LL) = FLWIN(LL)*DELT/AREATABLE(1,LL)+BOTTMS(LL)
             SURFA(LL) = AREATABLE(1,LL)
           END IF
         END DO
Cdep    End of adjustment of lake precipitation and evaporation.
      IF(ISS.LE.0) GO TO 905
      DO 903 LAKE=1,NLAKES
            DELH(LAKE)=STGNEW(LAKE)-STGOLD(LAKE)
            TDELH(LAKE)=STGNEW(LAKE)-STAGES(LAKE)
  903 CONTINUE
      GO TO 1350
  905     DO 1000 LAKE=1,NLAKES
           vol2 = 0.0
Cdep       STGON = (1.0-THETA)*STGOLD(LAKE) + THETA*STGNEW(LAKE)
Cdep       Changed THETA to THET1
         STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
Cdep   Changed WTHDRW(LAKE) TO WITHDRW(LAKE)
          WDRAW=WITHDRW(LAKE)*DELT
       IF(RNF(LAKE).GE.ZERO) RUNF = RNF(LAKE)
       IF(RNF(LAKE).LT.ZERO) RUNF =-RNF(LAKE)*PRCPLK(LAKE)*BGAREA(LAKE)
Cdep  Added runoff from Unsaturated Flow Package
       IF (IUNITUZF.GT.0) THEN
         RUNOFF = OVRLNDRNF(LAKE)
       ELSE
         RUNOFF = 0.0
       END IF
Cdep  Created RUNFD and added to STGNEW
         RUNFD = RUNF*DELT + RUNOFF*DELT
Cdep   Changed conditional if statement from volume to lake depth
       IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) GO TO 1110
Cdep       STGNEW(LAKE)=(STGOLD(LAKE)*SURFA(LAKE)+(PRECIP(LAKE)-EVAP(LAKE)
Cdep     1  -WDRAW+RUNFD+SURFIN(LAKE)-SURFOT(LAKE)-
Cdep     2 (1.0-THETA)*DELT*STGOLD(LAKE)*SUMCNN(LAKE)+
Cdep     3 DELT*SUMCHN(LAKE)))/(SURFA(LAKE)+THETA*DELT*SUMCNN(LAKE))
Cdep   Changed THETA to THET1
Cdep   Revised calculation of lake volume when lake goes dry
       IF(STGNEW(LAKE)-BOTTMS(LAKE).GT.CLOSEZERO) THEN
         STGNEW(LAKE)=(STGOLD(LAKE)*SURFA(LAKE)+(PRECIP(LAKE)-EVAP(LAKE)
     1                -WDRAW+RUNFD+SURFIN(LAKE)*DELT-SURFOT(LAKE)*DELT-
     2               (1.0D0-THET1)*DELT*STGOLD(LAKE)*SUMCNN(LAKE)+
     3               DELT*SUMCHN(LAKE)))/(SURFA(LAKE)+
     4               THET1*DELT*SUMCNN(LAKE))
         IF(STGOLD(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) THEN
           VOL(LAKE)=(STGNEW(LAKE)-BOTTMS(LAKE))*SURFA(LAKE)
           vol2 = STGNEW(LAKE)*SURFA(LAKE)
         ELSE
           STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
           VOL(LAKE)=VOL(LAKE) + (STGNEW(LAKE)-STGON)*SURFA(LAKE)
           vol2 = (STGNEW(LAKE)-STGOLD(LAKE))*SURFA(LAKE)
         END IF
       ELSE IF(STGNEW(LAKE)-BOTTMS(LAKE).LE.CLOSEZERO) THEN
         vol2 = VOL(LAKE)
         VOL(LAKE)=0.0
       ENDIF
Crgn  Compute lake budget terms for GSFLOW CSV file
      TOTGWIN_LAK = TOTGWIN_LAK + GWIN(LAKE)*DELT
      TOTGWOT_LAK = TOTGWOT_LAK - GWOUT(LAKE)*DELT
      TOTDELSTOR_LAK = TOTDELSTOR_LAK + vol2
      TOTSTOR_LAK = TOTSTOR_LAK + VOL(LAKE)
      TOTEVAP_LAK = TOTEVAP_LAK - EVAP(LAKE)
      TOTPPT_LAK = TOTPPT_LAK + PRECIP(LAKE)
      TOTRUNF_LAK = TOTRUNF_LAK + RUNFD
      TOTWTHDRW_LAK = TOTWTHDRW_LAK - WDRAW
      TOTSURFIN_LAK = TOTSURFIN_LAK + SURFIN(LAKE)*DELT
      TOTSURFOT_LAK = TOTSURFOT_LAK - SURFOT(LAKE)*DELT
C
Cdep  End of changes to lake volume when lake goes dry
      IF(VOL(LAKE).LE.ZERO) WRITE(IOUT,1114) LAKE
 1114 FORMAT(1X,'..........LAKE',I3,' HAS JUST GONE DRY..........')
            DELH(LAKE)=STGNEW(LAKE)-STGOLD(LAKE)
            TDELH(LAKE)=STGNEW(LAKE)-STAGES(LAKE)
      GO TO 1000
C
C------CHECK FOR CONDITION (AVERAGE HEAD IN UNDERLYING AQUIFER
C      GREATER THAN BOTTOM OF LAKE) FOR REWETTING A DRY LAKE
C
 1110 AVHD = ZERO
      BOTARE = ZERO
      WRITE(IOUT,1112) LAKE
 1112 FORMAT(1X,'..........LAKE',I3,' IS DRY..........')
      IF(NLAY.EQ.1) GO TO 1000
      DO 1115 L=1,LKNODE
      L1 = ILAKE(4,L)
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      ITYPE = (ILAKE(5,L)+1)/2
      IF(ITYPE.EQ.3) ITYPE=0
      IF(L1.NE.LAKE.OR.ITYPE.NE.0) GO TO 1115
      K = ILAKE(1,L)
      J = ILAKE(2,L)
      I = ILAKE(3,L)
      IF(K.EQ.NLAY.AND.IBOUND(I,J,K).EQ.0) GO TO 1000
      IF(K.EQ.1) GO TO 1115
      IF(BOTM(I,J,LBOTM(K-1)).GT.BOTTMS(LAKE)) GO TO 1115
c lfk  eliminate DO 1117 loop, ignore LAYHDT value below lake; and only
c           check layer below lake interface for IBOUND value:
c orig lines
c      DO 1117 K1=K,NLAY
c      IF(LAYHDT(K1).EQ.0) GO TO 1117
c      K2 = K1
c      IF(IBOUND(I,J,K1).LE.0) GO TO 1117
c      K2 = K
c      IF(IBOUND(I,J,K).LE.0) GO TO 1117
c      GO TO 1119
c 1117 CONTINUE
c      AVHD = AVHD + BOTM(I,J,LBOTM(K2))*DELR(I)*DELC(J)
c      GO TO 1121
c 1119 AVHD = AVHD + HNEW(I,J,K1)*DELR(I)*DELC(J)
c
c new lines
      IF(IBOUND(I,J,K).LE.0) THEN
        AVHD = AVHD + BOTM(I,J,LBOTM(K))*DELR(I)*DELC(J)
      ELSE
        AVHD = AVHD + HNEW(I,J,K)*DELR(I)*DELC(J)
      END IF
C
Cdep  1121 BOTARE = BOTARE + DELR(I)*DELC(J)
      BOTARE = BOTARE + DELR(I)*DELC(J)
 1115 CONTINUE
      IF(BOTARE.LE.ZERO) GO TO 1000
      AVHD = AVHD/BOTARE
      IF(AVHD.LT.BOTTMS(LAKE)) GO TO 1125
      WRITE(IOUT,1122)
 1122 FORMAT(/1X,'AQUIFER HEAD UNDERNEATH BOTTOM OF LAKE IS HIGHER THAN
     1LAKE BOTTOM ELEVATION')
Cdep  Revised to not reset lake to average ground-water level beneath lake.
 1120 STGNEW(LAKE) = BOTTMS(LAKE)
      SURFA(LAKE) = BOTARE
      VOL(LAKE) = (STGNEW(LAKE)-BOTTMS(LAKE))*SURFA(LAKE)
Cdep  revised program to allow lakes to rewet while keeping track of lake
Cdep   inflows and outflow
Cdep      WRITE(IOUT,1184) LAKE, STGNEW(LAKE)
Cdep 1184 FORMAT(/1X,'LAKE',I3,' HAS REWET.  SET STAGE OF LAKE TO',F10.2,
Cdep     1  2X,'FT'/)
      GO TO 1000
C
C-----CHECK FOR STREAM OR AUGMENTATION INFLOWS
C
 1125 TSTVOL = SURFIN(LAKE)
      IF(WDRAW.LT.ZERO) TSTVOL = TSTVOL - WDRAW
      IF(TSTVOL.LE.ZERO) GO TO 1000
      STGNEW(LAKE) = BOTTMS(LAKE)
      SURFA(LAKE) = BOTARE
      AVHD = BOTTMS(LAKE) + TSTVOL/BOTARE
Cdep  revised program to allow lakes to rewet while keeping track of lake
Cdep   inflows and outflow    
Cdep   WRITE(IOUT,1123)
Cdep 1123 FORMAT(/1X,'THERE ARE NON-ZERO SURFACE-WATER INFLUXES INTO THE DRY
Cdep     1 LAKE')
      GO TO 1120
C
 1000     CONTINUE
C
C-----ADJUST STAGES OF COALESCENT MULTIPLE-LAKE SYSTEMS
C
      KCNT = 0
      IF(NCLS.LE.0) GO TO 1350
      DO 1205 I=1,NCLS
      DO 1205 J=1,ICMX
 1205 JCLS(I,J) = 0
C
C   CHECK EACH LAKE SYSTEM (ICL) FOR CURRENT CONNECTIONS TO SUBLAKES
C
      DO 1300 ICL=1,NCLS
      DO 1206 K=1,NLAKES
      SVT(K) = ZERO
      NCNST(K) = 0
 1206 NCNT(K) = 0
C
C   ELIMINATE (JCLS=2) ALL LAKES THAT HAVE ALREADY HAD THEIR STAGES
C   ADJUSTED AS PART OF A CONNECTED SYSTEM OF LAKES AND SUBLAKES
C
      DO 1210 IC4=ICL,NCLS
      ICM4 = ICS(IC4)
      DO 1210 IC5=1,ICM4
      IF(JCLS(IC4,IC5).EQ.1) JCLS(IC4,IC5) = 2
 1210 CONTINUE
Cdep 1215 IF(JCLS(ICL,1).GE.2) GO TO 1300
      IF(JCLS(ICL,1).GE.2) GO TO 1300
C
C   TAG CENTER LAKE BY SETTING JCLS=1 AND THEN CHECK SUBLAKES FOR
C   CONNECTIONS.  IF CONNECTED, SET JCLS=1 AND NCNT=1.
C
      ICM = ICS(ICL)
      IS1 = ISUB(ICL,1)
      JCLS(ICL,1) = 1
      NCNT(IS1) = 1
      SVT(IS1) = -99999.
      DO 1220 J=2,ICM
      IS2 = ISUB(ICL,J)
      IF(IS2.LE.0) GO TO 1225
      IF(STGNEW(IS1).LE.SILLVT(ICL,J-1).AND.STGNEW(IS2).LE.
     1  SILLVT(ICL,J-1)) GO TO 1220
      JCLS(ICL,J) = 1
      NCNT(IS2) = 1
      SVT(IS2) = SILLVT(ICL,J-1)
 1220 CONTINUE
 1225 IF(ICL.EQ.NCLS) GO TO 1240
C
C   CHECK TO SEE IF CENTER LAKES OF REMAINING LAKE SYSTEMS ARE THE SAME
C   AS CONNECTED SUBLAKES OF THE PRESENT LAKE SYSTEM.  IF SO, CHECK
C   THEIR SUBLAKES FOR CONNECTIONS TO THE CENTER LAKES.  IF SUBLAKES
C   ARE ADDED TO THE SYSTEM, THEN CHECK REMAINING CENTER LAKES FOR AN
C   IDENTITY WITH THE NEWLY ADDED SUBLAKES.  ALL CONNECTED LAKES ARE
C   APPROPRIATELY TAGGED (JCLS=1 AND NCNT=1).
C
      ICL1 = ICL + 1
      DO 1230 LK=ICL1,NCLS
      IF(JCLS(LK,1).EQ.2) GO TO 1230
      LK3 = LK - 1
      DO 1227 LK1=1,LK3
      ICM2 = ICS(LK1)
      DO 1227 IC2=2,ICM2
      IF(ISUB(LK1,IC2).NE.ISUB(LK,1).OR.JCLS(LK1,IC2).NE.1) GO TO 1227
      JCLS(LK,1) = 1
      IS1 = ISUB(LK,1)
      NCNT(IS1) = 1
      ICM3 = ICS(LK)
      DO 1226 IC3=2,ICM3
      IS2 = ISUB(LK,IC3)
      IF(IS2.LE.0) GO TO 1227
      IF(STGNEW(IS1).LE.SILLVT(LK,IC3-1).AND.STGNEW(IS2).LE.
     1  SILLVT(LK,IC3-1)) GO TO 1226
      JCLS(LK,IC3) = 1
      NCNT(IS2) = 1
      SVT(IS2) = SILLVT(LK,IC3-1)
 1226 CONTINUE
 1227 CONTINUE
 1230 CONTINUE
C
C   COUNT NUMBER OF LAKES IDENTIFIED AS A CONNECTED PART OF THE PRESENT
C   LAKE SYSTEM, STORE LAKE NUMBERS IN KSUB, AND CUMULATE TOTAL SURFACE
C   AREA
C
 1240 ICNT = 0
      KCNT = KCNT + 1
      TOTARE = ZERO
      DO 1245 L=1,NLAKES
      IF(NCNT(L).NE.1) GO TO 1245
      ICNT = ICNT + 1
      TOTARE = TOTARE + SURFA(L)
      KSUB(ICNT) = L
      MSUB(ICNT,KCNT) = L
 1245 CONTINUE
      MSUB1(KCNT) = ICNT
      IF(ICNT.LE.1) KCNT = KCNT - 1
      IF(ICNT.LE.1) GO TO 1300
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1251
      WRITE(IOUT,1250) KSTP, ICNT, TOTARE
 1250 FORMAT(/1X,80('-')/1X,'TIME STEP ',I3,5X,'NUMBER OF CONNECTED LAKE
     1S IS',I3,5X,'TOTAL AREA = ',D16.9/)
 1251 CONTINUE
C
C   COMPUTE STAGE ADJUSTMENTS (STGADJ) REQUIRED FOR CONNECTED LAKES TO
C   HAVE THE SAME STAGE.
C
      DO 1270 I=1,ICNT
      L1 = KSUB(I)
      SUM = ZERO
      DO 1265 J=1,ICNT
      IF(J.EQ.I) GO TO 1265
      L = KSUB(J)
      SUM = SUM + SURFA(L)*(STGNEW(L)-STGNEW(L1))
 1265 CONTINUE
      STGADJ(L1) = SUM/TOTARE
 1270 CONTINUE
C
C   CHECK FOR NEWLY COMPUTED LAKE STAGES (STGNEW) LESS THAN SILL
C   ELEVATIONS OF THE LAKES (SVT)
C
 1272 ICNR = 0
      ICM3 = ICS(ICL)
      DO 1275 IC3=2,ICM3
      L = ISUB(ICL,IC3)
      IF(L.LE.0) GO TO 1275
      IF(NCNST(L).EQ.1) GO TO 1275
      IF(NCNT(L).EQ.0) GO TO 1275
      IF(STGNEW(L).LT.SVT(L)) GO TO 1275
      STGTST = STGNEW(L) + STGADJ(L)
      IF(STGTST.GE.SVT(L)) GO TO 1275
C
C   ADJUST STAGE TO SILL ELEVATION
C
      NCNST(L) = 1
      STGADJ(L) = SVT(L) - STGNEW(L)
      STGNEW(L) = SVT(L)
      FLXINL(L) = STGADJ(L)*SURFA(L)
      VOL(L) = VOL(L) + FLXINL(L)
      TOTARE = TOTARE - SURFA(L)
      NCNT(L) = 2
      ICNR = 1
      WRITE(IOUT,2238) L,L,STGADJ(L),STGNEW(L)
 1275 CONTINUE
      IF(ICL.EQ.NCLS) GO TO 1277
C
C   IF A LAKE STAGE IS ADJUSTED TO THE SILL ELEVATION, CHECK TO SEE
C   WHETHER THERE ARE SUBLAKES OF THIS LAKE AND ADJUST THEM TO THE SILL
C   ELEVATION UNLESS THE ORIGINAL STAGE IS ALREADY LOWER, IN WHICH CASE
C   THEY ARE NO LONGER CONNECTED.
C
      ICL1 = ICL + 1
      DO 2230 LK=ICL1,NCLS
      IS1 = ISUB(LK,1)
      IF(NCNT(IS1).EQ.0) GO TO 2230
      ICM1 = ICS(LK)
      DO 2225 IC2=2,ICM1
      IS2 = ISUB(LK,IC2)
      IF(NCNST(IS2).EQ.1) GO TO 2225
      IF(NCNT(IS2).EQ.0) GO TO 2225
      IF(STGNEW(IS2).LT.SVT(IS2)) GO TO 2225
      SVT1 = SVT(IS2)
      IF(SVT(IS1).GT.SVT1.AND.NCNT(IS1).EQ.2) SVT1 = SVT(IS1)
      STGTST = STGNEW(IS2) + STGADJ(IS2)
      IF(STGTST.GE.SVT1) GO TO 2225
      ICNR = 1
      NCNST(IS2) = 1
      NCNT(IS2) = 2
      STGADJ(IS2) = SVT1 - STGNEW(IS2)
      STGNEW(IS2) = SVT1
      FLXINL(IS2) = STGADJ(IS2)*SURFA(IS2)
      VOL(IS2) = VOL(IS2) + FLXINL(IS2)
      TOTARE = TOTARE - SURFA(IS2)
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 2225
      L11 = L
      IF(SVT(IS2).GT.SVT(L)) L11 = IS2
      WRITE(IOUT,2238) IS2,L11,STGADJ(IS2),STGNEW(IS2)
 2238 FORMAT(1X,'READJUST STAGE OF LAKE ',I3,' TO LAKE ',I3,
     1  ' SILL ELEVATION BY ',F5.2,' TO ',F7.2)
 2225 CONTINUE
 2230 CONTINUE
 1277 IF(ICNR.LE.0) GO TO 1280
C
C   RECOMPUTE STAGE ADJUSTMENTS CONSTRAINED NOT TO LOWER LAKES BELOW
C   SILL ELEVATIONS.
C
      ICNR1 = 0
      DO 1370 I=1,ICNT
      L1 = KSUB(I)
      IF(NCNT(L1).EQ.0) GO TO 1370
      IF(NCNST(L1).EQ.1) GO TO 1370
      SUM = ZERO
      DO 1365 J=1,ICNT
      IF(J.EQ.I) GO TO 1365
      L = KSUB(J)
      IF(NCNT(L).EQ.0) GO TO 1365
      IF(NCNST(L).EQ.0) SUM = SUM + SURFA(L)*(STGNEW(L)-STGNEW(L1))
      IF(NCNST(L).EQ.1) SUM = SUM - SURFA(L)*STGADJ(L)
 1365 CONTINUE
      STGADJ(L1) = SUM/TOTARE
      STGTST = STGNEW(L1) + STGADJ(L1)
      IF(STGTST.GE.SVT(L1)) GO TO 1370
      ICNR1 = 1
 1370 CONTINUE
      IF(ICNR1.NE.0) GO TO 1272
 1280 IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1281
      WRITE(IOUT,1286)
 1286 FORMAT(//11X,'SURFACE',7X,'SILL',3X,'WATER BUDGET',2X,'STAGE',2X,
     1 'CORRECTED',3X,'LAKE VOLUME'/2X,'LAKE',7X,'AREA',6X,'ELEVATION',
     2  3X,'STAGE',3X,'CORRECTION',2X,'STAGE',5X,'CORRECTION')
 1281 CONTINUE
      TVOLM = ZERO
      DO 1290 I=1,ICNT
      L = KSUB(I)
      STO = STGNEW(L)
      IF(NCNST(L).EQ.1) STO = STGNEW(L) - STGADJ(L)
      IF(NCNST(L).EQ.1) GO TO 1285
      STGNEW(L) = STGNEW(L) + STGADJ(L)
      FLXINL(L) = STGADJ(L)*SURFA(L)
      VOL(L) = VOL(L) + FLXINL(L)
 1327 FORMAT(/10X,'WARNING -- SUM OF INTERLAKE FLUXES ',F10.0,' EXCEEDS
     110**6 OF THE TOTAL VOLUME'/)
      WRITE(IOUT,1301)
 1301 FORMAT(1X,80('-')/)
 1285 TVOLM = TVOLM + VOL(L)
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1290
      WRITE(IOUT,1269) L,SURFA(L),SVT(L),STO,STGADJ(L),STGNEW(L),
     1  FLXINL(L)
 1269 FORMAT(1X,I3,1X,G15.5,4F10.2,G15.5)
 1290 CONTINUE
C
C   RECOMPUTE TIME STEP AND CUMULATIVE STAGE CHANGES FOR CONNECTED LAKES
C
      DO 1295 I=1,ICNT
      L = KSUB(I)
      DELH(L) = STGNEW(L) - STGOLD(L)
      TDELH(L) = STGNEW(L) - STAGES(L)
 1295 CONTINUE
 1300 CONTINUE
C
C   CHECK ON SUM OF CONNECTED-LAKE INTERCHANGE VOLUMES
C
      FLSUM = ZERO
      DO 1325 L=1,NLAKES
      FLSUM = FLSUM + FLXINL(L)
 1325 CONTINUE
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1350
      TV = TVOLM/1000000.
      IF(FLSUM.GE.TV) WRITE(IOUT,1327) FLSUM
 1350 CONTINUE
C
C   CHECK FOR LAKE CELLS GOING DRY
C
      LDR = 0
      DO 950 L=1,LKNODE
         IL=ILAKE(1,L)
         IR=ILAKE(2,L)
         IC=ILAKE(3,L)
         LAKE=ILAKE(4,L)
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
         ITYPE = (ILAKE(5,L)+1)/2
         IF(ITYPE.EQ.3) ITYPE=0
         IF(ITYPE.NE.0) GO TO 950
         IF(IBOUND(IC,IR,IL).GT.0) BOTLK = BOTM(IC,IR,LBOTM(IL-1))
         IF(IBOUND(IC,IR,IL).EQ.0) BOTLK = BOTM(IC,IR,LBOTM(IL))
Cdep  revised to set STGNEW to BOTTOM of LAKE when LESS than BOTLK.
Cdep         IF (STGNEW(LAKE).LE.BOTLK) LDR = LDR + 1
Cdep         IF (STGNEW(LAKE).LE.BOTLK) LDRY(LDR) = L
         IF (STGNEW(LAKE).LE.BOTLK) THEN
           LDR = LDR + 1
           LDRY(LDR) = L
Cdep           STGNEW(LAKE)=BOTLK
         END IF
  950 CONTINUE
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 951
      IF(LDR.EQ.0) WRITE(IOUT,875) LDR
  875 FORMAT(//1X,I5,2X,'LAKE CELLS ARE DRY.')
      IF(LDR.EQ.0) GO TO 951
      WRITE(IOUT,874) LDR
  874 FORMAT(/5X,'SECTIONS OF THE LAKE BOTTOM HAVE BECOME DRY.  THE DRY
     1SECTIONS'/5X,'LIE ABOVE THE FOLLOWING',I3,' AQUIFER CELLS (LAYER,R
     2OW,COLUMN):')
      LDR1 = 0
      DO 952 L=1,LDR
      LDR1 = LDR1 + 1
      L1 = LDRY(L)
      ILB(LDR1) = ILAKE(1,L1)
      IRB(LDR1) = ILAKE(2,L1)
      ICB(LDR1) = ILAKE(3,L1)
      IF(LDR1.LT.5) GO TO 952
      WRITE(IOUT,876) (ILB(I),IRB(I),ICB(I),I=1,5)
  876 FORMAT(5X,5('(',I3,',',I3,',',I3,')',2X))
      LDR1 = 0
  952 CONTINUE
      IF(LDR1.GT.0) WRITE(IOUT,876) (ILB(I),IRB(I),ICB(I),I=1,LDR1)
  951 CONTINUE
Cdep  Set Lake Stage to bottom of lake with lake is dry and set
Cdep       lake volume to zero.
      DO LAKE=1,NLAKES
        IF(STGNEW(LAKE).LE.BOTTMS(LAKE)) THEN
          STGNEW(LAKE) = BOTTMS(LAKE)
          VOL(LAKE) = 0.0
        END IF
      END DO
      IF(IUNITGWT.GT.0) GO TO 1086
Cdep      NDUM=1
Cdep  Buff was passed to GAGE package as a dummy array because 
C     transport is inactive and the variables were not defined.
      IF(IUNITGWT.LE.0 .AND. IUNITGAGE.GT.0) 
     *  CALL SGWF2GAG7LO(IUNITGWT,IUNITUZF,XLAKES,
     *  TOTIM,GWIN,GWOUT,FLXINL,VOLOLD,XLKOLD,XLAKINIT,NSOL)
 1086 IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1061
C
C   WRITE BUDGET SUMMARIES.
C
            WRITE(IOUT,1025) KPER, KSTP, DELT, PERTIM, TOTIM
 1025 FORMAT(/1X,'PERIOD ',I5,5X,'TIME STEP ',I5,5X,'TIME STEP LENGTH ',
     1   1PE11.4/1X,'PERIOD TIME ',E11.4,5X,'TOTAL SIMULATION TIME ',
     2   E11.4)
            WRITE(IOUT,1040)
 1040 FORMAT(//17X,'HYDROLOGIC BUDGET SUMMARIES FOR SIMULATED LAKES'
     1    ,/,5X,'(ALL FLUID FLUXES ARE VOLUMES ADDED TO THE LAKE DURING'
     2    ,' PRESENT TIME STEP)'
     3    ,/,5X,'------------------------------------------------------'
     4    ,'----------------------------------')
Cdep REVISED PRINT STATEMENT WHEN THERE IS RUNOFF FROM UZF PACKAGE
      IF (IUNITUZF.EQ.0) THEN
            WRITE(IOUT,1045)
 1045 FORMAT(1X,'LAKE',4X,'STAGE',7X,'PRECIP',9X,'EVAP',9X,'RUNOFF')
      ELSE 
            WRITE(IOUT,3045)
 3045 FORMAT(' LAKE     STAGE     PRECIP',7X,'EVAP',4X,' SPECIFIED ',
     +       'RUNOFF', 4X, 'COMPUTED RUNOFF', 4X, 'TOTAL RUNOFF')   
      END IF
 1061       DO 1100 NN=1,NLAKES
               PPTIN=PRECIP(NN)
               EOUT=EVAP(NN)
       IF(RNF(NN).GE.ZERO) RUNF = RNF(NN)
       IF(RNF(NN).LT.ZERO) RUNF =-RNF(NN)*PRCPLK(NN)*BGAREA(NN)
Cdep  Added runoff generated by Unsaturated Flow Package
       IF (IUNITUZF.GT.0) THEN
         RUNOFF = OVRLNDRNF(NN)
       ELSE
         RUNOFF = 0.0
       END IF
         RUNFD = RUNF*DELT
C
                  CUMPPT(NN)=CUMPPT(NN)+PPTIN
                  CUMEVP(NN)=CUMEVP(NN)+EOUT
                  CUMRNF(NN)=CUMRNF(NN)+RUNFD
Cdep  Added cumulative overland runoff from UZF.
        IF (IUNITUZF.GT.0) THEN
          CUMLNDRNF(NN) = CUMLNDRNF(NN) + RUNOFF*DELT
        END IF          
                  IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1100
                  IF(IUNITUZF.EQ.0) THEN
                  WRITE(IOUT,1050) NN,STGNEW(NN),PPTIN,EOUT,RUNFD
                  ELSE
                  WRITE(IOUT,3050) NN,STGNEW(NN),PPTIN,EOUT,RUNFD,
     1                             RUNOFF*DELT, RUNFD+(RUNOFF*DELT)
                  END IF
 1100       CONTINUE
 1050 FORMAT(1X,I3,4(1X,1PE13.6))
 3050 FORMAT(I4,1PE13.6,2(1X,1PE13.6),2X,E13.6,9X,E13.6,5X,E13.6)
       IF(LWRT.LE.0.AND.ICBCFL.GT.0) WRITE(IOUT,1046)
 1046 FORMAT(/12X,'GROUND WATER',12X,'SURFACE WATER'/1X,'LAKE',4X,
     1 'INFLOW',5X,'OUTFLOW',6X,'INFLOW',5X,'OUTFLOW')
            DO 1101 NN=1,NLAKES
               QIN=GWIN(NN)*DELT
               QOUT=GWOUT(NN)*DELT
Cdep  Multiplied SURFIN AND SURFOT by DELT to obtain QSIN and QSOUT
               QSIN=SURFIN(NN)*DELT
               QSOUT=SURFOT(NN)*DELT
C
                   CUMGWI(NN)=CUMGWI(NN)+QIN
                   CUMGWO(NN)=CUMGWO(NN)+QOUT
                   CUMSWI(NN)=CUMSWI(NN)+QSIN
                   CUMSWO(NN)=CUMSWO(NN)+QSOUT
 1101              IF(LWRT.LE.0.AND.ICBCFL.GT.0)
     1                 WRITE(IOUT,1051) NN,QIN,QOUT,QSIN,QSOUT
 1051 FORMAT(1X,I3,1P,4E12.4)
      IF(LWRT.LE.0.AND.ICBCFL.GT.0) WRITE(IOUT,1035)
 1035 FORMAT(/9X,'WATER',4X,'CONNECTED LAKE',3X,'UPDATED',5X,'TIME-STEP'
     1 ,10X,'STAGE CHANGE'/1X,'LAKE',5X,'USE',9X,'INFLUX',
     2  8X,'VOLUME',4X,'SURFACE AREA',3X,'TIME STEP',2X,'CUMULATIVE')
      DO 1105 NN=1,NLAKES
Cdep  Changed WTHDRW to WITHDRW
        WDRAW=WITHDRW(NN)*DELT
C
        CUMWDR(NN)=CUMWDR(NN)+WDRAW
        CUMFLX(NN)=CUMFLX(NN)+FLXINL(NN)
 1105 IF(LWRT.LE.0.AND.ICBCFL.GT.0)
     1       WRITE(IOUT,1055) NN,WDRAW,FLXINL(NN),VOL(NN),SURFA(NN),
     2           DELH(NN),TDELH(NN)
 1055 FORMAT(1X,I3,1P,3E13.4,1X,2E13.4,E12.4)
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1041
      WRITE(IOUT,1301)
C
C   WRITE CUMULATIVE BUDGET SUMMARIES.
C
            WRITE(IOUT,2040)
 2040 FORMAT(//12X,'CUMULATIVE HYDROLOGIC BUDGET SUMMARIES FOR SIMULATED
     1 LAKES'
     2    ,/,5X,'(ALL FLUID FLUXES ARE SUMS OF VOLUMES ADDED TO THE'
     3    ,' LAKE SINCE INITIAL TIME)'
     4    ,/,5X,'------------------------------------------------------'
     5    ,'---------------------')
Cdep  added computed runoff from UZF Package to lake budget
            IF(IUNITUZF.LE.0) THEN
            WRITE(IOUT,2045)
 2045 FORMAT(1X,'LAKE',7X,'PRECIP',7X,'EVAP',7X,'RUNOFF')
            ELSE
             WRITE(IOUT,4045)
 4045 FORMAT(1X,'LAKE',7X,'PRECIP',7X,'EVAP',5X,'SPECIFIED RUNOFF',3X,
     1       'COMPUTED RUNOFF',3X,'TOTAL RUNOFF')
            END IF           
            DO 2100 NN=1,NLAKES
            IF(IUNITUZF.LE.0) THEN
      WRITE(IOUT,2050) NN,CUMPPT(NN),CUMEVP(NN),CUMRNF(NN)
            ELSE
      WRITE(IOUT,4050) NN,CUMPPT(NN),CUMEVP(NN),CUMRNF(NN),
     1                 CUMLNDRNF(NN),CUMRNF(NN)+CUMLNDRNF(NN)
            END IF
 2100 CONTINUE
 2050 FORMAT(1X,I3,3X,1P,3E12.4)
 4050 FORMAT(1X,I3,3X,1P,2E12.4,3(5X,E12.4))
            WRITE(IOUT,2046)
 2046 FORMAT(/12X,'GROUND WATER',12X,'SURFACE WATER'/1X,'LAKE',4X,
     1 'INFLOW',5X,'OUTFLOW',6X,'INFLOW',5X,'OUTFLOW')
            DO 2101 NN=1,NLAKES
 2101 WRITE(IOUT,2051) NN,CUMGWI(NN),CUMGWO(NN),CUMSWI(NN),CUMSWO(NN)
 2051 FORMAT(1X,I3,1P,4E12.4)
      WRITE(IOUT,2035)
 2035 FORMAT(/9X,'WATER',4X,'CONNECTED LAKE'/
     1       1X,'LAKE',5X,'USE',9X,'INFLUX')
      DO 2105 NN=1,NLAKES
 2105 WRITE(IOUT,2055) NN,CUMWDR(NN),CUMFLX(NN)
 2055 FORMAT(1X,I3,1P,2E13.4)
      WRITE(IOUT,1301)
      IF(KCNT.LE.0) GO TO 11041
      IF (KCNT.GT.1) THEN
        WRITE(IOUT,11055) KCNT
11055 FORMAT(/1X,I3,' CONNECTED LAKE SETS'/)
        DO 11056 IIC=1,KCNT
          JIC = MSUB1(IIC)
          WRITE(IOUT,11057) JIC, (MSUB(LIC,IIC),LIC=1,JIC)
11057 FORMAT(1X,I3,' LAKES:  ',25I3)
11056   CONTINUE
      ELSE
        WRITE(IOUT,21055) KCNT
21055 FORMAT(/1X,I3,' CONNECTED LAKE SET'/)
        JIC = MSUB1(IIC)
        WRITE(IOUT,11057) JIC, (MSUB(LIC,IIC),LIC=1,JIC)
      END IF
11041 CONTINUE
 1041 CONTINUE
C
C13-----IF C-B-C TERMS WILL BE SAVED THEN WRITE TO DISK.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,ILKCB,BUFF,NCOL,NROW,
     1                         NLAY,IOUT)
C
C14A-----MOVE RATES INTO VBVL FOR PRINTING BY MODULE BAS OT.
1200  VBVL(3,MSUM)=RATIN
      VBVL(4,MSUM)=RATOUT
C
C14B-----MOVE PRODUCT OF RATE AND TIME STEP INTO VBVL ACCUMULATORS.
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT
C
C14C-----MOVE BUDGET TERM LABELS INTO VBVM FOR PRINTING BY BAS OT.
      VBNM(MSUM)=TEXT
C15------INCREASE BUDGET COUNTER.
      MSUM=MSUM+1
C
C        Substitute Lake Stage values for HNOFLO values at non-dry lake
C        cells in HNEW array; loop over all lake nodes.  If Lake Stage
C        is below bottom of lake cell, set HNEW = HNOFLO.
C
      DO 1900 L=1,LKNODE
         IL=ILAKE(1,L)
         ILL=IL-1
         IR=ILAKE(2,L)
         IC=ILAKE(3,L)
         LAKE=ILAKE(4,L)
         ITYPE = (ILAKE(5,L)+1)/2
         IF(ITYPE.EQ.3) ITYPE=0
         IF(ITYPE.NE.0) GO TO 1900
         IF(IBOUND(IC,IR,IL).GT.0) THEN
            BOTLK = BOTM(IC,IR,LBOTM(ILL))
            IF (STGNEW(LAKE).GT.BOTLK) HNEW(IC,IR,ILL)=STGNEW(LAKE)
            IF (ILL.GT.1) THEN
               ILL=ILL-1
               DO 1890 IL2=1,ILL
                 IF (STGNEW(LAKE).GT.BOTM(IC,IR,LBOTM(IL2))) THEN
                    HNEW(IC,IR,IL2)=STGNEW(LAKE)
                 ELSE
                   HNEW(IC,IR,IL2)=HNOFLO
                 END IF
1890           CONTINUE
            END IF
         ELSE IF(IBOUND(IC,IR,IL).EQ.0) THEN
            BOTLK = BOTM(IC,IR,LBOTM(IL))
            IF (STGNEW(LAKE).GT.BOTLK) HNEW(IC,IR,IL)=STGNEW(LAKE)
            IF (IL.GT.1) THEN
               ILL=IL-1
               DO 1892 IL2=1,ILL
                 IF (STGNEW(LAKE).GT.BOTM(IC,IR,LBOTM(IL2))) THEN
                    HNEW(IC,IR,IL2)=STGNEW(LAKE)
                 ELSE
                   HNEW(IC,IR,IL2)=HNOFLO
                 END IF
1892           CONTINUE
            END IF
         END IF
C
1900  CONTINUE
C
C12-----RETURN.
      RETURN
      END
C
      SUBROUTINE SGWF2LAK7SFR7RPS()
C
C    *******************************************************************
C--  IF STREAMS EXIST, DEFINE CONNECTIONS BETWEEN LAKES AND STREAMS
C    *******************************************************************
C
C    -------------------------------------------------------------------
C        SPECIFICATIONS:
C    -------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: NLAKES, NTRB, NDV, ITRB, IDIV, IRK
      USE GLOBAL,       ONLY: IOUT, NODES
      USE GWFSFRMODULE, ONLY: NSS, IDIVAR, IOTSG, SEG,  ISEG
C
C-- DOUBLE CHECK SIZE OF IRK (STORED IN BUFF) vs. NLAKES
C
      IF ((NLAKES*2).GT.NODES) THEN
         WRITE (IOUT,*) '***NLAKES too large for BUFF in Subroutine GWF2
     1LAK7SFR7RPS***  STOP EXECUTION'
         CALL USTOP(' ')
      END IF
C
C-- INITIALIZE ARRAYS
C
c     DO 50 I=1,NSS
c     DO 50 LK=1,NLAKES
c     ITRB(LK,I) = 0
c  50 IDIV(LK,I) = 0
      DO 55 LK=1,NLAKES
      IRK(1,LK) = 0
   55 IRK(2,LK) = 0
      NTRB = 0
      NDV = 0
C
C-- Build arrays to define lake tributary & diversion links ...
C        based on stream package input data
C
C---  Stream Inflow to Lakes
      DO 100 LSEG=1,NSS
      IF(IOTSG(LSEG).LT.0) THEN
        LAKE = -IOTSG(LSEG)
        IRK(1,LAKE) = IRK(1,LAKE) + 1
        K1 = IRK(1,LAKE)
        ITRB(LAKE,K1) = LSEG
        IF(IRK(1,LAKE).GT.NTRB) NTRB = IRK(1,LAKE)
      ENDIF
C
C---  Stream Outflow from Lakes
      IF(IDIVAR(1,LSEG).LT.0) THEN
        LAKE = -IDIVAR(1,LSEG)
        IRK(2,LAKE) = IRK(2,LAKE) + 1
        K1 = IRK(2,LAKE)
        IDIV(LAKE,K1) = LSEG
        IF(IRK(2,LAKE).GT.NDV) NDV = IRK(2,LAKE)
      ENDIF
  100 CONTINUE
C
C--  PRINT LAKE INFLOW STREAM SEGMENTS.
      WRITE(IOUT,10)
10    FORMAT(6X,'LAKE ',4X,'INFLOWING STREAM SEGMENT')
      DO 520 IK=1,NLAKES
      DO 519 JK=1,NSS
      IF(ITRB(IK,JK).LE.0) GO TO 521
  519 CONTINUE
  521 JK1 = JK - 1
      IF(JK1.GT.0) WRITE(IOUT,15) IK,(ITRB(IK,JK),JK=1,JK1)
15    FORMAT(5X,I5,14X,100I5)
  520 CONTINUE
      WRITE(IOUT,103) NTRB
103    FORMAT(/1X,'MAXIMUM NUMBER OF STREAMS INFLOWING TO A',
     1    ' LAKE IS',I5/)
C
C--  PRINT LAKE STREAM OUTFLOW SEGMENT (FROM A LAKE) NUMBERS.
C
      WRITE(IOUT,13)
13    FORMAT(6X,'LAKE ',4X,'OUTFLOWING STREAM',' SEGMENT')
      DO 600 IK=1,NLAKES
      DO 523 JK=1,NSS
      IF(IDIV(IK,JK).LE.0) GO TO 527
  523 CONTINUE
  527 JK1 = JK - 1
      IF(JK1.GT.0) WRITE(IOUT,15) IK,(IDIV(IK,JK),JK=1,JK1)
  600 CONTINUE
C
Cdep-- PRINT WARNING IF OUTFLOWING STREAM IS ASSIGNED ICALC =0.
Cdep    ADDED OCTOBER 15, 2004; DAVID PRUDIC
      DO ls = 1, NSS
        IF (IDIVAR(1,ls).LT.0) THEN
          lk = -IDIVAR(1,ls)
          IF (ISEG(1,ls).LE.0 .AND. SEG(2,ls).LE.0.0) THEN
            WRITE (IOUT, 9007) ls, lk, ISEG(1,ls), SEG(2,ls)
          END IF
        END IF
      END DO      
      WRITE(IOUT,133) NDV
133   FORMAT(/1X,'MAXIMUM NUMBER OF STREAMS OUTFLOWING',
     1    ' FROM A LAKE IS',I5/)
 9007 FORMAT(/, ' WARNING****  OUTFLOWING STREAM SEGMENT', I6,
     +       ' FROM LAKE', I6, ' HAS AN ICALC VALUE OF', I6,
     +       ' AND FLOW INTO THE SEGMENT IS', E12.4, /,
     +       ' NO OUTFLOW FROM THE LAKE INTO ',
     +       'SEGMENT WILL BE SIMULATED', /,
     +       ' SUGGEST CHANGING ICALC TO ANOTHER OPTION')
C
C-- RETURN
      RETURN
      END
      SUBROUTINE SGWF2LAK7BCF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN BCF PACKAGE IS USED
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, BEDLAK, LKARR1, ILAKE, CNDFCT
      USE GLOBAL,       ONLY: NLAY, IOUT, DELR, DELC, LAYHDT
      USE GWFBCFMODULE, ONLY: IWDFLG, HY, CVWD, TRPY
C
      WRITE(IOUT,108)
  108 FORMAT(//9X,'C',15X,'INTERFACE CONDUCTANCES BETWEEN LAKE AND ',
     1  'AQUIFER CELLS'/
     2  3X,'L',5X,'O',10X,'(IF TYPE = 6, CONDUCTANCE (L^2/T) IS ',
     3  'BETWEEN AQUIFER CELL AND OVERLYING LAKE CELL.)',/
     4  3X,'A',5X,'L',2X,'L',2X,'T',
     5  4X,'(IF TYPE = 1 TO 4, CONDUCTANCES ARE PER UNIT SATURATED ',
     6  'THICKNESS (L/T).)'/
     7  3X,'Y',2X,'R',2X,'U',2X,'A',2X,'Y'/
     8  3X,'E',2X,'O',2X,'M',2X,'K',2X,'P',
     9  24X,'LAKEBED',6X,'C O N D U C T A N C E S'/3X,'R',2X,'W',2X,
     1  'N',2X,'E',
     2  2X,'E',5X,'DELTA Y',3X,'DELTA X',2X,'LEAKANCE',3X,'LAKEBED',3X,
     3  'AQUIFER',2X,'COMBINED'/1X,79('_'))
C
      IWRN = 0
      IWRN1 = 0
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
      CNDFCT(II) = 0.0
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      NTYP = NTYP + 1
      IF(NTYP.EQ.1) THEN
C
C  Vertical Conductance
C    for vertical interface, "K" is layer below bottom of lake
C
        CNDFC1=0.0
        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) GO TO 315
        IF(BEDLAK(II).LE.0.0) GO TO 315
          IWRN1 = 1
        CNDFC1 = BEDLAK(II)*DELR(I)*DELC(J)
        IF (IWDFLG.EQ.0) THEN
          CNDFCT(II) = CNDFC1
        ELSE
          IF(CVWD(I,J,K-1).LE.0.0.OR.CNDFC1.LE.0.0) GO TO 315
          CNDFCT(II) = 1.0/(0.5/CVWD(I,J,K-1)+1.0/CNDFC1)
        END IF
  315   IF (IWDFLG.EQ.0) THEN
          WRITE(IOUT,7324) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1        BEDLAK(II),CNDFC1,CNDFCT(II)
 7324     FORMAT(1X,5I3,2X,1P,4E10.2,10X,E10.2)
        ELSE
          CVWD2= 2.0*CVWD(I,J,K-1)
          WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1        BEDLAK(II),CNDFC1,CVWD2,CNDFCT(II)
 7325     FORMAT(1X,5I3,2X,1P,6E10.2)
        END IF
      ELSE
C
C  Horizontal conductance
C
C  HY not read in, thus unavailable.
C
Cdep  348   IF(LAYHDT(K).EQ.0) THEN
        IF(LAYHDT(K).EQ.0) THEN
          IF(NTYP.EQ.2) CNDFCT(II) = BEDLAK(II)*DELC(J)
          IF(NTYP.EQ.3) CNDFCT(II) = BEDLAK(II)*DELR(I)
          WRITE(IOUT,7324) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1        BEDLAK(II),CNDFCT(II),CNDFCT(II)
          IWRN = 1
        ELSE
C
C  HY read in, thus available.
C
        TT = HY(I,J,K)
        IF(NTYP.EQ.2) CNDFC2 = 2.0*TT*DELC(J)/DELR(I)
        IF(NTYP.EQ.3) CNDFC2 = 2.0*TRPY(K)*TT*DELR(I)/DELC(J)
        IF(NTYP.EQ.2) CNDFC1 = BEDLAK(II)*DELC(J)
        IF(NTYP.EQ.3) CNDFC1 = BEDLAK(II)*DELR(I)
        IF (CNDFC1.GT.0.0.AND.CNDFC2.GT.0.0) 
     *         CNDFCT(II) = 1.0/(1.0/CNDFC2+1.0/CNDFC1)
        WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1    BEDLAK(II),CNDFC1,CNDFC2,CNDFCT(II)
        END IF
      END IF
  350 CONTINUE
C
C  WRITE WARNINGS ON LAKE/AQUIFER CONDUCTANCES, IF NECESSARY
          IF(IWRN.EQ.1.OR.IWRN1.EQ.1) WRITE(IOUT,345)
  345     FORMAT(//5X,'NOTE: INFORMATION ABOUT CALCULATED LAKE/AQUIFER C
     1ONDUCTANCES WHEN USING BCF PACKAGE FOLLOWS: '/)
          IF(IWRN.EQ.1) WRITE(IOUT,346)
  346     FORMAT(1X,'NODE(S) ADJACENT TO LAKE IN CONFINED LAYER:'/
     1    1X,'LAKE/AQUIFER CONDUCTANCES BASED SOLELY ON LAKEBED SPECIFIC
     2ATION'/)
          IF(IWRN1.EQ.1) WRITE(IOUT,347)
  347     FORMAT(1X,'IF WETDRY FLAG NOT TURNED ON, VERTICAL LEAKANCES AR
     1E NOT SAVED:'/1X,'THEREFORE, LAKE/AQUIFER CONDUCTANCES ARE BASED S
     2OLELY ON LAKEBED SPECIFICATION'/)
          IF(IWRN.EQ.1.OR.IWRN1.EQ.1) WRITE(IOUT,'(//)')
C
      RETURN
      END
C
      SUBROUTINE SGWF2LAK7LPF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN LPF PACKAGE IS USED
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, BEDLAK, LKARR1, ILAKE, CNDFCT
      USE GLOBAL,       ONLY: NLAY, IOUT, LBOTM, LAYCBD, DELR, DELC,
     +                        BOTM
      USE GWFLPFMODULE, ONLY: CHANI, LAYVKA, VKA, VKCB, HANI, HK
C
      WRITE(IOUT,108)
  108 FORMAT(//9X,'C',15X,'INTERFACE CONDUCTANCES BETWEEN LAKE AND ',
     1  'AQUIFER CELLS'/
     2  3X,'L',5X,'O',10X,'(IF TYPE = 6, CONDUCTANCE (L^2/T) IS ',
     3  'BETWEEN AQUIFER CELL AND OVERLYING LAKE CELL.)',/
     4  3X,'A',5X,'L',2X,'L',2X,'T',
     5  4X,'(IF TYPE = 1 TO 4, CONDUCTANCES ARE PER UNIT SATURATED ',
     6  'THICKNESS (L/T).)'/
     7  3X,'Y',2X,'R',2X,'U',2X,'A',2X,'Y'/
     8  3X,'E',2X,'O',2X,'M',2X,'K',2X,'P',
     9  24X,'LAKEBED',6X,'C O N D U C T A N C E S'/3X,'R',2X,'W',2X,
     1  'N',2X,'E',
     2  2X,'E',5X,'DELTA Y',3X,'DELTA X',2X,'LEAKANCE',3X,'LAKEBED',3X,
     3  'AQUIFER',2X,'COMBINED'/1X,79('_'))
C
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
      CAQ = 0.0
      CNDFCT(II) = 0.0
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      NTYP=NTYP + 1
      IF(NTYP.EQ.1) THEN
C
C  Vertical Conductance
C    for vertical interface, "K" is layer below bottom of lake
        CNDFC1=0.0
        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) GO TO 315
        IF(BEDLAK(II).LE.0.0) GO TO 315
        CNDFC1 = BEDLAK(II)*DELR(I)*DELC(J)
        IF(LAYVKA(K).EQ.0) THEN
           VK=VKA(I,J,K)
        ELSE
           VK=HK(I,J,K)/VKA(I,J,K)
        END IF
c   skip if zero vk
        IF(VK.LE.0.0) GO TO 350
        BBOT=BOTM(I,J,LBOTM(K))
        TTOP=BOTM(I,J,LBOTM(K)-1)
        CAQ=VK*DELR(I)*DELC(J)/((TTOP-BBOT)*0.5)
        IF(LAYCBD(K-1).GT.0) THEN
c   skip if zero vkcb
          IF(VKCB(I,J,LAYCBD(K)).LE.0.0) GO TO 350
          BBOT=BOTM(I,J,LBOTM(K)-1)
          TTOP=BOTM(I,J,LBOTM(K-1))
          CCB=VKCB(I,J,LAYCBD(K-1))*DELR(I)*DELC(J)/(TTOP-BBOT)
          !include VKCB
          CAQ = 1.0/(1.0/CAQ + 1.0/CCB)
        END IF
        CNDFCT(II) = 1.0/(1.0/CAQ+1.0/CNDFC1)
  315   WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1             BEDLAK(II),CNDFC1,CAQ,CNDFCT(II)
      ELSE
C
C  Horizontal conductance
C
        TT = HK(I,J,K)
C X-DIRECTION
        IF(NTYP.EQ.2) CNDFC2 = 2.0*TT*DELC(J)/DELR(I)
C Y-DIRECTION
        IF(NTYP.EQ.3) THEN
          IF(CHANI(K).LE.0) THEN
            KH=-CHANI(K)
            CNDFC2 = 2.0*HANI(I,J,KH)*TT*DELR(I)/DELC(J)
          ELSE
            CNDFC2 = 2.0*CHANI(K)*TT*DELR(I)/DELC(J)
          END IF
        END IF
        IF(NTYP.EQ.2) CNDFC1 = BEDLAK(II)*DELC(J)
        IF(NTYP.EQ.3) CNDFC1 = BEDLAK(II)*DELR(I)
        IF (CNDFC1.GT.0.0.AND.CNDFC2.GT.0.0) 
     *         CNDFCT(II) = 1.0/(1.0/CNDFC2+1.0/CNDFC1)
        WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1    BEDLAK(II),CNDFC1,CNDFC2,CNDFCT(II)
 7325   FORMAT(1X,5I3,2X,1P,6E10.2)
      END IF
  350 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SGWF2LAK7HUF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN HUF PACKAGE IS USED
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, BEDLAK, LKARR1, ILAKE, CNDFCT
      USE GLOBAL,       ONLY: NLAY, IOUT, LBOTM, DELR, DELC, BOTM
      USE GWFLPFMODULE, ONLY: VKA, HK
!gsf  USE GWFHUFMODULE, ONLY: HKCC
C
      WRITE(IOUT,108)
  108 FORMAT(//9X,'C',15X,'INTERFACE CONDUCTANCES BETWEEN LAKE AND ',
     1  'AQUIFER CELLS'/
     2  3X,'L',5X,'O',10X,'(IF TYPE = 6, CONDUCTANCE (L^2/T) IS ',
     3  'BETWEEN AQUIFER CELL AND OVERLYING LAKE CELL.)',/
     4  3X,'A',5X,'L',2X,'L',2X,'T',
     5  4X,'(IF TYPE = 1 TO 4, CONDUCTANCES ARE PER UNIT SATURATED ',
     6  'THICKNESS (L/T).)'/
     7  3X,'Y',2X,'R',2X,'U',2X,'A',2X,'Y'/
     8  3X,'E',2X,'O',2X,'M',2X,'K',2X,'P',
     9  24X,'LAKEBED',6X,'C O N D U C T A N C E S'/3X,'R',2X,'W',2X,
     1  'N',2X,'E',
     2  2X,'E',5X,'DELTA Y',3X,'DELTA X',2X,'LEAKANCE',3X,'LAKEBED',3X,
     3  'AQUIFER',2X,'COMBINED'/1X,79('_'))
C
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
      CAQ = 0.0
      CNDFCT(II) = 0.0
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      NTYP=NTYP + 1
      IF(NTYP.EQ.1) THEN
C
C  Vertical Conductance
C    for vertical interface, "K" is layer below bottom of lake
        CNDFC1=0.0
        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) GO TO 315
        IF(BEDLAK(II).LE.0.0) GO TO 315
        CNDFC1 = BEDLAK(II)*DELR(I)*DELC(J)
        VK=VKA(I,J,K)
c   skip if zero vk
        IF(VK.LE.0.0) GO TO 350
        BBOT=BOTM(I,J,LBOTM(K))
        TTOP=BOTM(I,J,LBOTM(K)-1)
        CAQ=VK*DELR(I)*DELC(J)/((TTOP-BBOT)*0.5)
        CNDFCT(II) = 1.0/(1.0/CAQ+1.0/CNDFC1)
  315   WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1             BEDLAK(II),CNDFC1,CAQ,CNDFCT(II)
      ELSE
C
C  Horizontal conductance
C
        TT = HK(I,J,K)
!gsf    TY = HKCC(I,J,K)
        TY = 0.0     !gsf
C X-DIRECTION
        IF(NTYP.EQ.2) CNDFC2 = 2.0*TT*DELC(J)/DELR(I)
C Y-DIRECTION
        IF(NTYP.EQ.3) CNDFC2 = 2.0*TY*DELC(J)/DELR(I)
        IF(NTYP.EQ.2) CNDFC1 = BEDLAK(II)*DELC(J)
        IF(NTYP.EQ.3) CNDFC1 = BEDLAK(II)*DELR(I)
        IF (CNDFC1.GT.0.0.AND.CNDFC2.GT.0.0) 
     *         CNDFCT(II) = 1.0/(1.0/CNDFC2+1.0/CNDFC1)
        WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1    BEDLAK(II),CNDFC1,CNDFC2,CNDFCT(II)
 7325   FORMAT(1X,5I3,2X,1P,6E10.2)
      END IF
  350 CONTINUE
C
      RETURN
      END
Cdep  Added function statements to compute derivatives for Newton method
Cdep     used in solving lake stage in the FORMULATE SUBROUTINE (LAK7FM).      
      FUNCTION FINTERP (STAGE,LN)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CACULATE LAKE AREA.
C         ADDED 5/15/2006
      USE GWFLAKMODULE, ONLY: AREATABLE, DEPTHTABLE
      DOUBLE PRECISION STAGE
      TOLF2=1.0E-4
      IF (STAGE.GT.DEPTHTABLE(151,LN))THEN
        FINTERP =  AREATABLE(151,LN) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(STAGE-DEPTHTABLE(I,LN))     
        IF (FOLD .LE. TOLF2) THEN  
          AREA=AREATABLE(I,LN)
          IFLG = 1
        ELSEIF (STAGE.GT.DEPTHTABLE(I,LN) .AND. STAGE.LT.
     1          DEPTHTABLE(I+1,LN))THEN
          AREA=((AREATABLE(I+1,LN)-AREATABLE(I,LN))/
     1         (DEPTHTABLE(I+1,LN)- DEPTHTABLE(I,LN)))*
     2         STAGE+AREATABLE(I+1,LN)-((AREATABLE(I+1,LN)-
     3         AREATABLE(I,LN))/(DEPTHTABLE(I+1,LN)-
     4         DEPTHTABLE(I,LN)))*DEPTHTABLE(I+1,LN)                 
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.150 ) IFLG = 1 
      END DO
      FINTERP = AREA
      RETURN
      END FUNCTION FINTERP
C------FUNCTION DERIVTERP FOR INTERPOLATING DERIVATIVE OF LAKE OUTFLOW. 
      FUNCTION DERIVTERP (STAGE,LSEG)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CACULATE LAKE OUTFLOW DERIVATIVE.
C         ADDED 5/16/2006
      USE GWFSFRMODULE, ONLY: DLKOTFLW, DLKSTAGE
      DOUBLE PRECISION STAGE, DEROTFLW
      TOLF2=1.0E-4
      IF (STAGE.GT.DLKSTAGE(200,LSEG))THEN
        DERIVTERP =  DLKOTFLW(200,LSEG)
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(STAGE-DLKSTAGE(I,LSEG))     
        IF (FOLD .LE. TOLF2) THEN  
          DEROTFLW=DLKOTFLW(I,LSEG)
          ISFLG = 1
        ELSEIF (STAGE.LT.DLKSTAGE(1,LSEG)) THEN
          DEROTFLW=0.0D0
          IFLG = 1
        ELSEIF (STAGE.GT.DLKSTAGE(I,LSEG) .AND. STAGE.LT.
     1          DLKSTAGE(I+1,LSEG))THEN
          DEROTFLW=((DLKOTFLW(I+1,LSEG)-DLKOTFLW(I,LSEG))/
     1         (DLKSTAGE(I+1,LSEG)- DLKSTAGE(I,LSEG)))*
     2         STAGE+DLKOTFLW(I+1,LSEG)-((DLKOTFLW(I+1,LSEG)-
     3         DLKOTFLW(I,LSEG))/(DLKSTAGE(I+1,LSEG)-
     4         DLKSTAGE(I,LSEG)))*DLKSTAGE(I+1,LSEG)
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.199) IFLG = 1
      END DO
      DERIVTERP = DEROTFLW
      RETURN
      END FUNCTION DERIVTERP 
C------FUNCTION OUTFLWTERP FOR INTERPOLATING DERIVATIVE OF LAKE OUTFLOW. 
      FUNCTION OUTFLWTERP (STAGE,LSEG)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE OUTFLOW STORED IN SLKOTFLW ARRAY.
C         ADDED 5/16/2006
      USE GWFSFRMODULE, ONLY: SLKOTFLW, DLKSTAGE
      DOUBLE PRECISION STAGE, OUTFLOW
      TOLF2=1.0E-4
      IF (STAGE.GT.DLKSTAGE(200,LSEG))THEN
        OUTFLWTERP =  SLKOTFLW(200,LSEG) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(STAGE-DLKSTAGE(I,LSEG))     
        IF (FOLD .LE. TOLF2) THEN
          OUTFLOW=SLKOTFLW(I,LSEG)
          IFLG = 1
        ELSEIF (STAGE.LT.DLKSTAGE(1,LSEG)) THEN
          OUTFLOW=0.0D0
          IFLG = 1
        ELSEIF (STAGE.GT.DLKSTAGE(I,LSEG) .AND. STAGE.LT.
     1          DLKSTAGE(I+1,LSEG))THEN
          OUTFLOW=((SLKOTFLW(I+1,LSEG)-SLKOTFLW(I,LSEG))/
     1         (DLKSTAGE(I+1,LSEG)- DLKSTAGE(I,LSEG)))*
     2         STAGE+SLKOTFLW(I+1,LSEG)-((SLKOTFLW(I+1,LSEG)-
     3         SLKOTFLW(I,LSEG))/(DLKSTAGE(I+1,LSEG)-
     4         DLKSTAGE(I,LSEG)))*DLKSTAGE(I+1,LSEG)
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.199) IFLG = 1
      END DO
      OUTFLWTERP = OUTFLOW
      RETURN
      END FUNCTION OUTFLWTERP       
      SUBROUTINE GWF2LAK7DA(IUNITLAK, IGRID)
Cdep  End of FUNCTIONS used for Newton method in 
Cdep     FORMULATE SUBROUTINE (LAK7FM).
C  Deallocate LAK data  
      USE GWFLAKMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: IUNITLAK, IGRID
C
      DEALLOCATE (GWFLAKDAT(IGRID)%NLAKES)
      DEALLOCATE (GWFLAKDAT(IGRID)%NLAKESAR)
      DEALLOCATE (GWFLAKDAT(IGRID)%THETA)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGNEW)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGOLD)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGITER)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOL)
      IF ( IUNITLAK.LT.1 ) RETURN

      DEALLOCATE (GWFLAKDAT(IGRID)%ILKCB)
      DEALLOCATE (GWFLAKDAT(IGRID)%NSSITR)
      DEALLOCATE (GWFLAKDAT(IGRID)%MXLKND)
      DEALLOCATE (GWFLAKDAT(IGRID)%LKNODE)
      DEALLOCATE (GWFLAKDAT(IGRID)%ICMX)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCLS)
      DEALLOCATE (GWFLAKDAT(IGRID)%LWRT)
      DEALLOCATE (GWFLAKDAT(IGRID)%NDV)
      DEALLOCATE (GWFLAKDAT(IGRID)%NTRB)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSCNCR)
      DEALLOCATE (GWFLAKDAT(IGRID)%ICS)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNCVR)
      DEALLOCATE (GWFLAKDAT(IGRID)%LIMERR)
      DEALLOCATE (GWFLAKDAT(IGRID)%ILAKE)
      DEALLOCATE (GWFLAKDAT(IGRID)%ITRB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IDIV)
      DEALLOCATE (GWFLAKDAT(IGRID)%ISUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IRK)
      DEALLOCATE (GWFLAKDAT(IGRID)%LKARR1)
      DEALLOCATE (GWFLAKDAT(IGRID)%STAGES)
      DEALLOCATE (GWFLAKDAT(IGRID)%FLOB)
      DEALLOCATE (GWFLAKDAT(IGRID)%DSRFOT)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRCPLK)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAPLK)
      DEALLOCATE (GWFLAKDAT(IGRID)%BEDLAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%WTHDRW)
      DEALLOCATE (GWFLAKDAT(IGRID)%RNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMRNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMPPT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMEVP)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMGWI)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMGWO)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMSWI)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMSWO)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMWDR)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMFLX)
      DEALLOCATE (GWFLAKDAT(IGRID)%CNDFCT)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLINIT)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGOLD2)
      DEALLOCATE (GWFLAKDAT(IGRID)%BOTTMS)
      DEALLOCATE (GWFLAKDAT(IGRID)%BGAREA)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSMN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSMX)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAP)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRECIP)
      DEALLOCATE (GWFLAKDAT(IGRID)%SEEP)
      DEALLOCATE (GWFLAKDAT(IGRID)%SEEP3)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFA)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFOT)
      DEALLOCATE (GWFLAKDAT(IGRID)%SUMCNN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SUMCHN)
      DEALLOCATE (GWFLAKDAT(IGRID)%CLAKE)
      DEALLOCATE (GWFLAKDAT(IGRID)%CRNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%SILLVT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CAUG)
      DEALLOCATE (GWFLAKDAT(IGRID)%CPPT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CLAKINIT)
      DEALLOCATE (GWFLAKDAT(IGRID)%BDLKN1)
Cdep  Added arrays that track lake budgets for dry lakes
      DEALLOCATE (GWFLAKDAT(Igrid)%EVAPO)
      DEALLOCATE (GWFLAKDAT(Igrid)%WITHDRW)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWIN)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWITER)
      DEALLOCATE (GWFLAKDAT(Igrid)%GWRATELIM)
Cdep  Deallocate arrays used in conjunction with UZF Package
      DEALLOCATE (GWFLAKDAT(Igrid)%OVRLNDRNF)
      DEALLOCATE (GWFLAKDAT(Igrid)%CUMLNDRNF)
Cdep  Deallocate arrays for storing depth, and area arrays
      DEALLOCATE (GWFLAKDAT(Igrid)%DEPTHTABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%AREATABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLAKES)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLAKINIT)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLKOLD)
Crsr allocate BD arrays
      DEALLOCATE (GWFLAKDAT(IGRID)%LDRY)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNT)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNST)
      DEALLOCATE (GWFLAKDAT(IGRID)%KSUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%MSUB1)
      DEALLOCATE (GWFLAKDAT(IGRID)%MSUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%FLXINL)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLOLD)
      DEALLOCATE (GWFLAKDAT(IGRID)%GWIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%GWOUT)
      DEALLOCATE (GWFLAKDAT(IGRID)%DELH)
      DEALLOCATE (GWFLAKDAT(IGRID)%TDELH)
      DEALLOCATE (GWFLAKDAT(IGRID)%SVT)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGADJ)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTGWIN_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTGWOT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTDELSTOR_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSTOR_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTEVAP_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTPPT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTRUNF_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTWTHDRW_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSURFIN_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSURFOT_LAK)
      END SUBROUTINE GWF2LAK7DA

      SUBROUTINE SGWF2LAK7PNT(IGRID)
C  Set pointers to LAK data for grid      
      USE GWFLAKMODULE
C
      NLAKES=>GWFLAKDAT(IGRID)%NLAKES
      NLAKESAR=>GWFLAKDAT(IGRID)%NLAKESAR
      ILKCB=>GWFLAKDAT(IGRID)%ILKCB
      NSSITR=>GWFLAKDAT(IGRID)%NSSITR
      MXLKND=>GWFLAKDAT(IGRID)%MXLKND
      LKNODE=>GWFLAKDAT(IGRID)%LKNODE
      ICMX=>GWFLAKDAT(IGRID)%ICMX
      NCLS=>GWFLAKDAT(IGRID)%NCLS
      LWRT=>GWFLAKDAT(IGRID)%LWRT
      NDV=>GWFLAKDAT(IGRID)%NDV
      NTRB=>GWFLAKDAT(IGRID)%NTRB
      THETA=>GWFLAKDAT(IGRID)%THETA
      SSCNCR=>GWFLAKDAT(IGRID)%SSCNCR
      ICS=>GWFLAKDAT(IGRID)%ICS
      NCNCVR=>GWFLAKDAT(IGRID)%NCNCVR
      LIMERR=>GWFLAKDAT(IGRID)%LIMERR
      ILAKE=>GWFLAKDAT(IGRID)%ILAKE
      ITRB=>GWFLAKDAT(IGRID)%ITRB
      IDIV=>GWFLAKDAT(IGRID)%IDIV
      ISUB=>GWFLAKDAT(IGRID)%ISUB
      IRK=>GWFLAKDAT(IGRID)%IRK
      LKARR1=>GWFLAKDAT(IGRID)%LKARR1
      STAGES=>GWFLAKDAT(IGRID)%STAGES
      STGNEW=>GWFLAKDAT(IGRID)%STGNEW
      STGOLD=>GWFLAKDAT(IGRID)%STGOLD
      STGITER=>GWFLAKDAT(IGRID)%STGITER
      VOL=>GWFLAKDAT(IGRID)%VOL
      FLOB=>GWFLAKDAT(IGRID)%FLOB
      DSRFOT=>GWFLAKDAT(IGRID)%DSRFOT
      PRCPLK=>GWFLAKDAT(IGRID)%PRCPLK
      EVAPLK=>GWFLAKDAT(IGRID)%EVAPLK
      BEDLAK=>GWFLAKDAT(IGRID)%BEDLAK
      WTHDRW=>GWFLAKDAT(IGRID)%WTHDRW
      RNF=>GWFLAKDAT(IGRID)%RNF
      CUMRNF=>GWFLAKDAT(IGRID)%CUMRNF
      CUMPPT=>GWFLAKDAT(IGRID)%CUMPPT
      CUMEVP=>GWFLAKDAT(IGRID)%CUMEVP
      CUMGWI=>GWFLAKDAT(IGRID)%CUMGWI
      CUMGWO=>GWFLAKDAT(IGRID)%CUMGWO
      CUMSWI=>GWFLAKDAT(IGRID)%CUMSWI
      CUMSWO=>GWFLAKDAT(IGRID)%CUMSWO
      CUMWDR=>GWFLAKDAT(IGRID)%CUMWDR
      CUMFLX=>GWFLAKDAT(IGRID)%CUMFLX
      CNDFCT=>GWFLAKDAT(IGRID)%CNDFCT
      VOLINIT=>GWFLAKDAT(IGRID)%VOLINIT
      STGOLD2=>GWFLAKDAT(IGRID)%STGOLD2
      BOTTMS=>GWFLAKDAT(IGRID)%BOTTMS
      BGAREA=>GWFLAKDAT(IGRID)%BGAREA
      SSMN=>GWFLAKDAT(IGRID)%SSMN
      SSMX=>GWFLAKDAT(IGRID)%SSMX
      EVAP=>GWFLAKDAT(IGRID)%EVAP
      PRECIP=>GWFLAKDAT(IGRID)%PRECIP
      SEEP=>GWFLAKDAT(IGRID)%SEEP
      SEEP3=>GWFLAKDAT(IGRID)%SEEP3
      SURFA=>GWFLAKDAT(IGRID)%SURFA
      SURFIN=>GWFLAKDAT(IGRID)%SURFIN
      SURFOT=>GWFLAKDAT(IGRID)%SURFOT
      SUMCNN=>GWFLAKDAT(IGRID)%SUMCNN
      SUMCHN=>GWFLAKDAT(IGRID)%SUMCHN
      CLAKE=>GWFLAKDAT(IGRID)%CLAKE
      CRNF=>GWFLAKDAT(IGRID)%CRNF
      SILLVT=>GWFLAKDAT(IGRID)%SILLVT
      CAUG=>GWFLAKDAT(IGRID)%CAUG
      CPPT=>GWFLAKDAT(IGRID)%CPPT
      CLAKINIT=>GWFLAKDAT(IGRID)%CLAKINIT
      BDLKN1=>GWFLAKDAT(IGRID)%BDLKN1
Cdep  Added arrays that track lake budgets for dry lakes
      EVAPO=>GWFLAKDAT(Igrid)%EVAPO
      WITHDRW=>GWFLAKDAT(Igrid)%WITHDRW
      FLWIN=>GWFLAKDAT(Igrid)%FLWIN
      FLWITER=>GWFLAKDAT(Igrid)%FLWITER
      GWRATELIM=>GWFLAKDAT(Igrid)%GWRATELIM
Cdep  added two variable arrays
      OVRLNDRNF=>GWFLAKDAT(Igrid)%OVRLNDRNF
      CUMLNDRNF=>GWFLAKDAT(Igrid)%CUMLNDRNF
Cdep  added two variable arrays for depth, and area
      DEPTHTABLE=>GWFLAKDAT(Igrid)%DEPTHTABLE
      AREATABLE=>GWFLAKDAT(Igrid)%AREATABLE    
      XLAKES=>GWFLAKDAT(Igrid)%XLAKES
      XLAKINIT=>GWFLAKDAT(Igrid)%XLAKINIT
      XLKOLD=>GWFLAKDAT(Igrid)%XLKOLD
Crsr allocate BD arrays
      LDRY=>GWFLAKDAT(IGRID)%LDRY
      NCNT=>GWFLAKDAT(IGRID)%NCNT
      NCNST=>GWFLAKDAT(IGRID)%NCNST
      KSUB=>GWFLAKDAT(IGRID)%KSUB
      MSUB1=>GWFLAKDAT(IGRID)%MSUB1
      MSUB=>GWFLAKDAT(IGRID)%MSUB
      FLXINL=>GWFLAKDAT(IGRID)%FLXINL
      VOLOLD=>GWFLAKDAT(IGRID)%VOLOLD
      GWIN=>GWFLAKDAT(IGRID)%GWIN
      GWOUT=>GWFLAKDAT(IGRID)%GWOUT
      DELH=>GWFLAKDAT(IGRID)%DELH
      TDELH=>GWFLAKDAT(IGRID)%TDELH
      SVT=>GWFLAKDAT(IGRID)%SVT
      STGADJ=>GWFLAKDAT(IGRID)%STGADJ
      TOTGWIN_LAK=>GWFLAKDAT(IGRID)%TOTGWIN_LAK
      TOTGWOT_LAK=>GWFLAKDAT(IGRID)%TOTGWOT_LAK
      TOTDELSTOR_LAK=>GWFLAKDAT(IGRID)%TOTDELSTOR_LAK
      TOTSTOR_LAK=>GWFLAKDAT(IGRID)%TOTSTOR_LAK
      TOTEVAP_LAK=>GWFLAKDAT(IGRID)%TOTEVAP_LAK
      TOTPPT_LAK=>GWFLAKDAT(IGRID)%TOTPPT_LAK
      TOTRUNF_LAK=>GWFLAKDAT(IGRID)%TOTRUNF_LAK
      TOTWTHDRW_LAK=>GWFLAKDAT(IGRID)%TOTWTHDRW_LAK
      TOTSURFIN_LAK=>GWFLAKDAT(IGRID)%TOTSURFIN_LAK
      TOTSURFOT_LAK=>GWFLAKDAT(IGRID)%TOTSURFOT_LAK
      END SUBROUTINE SGWF2LAK7PNT

      SUBROUTINE SGWF2LAK7PSV1(IGRID)
C  Save LAK data for a grid for data shared with SFR
      USE GWFLAKMODULE
C
      GWFLAKDAT(IGRID)%NLAKES=>NLAKES
      GWFLAKDAT(IGRID)%NLAKESAR=>NLAKESAR
      GWFLAKDAT(IGRID)%THETA=>THETA
      GWFLAKDAT(IGRID)%STGOLD=>STGOLD
      GWFLAKDAT(IGRID)%STGNEW=>STGNEW
      GWFLAKDAT(IGRID)%STGITER=>STGITER
      GWFLAKDAT(IGRID)%VOL=>VOL
      END SUBROUTINE SGWF2LAK7PSV1

      SUBROUTINE SGWF2LAK7PSV(IGRID)
C  Save LAK data for a grid
      USE GWFLAKMODULE
C
      GWFLAKDAT(IGRID)%ILKCB=>ILKCB
      GWFLAKDAT(IGRID)%NSSITR=>NSSITR
      GWFLAKDAT(IGRID)%MXLKND=>MXLKND
      GWFLAKDAT(IGRID)%LKNODE=>LKNODE
      GWFLAKDAT(IGRID)%ICMX=>ICMX
      GWFLAKDAT(IGRID)%NCLS=>NCLS
      GWFLAKDAT(IGRID)%LWRT=>LWRT
      GWFLAKDAT(IGRID)%NDV=>NDV
      GWFLAKDAT(IGRID)%NTRB=>NTRB
      GWFLAKDAT(IGRID)%SSCNCR=>SSCNCR
      GWFLAKDAT(IGRID)%ICS=>ICS
      GWFLAKDAT(IGRID)%NCNCVR=>NCNCVR
      GWFLAKDAT(IGRID)%LIMERR=>LIMERR
      GWFLAKDAT(IGRID)%ILAKE=>ILAKE
      GWFLAKDAT(IGRID)%ITRB=>ITRB
      GWFLAKDAT(IGRID)%IDIV=>IDIV
      GWFLAKDAT(IGRID)%ISUB=>ISUB
      GWFLAKDAT(IGRID)%IRK=>IRK
      GWFLAKDAT(IGRID)%LKARR1=>LKARR1
      GWFLAKDAT(IGRID)%STAGES=>STAGES
      GWFLAKDAT(IGRID)%FLOB=>FLOB
      GWFLAKDAT(IGRID)%DSRFOT=>DSRFOT
      GWFLAKDAT(IGRID)%PRCPLK=>PRCPLK
      GWFLAKDAT(IGRID)%EVAPLK=>EVAPLK
      GWFLAKDAT(IGRID)%BEDLAK=>BEDLAK
      GWFLAKDAT(IGRID)%WTHDRW=>WTHDRW
      GWFLAKDAT(IGRID)%RNF=>RNF
      GWFLAKDAT(IGRID)%CUMRNF=>CUMRNF
      GWFLAKDAT(IGRID)%CUMPPT=>CUMPPT
      GWFLAKDAT(IGRID)%CUMEVP=>CUMEVP
      GWFLAKDAT(IGRID)%CUMGWI=>CUMGWI
      GWFLAKDAT(IGRID)%CUMGWO=>CUMGWO
      GWFLAKDAT(IGRID)%CUMSWI=>CUMSWI
      GWFLAKDAT(IGRID)%CUMSWO=>CUMSWO
      GWFLAKDAT(IGRID)%CUMWDR=>CUMWDR
      GWFLAKDAT(IGRID)%CUMFLX=>CUMFLX
      GWFLAKDAT(IGRID)%CNDFCT=>CNDFCT
      GWFLAKDAT(IGRID)%VOLINIT=>VOLINIT
      GWFLAKDAT(IGRID)%STGOLD2=>STGOLD2
      GWFLAKDAT(IGRID)%BOTTMS=>BOTTMS
      GWFLAKDAT(IGRID)%BGAREA=>BGAREA
      GWFLAKDAT(IGRID)%SSMN=>SSMN
      GWFLAKDAT(IGRID)%SSMX=>SSMX
      GWFLAKDAT(IGRID)%EVAP=>EVAP
      GWFLAKDAT(IGRID)%PRECIP=>PRECIP
      GWFLAKDAT(IGRID)%SEEP=>SEEP
      GWFLAKDAT(IGRID)%SEEP3=>SEEP3
      GWFLAKDAT(IGRID)%SURFA=>SURFA
      GWFLAKDAT(IGRID)%SURFIN=>SURFIN
      GWFLAKDAT(IGRID)%SURFOT=>SURFOT
      GWFLAKDAT(IGRID)%SUMCNN=>SUMCNN
      GWFLAKDAT(IGRID)%SUMCHN=>SUMCHN
      GWFLAKDAT(IGRID)%CLAKE=>CLAKE
      GWFLAKDAT(IGRID)%CRNF=>CRNF
      GWFLAKDAT(IGRID)%SILLVT=>SILLVT
      GWFLAKDAT(IGRID)%CAUG=>CAUG
      GWFLAKDAT(IGRID)%CPPT=>CPPT
      GWFLAKDAT(IGRID)%CLAKINIT=>CLAKINIT
      GWFLAKDAT(IGRID)%BDLKN1=>BDLKN1
Cdep  Added arrays that track lake budgets for dry lakes
      GWFLAKDAT(Igrid)%EVAPO=>EVAPO
      GWFLAKDAT(Igrid)%WITHDRW=>WITHDRW
      GWFLAKDAT(Igrid)%FLWIN=>FLWIN
      GWFLAKDAT(Igrid)%FLWITER=>FLWITER
      GWFLAKDAT(Igrid)%GWRATELIM=>GWRATELIM
Cdep  added two variable arrays
      GWFLAKDAT(Igrid)%OVRLNDRNF=>OVRLNDRNF
      GWFLAKDAT(Igrid)%CUMLNDRNF=>CUMLNDRNF
Cdep  added two variable arrays for depth and  area
      GWFLAKDAT(Igrid)%DEPTHTABLE=>DEPTHTABLE
      GWFLAKDAT(Igrid)%AREATABLE=>AREATABLE
      GWFLAKDAT(Igrid)%XLAKES=>XLAKES
      GWFLAKDAT(Igrid)%XLAKINIT=>XLAKINIT
      GWFLAKDAT(Igrid)%XLKOLD=>XLKOLD
Crsr allocate BD arrays
      GWFLAKDAT(IGRID)%LDRY=>LDRY
      GWFLAKDAT(IGRID)%NCNT=>NCNT
      GWFLAKDAT(IGRID)%NCNST=>NCNST
      GWFLAKDAT(IGRID)%KSUB=>KSUB
      GWFLAKDAT(IGRID)%MSUB1=>MSUB1
      GWFLAKDAT(IGRID)%MSUB=>MSUB
      GWFLAKDAT(IGRID)%FLXINL=>FLXINL
      GWFLAKDAT(IGRID)%VOLOLD=>VOLOLD
      GWFLAKDAT(IGRID)%GWIN=>GWIN
      GWFLAKDAT(IGRID)%GWOUT=>GWOUT
      GWFLAKDAT(IGRID)%DELH=>DELH
      GWFLAKDAT(IGRID)%TDELH=>TDELH
      GWFLAKDAT(IGRID)%SVT=>SVT
      GWFLAKDAT(IGRID)%STGADJ=>STGADJ
crgn Allocate budget arrays for GSFLOW CSV file
      GWFLAKDAT(IGRID)%TOTGWIN_LAK=>TOTGWIN_LAK
      GWFLAKDAT(IGRID)%TOTGWOT_LAK=>TOTGWOT_LAK
      GWFLAKDAT(IGRID)%TOTDELSTOR_LAK=>TOTDELSTOR_LAK
      GWFLAKDAT(IGRID)%TOTSTOR_LAK=>TOTSTOR_LAK
      GWFLAKDAT(IGRID)%TOTEVAP_LAK=>TOTEVAP_LAK
      GWFLAKDAT(IGRID)%TOTPPT_LAK=>TOTPPT_LAK
      GWFLAKDAT(IGRID)%TOTRUNF_LAK=>TOTRUNF_LAK
      GWFLAKDAT(IGRID)%TOTWTHDRW_LAK=>TOTWTHDRW_LAK
      GWFLAKDAT(IGRID)%TOTSURFIN_LAK=>TOTSURFIN_LAK
      GWFLAKDAT(IGRID)%TOTSURFOT_LAK=>TOTSURFOT_LAK        
      END SUBROUTINE SGWF2LAK7PSV
