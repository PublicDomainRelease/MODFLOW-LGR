      MODULE LGRMODULE
         INTEGER, SAVE, POINTER ::ISCHILD,NPLBEG,NPRBEG,NPCBEG,NPLEND
         INTEGER, SAVE, POINTER ::NPREND,NPCEND,NCPP,NPL,IBOTFLG
         INTEGER, SAVE, POINTER ::ISHFLG,IBFLG,IUPBHSV,IUCBHSV
         INTEGER, SAVE, POINTER ::IUPBFSV,IUCBFSV,MXLGRITER,IOUTLGR 
         REAL, SAVE, POINTER ::RELAXH,RELAXF,HCLOSELGR,FCLOSELGR
         REAL, SAVE, POINTER ::HDIFFM,FDIFFM
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::IBPFLG
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::IBSHRD
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NCPPL
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NODEH
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NODEF
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::KPLC
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::IPLC
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::JPLC
         INTEGER, SAVE, POINTER, DIMENSION(:)     ::NPINDX
         INTEGER, SAVE, POINTER, DIMENSION(:,:,:) ::ICBOUND
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::HOLDC
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::CCC
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::CCR
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::CCV
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::PFLUX
         REAL, SAVE,    POINTER, DIMENSION(:,:,:) ::PFLUXOLD
         REAL, SAVE,    POINTER, DIMENSION(:)     ::VCB
        TYPE LGRTYPE
         INTEGER, POINTER ::ISCHILD,NPLBEG,NPRBEG,NPCBEG,NPLEND
         INTEGER, POINTER ::NPREND,NPCEND,NCPP,NPL,IBOTFLG
         INTEGER, POINTER ::ISHFLG,IBFLG,IUPBHSV,IUCBHSV
         INTEGER, POINTER ::IUPBFSV,IUCBFSV,MXLGRITER,IOUTLGR
         REAL, POINTER ::RELAXH,RELAXF,HCLOSELGR,FCLOSELGR,HDIFFM,FDIFFM
         INTEGER, POINTER,    DIMENSION(:)     ::IBPFLG
         INTEGER, POINTER,    DIMENSION(:)     ::IBSHRD
         INTEGER, POINTER,    DIMENSION(:)     ::NCPPL
         INTEGER, POINTER,    DIMENSION(:)     ::NODEH
         INTEGER, POINTER,    DIMENSION(:)     ::NODEF
         INTEGER, POINTER,    DIMENSION(:)     ::KPLC
         INTEGER, POINTER,    DIMENSION(:)     ::IPLC
         INTEGER, POINTER,    DIMENSION(:)     ::JPLC
         INTEGER, POINTER,    DIMENSION(:)     ::NPINDX
         INTEGER, POINTER,    DIMENSION(:,:,:) ::ICBOUND
         REAL,    POINTER,    DIMENSION(:,:,:) ::HOLDC
         REAL,    POINTER,    DIMENSION(:,:,:) ::CCC
         REAL,    POINTER,    DIMENSION(:,:,:) ::CCR
         REAL,    POINTER,    DIMENSION(:,:,:) ::CCV
         REAL,    POINTER,    DIMENSION(:,:,:) ::PFLUX
         REAL,    POINTER,    DIMENSION(:,:,:) ::PFLUXOLD
         REAL,    POINTER,    DIMENSION(:)     ::VCB
        END TYPE
        TYPE(LGRTYPE), SAVE  ::LGRDAT(10)
      END MODULE LGRMODULE

C-----VERSION 1.0 15FEBRUARY2006 GETNAMFILLGR
      SUBROUTINE GETNAMFILLGR(INLGR,FNAME,IGRID)
C     ******************************************************************
C     READ NAMES OF THE CORRESPONDING NAME FILES FROM LGR CONTROL FILE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*200 LINE, FNAME
      LOGICAL EXISTS
C     ------------------------------------------------------------------
C1-----READ IN THE NAME OF THE NAME FILE FOR THIS GRID
      CALL URDCOM(INLGR,0,LINE)
      ICOL = 1
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,0,N,R,0,0)
      FNAME=LINE(ISTART:ISTOP)
      INQUIRE (FILE=FNAME,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
        NC=INDEX(FNAME,' ')
        FNAME(NC:NC+3)='.nam'
        INQUIRE (FILE=FNAME,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          WRITE (*,480) FNAME(1:NC-1),FNAME(1:NC+3)
  480     FORMAT(1X,'Can''t find name file ',A,' or ',A)
          CALL USTOP(' ')
        ENDIF
      ENDIF
C
      RETURN
      END
C***********************************************************************
C-----VERSION 1.1 10OCTOBER2006 GWF2LGR1AR      
      SUBROUTINE GWF2LGR1AR(INLGR,FNAME,NGRIDS,IGRID)
C     ******************************************************************
C     ALLOCATE SPACE FOR LOCAL GRID REFINEMENT AND READ DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IOUT,IBOUND,GLOBALDAT
      USE LGRMODULE,   ONLY:ISCHILD,NPLBEG,NPRBEG,NPCBEG,NPLEND,NPREND,
     1                      NPCEND,NCPP,NPL,IBOTFLG,ISHFLG,IBFLG,
     2                      IUPBHSV,IUCBHSV,IUPBFSV,IUCBFSV,MXLGRITER,
     3                      IOUTLGR,RELAXH,RELAXF,HCLOSELGR,FCLOSELGR,
     4                      HDIFFM,FDIFFM,IBPFLG,IBSHRD,NCPPL,NODEH,
     5                      NODEF,KPLC,IPLC,JPLC,NPINDX,ICBOUND,HOLDC,
     6                      CCC,CCR,CCV,PFLUX,PFLUXOLD,VCB,LGRDAT 
C
      CHARACTER*200 LINE, FNAME 
      CHARACTER*14 GRIDSTATUS
C     ------------------------------------------------------------------
      ALLOCATE(ISCHILD,NPLBEG,NPRBEG,NPCBEG,NPLEND,NPREND,NPCEND,NCPP,
     1         NPL,IBOTFLG,ISHFLG,IBFLG,IUPBHSV,IUCBHSV,IUPBFSV,
     2         IUCBFSV,MXLGRITER,IOUTLGR,RELAXH,RELAXF,HCLOSELGR,
     3         FCLOSELGR,HDIFFM,FDIFFM,IBPFLG(NGRIDS))      
C1------PRINT A MESSAGE IDENTIFYING LGR PACKAGE
      WRITE(IOUT,500)IGRID,TRIM(FNAME)
  500 FORMAT(1X,/1X,'LGR -- LOCAL GRID REFINEMENT',
     &    ', VERSION 1.2.0 05/18/2010',/8X,'INPUT READ FOR MODEL ',
     &    I2,' DEFINED BY NAME FILE ',A)

      ZERO = 0.
C
C2------INITIALIZE IBPLG TO ZERO
      DO LG=1,NGRIDS
        IBPFLG(LG) = 0
      ENDDO
C
C3------READ IN GRIDSTATUS, SET ISCHILD, AND PRINT COMMENTS
      CALL URDCOM(INLGR,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INLGR)
      GRIDSTATUS = LINE(ISTART:ISTOP)
C
C3A-----PRINT COMMENTS
      IF(GRIDSTATUS .EQ. 'PARENTONLY')THEN
        ISCHILD = -1
        WRITE(IOUT,410) 
      ELSEIF(GRIDSTATUS .EQ. 'CHILDONLY')THEN
        ISCHILD = 1
        WRITE(IOUT,411) 
      ELSEIF(GRIDSTATUS .EQ. 'PARENTANDCHILD')THEN
        ISCHILD = 0
        WRITE(IOUT,412) 
      ELSE
        WRITE(IOUT,413) GRIDSTATUS
        STOP
      ENDIF
  410 FORMAT (1X,/1X,'LOCAL GRID REFINEMENT IS ACTIVE FOR PARENT ONLY')
  411 FORMAT (1X,/1X,'LOCAL GRID REFINEMENT IS ACTIVE FOR CHILD ONLY')
  412 FORMAT (1X,'LOCAL GRID REFINEMENT IS ACTIVE FOR PARENT AND',
     &        ' CHILD') 
  413 FORMAT (1X,'INVALID INPUT FOR GRIDSTATUS IN LGR INPUT FILE: ',A,/,
     &        1X,'GRIDSTATUS MUST BE PARENTONLY, CHILDONLY, OR ',
     &         'PARENTANDCHILD')
C
C4------READ, PRINT, AND CHECK LGR DATA
C4A-----IF A PARENT GRID, READ IN OPTIONS FOR SAVING BOUNDARY HEADS 
C4A-----AND FLUXES
      IF(ISCHILD .LT. 0)THEN
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  IUPBHSV,IUPBFSV
        IF(IUPBHSV .NE. 0) WRITE(IOUT,512) IUPBHSV
        IF(IUPBFSV .NE. 0) WRITE(IOUT,513) IUPBFSV
      ENDIF
C4B-----CHECK IF A CHILD GRID
C4B-----READ IN ISHFLG,IUCBHSV,IUCBFSV,MXLGRITER,RELAXH,RELAXF,HCLOSELGR,
C4B-----AND FCLOSELGR.  STORE IBFLG IN IBPFLG OF THE PARENT GRID.
      IF(ISCHILD .GE. 0)THEN
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  ISHFLG,IBFLG,IUCBHSV,IUCBFSV 
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  MXLGRITER,IOUTLGR
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  RELAXH,RELAXF
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  HCLOSELGR,FCLOSELGR
C  NOTE:  THIS IS HARDWIRED FOR A SINGLE PARENT GRID.  THE INDEX SHOULD
C  BE INDEXED TO WHATEVER THE CURRENT PARENT IS FOR THIS CHILD.
        LGRDAT(1)%IBPFLG(IGRID)=-IBFLG

C4B1----PRINT COMMENTS
        IF(ISHFLG .EQ. 1) WRITE(IOUT,510) ISHFLG
        IF(IBFLG  .LT. 0)THEN
          WRITE(IOUT,511) IBFLG
        ELSE
          CALL USTOP('IBFLG MUST BE < 0 FOR CHILD GRID')
        ENDIF
        IF(IUCBHSV .NE. 0) WRITE(IOUT,512) IUCBHSV
        IF(IUCBFSV .NE. 0) WRITE(IOUT,513) IUCBFSV
        WRITE(IOUT,515) MXLGRITER
        IF(IOUTLGR .LT. 0) WRITE(IOUT,516) IOUTLGR
        IF(IOUTLGR .EQ. 0) WRITE(IOUT,517) IOUTLGR
        IF(IOUTLGR .GT. 0) WRITE(IOUT,518) IOUTLGR
        WRITE(IOUT,520) RELAXH,RELAXF
        WRITE(IOUT,521) HCLOSELGR,FCLOSELGR
        IF(RELAXH .LE. ZERO .OR. RELAXF .LE. ZERO)THEN
          WRITE(IOUT,*) 'RELAXATION FACTORS RELAXH AND RELAXF MUST ',
     &                  'BE > 0'
          CALL USTOP(' ')
        ENDIF
  510   FORMAT (15X, 'STARTING HEADS FROM PARENT WILL BE USED:',
     &              ' ISHFLG = ',I3)
  511   FORMAT (15X,'VALUE IN IBOUND INDICATING BOUNDARY ',
     &                  'INTERFACE = ', I4)
  512   FORMAT (15X,'BOUNDARY HEADS WILL BE SAVED ON UNIT ', I3)
  513   FORMAT (15X,'BOUNDARY FLUXES WILL BE SAVED ON UNIT ', I3)
  515   FORMAT (15X,'MAX NUMBER OF LGR ITERATIONS =', I3)
  516   FORMAT (15X,'LGR ITERATION RESULTS WRITTEN TO SCREEN: ',
     &                   'IOUTLGR = ', I3)
  517   FORMAT (15X,'LGR ITERATION RESULTS NOT WRITTEN: ',
     &                   'IOUTLGR = ', I3)
  518   FORMAT (15X,'LGR ITERATION RESULTS WRITTEN TO FILE: ',
     &                   'IOUTLGR = ', I3)
  520   FORMAT (1X,/15X,'WEIGHTING FACTORS FOR RELAXATION',/,
     &            15x,'RELAXH(HEAD)' ,1x,'RELAXF(FLUX)',/,15X,25('-'),/,
     &            14x,E10.3,5X,E10.3) 
  521   FORMAT (1X,/15x,'CLOSURE CRITERIA FOR LGR ITERATIONS',/,
     &            15x,'HCLOSELGR' ,7x,'FCLOSELGR',/,15X,25('-'),/,14x,
     &            1P,E10.3,5X,E10.3)
C
C4C-----READ IN DATA FOR CHILD/PARENT MESH INTERFACE LOCATION AND 
C4C-----REFINEMENT INFO
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  NPLBEG,NPRBEG,NPCBEG
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  NPLEND,NPREND,NPCEND  
        CALL URDCOM(INLGR,IOUT,LINE)
        READ (LINE,*)  NCPP
C4C1----ALLOCATE AND READ LAYER REFINEMENT
        NPL = NPLEND - NPLBEG + 1
        ALLOCATE(NCPPL(NPL))
        READ(INLGR,*) (NCPPL(K),K=1,NPL)
C
C4D-----PRINT REFINEMENT INFO
        WRITE(IOUT, 525) NPLBEG,NPRBEG,NPCBEG,
     &                   NPLEND,NPREND,NPCEND
  525   FORMAT (1x,/,15x,'STARTING LAYER, ROW, COLUMN=',I4,',',I4,',',
     &        I4,/15x,'ENDING LAYER, ROW, COLUMN=  ',I4,',',I4,',',I4)
        WRITE(IOUT,530) NCPP
  530   FORMAT (1x,14x,'NCPP: NUMBER OF CHILD CELLS PER WIDTH OF',
     &                 ' PARENT CELL=',I2)
        WRITE(IOUT,535) (K,NCPPL(K),K=1,NPL)
  535   FORMAT (1x,14X,'NCPPL: NUMBER OF CHILD LAYERS IN LAYER ',I2,
     &                 ' OF PARENT =',I2)
C
C4D1----CHECK REFINEMENT RATIOS AND GRID DISCRETIZATION
        IF(NPLBEG .NE. 1)THEN
          WRITE(IOUT,*)'NPLBEG IS NOT = 1  REFINEMENT MUST BEGIN IN ',
     &                 'TOP LAYER'
          CALL USTOP(' ')
        ENDIF
        IF(MOD(NCPP,2) .EQ. 0)THEN
          WRITE(IOUT,*)'NCPP MUST BE AN ODD INTEGER'
          CALL USTOP(' ')
        ENDIF
        IF(NCPP*(NPREND-NPRBEG) + 1 .NE. NROW)THEN
          WRITE(IOUT,*)'NROW AND NCPP DOES NOT ALIGN WITH',
     &                 ' NPREND - NPRBEG'
          CALL USTOP(' ')
        ELSEIF(NCPP*(NPCEND-NPCBEG) + 1 .NE. NCOL)THEN
          WRITE(IOUT,*)'NCOL AND NCPP DOES NOT ALIGN WITH',
     &                 ' NPCEND - NPCBEG'
          CALL USTOP(' ')
        ENDIF
C
C4D2----CHECK VERTICAL REFINEMENT 
C4D2----HARDWIRED TO TOP PARENT. IT SHOULD BE INDEXED TO 
C4D2----CURRENT PARENT FOR THIS GRID.
        IBOTFLG = 1
        IF(NPLBEG .EQ. NPLEND .OR.
     &     NPLEND .EQ. GLOBALDAT(1)%NLAY) IBOTFLG=0
        NSUM = 0
        IF(NPL .GT. 1)THEN
          DO K=1,NPL
            NSUM = NSUM + NCPPL(K) 
            IF(MOD(NCPPL(K),2) .EQ. 0)THEN 
              WRITE(IOUT,*) 'NCPPL MUST BE AN ODD INTEGER'
              CALL USTOP(' ')
            ENDIF
          ENDDO
          IF(IBOTFLG .EQ. 1)THEN
            IF(NSUM - ((NCPPL(NPL) + 1)/2 - 1) .NE. NLAY)THEN
              WRITE(IOUT,*) 'VERTICAL REFINEMENT DOES NOT ALIGN', 
     &                      ' WITH NLAY'
              CALL USTOP(' ')
            ENDIF
          ELSE
            IF(NSUM .NE. NLAY)THEN
              WRITE(IOUT,*)'VERTICAL REFINEMENT DOES NOT ALIGN', 
     &                     ' WITH NLAY'
              CALL USTOP(' ')
            ENDIF
          ENDIF 
        ENDIF

C END CHILD GRID DATA
      ENDIF
C

C SET NUMBER OF BOUNDARY NODES FOR STORING CHILD TO PARENT LOCATIONS
      NBNODES = COUNT(IBOUND .EQ. IBFLG) 
C     
C  DETERMINE IF THIS IS A PARENT, CHILD, OR CHILD/PARENT GRID 
C  AND ALLOCATE AS SUCH.
C  INITIALIZE PFLUXOLD TO ZERO (PFLUX GETS ZEROED EACH ITERATION IN
C  GWF2LGR1FMBF
      IF(ISCHILD .GE. 0)THEN
        ALLOCATE(NODEH(3),NODEF(3))
C
C  ALLOCATE SPACE FOR MAPPING CHILD LOCATIONS TO PARENT LOCATIONS
        ALLOCATE(KPLC(NBNODES),IPLC(NBNODES),JPLC(NBNODES),
     &           NPINDX(NBNODES),IBSHRD(NBNODES))
        ALLOCATE(ICBOUND(NCOL,NROW,NLAY),
     &           HOLDC(NCOL,NROW,NLAY),
     &           CCC(NCOL,NROW,NLAY),
     &           CCR(NCOL,NROW,NLAY),
     &           CCV(NCOL,NROW,NLAY))
C
        ALLOCATE(PFLUX(NPCBEG:NPCEND,
     &                        NPRBEG:NPREND,
     &                        NPLBEG:NPLEND),
     &           PFLUXOLD(NPCBEG:NPCEND,
     &                        NPRBEG:NPREND,
     &                        NPLBEG:NPLEND))
        ALLOCATE(VCB(4))
C
        DO K = NPLBEG,NPLEND
          DO I = NPRBEG,NPREND
            DO J = NPCBEG,NPCEND
              PFLUXOLD(J,I,K) = ZERO
             ENDDO
           ENDDO
         ENDDO
C
      ENDIF
C
C------SAVE POINTER DATA TO ARRARYS
      CALL SGWF2LGR1PSV(IGRID)
C
      RETURN
      END 
C***********************************************************************
      SUBROUTINE GWF2LGR1DA(IGRID)
C     ******************************************************************
C     DEALLOCATE LGR DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE LGRMODULE
C     ------------------------------------------------------------------
C
      DEALLOCATE(LGRDAT(IGRID)%NPLBEG)
      DEALLOCATE(LGRDAT(IGRID)%NPRBEG)
      DEALLOCATE(LGRDAT(IGRID)%NPCBEG)
      DEALLOCATE(LGRDAT(IGRID)%NPLEND)
      DEALLOCATE(LGRDAT(IGRID)%NPREND)
      DEALLOCATE(LGRDAT(IGRID)%NPCEND)
      DEALLOCATE(LGRDAT(IGRID)%NCPP)
      DEALLOCATE(LGRDAT(IGRID)%NPL)
      DEALLOCATE(LGRDAT(IGRID)%IBOTFLG)
      DEALLOCATE(LGRDAT(IGRID)%ISHFLG)
      DEALLOCATE(LGRDAT(IGRID)%IBFLG)
      DEALLOCATE(LGRDAT(IGRID)%IUPBHSV)
      DEALLOCATE(LGRDAT(IGRID)%IUCBHSV)
      DEALLOCATE(LGRDAT(IGRID)%IUPBFSV)
      DEALLOCATE(LGRDAT(IGRID)%IUCBFSV)
      DEALLOCATE(LGRDAT(IGRID)%MXLGRITER)
      DEALLOCATE(LGRDAT(IGRID)%IOUTLGR)
      DEALLOCATE(LGRDAT(IGRID)%RELAXH)
      DEALLOCATE(LGRDAT(IGRID)%RELAXF)
      DEALLOCATE(LGRDAT(IGRID)%HCLOSELGR)
      DEALLOCATE(LGRDAT(IGRID)%FCLOSELGR)
      DEALLOCATE(LGRDAT(IGRID)%HDIFFM)
      DEALLOCATE(LGRDAT(IGRID)%FDIFFM)
      DEALLOCATE(LGRDAT(IGRID)%IBPFLG)
      IF(LGRDAT(IGRID)%ISCHILD .GE. 0)THEN
        DEALLOCATE(LGRDAT(IGRID)%IBSHRD)
        DEALLOCATE(LGRDAT(IGRID)%NCPPL)
        DEALLOCATE(LGRDAT(IGRID)%NODEH)
        DEALLOCATE(LGRDAT(IGRID)%NODEF)
        DEALLOCATE(LGRDAT(IGRID)%KPLC)
        DEALLOCATE(LGRDAT(IGRID)%IPLC)
        DEALLOCATE(LGRDAT(IGRID)%JPLC)
        DEALLOCATE(LGRDAT(IGRID)%NPINDX)
        DEALLOCATE(LGRDAT(IGRID)%ICBOUND)
        DEALLOCATE(LGRDAT(IGRID)%HOLDC)
        DEALLOCATE(LGRDAT(IGRID)%CCC)
        DEALLOCATE(LGRDAT(IGRID)%CCR)
        DEALLOCATE(LGRDAT(IGRID)%CCV)
        DEALLOCATE(LGRDAT(IGRID)%PFLUX)
        DEALLOCATE(LGRDAT(IGRID)%PFLUXOLD)
        DEALLOCATE(LGRDAT(IGRID)%VCB)
      ENDIF
      DEALLOCATE(LGRDAT(IGRID)%ISCHILD)
C
      RETURN
      END 
C***********************************************************************
      SUBROUTINE SGWF2LGR1PNT(IGRID)
C     ******************************************************************
C     CHANGE POINTERS FOR LGR DATA TO A DIFFERENT GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE LGRMODULE
C     ------------------------------------------------------------------
C
      ISCHILD=>LGRDAT(IGRID)%ISCHILD
      NPLBEG=>LGRDAT(IGRID)%NPLBEG
      NPRBEG=>LGRDAT(IGRID)%NPRBEG
      NPCBEG=>LGRDAT(IGRID)%NPCBEG
      NPLEND=>LGRDAT(IGRID)%NPLEND
      NPREND=>LGRDAT(IGRID)%NPREND
      NPCEND=>LGRDAT(IGRID)%NPCEND
      NCPP=>LGRDAT(IGRID)%NCPP
      NPL=>LGRDAT(IGRID)%NPL
      IBOTFLG=>LGRDAT(IGRID)%IBOTFLG
      ISHFLG=>LGRDAT(IGRID)%ISHFLG
      IBFLG=>LGRDAT(IGRID)%IBFLG
      IUPBHSV=>LGRDAT(IGRID)%IUPBHSV
      IUCBHSV=>LGRDAT(IGRID)%IUCBHSV
      IUPBFSV=>LGRDAT(IGRID)%IUPBFSV
      IUCBFSV=>LGRDAT(IGRID)%IUCBFSV
      MXLGRITER=>LGRDAT(IGRID)%MXLGRITER
      IOUTLGR=>LGRDAT(IGRID)%IOUTLGR
      RELAXH=>LGRDAT(IGRID)%RELAXH
      RELAXF=>LGRDAT(IGRID)%RELAXF
      HCLOSELGR=>LGRDAT(IGRID)%HCLOSELGR
      FCLOSELGR=>LGRDAT(IGRID)%FCLOSELGR
      HDIFFM=>LGRDAT(IGRID)%HDIFFM
      FDIFFM=>LGRDAT(IGRID)%FDIFFM
      IBPFLG=>LGRDAT(IGRID)%IBPFLG
      IBSHRD=>LGRDAT(IGRID)%IBSHRD
      NCPPL=>LGRDAT(IGRID)%NCPPL
      NODEH=>LGRDAT(IGRID)%NODEH
      NODEF=>LGRDAT(IGRID)%NODEF
      KPLC=>LGRDAT(IGRID)%KPLC
      IPLC=>LGRDAT(IGRID)%IPLC
      JPLC=>LGRDAT(IGRID)%JPLC
      NPINDX=>LGRDAT(IGRID)%NPINDX
      ICBOUND=>LGRDAT(IGRID)%ICBOUND
      HOLDC=>LGRDAT(IGRID)%HOLDC
      CCC=>LGRDAT(IGRID)%CCC
      CCR=>LGRDAT(IGRID)%CCR
      CCV=>LGRDAT(IGRID)%CCV
      PFLUX=>LGRDAT(IGRID)%PFLUX
      PFLUXOLD=>LGRDAT(IGRID)%PFLUXOLD
      VCB=>LGRDAT(IGRID)%VCB
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE SGWF2LGR1PSV(IGRID)
C     ******************************************************************
C     SAVE POINTERS ARRAYS FOR LGR DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE LGRMODULE
C     ------------------------------------------------------------------
C
      LGRDAT(IGRID)%ISCHILD=>ISCHILD
      LGRDAT(IGRID)%NPLBEG=>NPLBEG 
      LGRDAT(IGRID)%NPRBEG=>NPRBEG
      LGRDAT(IGRID)%NPCBEG=>NPCBEG
      LGRDAT(IGRID)%NPLEND=>NPLEND
      LGRDAT(IGRID)%NPREND=>NPREND
      LGRDAT(IGRID)%NPCEND=>NPCEND
      LGRDAT(IGRID)%NCPP=>NCPP 
      LGRDAT(IGRID)%NPL=>NPL 
      LGRDAT(IGRID)%IBOTFLG=>IBOTFLG
      LGRDAT(IGRID)%ISHFLG=>ISHFLG
      LGRDAT(IGRID)%IBFLG=>IBFLG
      LGRDAT(IGRID)%IUPBHSV=>IUPBHSV
      LGRDAT(IGRID)%IUCBHSV=>IUCBHSV
      LGRDAT(IGRID)%IUPBFSV=>IUPBFSV
      LGRDAT(IGRID)%IUCBFSV=>IUCBFSV
      LGRDAT(IGRID)%MXLGRITER=>MXLGRITER 
      LGRDAT(IGRID)%IOUTLGR=>IOUTLGR
      LGRDAT(IGRID)%RELAXH=>RELAXH 
      LGRDAT(IGRID)%RELAXF=>RELAXF 
      LGRDAT(IGRID)%HCLOSELGR=>HCLOSELGR 
      LGRDAT(IGRID)%FCLOSELGR=>FCLOSELGR 
      LGRDAT(IGRID)%HDIFFM=>HDIFFM 
      LGRDAT(IGRID)%FDIFFM=>FDIFFM 
      LGRDAT(IGRID)%IBPFLG=>IBPFLG
      LGRDAT(IGRID)%IBSHRD=>IBSHRD
      LGRDAT(IGRID)%NCPPL=>NCPPL 
      LGRDAT(IGRID)%NODEH=>NODEH 
      LGRDAT(IGRID)%NODEF=>NODEF
      LGRDAT(IGRID)%KPLC=>KPLC 
      LGRDAT(IGRID)%IPLC=>IPLC 
      LGRDAT(IGRID)%JPLC=>JPLC 
      LGRDAT(IGRID)%NPINDX=>NPINDX
      LGRDAT(IGRID)%ICBOUND=>ICBOUND 
      LGRDAT(IGRID)%HOLDC=>HOLDC 
      LGRDAT(IGRID)%CCC=>CCC 
      LGRDAT(IGRID)%CCR=>CCR
      LGRDAT(IGRID)%CCV=>CCV
      LGRDAT(IGRID)%PFLUX=>PFLUX
      LGRDAT(IGRID)%PFLUXOLD=>PFLUXOLD 
      LGRDAT(IGRID)%VCB=>VCB
C
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 GWF2LGR1RP
      SUBROUTINE  GWF2LGR1RP(KPER,IGRID)
C     ******************************************************************
C     FIND A MAPPING BETWEEN THE COL, ROW, AND LAYER OF AN INTERFACE 
C     CHILD CELL TO THE CORRESPONDING LOCATION OF THE PARENT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,NCOL,NROW,NLAY  
      USE LGRMODULE,   ONLY:NCPP,NCPPL,NPL,KPLC,IPLC,JPLC,IBSHRD,NPINDX,
     1                      NPCBEG,NPRBEG,NPCEND,NPREND,IBFLG,ISCHILD
C     ------------------------------------------------------------------
      CALL SGWF2LGR1PNT(IGRID)
C
C1------ONLY CREATING MAPPING IF ON THE FIRST STRESS PERIOD OF A CHILD.  
      IF(KPER .NE. 1 .OR. ISCHILD .LT. 0) RETURN
C  
      NCPP2=(NCPP + 1)/2
C
C2------LOOP THROUGH ALL INTERFACE CELLS ON THE CHILD GRID
      IB=0
      KK=1
      KCEND = 0
      DO K = 1,NLAY
        KCEND = KCEND + NCPPL(KK) 
        KCEND2 = KCEND - ((NCPPL(KK) + 1)/2 - 1)
        IF(K .EQ. KCEND) KK=KK+1
        DO I = 1,NROW
          DO J = 1,NCOL
C
C2A-----IF CELL IS NOT AN INTERFACE CELL SKIP IT & GO ON TO NEXT CELL.
            IF (IBOUND(J,I,K) .NE. IBFLG) CYCLE
            IB = IB + 1

C3------FIND COLUMN LOCATION
C3A-----INITIALIZE CHILD CELL COLUMN LOCATIONS
            JCCBEG=1
            JCCEND=JCCBEG+NCPP2-1
C3B-----LOOP THROUGH PARENT COLUMNS.  CHECK IF CURRENT COLUMN INDEX (J)
C3B-----FALLS WITHIN THIS PARENT CELL.  IF SO, STORE THE LOCATION AND 
C3B-----EXIT.  IF NOT, SHIFT TO NEXT PARENT CELL AND CONTNUE LOOP.
      
            DO JP=NPCBEG,NPCEND
              IF(J .GE. JCCBEG .AND. J .LE. JCCEND)THEN
                JPLC(IB)=JP
                EXIT    
              ENDIF
              JCCBEG=JCCEND+1
              JCCEND=JCCBEG+NCPP-1
            ENDDO
C
C4------FIND ROW LOCATION
C4A
            ICCBEG=1
            ICCEND=ICCBEG+NCPP2-1
C4B
            DO IP=NPRBEG,NPREND
              IF(I .GE. ICCBEG .AND. I .LE. ICCEND)THEN
                IPLC(IB)=IP
                EXIT
              ENDIF
              ICCBEG=ICCEND+1
              ICCEND=ICCBEG+NCPP-1      
            ENDDO   
C
C5------FIND LAYER  LOCATION.  
C5------NOTE: HARDWIRED FOR TOP CHILD LAYER IS TOP OF THE PARENT MODEL
C5A
            KCCBEG=1
            KCCEND=KCCBEG+NCPPL(1)-1
C5A
            DO KP=1,NPL
              IF(K .GE. KCCBEG .AND. K .LE. KCCEND)THEN
                KPLC(IB)=KP
                EXIT 
              ENDIF
              KCCBEG=KCCEND+1
              KCCEND=KCCBEG+NCPPL(KP+1)-1
            ENDDO    
C
C6------FIND LOCATIONS OF SHARED NODES
C-------NOTE: HARDWIRED FOR RECTANGULAR REFINEMENT
            IBSHRD(IB) = 0
            IF(K .EQ. KCEND2)THEN 
              IF(I .EQ. 1 .OR. I .EQ. NROW)THEN
                IF(MOD(J-1,NCPP) .EQ. 0) IBSHRD(IB) = 1
              ELSEIF(J .EQ. 1 .OR. J .EQ. NCOL)THEN
                IF(MOD(I-1,NCPP) .EQ. 0) IBSHRD(IB) = 1
              ENDIF  
            ENDIF
C-------PICK UP BOTTOM INTERIOR IF ON LAST LAYER 
            IF(KCEND .GT. NLAY)THEN
              IF(MOD(J-1,NCPP) .EQ. 0 .AND. 
     &           MOD(I-1,NCPP) .EQ. 0) IBSHRD(IB) = 1
            ENDIF
C
C-------END LOOPS FOR CHILD INTERFACE CELLS
          ENDDO    
        ENDDO    
      ENDDO    
C
C6------CREATE AN ORDERING FOR THE BOUNDARY NODES IN TERMS OF THE PARENT
C6------ARRAY LOOPING INDICES.  LOOP THROUGH PARENT INDICES, CHECK IF 
C6------COLUMN, ROW, AND LAYER MATCH ANY OF THE INTERFACE CELLS.  IF SO,
C6------INCREMENT INDEX COUNTER (ON FIRST OCCURENCE) AND STORE INDEX
      NBNODES=IB
      IB=0 
      DO KP=1,NPL
        DO IP = NPRBEG,NPREND
          DO JP = NPCBEG,NPCEND
            IBSTART=0
            DO N=1,NBNODES
              IF(JPLC(N) .EQ. JP .AND. IPLC(N) .EQ. IP .AND. 
     &           KPLC(N) .EQ. KP)THEN
                IF(IBSTART .EQ. 0)THEN
                  IB=IB+1  
                  IBSTART=1
                ENDIF
                NPINDX(N) = IB
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
C***********************************************************************
C-----VERSION 1.1 10OCTOBER2006 GWF2LGR1INITP
      SUBROUTINE GWF2LGR1INITP(KPER,KSTP,LGRITER,NPCBEG,NPRBEG,
     1                         NPLBEG,NPCEND,NPREND,NPLEND,IBOTFLG,
     2                         ISHFLG,MXLGRITER,LG,IGRID)
C     ******************************************************************
C     THIS ROUTINE ZEROS OUT THE INTERIOR OF THE PARENT GRID WHERE THE
C     THE CHILD WILL BE.  
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NLAY,IBOUND,HNEW,CR,CC,CV
      USE GWFBASMODULE,ONLY:HNOFLO
      USE LGRMODULE,   ONLY:IBPFLG

      DOUBLE PRECISION HNF
C     ------------------------------------------------------------------
C
      ZERO = 0.
      HNF = HNOFLO
C
C1------INITIALIZE VERTICAL DISCRETIZATION LOOP LIMITS
      IF(NPLBEG .EQ. NPLEND)THEN
        KBEG=NPLBEG
        KEND=KBEG
      ELSE
        KBEG=1
        KEND=NPLEND
      ENDIF
C
C2------CALLED AFTER THE FIRST ITERATION IF NOT RUNNING 1-WAY COUPLED
C2------AFTER THE FIRST ITERATION OF THE FIRST TIME STEP OF THE FIRST 
C2------STRESS  PERIOD, ZERO OUT THE INTERIOR.
C2------THIS REMAINS ZEROED OUT FOR THE REST OF THE SIMULATION.
      IF(MXLGRITER .GT. 1 .AND. LGRITER .EQ. 1 .AND. IGRID .EQ. 1 .AND. 
     &   KPER .EQ. 1 .AND. KSTP .EQ. 1) THEN
        DO K = KBEG,KEND
          DO I = NPRBEG,NPREND
            DO J= NPCBEG,NPCEND
              IF(I .EQ. NPRBEG .OR. I .EQ. NPREND .OR. 
     &           J .EQ. NPCBEG .OR. J .EQ. NPCEND)THEN
                IBOUND(J,I,K) = IBPFLG(LG) 
C2A-----PICK UP BOTTOM  
              ELSEIF(K .EQ. KEND .AND. IBOTFLG .EQ. 1)THEN
                IBOUND(J,I,K) = IBPFLG(LG)
              ELSE
C2B-----ZERO OUT INTERIOR
                IBOUND(J,I,K)=0
                IF(ISHFLG .EQ. 0) HNEW(J,I,K) = HNF
                IF(K.NE.NLAY) CV(J,I,K)=ZERO
                IF(K.NE.1) CV(J,I,K-1)=ZERO
                CC(J,I,K)=ZERO
                IF(I.NE.1) CC(J,I-1,K)=ZERO
                CR(J,I,K)=ZERO
                IF(J.NE.1) CR(J-1,I,K)=ZERO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C3
C3------SET HNEW TO HNOFLO ON SECOND LGR ITERATION. NOT OVERWRITTEN ON
C3------FIRST ITERATION BECAUSE HNEW WAS USED FOR CHILD START HEAD.
      ELSEIF(ISHFLG .EQ. 1 .AND. LGRITER .EQ. 2 .AND. IGRID .EQ. 1 .AND.
     &       KPER .EQ. 1 .AND. KSTP .EQ. 1)THEN
        DO K = KBEG,KEND
          DO I = NPRBEG,NPREND
            DO J= NPCBEG,NPCEND
              IF(IBOUND(J,I,K) .EQ. 0) HNEW(J,I,K) = HNF 
            ENDDO
          ENDDO
        ENDDO
      ENDIF     
C4
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 GWF2LGR1FMCBS      
      SUBROUTINE GWF2LGR1FMCBS(KPER,KSTP,LGRITER,IUBCF,IULPF,IUHUF,
     1                         IGRID)
C     ******************************************************************
C     ADJUST CHILD BOUNDARY STORAGE COEFFICIENTS ALONG INTERFACE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C1------SET POINTERS DEPENDING ON WHICH FLOW PACKAGE IS ACTIVE
      IF(IUBCF .NE. 0) CALL SGWF2BCF7PNT(IGRID)
      IF(IULPF .NE. 0) CALL SGWF2LPF7PNT(IGRID)
      IF(IUHUF .NE. 0) CALL SGWF2HUF7PNT(IGRID)
C
C2------ADJUST STORAGE IF ON THE FIRST LGR ITERATION OF THE SIMULATION.
      IF(LGRITER .EQ. 1 .AND. KSTP .EQ. 1 .AND. KPER .EQ.1)THEN
        CALL SGWF2LGR1FMCS(IUBCF,IULPF,IUHUF)
      ENDIF
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.1 10OCTOBER2006 GWF2LGR1FMPBS      
      SUBROUTINE GWF2LGR1FMPBS(KPER,KSTP,LGRITER,IUBCF,IULPF,IUHUF,
     1                         LG,IGRID)
C     ******************************************************************
C     ADJUST PARENT BOUNDARY STORAGE COEFFICIENTS ALONG INTERFACE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE LGRMODULE,   ONLY:IBPFLG
C     ------------------------------------------------------------------
C1------SET POINTERS DEPENDING ON WHICH FLOW PACKAGE IS ACTIVE
      IF(IUBCF .NE. 0) CALL SGWF2BCF7PNT(IGRID)
      IF(IULPF .NE. 0) CALL SGWF2LPF7PNT(IGRID)
      IF(IUHUF .NE. 0) CALL SGWF2HUF7PNT(IGRID)
C
C2------IF A PARENT GRID, THEN ADJUST STORAGE IF ON THE SECOND LGR 
C2------ITERATION OF THE SIMULATION.  FIRST ITERATION IS A FULL GRID.
      IF(LGRITER .EQ. 2 .AND. KSTP .EQ. 1 .AND. KPER .EQ.1)THEN
        CALL SGWF2LGR1FMPS(IUBCF,IULPF,IUHUF,IBPFLG(LG))
      ENDIF
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1FMCS      
      SUBROUTINE SGWF2LGR1FMCS(IUBCF,IULPF,IUHUF)
C     ******************************************************************
C     ADJUST CHILD STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LAYHDS
      USE LGRMODULE,   ONLY:IBOTFLG,IBFLG   
C     ------------------------------------------------------------------
C1
C1------ADJUST THE STORAGE COEFFICIENT IF ON THE FIRST ITERATION OF THE
C1------THE FIRST TIME STEP OF THE FIRST STRESS PERIOD.
C1------IF THERE IS AN UNCONFINED LAYER, THEN ADJUST SPECIFIC YIELD (SC2) 
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            JP1=J+1
            JM1=J-1
            IP1=I+1
            IM1=I-1
            JEDGE=0
            IEDGE=0
            KEDGE=0
C1
C1A-----CHECK NEIGHBOR IN THE +COL DIRECTION.  IF AN INTERFACE CELL,
C1A-----THEN HALVE SC.  
C1A-----CHECK +/- ROW DIRECTION TO SEE IF THIS CELL IS ALONG AN EDGE.  
C1A-----IF SO, SET JEDGE=1.  JEDGE IS USED LATER TO ADJUST STORAGES OF 
C1A-----EDGE CELLS.
            IF(IBOUND(J,I,K) .EQ. IBFLG)THEN

              IF(IM1 .GE. 1)THEN
                IF(IBOUND(J,IM1,K) .NE. IBFLG)JEDGE=1
              ELSE
                JEDGE=1
              ENDIF
              IF(IP1 .LE. NROW)THEN
                IF(IBOUND(J,IP1,K) .NE. IBFLG)JEDGE=1
              ELSE
                JEDGE=1
              ENDIF
C1B
              IF(JM1 .GE. 1)THEN
                IF(IBOUND(JM1,I,K) .NE. IBFLG)IEDGE=1
              ELSE   
                IEDGE=1
              ENDIF
              IF(JP1 .LE. NCOL)THEN
                IF(IBOUND(JP1,I,K) .NE. IBFLG)IEDGE=1
              ELSE
                IEDGE=1
              ENDIF
C1C
C1C-----SC1 AND SC2 ARE MULTIPED BY 1/2**(# OF EDGES)  
C1C------> 1/2*1/2=1/4 WHICH IS WHAT IS NEEDED ALONG THE CORNERS 
C1C-----MULTIPLY BY 1/2 AGAIN FOR BOTTOM. 
              IF(K .EQ. NLAY .AND. IBOTFLG .EQ. 1) KEDGE=1
              IF(IUBCF .NE. 0) CALL SGWF2LGR1BCFSA(0,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IULPF .NE. 0) CALL SGWF2LGR1LPFSA(0,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IUHUF .NE. 0) CALL SGWF2LGR1HUFSA(0,J,I,K,
     1                              JEDGE,IEDGE,KEDGE)
            ENDIF
          ENDDO  
        ENDDO   
      ENDDO   
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1FMPS      
      SUBROUTINE SGWF2LGR1FMPS(IUBCF,IULPF,IUHUF,IBFLG)
C     ******************************************************************
C     ADJUST PARENT STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LAYHDS  
C     ------------------------------------------------------------------
C1
C1------ADJUST THE STORAGE COEFFICIENT IF ON THE SECOND ITERATION OF THE
C1------THE FIRST TIME STEP OF THE FIRST STRESS PERIOD. (FIRST LGR
C1------ITERATION IS A FULL SOLUTION, SO DON'T ADJUST).
C1------IF THERE IS AN UNCONFINED LAYER, THEN ADJUST SPECIFIC YIELD (SC2) 
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            JP1=J+1
            JM1=J-1
            IP1=I+1
            IM1=I-1
            KP1=K+1
            JEDGE=0
            IEDGE=0
            KEDGE=0
C1
C1A-----CHECK NEIGHBOR IN THE +COL DIRECTION.  IF AN INTERFACE CELL,
C1A-----THEN HALVE SC.  
C1A-----CHECK +/- ROW DIRECTION TO SEE IF THIS CELL IS ALONG AND EDGE.  
C1A-----IF SO, SET JEDGE=1.  JEDGE IS USED LATER TO ADJUST STORAGES OF 
C1A-----EDGE CELLS.
            IF(IBOUND(J,I,K) .EQ. IBFLG)THEN

              IF(IM1 .GE. 1)THEN
                IF(IBOUND(J,IM1,K) .NE. IBFLG)JEDGE=1
              ELSE
                JEDGE=1
              ENDIF
              IF(IP1 .LE. NROW)THEN
                IF(IBOUND(J,IP1,K) .NE. IBFLG)JEDGE=1
              ELSE
                JEDGE=1
              ENDIF
C1B
              IF(JM1 .GE. 1)THEN
                IF(IBOUND(JM1,I,K) .NE. IBFLG)IEDGE=1
              ELSE   
                IEDGE=1
              ENDIF
              IF(JP1 .LE. NCOL)THEN
                IF(IBOUND(JP1,I,K) .NE. IBFLG)IEDGE=1
              ELSE
                IEDGE=1
              ENDIF
              IF(KP1 .LE. NLAY)THEN
                IF(IBOUND(J,I,KP1) .NE. IBFLG)KEDGE=1 
              ENDIF
C1C
C1C-----SKIP VERTICAL ADJUSTMENTS IF ONLY 1 LAYER MODEL
C1C-----SC1 AND SC2 ARE MULTIPED BY 1 - 1/2**(# OF EDGES)  
C1C------> 1/2*1/2=1/4 WHICH IS WHAT IS NEEDED ALONG THE CORNERS 
C1C-----MULTIPLY BY 1/2 AGAIN FOR BOTTOM. 
              IF(IUBCF .NE. 0) CALL SGWF2LGR1BCFSA(1,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IULPF .NE. 0) CALL SGWF2LGR1LPFSA(1,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IUHUF .NE. 0) CALL SGWF2LGR1HUFSA(1,J,I,K,
     1                              JEDGE,IEDGE,KEDGE)
            ENDIF
          ENDDO  
        ENDDO   
      ENDDO   
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 GWF2LGR1FMIB
      SUBROUTINE GWF2LGR1FMIB(IBSOLV,IBSKIP2) 
C     ******************************************************************
C     IF IBSOLV = 1, STORE CONDUCTANCES, IBOUND, AND HNEW OF CHILD MESH 
C     SET INTERIOR IBOUND AND CONDUCTANCES TO 0 FOR CAGE SOLUTION.      
C     IF IBSOLV = 2, RESTORE IBOUND AND CONDUCTANCES TO THE ORGINAL 
C     ARRAYS.  THEN ZERO OUT INTERIOR IBOUND AND CONDUCTANCES FOR FULL 
C     SHELL SOLUTION. IF NO VERTICAL REFINEMENT IN ALL LAYERS, THEN
C     SET IBSKIP2 AND SKIP SHELL SOLUTION.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,CR,CC,CV
      USE LGRMODULE,   ONLY:HOLDC,CCR,CCC,CCV,ICBOUND,NCPP,NPL,NCPPL,
     1                      IBOTFLG,IBFLG
C     ------------------------------------------------------------------
      IBSKIP2 = 0
      KSHARE1=(NCPPL(1)+1)/2
C1------IF ON THE CAGE SOLVE, 
C1------STORE ORIGINAL IBOUND, CONDUCTANCES, AND INTIAL HEAD.
      IF(IBSOLV .EQ. 1)THEN
C1A-----STORE CONDUCTANCES 
        DO  K=1,NLAY
          DO  I=1,NROW
            DO  J=1,NCOL
              IF(J.NE.NCOL)THEN
                CCR(J,I,K)=CR(J,I,K)
              ENDIF
              IF(I.NE.NROW)THEN
                CCC(J,I,K)=CC(J,I,K)
              ENDIF
              IF(K.NE.NLAY)THEN
                CCV(J,I,K)=CV(J,I,K) 
              ENDIF
C1B-----STORE IBOUND AND HEAD 
              ICBOUND(J,I,K)=IBOUND(J,I,K)
              HOLDC(J,I,K) = HNEW(J,I,K)
            ENDDO
          ENDDO
        ENDDO
C1
C1C-----SET SHARED NODES FOR CAGE SOLUTION
C1C-----ZERO OUT IBOUND AND CONDUCTANCE ARRAYS TO CREATE CAGE
        CALL SGWF2IBSHARE1(IBOUND,NROW,NCOL,NLAY,CC,CR,CV,NCPP,NPL,
     &                     NCPPL,IBOTFLG,IBFLG)
C2
C2------RESTORE HNEW, IBOUND, AND CONDUCTANCES FOR FULL SHELL (IBSOLV=2)
      ELSEIF(IBSOLV .EQ. 2)THEN
C2A-----UPDATE HOLDC WITH NEW HEADS ALONG BOUNDARY.  THIS WAY, IT DOES
C2A-----OVERWRITE CAGE HEADS WITH OLD SOLUTION IN THE 2B LOOP BELOW
        DO K= 1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(IBOUND(J,I,K) .NE. 0)HOLDC(J,I,K)=HNEW(J,I,K)
            ENDDO
          ENDDO
        ENDDO
C2B
C2B-----RESTORE ORIGINAL HEAD, IBOUND, AND THE ORIGINAL CONDUCTANCES.
        DO  K=1,NLAY
          DO  I=1,NROW
            DO  J=1,NCOL
              IBOUND(J,I,K) = ICBOUND(J,I,K)
              IF(IBOUND(J,I,K) .NE. 0) HNEW(J,I,K) = HOLDC(J,I,K)
              IF(J .NE. NCOL) CR(J,I,K)=CCR(J,I,K)
              IF(I .NE. NROW) CC(J,I,K)=CCC(J,I,K)
              IF(K .NE. NLAY) CV(J,I,K)=CCV(J,I,K)
            ENDDO
          ENDDO
        ENDDO
C2C1-----MAKE THE INTERFACE HEADS IN THE LAYERS ABOVE THE FIRST SHARED
C2C1-----NODE EQUAL TO INTERFACE HEADS IN THE LAYER OF THE FIRST SHARED 
C2C1-----NODE.
C  NOTE:  SUPPOSEDLY, IF I HAD INFO ABOUT THE VERTICAL FLUX THROUGH THE
C         TOP LAYER (SUCH AS RCH OR EVT) THE CAGE SOLUTION WOULD
C         GIVE ME THE APPROPRIATE HEADS, BUT RIGHT NOW, I DON'T KNOW IF
C         I HAVE THAT, SO I'M OVERWRITING IT WITH THE HEAD IN THE FIRST
C         SHARED NODE.  THIS MEANS THAT I HAVE NO VERTICAL GRADIENT!!!

        DO  K=1,KSHARE1-1
          DO I=1,NROW
            DO  J=1,NCOL
              HNEW(J,I,K) = HNEW(J,I,KSHARE1)
            ENDDO
          ENDDO
        ENDDO
C2C2-----REPEAT FOR LAYERS BELOW SHARED NODE OF BOTTOM LAYER, IF REFINED
        IF(IBOTFLG .EQ. 0)THEN
          KSHARE2=NLAY-(NCPPL(NPL)+1)/2 + 1
          DO  K=KSHARE2+1,NLAY
            DO I=1,NROW
              DO  J=1,NCOL
                HNEW(J,I,K) = HNEW(J,I,KSHARE2)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C2D
C2D-----IF NO VERTICAL REFINEMENT IN ALL LAYERS OR REFINEMENT OF A
C2D-----1 LAYER MODEL, THEN SKIP SHELL SOLUTION
        IBSKIP2 = 1
        DO KP=1,NPL
          IF(NCPPL(KP) .NE. 1)THEN
            IBSKIP2 = 0
            EXIT
          ENDIF
        ENDDO
        IF(NPL .EQ.  1) IBSKIP2 = 1
C2D-----SET SHARED NODES FOR FULL SHELL SOLUTION
C2D-----ZERO OUT IBOUND AND CONDUCTANCE ARRAYS TO CREATE FULL SHELL
        IF(IBSKIP2 .EQ. 0) CALL SGWF2IBSHARE2(IBOUND,NROW,NCOL,NLAY,
     &                          CC,CR,CV,NCPP,NPL,NCPPL,IBOTFLG,IBFLG)
C3------IBSOLV=3
C3------RESTORE HNEW, IBOUND, AND CONDUCTANCES FOR FULL SOLUTION 
      ELSE
C3------RESTORE ORIGINAL IBOUND AND THE ORIGINAL CONDUCTANCES.
C3------IF NOT ON THE INTERFACE, RESTORE ORIGINAL HEAD (INTERFACE HEADS
C3------ARE UPDATED FROM SHELL SOLUTIONS, SO WE DON'T WANT TO OVERWRITE)
        DO  K=1,NLAY
          DO  I=1,NROW
            DO  J=1,NCOL
              IBOUND(J,I,K) = ICBOUND(J,I,K)
              IF(IBOUND(J,I,K) .NE. IBFLG) HNEW(J,I,K)=HOLDC(J,I,K)
              IF(J .NE. NCOL) CR(J,I,K)=CCR(J,I,K)
              IF(I .NE. NROW) CC(J,I,K)=CCC(J,I,K)
              IF(K .NE. NLAY) CV(J,I,K)=CCV(J,I,K)
            ENDDO
          ENDDO
        ENDDO
C3
C3A1----MIRRORING LAYERS ABOVE THE SHARED NODE TO MATCH THE SHARED NODE.
C3A1----THIS IS FORCING NO VERTICAL FLUX (CONSISTENT WITH THE PARENT 
C3A1----GRID.  FOR UNCONFINED CASES, I CAN STILL GET VERTICAL FLUX EVEN
C3A1----IF I SET THE NODES ABOVE TO MATCH THE SHARED NODES.  THE REASON
C3A1----IS THAT IF IT IS UNCONFINED, THE THICKNESS CHANGES COMPARED TO 
C3A1----THE LAYER BELOW, AND THUS IT WILL HAVE A DIFFERENT CONDUCTIVITY
C3A1----AND PRODUCE DIFFERENT HEADS IN THE INPTERPOLATION. 
        DO  K=1,KSHARE1-1
          DO I=1,NROW
            DO  J=1,NCOL
              HNEW(J,I,K) = HNEW(J,I,KSHARE1)
            ENDDO
          ENDDO
        ENDDO
C3A2-----REPEAT FOR LAYERS BELOW SHARED NODE OF BOTTOM LAYER, IF REFINED
        IF(IBOTFLG .EQ. 0)THEN
          KSHARE2=NLAY-(NCPPL(NPL)+1)/2 + 1
          DO  K=KSHARE2+1,NLAY
            DO I=1,NROW
              DO  J=1,NCOL
                HNEW(J,I,K) = HNEW(J,I,KSHARE2)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C3 
      ENDIF
C4
      RETURN
      END      
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 GWF2LGR1FMCBC      
      SUBROUTINE GWF2LGR1FMCBC(KPER,KSTP,KITER,LGRITER,IUBCF)
C     ******************************************************************
C     ADJUST CHILD CONDUCTANCES ALONG INTERFACING BOUNDARY.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LAYHDT,CR,CC,CV
      USE LGRMODULE,   ONLY:IBOTFLG,IBFLG
C     ------------------------------------------------------------------
C1
C1------HALVE THE CONDUCTANCES IF ON THE FIRST ITERATION OR IF THERE
C1------IS AN UNCONFINED LAYER (FOR UNCONFINED, CONDUCTANCES ARE 
C1------RECALCULATED IN THE FLOW PACKAGES).
      DO K=1,NLAY
        IF (LAYHDT(K) .EQ. 0 .AND. (LGRITER .NE. 1 .OR. KITER .NE. 1 
     &      .OR. KSTP .NE. 1 .OR. KPER .NE. 1))THEN
            CYCLE
        ELSE
          DO I=1,NROW
            DO J=1,NCOL
              JP1=J+1
              JM1=J-1
              IP1=I+1
              IM1=I-1
              JEDGE=0
              IEDGE=0
              KEDGE=0
              ICR=0
              ICC=0
C1
C1A-----CHECK NEIGHBOR IN THE +COL DIRECTION.  IF AN INTERFACE CELL,
C1A-----THEN HALVE CR AND SET ICR=1.  CHECK +/- ROW DIRECTION TO SEE
C1A-----IF THIS CELL IS ALONG AND EDGE.  IF SO, SET JEDGE=1.  ICR AND
C1A-----JEDGE ARE USED LATER TO ADJUST CONDUCTANCES OF EDGE CELLS.
              IF(ABS(IBOUND(J,I,K)) .EQ. -IBFLG)THEN
                IF(JP1 .LE. NCOL)THEN
                  IF(ABS(IBOUND(JP1,I,K)) .EQ. -IBFLG)THEN
                    CR(J,I,K)=0.5*CR(J,I,K)
                    ICR=1
                  ENDIF
                ENDIF
                IF(IM1 .GE. 1)THEN
                ELSEIF(ABS(IBOUND(J,I,K)) .EQ. -IBFLG)THEN
                  JEDGE=1
                ENDIF
                IF(IP1 .LE. NROW)THEN
                ELSEIF(ABS(IBOUND(J,I,K)) .EQ. -IBFLG)THEN
                  JEDGE=1
                ENDIF
C1B
                IF(IP1 .LE. NROW)THEN
                  IF(ABS(IBOUND(J,IP1,K)) .EQ. -IBFLG)THEN
                    CC(J,I,K)=0.5*CC(J,I,K)
                    ICC=1
                  ENDIF
                ENDIF
                IF(JM1 .GE. 1)THEN
                ELSEIF(ABS(IBOUND(J,I,K)) .EQ. -IBFLG)THEN
                  IEDGE=1
                ENDIF
                IF(JP1 .LE. NCOL)THEN
                ELSEIF(ABS(IBOUND(J,I,K)) .EQ. -IBFLG)THEN
                  IEDGE=1
                ENDIF
C1C-----NOTE:  HARDWIRED FOR TOP LAYER BEING UNREFINED AND BOTTOM
C1C-----       LAYER BEING REFINED
                IF(K .EQ. NLAY .AND. IBOTFLG .EQ. 1) KEDGE=1
C1D
C1D-----SKIP VERTICAL ADJUSTMENTS IF ONLY 1 LAYER MODEL
C1D-----CR AND CC ARE MULTIPED BY 1/2 BECAUSE THEY WERE ALREADY 
C1D-----HALVED --> 1/2*1/2=1/4 WHICH IS WHAT IS NEEDED ALONG THE EDGES
C1D-----HALVE VERTICAL CONDUCTANCES ON THE FIRST ITERATION OR IF BCF
C1D-----IS NOT ACTIVE (IUBCF=0)...(BCF DOESN'T REFORMULATE CV)
                IF(NLAY .NE. 1)THEN
                  IF(KEDGE .EQ. 1)THEN
                    IF(JEDGE .EQ. 1 .AND. ICR .EQ. 1)
     &                 CR(J,I,K)=0.5*CR(J,I,K)
                    IF(IEDGE .EQ. 1 .AND. ICC .EQ. 1)
     &                 CC(J,I,K)=0.5*CC(J,I,K)
                  ELSEIF((LGRITER .EQ. 1 .AND. KITER .EQ. 1 .AND. 
     &                   KSTP .EQ. 1 .AND. KPER .EQ. 1) .OR. 
     &                   IUBCF .EQ. 0)THEN
                    IF(JEDGE + IEDGE .EQ. 1 ) CV(J,I,K)=0.5*CV(J,I,K)
                    IF(JEDGE + IEDGE .EQ. 2 ) CV(J,I,K)=0.25*CV(J,I,K)
                  ENDIF
                ENDIF
              ENDIF
            ENDDO  
          ENDDO   
        ENDIF
      ENDDO   
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.1 10OCTOBER2006 GWF2LGR1FMPBC      
      SUBROUTINE GWF2LGR1FMPBC(KPER,KSTP,KITER,LGRITER,IUBCF,NPCBEG,
     1                         NPRBEG,NPLBEG,NPCEND,NPREND,NPLEND,
     2                         IBOTFLG,PFLUX,LG)
C
C     ******************************************************************
C     ADJUST CONDUCTANCES ON BOUNDARIES OF PARENT MESH INTERFACE AND ADD
C     FLUXES FROM FEEDBACK (CALCULATED FROM PREVIOUS CHILD) TO RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LAYHDT,CR,CC,CV,RHS
      USE LGRMODULE,   ONLY:IBPFLG
      DIMENSION PFLUX(NPCBEG:NPCEND,NPRBEG:NPREND,NPLBEG:NPLEND)
C
C     ------------------------------------------------------------------
C
C1------IF ON THE VERY FIRST ITERATION, RETURN
      IF(LGRITER .EQ. 1 .AND. KSTP .EQ. 1 .AND. KPER .EQ. 1) RETURN
C2
C2------DEPENDING ON FLOW PACKAGE, FIND IF THERE IS AN UNCONFINED LAYER.
C2------IF ON THE FIRST SOLVER ITERATION, OR THE FIRST TRUE LGR ITERATION 
C2------OR IF THERE IS AN UNCONFINED LAYER, LOOP THROUGH IBOUND AND HALVE 
C2------THE CONDUCTANCES FOR INTERFACE CELLS.  IF AT AN EDGE CELL, THEN 
C2------THREE-QUARTER THE CONDUCTANCES.
      DO K=1,NLAY
        IF (LAYHDT(K) .EQ. 0 .AND. (LGRITER .NE. 2 .OR. KITER .NE. 1 
     &      .OR. KSTP .NE. 1 .OR. KPER .NE. 1))THEN
            CYCLE
        ELSE
          DO I=1,NROW
            DO J=1,NCOL
              JP1=J+1
              JM1=J-1
              IP1=I+1
              IM1=I-1
              KP1=K+1
              JEDGE=0
              IEDGE=0
              KEDGE=0
              ICR=0
              ICC=0
              IF(IBOUND(J,I,K).EQ.IBPFLG(LG))THEN
C2A
C2A-----CHECK NEIGHBOR IN THE +COL DIRECTION.  IF AN INTERFACE CELL,
C2A-----THEN HALVE CR AND SET ICR=1.  CHECK +/- ROW DIRECTION TO SEE
C2A-----IF THIS CELL IS ALONG AND EDGE.  IF SO, SET JEDGE=1.  ICR AND
C2A-----JEDGE ARE USED LATER TO ADJUST CONDUCTANCES OF EDGE CELLS.
                IF(JP1 .LE. NCOL)THEN
                  IF(IBOUND(JP1,I,K).EQ.IBPFLG(LG))THEN
                    CR(J,I,K)=0.5*CR(J,I,K)
                    ICR=1
                  ENDIF
                ENDIF
                IF(IM1 .GE. 1)THEN
                  IF(IBOUND(J,IM1,K).NE.IBPFLG(LG))JEDGE=1
                ENDIF
                IF(IP1 .LE. NROW)THEN
                  IF(IBOUND(J,IP1,K).NE.IBPFLG(LG))JEDGE=1
                ENDIF
C2B
                IF(IP1 .LE. NROW)THEN
                  IF(IBOUND(J,IP1,K).EQ.IBPFLG(LG))THEN
                    CC(J,I,K)=0.5*CC(J,I,K)
                    ICC=1
                  ENDIF
                ENDIF
                IF(JM1 .GE. 1)THEN
                  IF(IBOUND(JM1,I,K).NE.IBPFLG(LG))IEDGE=1
                ENDIF 
                IF(JP1 .LE. NCOL)THEN
                  IF(IBOUND(JP1,I,K).NE.IBPFLG(LG))IEDGE=1
                ENDIF 
C2C-----NOTE:  HARDWIRED FOR TOP LAYER BEING UNREFINED 
                IF(KP1 .LE. NLAY)THEN
                  IF(IBOUND(J,I,KP1).NE.IBPFLG(LG))KEDGE=1 
                ENDIF
C2D
C2D-----SKIP VERTICAL ADJUSTMENTS IF ONLY 1 LAYER MODEL
C2D-----CR AND CC ARE MULTIPED BY 3/2 (1.5) BECAUSE THEY WERE ALREADY 
C2D-----HALVED --> 1/2*3/2=3/4 WHICH IS WHAT IS NEEDED ALONG THE EDGES
C2D-----HALVE VERTICAL CONDUCTANCES ON THE FIRST ITERATION OR IF BCF
C2D-----IS NOT ACTIVE (IUBCF=0)...(BCF DOESN'T REFORMULATE CV)
                IF(NLAY .NE. 1)THEN
                  IF(KEDGE .EQ. 1)THEN
                    IF(JEDGE .EQ. 1 .AND. ICR .EQ. 1)
     &                  CR(J,I,K)=1.5*CR(J,I,K)
                    IF(IEDGE .EQ. 1 .AND. ICC .EQ. 1)
     &                  CC(J,I,K)=1.5*CC(J,I,K)
                  ELSEIF((LGRITER .EQ. 1 .AND. KITER .EQ. 1 .AND. 
     &                   KSTP .EQ. 1 .AND. KPER .EQ. 1) .OR. 
     &                   IUBCF .EQ. 0)THEN
                    IF(JEDGE+IEDGE .EQ. 1) CV(J,I,K)=0.5*CV(J,I,K)  
                    IF(JEDGE+IEDGE .EQ. 2) CV(J,I,K)=0.75*CV(J,I,K)
                  ENDIF
                ENDIF
              ENDIF
            
            ENDDO   
          ENDDO   
        ENDIF
      ENDDO   
C 
C3-----ADJUST RHS OF PARENT FOR THE FLUX FROM THE CHILD
C3-----SKIP IF ON THE FIRST LGR ITERATION OF THE FIRST TIME STEP
C3-----OF THE FIRST STRESS PERIOD BECAUSE IT IS A FULL PARENT GRID
      IF(KPER .NE. 1 .OR. KSTP .NE. 1 .OR. LGRITER .NE. 1) THEN
C
C3-----LOOP THROUGH INTERFACE LAYERS, ROWS, AND COLUMNS 
        DO IPLAY=NPLBEG,NPLEND
          DO I=1,NROW
            DO J=1,NCOL
              IF(IBOUND(J,I,IPLAY).EQ.IBPFLG(LG))
     &          RHS(J,I,IPLAY)=RHS(J,I,IPLAY)-PFLUX(J,I,IPLAY)
            ENDDO     
          ENDDO     
        ENDDO     
      ENDIF
C4
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 GWF2LGR1BH
      SUBROUTINE GWF2LGR1BH(KPER,KSTP,LGRITER,HNEWP,NCOLP,NROWP,NLAYP)
C     ******************************************************************
C     THIS ROUTINE TRANSFERS THE PARENT HEADS AT THE SHARED NODES TO THE
C     SHARED NODES OF THE CHILD GRID.  
C     IF ISHFLG .NE. 0 AND ON THE FIRST ITERATION OF THE FIRST TIME STEP 
C     OF THE FIRST STRESS PERIOD, THEN ALSO USE THE PARENT SOLUTION TO 
C     INTIALIZE THE INTERIOR SOLUTION OF THE CHILD GRID.
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,HNEW,IBOUND
      USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPCEND,NPREND,NPLEND,NCPP,NPL,
     1                      NCPPL,ISHFLG,IBOTFLG,IBFLG,NODEH,MXLGRITER,
     2                      HDIFFM,RELAXH
      DOUBLE PRECISION HNEWP(NCOLP,NROWP,NLAYP)
C     ------------------------------------------------------------------
C
      NCPP2=(NCPP+1)/2
C
C1------INITIALIZE MAXIMUM HEAD DIFFERENCE AND ITS LOCATION
      HDIFFM=0.
      NODEH(1)=1
      NODEH(2)=1
      NODEH(3)=1

C2------IF ON THE VERY FIRST ITERATION AND IF ISHFLG IS SET TO 1
C2------INITIALIZE WITH PARENT HEADS 
      IF(ISHFLG .EQ. 1 .AND. KPER .EQ. 1 .AND. KSTP .EQ. 1 .AND. 
     &   LGRITER .EQ. 1)THEN
C2A-----BIN PARENT HEADS ONTO THE ENTIRE CHILD GRID
        KCLAY=1
        DO KPLAY = 1,NPL
          KCEND=NCPPL(KPLAY)       
          IF(KPLAY .EQ. NPLEND .AND. IBOTFLG .EQ. 1)KCEND=(KCEND+1)/2
        
          ICR=1
          DO IPR = NPRBEG,NPREND
            JCC=1
            DO JPC = NPCBEG,NPCEND
              ICEND=NCPP
              IF(IPR .EQ. NPRBEG .OR. IPR .EQ. NPREND) ICEND=NCPP2

              DO K=1,KCEND
                KM1=K-1
                DO I=1,ICEND
                  IM1=I-1
                  JCEND=NCPP
                  IF(JPC .EQ. NPCBEG .OR. JPC .EQ. NPCEND) JCEND=NCPP2
                  DO J=1,JCEND
                    JM1=J-1
C-------BE CAREFUL NOT TO OVERWRITE CONSTANT HEADS UNLESS THEY ARE 
C-------ALONG THE BOUNDARY
                    IF(IBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1) .EQ. IBFLG .OR.
     &                 IBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1) .GT. 0)   
     &              HNEW(JCC+JM1,ICR+IM1,KCLAY+KM1)=HNEWP(JPC,IPR,KPLAY)
                  ENDDO
                ENDDO
              ENDDO

              JCC=JCC+JCEND
            ENDDO 
            ICR=ICR+ICEND
          ENDDO
          KCLAY=KCLAY+KCEND
        ENDDO
C
C3------NOT ON FIRST ITERATION, OR RUNNING 1-WAY COUPLED, SO ONLY 
C3------UPDATE SHARED NODES AND RELAX.  
C3------IF 1-WAY COUPLED OR FIRST LGR ITERATION, NO RELAXATION
      ELSE
        TMP=RELAXH
        IF(MXLGRITER .LE. 1 .OR. (LGRITER .EQ. 1 .AND. KSTP .EQ. 1
     &                            .AND. KPER .EQ. 1)) RELAXH=1.0
C
        DO KPLAY = 1,NPL
          KSHARE=(NCPPL(KPLAY) +1)/2
          IF(KPLAY .EQ. 1)THEN
            KCLAY=KSHARE
          ELSE
            KCLAY=KCLAY + (NCPPL(KPLAY-1) +1)/2 -1 + KSHARE  
          ENDIF
C3A-----LOOP ACROSS THE BACK FACE
          IPR = NPRBEG
          ICR=1
          JCC=1
          DO JPC = NPCBEG,NPCEND
            CALL SLGR1HRLX(NCOLP,NROWP,NLAYP,HNEWP,JCC,ICR,KCLAY,JPC,
     &                     IPR,KPLAY)
            JCC=JCC+NCPP
          ENDDO
C
C3B-----LOOP ACROSS THE RIGHT FACE - ALREADY HAVE UPPER RIGHT CORNER
          JPC = NPCEND
          JCC=NCOL
          ICR=1+NCPP
          DO IPR = NPRBEG+1,NPREND
            CALL SLGR1HRLX(NCOLP,NROWP,NLAYP,HNEWP,JCC,ICR,KCLAY,JPC,
     &                     IPR,KPLAY)
            ICR=ICR+NCPP
          ENDDO
C
C3C-----LOOP ACROSS THE FRONT FACE - ALREADY HAVE LOWER RIGHT CORNER
          IPR = NPREND
          ICR=NROW
          JCC=1
          DO JPC = NPCBEG,NPCEND-1
            CALL SLGR1HRLX(NCOLP,NROWP,NLAYP,HNEWP,JCC,ICR,KCLAY,JPC,
     &                     IPR,KPLAY)
            JCC=JCC+NCPP
          ENDDO
C
C3D-----LOOP ACROSS THE LEFT FACE - ALREADY HAVE ALL CORNERS
          JPC = NPCBEG
          JCC=1
          ICR=1+NCPP        
          DO IPR = NPRBEG+1,NPREND-1
            CALL SLGR1HRLX(NCOLP,NROWP,NLAYP,HNEWP,JCC,ICR,KCLAY,JPC,
     &                     IPR,KPLAY)
            ICR=ICR+NCPP
          ENDDO
        ENDDO
C
C3E-----LOOP ACROSS INTERIOR OF THE BOTTOM FACE IF NEEDED
        IF(IBOTFLG .EQ. 1)THEN
          KPLAY = NPLEND 
          KCLAY=NLAY
          ICR=1+NCPP 
          DO IPR = NPRBEG+1,NPREND-1
            JCC=1+NCPP 
            DO JPC = NPCBEG+1,NPCEND-1
              CALL SLGR1HRLX(NCOLP,NROWP,NLAYP,HNEWP,JCC,ICR,KCLAY,JPC,
     &                       IPR,KPLAY)
              JCC=JCC+NCPP
            ENDDO
            ICR=ICR+NCPP 
          ENDDO
        ENDIF
C4 
        RELAXH=TMP
      ENDIF
C5
      RETURN
      END
c***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SLGR1HRLX
      SUBROUTINE SLGR1HRLX(NCOLP,NROWP,NLAYP,HNEWP,JCC,ICR,KCLAY,JPC,
     1                     IPR,KPLAY)
C     ******************************************************************
C     APPLY RELAXATION ON HEADS FOR CHILD BOUNDARIES 
C     CHECK IF HEAD IS BELOW THE BOTTOM.  IF SO, STOP.
C     FIND MAXIMUM HEAD CHANGE AND ITS LOCATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:HNEW,BOTM,LAYHDT
      USE LGRMODULE,   ONLY:NODEH,RELAXH,HDIFFM,HOLDC  
      DOUBLE PRECISION HNEWP
      DIMENSION HNEWP(NCOLP,NROWP,NLAYP)
C     ------------------------------------------------------------------


C1------FIND HEAD AT SHARED NODE 
      HNEW(JCC,ICR,KCLAY) = DBLE(RELAXH)*HNEWP(JPC,IPR,KPLAY) + 
     &                   (1.0D0-DBLE(RELAXH))*DBLE(HOLDC(JCC,ICR,KCLAY))
C
C2------CHECK IF HEAD AT SHARED NODE IS BELOW BOTTOM
      IF(LAYHDT(KCLAY) .NE. 0 .AND. HNEW(JCC,ICR,KCLAY) .LT. 
     &          BOTM(JCC,ICR,KCLAY)) 
     &          CALL USTOP('HEAD BELOW BOTTOM AT SHARED NODE')
C
C3------NODEH IS THE LAYER, ROW, AND COLUMN OF THE MAXIMUM HEAD CHANGE
      HDIFF = HNEW(JCC,ICR,KCLAY)-HOLDC(JCC,ICR,KCLAY)
      IF(ABS(HDIFF) .GT. ABS(HDIFFM))THEN
        HDIFFM=HDIFF
        NODEH(1)=KCLAY
        NODEH(2)=ICR
        NODEH(3)=JCC
      ENDIF
C4
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 GWF2LGR1FMBF     
      SUBROUTINE GWF2LGR1FMBF(LGRITER,KPER,KSTP,IUBCF,IULPF,IUHUF)
C     ******************************************************************
C     CALCULATE THE CHILD INTERFACE FLUXES AND CREATE SPECIFIED
C     FLUX BOUNDARIES FOR PARENT GRID.
C     ADAPTED FROM SGWF2BCF7F
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LBOTM,BOTM,LAYHDS,
     1                      ISSFLG,HNEW,CR,CC,CV
      USE GWFBASMODULE,ONLY:DELT
      USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPLBEG,NPCEND,NPREND,NPLEND,
     1                      KPLC,IPLC,JPLC,IBSHRD,IBOTFLG,IBFLG,PFLUX,
     2                      PFLUXOLD,FDIFFM,RELAXF,NODEF,MXLGRITER  
      DOUBLE PRECISION HD, CD, HDIFF, DZERO,CHCH1, CHCH2, CHCH3, CHCH4, 
     1                 CHCH5, CHCH6
C     ------------------------------------------------------------------

C1------IF RUNNING 1-WAY COUPLED, DO NOT CALCULATE FLUXES
      IF(MXLGRITER .LE. 1) RETURN   

C2------INTIALIZE PARENT FLUX ARRAY
      ISS=ISSFLG(KPER)
      ONE=1.
      IF(ISS .EQ. 0) TLED=ONE/DELT         
      ZERO=0.
      DZERO=0.D0
      DO K=NPLBEG,NPLEND
        DO I=NPRBEG,NPREND
          DO J=NPCBEG,NPCEND
            PFLUX(J,I,K)=ZERO
          ENDDO
        ENDDO
      ENDDO

C3------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C3------INTERFACE CELL (IBOUND .EQ. IBFLG)
      IB = 0
      DO K=1,NLAY
        LAYFLG=LAYHDS(K)
        DO I=1,NROW
          DO J=1,NCOL
C
C4------IF CELL IS NOT AN INTERFACE CELL SKIP IT & GO ON TO NEXT CELL.
            IF (IBOUND(J,I,K) .NE. IBFLG) CYCLE
            IB = IB + 1
C
C5------SET MAPPING FROM CHILD CELL TO PARENT CELL AND SET ACCUMULATORS
C5------TO ZERO.
            KP=KPLC(IB)
            IP=IPLC(IB)
            JP=JPLC(IB)
C
            CHCH1=DZERO
            CHCH2=DZERO
            CHCH3=DZERO
            CHCH4=DZERO
            CHCH5=DZERO
            CHCH6=DZERO
C
C6------CALCULATE FLOW THROUGH THE LEFT FACE.
C6------COMMENTS A-B APPEAR ONLY IN THE SECTION HEADED BY COMMENT 5,
C6------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 6-10.
C
C6A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C6A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, OR TO AN
C6A-----ADJACENT NO-FLOW CELL. 
            IF(J.EQ.1) GO TO 30
            IF(IBOUND(J-1,I,K).EQ.0) GO TO 30 
C
C6B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
            HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
            CD=CR(J-1,I,K)
            CHCH1=HDIFF*CD
C
C7------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30       IF(J.EQ.NCOL) GO TO 60
            IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
            HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
            CD=CR(J,I,K)
            CHCH2=HDIFF*CD
C
C8------CALCULATE FLOW THROUGH THE BACK FACE.
   60       IF(I.EQ.1) GO TO 90
            IF(IBOUND(J,I-1,K).EQ.0) GO TO 90
            HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
            CD=CC(J,I-1,K)
            CHCH3=HDIFF*CD
C
C9------CALCULATE FLOW THROUGH THE FRONT FACE.
   90       IF(I.EQ.NROW) GO TO 120
            IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
            HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
            CD=CC(J,I,K)
            CHCH4=HDIFF*CD
C
C10-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120       IF(K.EQ.1) GO TO 150
            IF(IBOUND(J,I,K-1).EQ.0) GO TO 150
            HD=HNEW(J,I,K)
            IF(LAYFLG .EQ. 0) GO TO 122
            TMP=HD
            IF(TMP.LT.BOTM(J,I,LBOTM(K)-1)) HD=BOTM(J,I,LBOTM(K)-1)
  122       HDIFF=HD-HNEW(J,I,K-1)
            CD=CV(J,I,K-1)
            CHCH5=HDIFF*CD
C
C11-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150       IF(K.EQ.NLAY) GO TO 180
            IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
            HD=HNEW(J,I,K+1)
            IF(LAYHDS(K+1) .EQ. 0) GO TO 152
            TMP=HD
            IF(TMP.LT.BOTM(J,I,LBOTM(K+1)-1)) HD=BOTM(J,I,LBOTM(K+1)-1)
  152       HDIFF=HNEW(J,I,K)-HD
            CD=CV(J,I,K)
            CHCH6=HDIFF*CD
C
C12-----SUM THE FLOWS THROUGH SIX FACES OF THE INTERFACE CELL, AND
C12-----STORE SUM IN RATE.   
C12-----NOTE: RATE ACCUMULATOR IS REVERSED FROM STANDARD BECAUSE IT WILL
C12-----      BE USED ON THE PARENT SIDE (PARENT OUTFLOW = CHILD INFLOW)
 180        RATE=-CHCH1-CHCH2-CHCH3-CHCH4-CHCH5-CHCH6
C
C13-----IF TRANSIENT, ACCOUNT FOR FLUX TO/FROM STORAGE FOR SHARED NODES
            IF(ISS .EQ. 0)THEN
              IF(IBSHRD(IB) .EQ. 1)THEN
                IF(IUBCF .NE. 0) CALL SGWF2LGR1BCFSF(J,I,K,TLED,LAYFLG,
     1                                    STRG)
                IF(IULPF .NE. 0) CALL SGWF2LGR1LPFSF(J,I,K,TLED,LAYFLG,
     1                                    STRG)
                IF(IUHUF .NE. 0) CALL SGWF2LGR1HUFSF(J,I,K,TLED,LAYFLG,
     1                                    STRG)
C------ADD STORAGE CONTRIBUTION TO RATE
C------NOTE: RATE ACCUMULATED ALL INFLOWS TO THE CELL.  A NEGATIVE
C------      VALUE FOR STRG IS WATER GOING INTO STORAGE, WHICH IS LIKE
C------      AN OUTFLOW.  A POSITIVE VALUE IS WATER LEAVING STORAGE AND
C------      CONTRIBUTING TO THE CELL.
                 RATE=RATE+STRG
              ENDIF
            ENDIF
C
C14-----APPLY FLUX TO THE PARENT CELL 
            PFLUX(JP,IP,KP) = PFLUX(JP,IP,KP) + RATE
          ENDDO
        ENDDO
      ENDDO

C15-----APPLY RELAXATION.  SET RELAXATION TO 1.0 IF ON FIRST ITERATION
      TMP=RELAXF
      IF(LGRITER .EQ. 1 .AND. KSTP .EQ. 1 .AND. KPER .EQ. 1)RELAXF=1.0
      CALL SLGR1FRLX(RELAXF,PFLUX,PFLUXOLD,NPLBEG,NPRBEG,NPCBEG,
     &               NPLEND,NPREND,NPCEND,IBOTFLG,FDIFFM,NODEF)
      RELAXF=TMP
C16
      RETURN
      END
C***********************************************************************
C     VERSION 1.0 15FEBRUARY2006 SLGR1FRLX      
      SUBROUTINE SLGR1FRLX(RELAXF,PFLUX,PFLUXOLD,NPLBEG,NPRBEG,NPCBEG,
     1                     NPLEND,NPREND,NPCEND,IBOTFLG,FDIFFM,NODEF)
C     ******************************************************************
C     APPLY RELAXATION ON FLUXES FOR PARENT BOUNDARY CONDITION
C     STORE CURRENT FLUXES (PFLUX) IN OLD FLUXES (PFLUXOLD)
C     ******************************************************************
C
C        SPECIFICATIONS
C     ------------------------------------------------------------------
      DIMENSION PFLUX(NPCBEG:NPCEND,NPRBEG:NPREND,NPLBEG:NPLEND),
     &     PFLUXOLD(NPCBEG:NPCEND,NPRBEG:NPREND,NPLBEG:NPLEND),
     &     NODEF(3)
C     ------------------------------------------------------------------
C
C1------FIND THE MAX CHANGE IN BOUNDARY FLUX     
C1------INITIALIZE WITH UPPER LEFT CORNER
      IPLAY=1                   
      IRP = NPRBEG
      ICP = NPCBEG
      TMP=RELAXF*PFLUX(ICP,IRP,IPLAY)+(1.0-RELAXF)*
     &     PFLUXOLD(ICP,IRP,IPLAY)
      FDIFFM=(TMP-PFLUXOLD(ICP,IRP,IPLAY))/
     &        MAX(ABS(PFLUX(ICP,IRP,IPLAY)),1.0)
      NODEF(1)=IPLAY
      NODEF(2)=IRP
      NODEF(3)=ICP
C
C2-----LOOP THROUGH LAYERS READING THE ALONG EACH OF THE INTERFACES
      DO 100 IPLAY=NPLBEG,NPLEND
C2
C2A-----BACK FACE
        DO 110 ICP=NPCBEG,NPCEND
          PFLUX(ICP,IRP,IPLAY)=RELAXF*PFLUX(ICP,IRP,IPLAY)+(1.0-RELAXF)*
     &       PFLUXOLD(ICP,IRP,IPLAY)
          FDIFF=(PFLUX(ICP,IRP,IPLAY)-PFLUXOLD(ICP,IRP,IPLAY))/
     &       MAX(ABS(PFLUX(ICP,IRP,IPLAY)),1.0)
          IF(ABS(FDIFF) .GT. ABS(FDIFFM))THEN
            FDIFFM = FDIFF
            NODEF(1)=IPLAY
            NODEF(2)=IRP
            NODEF(3)=ICP
          ENDIF
          PFLUXOLD(ICP,IRP,IPLAY) = PFLUX(ICP,IRP,IPLAY)
 110    CONTINUE
C2
C2B-----RIGHT FACE
        ICP=NPCEND
        DO 120 IRP=NPRBEG+1,NPREND
          PFLUX(ICP,IRP,IPLAY)=RELAXF*PFLUX(ICP,IRP,IPLAY)+(1.0-RELAXF)*
     &       PFLUXOLD(ICP,IRP,IPLAY)
          FDIFF=(PFLUX(ICP,IRP,IPLAY)-PFLUXOLD(ICP,IRP,IPLAY))/
     &       MAX(ABS(PFLUX(ICP,IRP,IPLAY)),1.0)
          IF(ABS(FDIFF) .GT. ABS(FDIFFM))THEN
            FDIFFM = FDIFF
            NODEF(1)=IPLAY
            NODEF(2)=IRP
            NODEF(3)=ICP
          ENDIF
          PFLUXOLD(ICP,IRP,IPLAY) = PFLUX(ICP,IRP,IPLAY)
 120    CONTINUE
C2
C2C-----FRONT FACE
        IRP = NPREND
        DO 130 ICP=NPCBEG,NPCEND-1
          PFLUX(ICP,IRP,IPLAY)=RELAXF*PFLUX(ICP,IRP,IPLAY)+(1.0-RELAXF)*
     &       PFLUXOLD(ICP,IRP,IPLAY)
          FDIFF=(PFLUX(ICP,IRP,IPLAY)-PFLUXOLD(ICP,IRP,IPLAY))/
     &       MAX(ABS(PFLUX(ICP,IRP,IPLAY)),1.0)
          IF(ABS(FDIFF) .GT. ABS(FDIFFM))THEN
            FDIFFM = FDIFF
            NODEF(1)=IPLAY
            NODEF(2)=IRP
            NODEF(3)=ICP
          ENDIF
          PFLUXOLD(ICP,IRP,IPLAY) = PFLUX(ICP,IRP,IPLAY)
 130    CONTINUE
C2
C2D-----LEFT FACE
        ICP=NPCBEG
        DO 140 IRP=NPRBEG+1,NPREND-1
          PFLUX(ICP,IRP,IPLAY)=RELAXF*PFLUX(ICP,IRP,IPLAY)+(1.0-RELAXF)*
     &       PFLUXOLD(ICP,IRP,IPLAY)
          FDIFF=(PFLUX(ICP,IRP,IPLAY)-PFLUXOLD(ICP,IRP,IPLAY))/
     &       MAX(ABS(PFLUX(ICP,IRP,IPLAY)),1.0)
          IF(ABS(FDIFF) .GT. ABS(FDIFFM))THEN
            FDIFFM = FDIFF
            NODEF(1)=IPLAY
            NODEF(2)=IRP
            NODEF(3)=ICP
          ENDIF
          PFLUXOLD(ICP,IRP,IPLAY) = PFLUX(ICP,IRP,IPLAY)
 140    CONTINUE
 100  CONTINUE
C2
C2E----READ INTERIOR BOTTOM FACE IF IT IS REFINED
      IF(IBOTFLG .EQ. 1)THEN
        IPLAY = NPLEND
        DO 150 ICP = NPCBEG+1, NPCEND-1
          DO 155 IRP = NPRBEG+1,NPREND -1
            PFLUX(ICP,IRP,IPLAY)=RELAXF*PFLUX(ICP,IRP,IPLAY)+
     &         (1.0-RELAXF)*PFLUXOLD(ICP,IRP,IPLAY)
            FDIFF=(PFLUX(ICP,IRP,IPLAY)-PFLUXOLD(ICP,IRP,IPLAY))/
     &         MAX(ABS(PFLUX(ICP,IRP,IPLAY)),1.0)
            IF(ABS(FDIFF) .GT. ABS(FDIFFM))THEN
              FDIFFM = FDIFF
              NODEF(1)=IPLAY
              NODEF(2)=IRP
              NODEF(3)=ICP
            ENDIF
            PFLUXOLD(ICP,IRP,IPLAY) = PFLUX(ICP,IRP,IPLAY)
  155     CONTINUE
  150   CONTINUE
      ENDIF
C3      
      RETURN
      END
C***********************************************************************
C-----VERSION 1.1 13OCTOBER2006 GWF2LGR1CNVG
      SUBROUTINE GWF2LGR1CNVG(IGRID,NGRIDS,LGRCNVG,LGRITER,KPER,KSTP)
C     ******************************************************************
C     CHECK CONVERGENCE OF LGR
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:HNEW,IOUT
      USE LGRMODULE,   ONLY:MXLGRITER,IOUTLGR,HCLOSELGR,FCLOSELGR,
     1                      HDIFFM,FDIFFM,NODEH,NODEF,PFLUX
C     ------------------------------------------------------------------
  500 FORMAT(1X,'LGRITER= ',I3)
  501 FORMAT(1X,'GRID NUMBER= ',I3)
  505 FORMAT(1X,'HCLOSELGR= ',1P,E10.3,' HDIFFM= ', E10.3,2X,
     &          '(',I0,',',I0,',',I0,')',' HNEW= ', E10.3)
  510 FORMAT(1X,'FCLOSELGR= ',1PE10.3,' FDIFFM= ', E10.3,2X,
     &          '(',I0,',',I0,',',I0,')',' PFLUX= ',E10.3,/)
  515 FORMAT(1X,'MXLGRITER EXCEEDED FOR GRID NUMBER',I3,/,
     &          'CHECK BUDGET OF PARENT GRID TO ASSESS QUALITY OF THE ',
     &          'LGR SOLUTION',/)
C1------CHECK IF ALL GRIDS ARE RUNNING 1-WAY COUPLED.  IF SO, RETURN
      LGRCNVG = 1
      DO LG=2,NGRIDS
        CALL SGWF2LGR1PNT(LG)            
        IF(MXLGRITER .GT. 1)THEN
          LGRCNVG = 0
          EXIT 
        ENDIF
      ENDDO
      IF(LGRCNVG .EQ. 1) RETURN
C
C2------NOT RUNNING 1-WAY COUPLED SO LOOP THROUGH EACH GRID AND
C2------A: IF REQUESTED, WRITE CONVERGENCE OF LGR ITERATIONS  
C2------B: CHECK CONVERGENCE OF EACH GRID
C2------C: EXIT AND PRINT A MESSAGE IF MXLGRITER IS EXCEEDED
      LGRCNVG = 1
      IOFLG=0
      DO LG=2,NGRIDS
        CALL SGWF2LGR1PNT(LG)            
        CALL SGWF2BAS7PNT(LG)            
C2A
        IF(IOUTLGR .LT. 0 .AND. IOFLG .EQ. 0)THEN
          WRITE(*,500) LGRITER
          IOFLG=1
        ENDIF
        IF(IOUTLGR .LT. 0)THEN
          WRITE(*,501) LG
          WRITE(*,505) HCLOSELGR,HDIFFM,NODEH(1),NODEH(2),NODEH(3),
     &                 HNEW(NODEH(3),NODEH(2),NODEH(1))
          WRITE(*,510) FCLOSELGR,FDIFFM,NODEF(1),NODEF(2),NODEF(3),
     &                 PFLUX(NODEF(3),NODEF(2),NODEF(1))
        ELSEIF(IOUTLGR .GT. 0)THEN
          WRITE(IOUT,500) LGRITER
          WRITE(IOUT,505) HCLOSELGR,HDIFFM,NODEH(1),NODEH(2),NODEH(3),
     &                   HNEW(NODEH(3),NODEH(2),NODEH(1))
          WRITE(IOUT,510) FCLOSELGR,FDIFFM,NODEF(1),NODEF(2),NODEF(3),
     &                   PFLUX(NODEF(3),NODEF(2),NODEF(1))
        ENDIF
C2
C2B-----CHECK FOR CONVERGENCE IF NOT ON THE VERY FIRST LGR ITERATION 
C2B-----OF THE FIRST STRESS PERIOD OF THE FIRST TIME STEP. 
C2B-----(FORCE INITIAL LGR ITERATION)  
        IF(LGRITER .GT. 1 .OR. KPER .NE. 1 .OR. KSTP .NE. 1)THEN
          IF(ABS(HDIFFM) .GT. HCLOSELGR .OR. 
     &       ABS(FDIFFM) .GT. FCLOSELGR) LGRCNVG = 0
        ELSE
          LGRCNVG = 0
        ENDIF
C2 
C2C-----IF MXLGRITER IS EXCEEDED, END ITERATION AND PRINT MESSAGE
        IF(LGRITER .GE. MXLGRITER)THEN
          LGRCNVG = 2
          IF(IOUTLGR .LT. 0)WRITE(*,515) LG
          IF(IOUTLGR .GE. 0)WRITE(IOUT,515) LG
        ENDIF
      ENDDO
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.1 13OCTOBER2006 GWF2LGR1PBD
      SUBROUTINE GWF2LGR1PBD(KSTP,KPER,NGRIDS)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR PARENT FLUX B.C.  
C     THIS ROUTINE IS ADAPTED FROM GWF2WEL6BD
C     NOTE: NEED TO HANDLE THE JOINT PARENT/CHILD CASE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,NCOL,NROW,NLAY
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,DELT
      USE LGRMODULE,   ONLY:IUPBFSV,IUPBHSV,IBPFLG,LGRDAT
      CHARACTER(LEN=16):: TEXT
      DOUBLE PRECISION RATIN,RATOUT,QQ,DZERO
      DATA TEXT /'PARENT FLUX B.C.'/
C     ------------------------------------------------------------------
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS 
      DZERO=0.D0
      RATIN=DZERO
      RATOUT=DZERO
      VBVL(3,MSUM)= 0.
      VBVL(4,MSUM)= 0.
       
C2------LOOP THROUGH ALL SUBGRIDS
      DO LG=2,NGRIDS
C
C3------LOOP THROUGH EACH PARENT FLUX B.C. CALCULATING FLOW.
C3------SEE IF FLOW IS POSITIVE OR NEGATIVE.
C3------FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
C3------FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(IBOUND(J,I,K) .EQ. IBPFLG(LG))THEN
                Q=LGRDAT(LG)%PFLUX(J,I,K)
                QQ=Q
                IF(Q .GT.  0) RATIN=RATIN+QQ
                IF(Q .LT.  0) RATOUT=RATOUT-QQ
              ENDIF
            ENDDO
          ENDDO
        ENDDO

      ENDDO
C
C4------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=VBVL(3,MSUM)+RIN
      VBVL(4,MSUM)=VBVL(4,MSUM)+ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C5------INCREMENT BUDGET TERM COUNTER(MSUM) 
      MSUM=MSUM+1
C
C6------CALL ROUTINE TO OUTPUT PARENT B.C.'s FOR BFH IF REQUESTED
      IF(IUPBHSV .NE. 0 .OR. IUPBFSV .NE. 0)
     & CALL SGWF2LGR1BFHPOT(KSTP,KPER,NGRIDS)
C7------RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.1 10OCTOBER2006 SGWF2LGR1BFHPOT
      SUBROUTINE SGWF2LGR1BFHPOT(KSTP,KPER,NGRIDS)
C     ******************************************************************
C     OUTPUT PARENT GRID INTERFACE BOUNDARY FLUXES AND HEADS FOR BFH
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,NSTP,IBOUND,HNEW
      USE GWFBASMODULE,ONLY:TOTIM
      USE LGRMODULE,   ONLY:ISCHILD,IUPBHSV,IUPBFSV,IBPFLG,LGRDAT
      CHARACTER(LEN=17):: TEXTH,TEXTF,TEXTR
      DATA TEXTH /'PARENT HEAD CHECK'/
      DATA TEXTF /' PARENT FLUX B.C.'/
C     ------------------------------------------------------------------
C
C1------IF ON FIRST ITERATION, COUNT THE NUMBER OF INTERFACE CELLS, 
C1-------WRITE HEADER INFO, AND NODE LOCATIONS FOR INTERFACE CELLS
      IF(KPER .EQ. 1 .AND. KSTP .EQ. 1)THEN
        NBNODES = 0
        DO LG=2,NGRIDS
          NBNODES = NBNODES + COUNT(IBOUND .EQ. IBPFLG(LG)) 
        ENDDO
        TEXTR = 'SINGLE AREA' 
        IF(NGRIDS .GT. 2) TEXTR = 'MULTIPLE AREAS'
        IF(IUPBHSV .NE. 0)THEN 
          WRITE(IUPBHSV,200) TEXTR, NGRIDS
          WRITE(IUPBHSV,300) TEXTH,ISCHILD,NLAY,NROW,NCOL,SUM(NSTP),
     &                       NBNODES,NBNODES,IUPBHSV 
          WRITE(IUPBHSV,*) (IBPFLG(LG),LG=2,NGRIDS)
        ENDIF
        IF(IUPBFSV .NE. 0) THEN
          WRITE(IUPBFSV,200) TEXTR,NGRIDS
          WRITE(IUPBFSV,300) TEXTF,ISCHILD,NLAY,NROW,NCOL,SUM(NSTP),
     &                       NBNODES,NBNODES,IUPBHSV 
          WRITE(IUPBFSV,*) (IBPFLG(LG),LG=2,NGRIDS)
        ENDIF
C
        DO LG=2,NGRIDS
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                IF(IBOUND(J,I,K) .EQ. IBPFLG(LG))THEN
                  IF(IUPBHSV .NE. 0) WRITE(IUPBHSV,400) K,I,J,IBPFLG(LG)
                  IF(IUPBFSV .NE. 0) WRITE(IUPBFSV,400) K,I,J,IBPFLG(LG)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF   
C
C2------WRITE HEADER TIME STEP INFO
      IF(IUPBHSV .NE. 0) WRITE(IUPBHSV,500) KPER,KSTP,TOTIM
      IF(IUPBFSV .NE. 0) WRITE(IUPBFSV,500) KPER,KSTP,TOTIM
C
C3------LOOP THROUGH ALL CELLS.  IF A BOUNDARY INTERFACE CELL THEN WRITE
C3------BOUNDARY HEADS AND FLUXES IF DESIRED
      DO LG=2,NGRIDS
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(IBOUND(J,I,K) .EQ. IBPFLG(LG))THEN
                IF(IUPBHSV .NE. 0) WRITE(IUPBHSV,600) HNEW(J,I,K)
                IF(IUPBFSV .NE. 0) WRITE(IUPBFSV,600) 
     &                              LGRDAT(LG)%PFLUX(J,I,K)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
 200  FORMAT(1X,A,1X,I4)
 300  FORMAT(1X,A,8(I4,2X))
 400  FORMAT(1X,4(I4,2X)) 
 500  FORMAT(1X,'KPER=',I4,2x,'KSTP=',I4,2X,'TOTIM=',G14.7) 
 600  FORMAT(1X,G16.9) 
C
C4----RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 GWF2LGR1CBD
      SUBROUTINE GWF2LGR1CBD(KSTP,KPER,IGRID,NCOLP,NROWP,NLAYP,IBPARENT)
C     ******************************************************************
C     CALCULATE FLUXES FROM THE CHILD SPECIFIED HEAD BOUNDARIES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT
      USE GWFBASMODULE,ONLY:DELT,IBUDFL
      USE LGRMODULE,   ONLY:IBFLG,IUCBHSV,IUCBFSV,PFLUX,VCB,MXLGRITER
      DIMENSION IBPARENT(NCOLP,NROWP,NLAYP)
      CHARACTER(LEN=17):: VAL1,VAL2
C     ------------------------------------------------------------------
C
C1------IF RUNNING 1-WAY COUPLED, DO NOT OUTPUT BOUNDARY FLUXES
      IF(MXLGRITER .LE. 1) RETURN
C      
C2------CLEAR RATIN AND RATOUT ACCUMULATORS AND SET VALUES FOR PRINTING
      DZERO=0.D0
      RATIN=DZERO
      RATOUT=DZERO
      SMALL=0.1
      BIGVL1=9.99999E11
      ZERO=0.0
      IF(KPER .EQ. 1 .AND. KSTP .EQ. 1)THEN
        VCB(1) = ZERO
        VCB(2) = ZERO
      ENDIF
C
C3------LOOP THROUGH FLUX B.C. CALCULATING FLOW.
C3------SEE IF FLOW IS POSITIVE OR NEGATIVE.
C3------FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATOUT.
C3------FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATIN.
C3------NOTE: Q = -PFLUX  BECAUSE PFLUX WAS DEFINED WITH RESPECT
C3------TO THE PARENT GRID.  PARENT LOSS = CHILD GAIN     
      DO K=1,NLAYP
        DO I=1,NROWP
          DO J=1,NCOLP
            IF(IBPARENT(J,I,K) .EQ. -IBFLG)THEN 
              Q=-PFLUX(J,I,K)
              QQ=Q
              IF(Q .GT. 0) RATIN=RATIN+QQ
              IF(Q .LT. 0) RATOUT=RATOUT-QQ
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C4------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VCB(3)=RIN
      VCB(4)=ROUT
      VCB(1)=VCB(1)+RIN*DELT
      VCB(2)=VCB(2)+ROUT*DELT
C
C5------PRINT RATES TO OUTPUT FILE IF REQUESTED
      IF(IBUDFL .NE. 0)THEN
        WRITE(IOUT,260) KSTP, KPER
        WRITE(IOUT,265) 
C
C5A-----INFLOWS: CHECK MAGNITUDES FOR PRINT FORMATTING     
C5A-----TOTALS IN VAL1 AND RATES IN VAL2
        IF(VCB(1).NE.ZERO .AND.
     1      (VCB(1).GE.BIGVL1 .OR. VCB(1).LT.SMALL)) THEN
           WRITE(VAL1,'(1PE17.4)') VCB(1)
        ELSE
           WRITE(VAL1,'(F17.4)') VCB(1)
        END IF
        IF(VCB(3).NE.ZERO .AND.
     1      (VCB(3).GE.BIGVL1 .OR. VCB(3).LT.SMALL)) THEN
           WRITE(VAL2,'(1PE17.4)') VCB(3)
        ELSE
           WRITE(VAL2,'(F17.4)') VCB(3)
        END IF
        WRITE(IOUT,270) VAL1,VAL2
C
C5B-----OUTFLOWS: CHECK MAGNITUDES FOR PRINT FORMATTING     
C5B-----TOTALS IN vAL1 AND RATES IN VAL2
        IF(VCB(2).NE.ZERO .AND.
     1      (VCB(2).GE.BIGVL1 .OR. VCB(2).LT.SMALL)) THEN
           WRITE(VAL1,'(1PE17.4)') VCB(2)
        ELSE
           WRITE(VAL1,'(F17.4)') VCB(2)
        END IF
        IF(VCB(4).NE.ZERO .AND.
     1      (VCB(4).GE.BIGVL1 .OR. VCB(4).LT.SMALL)) THEN
           WRITE(VAL2,'(1PE17.4)') VCB(4)
        ELSE
           WRITE(VAL2,'(F17.4)') VCB(4)
        END IF
        WRITE(IOUT,280) VAL1,VAL2
      ENDIF

  260 FORMAT('1',/2X,'FLUX ACROSS PARENT-CHILD INTERFACE AT'
     1,' TIME STEP',I3,' IN STRESS PERIOD ',I4/2X,72('-'))
  265 FORMAT(1X,/5X,'CUMULATIVE VOLUMES',6X,'L**3',7X
     1,'RATES FOR THIS TIME STEP',6X,'L**3/T'/5X,18('-'),17X,24('-'))
  270 FORMAT(1X,/3X,'TOTAL IN TO CHILD =',A,5X,'TOTAL IN TO CHILD =',A)
  280 FORMAT(1X,/1X,'TOTAL OUT TO PARENT =',A,3X,'TOTAL OUT TO ',
     1              'PARENT =',A,/)
C
C6------CALL ROUTINE TO OUTPUT CHILD B.C.'s FOR BFH IF REQUESTED
      IF(IUCBHSV .NE. 0 .OR. IUCBFSV .NE. 0) CALL SGWF2LGR1BFHCOT(KSTP,
     &  KPER,NCOLP,NROWP,NLAYP,IBPARENT) 
C7----RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1BFHCOT
      SUBROUTINE SGWF2LGR1BFHCOT(KSTP,KPER,NCOLP,NROWP,NLAYP,IBPARENT) 
C     ******************************************************************
C     OUTPUT CHILD GRID INTERFACE BOUNDARY FLUXES AND HEADS FOR BFH 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,NSTP,IBOUND,HNEW
      USE GWFBASMODULE,ONLY:TOTIM
      USE LGRMODULE,   ONLY:ISCHILD,NCPP,IBOTFLG,IBFLG,IUCBHSV,IUCBFSV,
     &                      PFLUX,KPLC,IPLC,JPLC,NPINDX,IBSHRD 
      DIMENSION IBPARENT(NCOLP,NROWP,NLAYP)
      CHARACTER(LEN=17):: TEXTH,TEXTF
      DATA TEXTH /'  CHILD HEAD B.C.'/
      DATA TEXTF /' CHILD FLUX CHECK'/
C     ------------------------------------------------------------------
C
C1------IF ON FIRST ITERATION, COUNT THE NUMBER OF INTERFACE CELLS, 
C1------WRITE HEADER INFO, AND NODE LOCATIONS FOR INTERFACE CELLS
C1------NOTE THAT HEADS ARE ASSOCIATED WITH CHILD GRID INDICES WHILE 
C1------FLUXES ARE ASSOCIATED WITH PARENT GRID INDICES
      IF(KPER .EQ. 1 .AND. KSTP .EQ. 1)THEN
         NBNODES = COUNT(IBOUND .EQ. IBFLG) 
         NPBNODES = MAXVAL(NPINDX)
C2------CHECK IF SAVING BOUNDARY HEADS.
        IF(IUCBHSV .NE. 0)THEN 
          WRITE(IUCBHSV,300) TEXTH,ISCHILD,NLAY,NROW,NCOL,SUM(NSTP),
     &                      NBNODES,NPBNODES,NCPP,IBOTFLG,IBFLG,IUCBFSV 
          IB=0
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                IF(IBOUND(J,I,K) .EQ. IBFLG)THEN 
                  IB=IB+1
                  WRITE(IUCBHSV,400) K,I,J,KPLC(IB),IPLC(IB),JPLC(IB),
     &                               NPINDX(IB),IBSHRD(IB)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF   
C
C3------CHECK IF SAVING BOUNDARY FLOWS.
C3------WRITE CELL INDICES FOR CHILD GRID FLUXES AND THEIR CORESPONDING 
C3------LOCATIONS IN THE PARENT GRID.
        IF(IUCBFSV .NE. 0)THEN
          NPBNODES = MAXVAL(NPINDX)
          WRITE(IUCBFSV,300) TEXTF,ISCHILD,NLAYP,NROWP,NCOLP,SUM(NSTP),
     &                      NBNODES,NPBNODES,NCPP,IBOTFLG,IBFLG,IUCBFSV 
          IB=0
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                IF(IBOUND(J,I,K) .EQ. IBFLG) THEN
                  IB=IB+1
                  WRITE(IUCBFSV,400) K,I,J, KPLC(IB),IPLC(IB),JPLC(IB),
     &                              NPINDX(IB),IBSHRD(IB)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDIF   
C 
      ENDIF   !END FIRST TIME STEP OF FIRST STRESS PERIOD 
C
C4------IF BOUNDARY HEADS ARE BEING SAVED, WRITE HEADER TIME STEP INFO
C4------LOOP THROUGH ALL CELLS.  IF A BOUNDARY INTERFACE CELL THEN 
C4------WRITE BOUNDARY HEAD
      IF(IUCBHSV .NE. 0)THEN
        WRITE(IUCBHSV,500) KPER,KSTP,TOTIM 
        DO K=1,NLAY
          DO I=1,NROW
            DO J=1,NCOL
              IF(IBOUND(J,I,K) .EQ. IBFLG) 
     &           WRITE(IUCBHSV,600) HNEW(J,I,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C5------IF BOUNDARY FLUX IS BEING SAVED, WRITE HEADER TIME STEP INFO
C5------LOOP THROUGH ALL CELLS.  IF A BOUNDARY INTERFACE CELL THEN 
C5------WRITE BOUNDARY FLUX.
C5------NOTE: THESE ARE WRITTEN IN TERMS OF PARENT GRID INDICES 
C5------THE SIGN IS REVERSED BECAUSE PFLUX WAS DEFINED IN TERMS OF THE
C5------PARENT BUT WE WANT FLUX FROM THE PERSPECTIVE OF THE CHILD.
      IF(IUCBFSV .NE. 0)THEN
        WRITE(IUCBFSV,500) KPER,KSTP,TOTIM 
        DO K=1,NLAYP
          DO I=1,NROWP
            DO J=1,NCOLP
              IF(IBPARENT(J,I,K) .EQ. -IBFLG)
     &            WRITE(IUCBFSV,600) -PFLUX(J,I,K)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
 300  FORMAT(1X,A,5(I4,2X),2(I6,2x),4(I4,2X)) 
 400  FORMAT(1X,8(I4,2X)) 
 500  FORMAT(1X,'KPER=',I4,2x,'KSTP=',I4,2X,'TOTIM=',G14.7) 
 600  FORMAT(1X,G16.9) 
C
C6----RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1BCFSA
      SUBROUTINE SGWF2LGR1BCFSA(ISPARENT,J,I,K,JEDGE,IEDGE,KEDGE,
     1                          IFLGSC2)
C     ******************************************************************
C     ADJUST BCF STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBCFMODULE,      ONLY:SC1,SC2
C     ------------------------------------------------------------------
C      
C1------CHECK IF THIS IS A PARENT OR CHILD MODEL AND ADJUST STORAGE 
C1------ACCORDINGLY.  CHECK IF SECONDARY STORAGE NEEDS ADJUSTMENT
      IF(ISPARENT .EQ. 0)THEN      
        SC1(J,I,K)=0.5**(JEDGE+IEDGE+KEDGE)*SC1(J,I,K)
        IF(IFLGSC2 .NE. 0)
     &            SC2(J,I,K)=0.5**(JEDGE+IEDGE+KEDGE)*SC2(J,I,K) 
C PARENT ADJUSTMENT
      ELSE
        SC1(J,I,K)=(1.0 - 0.5**(JEDGE+IEDGE+KEDGE))*SC1(J,I,K)
        IF(IFLGSC2 .NE. 0)
     &            SC2(J,I,K)=(1.0 - 0.5**(JEDGE+IEDGE+KEDGE))*SC2(J,I,K)
      ENDIF
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1LPFSA
      SUBROUTINE SGWF2LGR1LPFSA(ISPARENT,J,I,K,JEDGE,IEDGE,KEDGE,
     1                          IFLGSC2)
C     ******************************************************************
C     ADJUST LPF STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLPFMODULE,      ONLY:SC1,SC2
C     ------------------------------------------------------------------
C      
C1------CHECK IF THIS IS A PARENT OR CHILD MODEL AND ADJUST STORAGE 
C1------ACCORDINGLY.  CHECK IF SECONDARY STORAGE NEEDS ADJUSTMENT
      IF(ISPARENT .EQ. 0)THEN      
        SC1(J,I,K)=0.5**(JEDGE+IEDGE+KEDGE)*SC1(J,I,K)
        IF(IFLGSC2 .NE. 0)
     &            SC2(J,I,K)=0.5**(JEDGE+IEDGE+KEDGE)*SC2(J,I,K) 
C PARENT ADJUSTMENT
      ELSE
        SC1(J,I,K)=(1.0 - 0.5**(JEDGE+IEDGE+KEDGE))*SC1(J,I,K)
        IF(IFLGSC2 .NE. 0)
     &            SC2(J,I,K)=(1.0 - 0.5**(JEDGE+IEDGE+KEDGE))*SC2(J,I,K)
      ENDIF
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1HUFSA
      SUBROUTINE SGWF2LGR1HUFSA(ISPARENT,J,I,K,JEDGE,IEDGE,KEDGE)
C     ******************************************************************
C     ADJUST HUF STORAGE COEFFICIENT ALONG INTERFACING BOUNDARY 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE,      ONLY:SC1
C     ------------------------------------------------------------------
C      
C1------CHECK IF THIS IS A PARENT OR CHILD MODEL AND ADJUST STORAGE 
C1------ACCORDINGLY. 
      IF(ISPARENT .EQ. 0)THEN      
        SC1(J,I,K)=0.5**(JEDGE+IEDGE+KEDGE)*SC1(J,I,K)
C PARENT ADJUSTMENT
      ELSE
        SC1(J,I,K)=(1.0 - 0.5**(JEDGE+IEDGE+KEDGE))*SC1(J,I,K)
      ENDIF
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1BCFSF
      SUBROUTINE SGWF2LGR1BCFSF(J,I,K,TLED,LAYFLG,STRG)
C     ******************************************************************
C     CALCULATE FLOW IN/OUT OF STORAGE FOR THE BCF PACKAGE 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,            ONLY:HNEW,HOLD,BOTM,LBOTM
      USE GWFBCFMODULE,      ONLY:SC1,SC2
C     ------------------------------------------------------------------
C      
      HSING=HNEW(J,I,K)
C1------CHECK LAYER TO SEE IF ONE STORAGE CAPACITY OR TWO
      IF(LAYFLG .EQ. 1)THEN
        TP=BOTM(J,I,LBOTM(K)-1)
        RHO2=SC2(J,I,K)*TLED
        RHO1=SC1(J,I,K)*TLED
        SOLD=RHO2
        IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
        SNEW=RHO2
        IF(HSING.GT.TP) SNEW=RHO1
        STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
      ELSE 
        RHO=SC1(J,I,K)*TLED
        STRG=RHO*HOLD(J,I,K) - RHO*HSING
      ENDIF
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1LPFSF
      SUBROUTINE SGWF2LGR1LPFSF(J,I,K,TLED,LAYFLG,STRG)
C     ******************************************************************
C     CALCULATE FLOW IN/OUT OF STORAGE FOR THE LPF PACKAGE 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,            ONLY:HNEW,HOLD,BOTM,LBOTM
      USE GWFLPFMODULE,      ONLY:SC1,SC2
C     ------------------------------------------------------------------
C      
      HSING=HNEW(J,I,K)
C1------CHECK LAYER TO SEE IF ONE STORAGE CAPACITY OR TWO
      IF(LAYFLG .EQ. 1)THEN
        TP=BOTM(J,I,LBOTM(K)-1)
        RHO2=SC2(J,I,K)*TLED
        RHO1=SC1(J,I,K)*TLED
        SOLD=RHO2
        IF(HOLD(J,I,K).GT.TP) SOLD=RHO1
        SNEW=RHO2
        IF(HSING.GT.TP) SNEW=RHO1
        STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
      ELSE 
        RHO=SC1(J,I,K)*TLED
        STRG=RHO*HOLD(J,I,K) - RHO*HSING
      ENDIF
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2LGR1HUFSF
      SUBROUTINE SGWF2LGR1HUFSF(J,I,K,TLED,LAYFLG,STRG)
C     ******************************************************************
C     CALCULATE FLOW IN/OUT OF STORAGE FOR THE HUF PACKAGE 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,            ONLY:HNEW,HOLD,BOTM,LBOTM,NCOL,NROW,DELR,
     1                            DELC,IOUT
      USE GWFHUFMODULE,      ONLY:NHUF,SC1,HUFTHK
      DOUBLE PRECISION HN
C     ------------------------------------------------------------------
C1-----INITIALIZE VARIABLES     
      HN=HNEW(J,I,K)
      HO=HOLD(J,I,K)
      TOP=BOTM(J,I,LBOTM(K)-1)
      BOT=BOTM(J,I,LBOTM(K))
      STRG=0.
C
C2-----CHECK LAYER TYPE AND HEAD VALUES TO SEE IF ONE STORAGE 
c2-----CAPACITY OR TWO.
      IF(LAYFLG.EQ.0 .OR. (HO .GT. TOP .AND. HN .GT. TOP))THEN 
        RHO=SC1(J,I,K)*TLED
        STRG=RHO*(HO-HN)
      ELSE
C
C2A----TWO STORAGE CAPACITIES.
C---------------Compute SC1 Component
        IF(HO.GT.TOP) THEN
          STRG=SC1(J,I,K)*(HO-TOP)*TLED
        ELSEIF(HN.GT.TOP) THEN
          STRG=SC1(J,I,K)*TLED*(TOP-HN)
        ENDIF
C---------------Compute SC2 Component
        CALL SGWF2HUF7SC2(1,J,I,K,TOP,BOT,HN,HO,TLED,CHCOF,STRG,
     &           HUFTHK,NCOL,NROW,NHUF,DELR(J)*DELC(I),IOUT)
C
      ENDIF
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2IBSHARE1 
      SUBROUTINE SGWF2IBSHARE1(IBOUND,NROW,NCOL,NLAY,CC,CR,CV,NCPP,
     &                         NPL,NCPPL,IBOTFLG,IBFLG) 
C     ******************************************************************
C     CHANGE IBOUND ARRAY SO THAT ONLY CELLS DIRECTLY BETWEEN SHARED 
C     NODES (INCLUDING SHARED NODES, WHICH ARE CONSTANT HEADS) ARE 
C     ACTIVE SO THAT WE CAN FIND A HEAD SOLUTION THAT WILL BE CONSISTENT
C     WITH THE DARCY OR DUPUIT INTERPOLATION BETWEEN SHARED NODES.  THIS 
C     REQUIRES ZERO-ING OUT THE INTERFACE NODES THAT ARE NOT DIRECTLY 
C     BETWEEN THE SHARED NODES AS WELL AS THE INTERIOR NODES. 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IBOUND(NCOL,NROW,NLAY),NCPPL(NPL),CC(NCOL,NROW,NLAY),
     &          CR(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY)   
C     ------------------------------------------------------------------

      KSHARE1=(NCPPL(1) +1)/2
      IZERO=0
      ZERO=0.
C1----FIRST SET ALL CELLS AS INACTIVE CELLS
      DO K=1,NLAY
        DO I=1,NROW
          DO J=1,NCOL
            IBOUND(J,I,K)=IZERO
          ENDDO
        ENDDO
      ENDDO

C NOTE:  THIS IS HARDWIRED FOR THE TOP LAYER OF THE CHILD NOT BEING 
C        CONNECTED TO THE PARENT (ie, TOP OF CHILD IS TOP OF MODEL)
C2
C2------LOOP AROUND EXTERIOR AND SET IBOUND FOR SHARED NODES TO IBFLG
C2------AND CONNECTIONS BETWEEN SHARED NODES TO -IBFLG AND ALL OTHERS 
C2------TO 0 FOR FIRST CAGE SOLUTION.  NOTE: IBFLAG IS NEGATIVE ON INPUT
C2
C2------LOOP THROUGH LAYERS OF SHARED NODES MOVING ALONG EACH OF THE 
C2------INTERFACES
C2      
      DO 200 KK=1,MAX(1,NPL)
        KSHARE=(NCPPL(KK) +1)/2
        IF(KK .EQ. 1)THEN
          K=KSHARE
        ELSE
          K=K + (NCPPL(KK-1) +1)/2 -1  + KSHARE  
        ENDIF
C2A-----BACK FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        IC=1
        DO 210 J=1,NCOL
          IF(MOD(J-1,NCPP) .EQ. 0)THEN
            IF(K .EQ. KSHARE1)THEN
              DO 205 KC=1,NLAY
                IBOUND(J,IC,KC)=-IBFLG
  205         CONTINUE
            ENDIF
            IBOUND(J,IC,K)=IBFLG
          ELSE
            IBOUND(J,IC,K)=-IBFLG
          ENDIF
  210   CONTINUE
C
C2B-----RIGHT FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        JC=NCOL
        DO 220 I=1,NROW
          IF(MOD(I-1,NCPP) .EQ. 0)THEN
            IF(K .EQ. KSHARE1)THEN
              DO 215 KC=1,NLAY
                IBOUND(JC,I,KC)=-IBFLG
  215         CONTINUE
            ENDIF
            IBOUND(JC,I,K)=IBFLG
          ELSE
            IBOUND(JC,I,K)=-IBFLG
          ENDIF
  220   CONTINUE
C
C2C-----FRONT FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        IC=NROW
        DO 230 J=1,NCOL
          IF(MOD(J-1,NCPP) .EQ. 0)THEN
            IF(K .EQ. KSHARE1)THEN
              DO 225 KC=1,NLAY
                IBOUND(J,IC,KC)=-IBFLG
  225         CONTINUE
            ENDIF
            IBOUND(J,IC,K)=IBFLG
          ELSE
            IBOUND(J,IC,K)=-IBFLG
          ENDIF
  230   CONTINUE
C
C2D-----LEFT FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        JC=1
        DO 240 I=1,NROW
          IF(MOD(I-1,NCPP) .EQ. 0)THEN
            IF(K .EQ. KSHARE1)THEN
              DO 235 KC=1,NLAY
                IBOUND(JC,I,KC)=-IBFLG
  235         CONTINUE
            ENDIF
            IBOUND(JC,I,K)=IBFLG
          ELSE
            IBOUND(JC,I,K)=-IBFLG
          ENDIF
  240   CONTINUE
C2
  200 CONTINUE
C
C3------INTERIOR BOTTOM FACE IF NEEDED 
      IF(IBOTFLG .EQ. 1)THEN
        K=NLAY
C3A-----BOTTOM FACE - MOVE ALONG COLUMNS
        DO 300 I=1,NROW, NCPP
          DO 310 J=1,NCOL
            IF(MOD(J-1, NCPP) .EQ. 0)THEN
              IBOUND(J,I,K)=IBFLG
            ELSE
              IBOUND(J,I,K)=-IBFLG
            ENDIF
  310     CONTINUE
  300   CONTINUE
C
C3B-----BOTTOM FACE - MOVE DOWN ROWS
        DO 320 I=1,NROW
          DO 330 J=1,NCOL,NCPP
            IF(MOD(I-1, NCPP) .EQ. 0)THEN
              IBOUND(J,I,K)=IBFLG
            ELSE
              IBOUND(J,I,K)=-IBFLG
            ENDIF
  330     CONTINUE
  320   CONTINUE
      ENDIF
        
C4
C4A-----SET CONDUCTANCES TO ZERO DEPENDING ON IBOUND AND ADJACENT 
C4A-----CC,CR, AND CV CONNECTIONS
      DO 400 K=1, NLAY
        DO 410 I=1, NROW
          DO 420 J=1, NCOL
            IF(IBOUND(J,I,K) .EQ. 0)THEN
              CC(J,I,K)=ZERO
              CR(J,I,K)=ZERO
              IF(NLAY .NE. 1)CV(J,I,K)=ZERO
            ELSE
              IF(J .NE. NCOL)THEN
                IF(IBOUND(J+1,I,K) .EQ. 0) CR(J,I,K)=ZERO 
              ENDIF
              IF(I .NE. NROW)THEN
                IF(IBOUND(J,I+1,K) .EQ. 0) CC(J,I,K)=ZERO 
              ENDIF
              IF(K .NE. NLAY)THEN
                IF(IBOUND(J,I,K+1) .EQ. 0) CV(J,I,K)=ZERO 
              ENDIF
            ENDIF
  420     CONTINUE
  410   CONTINUE
  400 CONTINUE
C5----RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY2006 SGWF2IBSHARE2
      SUBROUTINE SGWF2IBSHARE2(IBOUND,NROW,NCOL,NLAY,CC,CR,CV,NCPP,
     &                         NPL,NCPPL,IBOTFLG,IBFLG)
C     ******************************************************************
C     CHANGE IBOUND ARRAY SO THAT ONLY CELLS ALONG THE INTERFACE ARE
C     NODES ACTIVE (NODES BETWEEN SHARED NODES ARE ALSO CONSTANT HEADS) 
C     SO THAT WE CAN SOLVE FOR THE SHELL.  THIS REQUIRES ZERO-ING OUT 
C     THE INTERIOR. 
C     NOTE:  LAYERS ABOVE THE FIRST SHARED NODE ARE ALSO ADJUSTED.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IBOUND(NCOL,NROW,NLAY),NCPPL(NPL),CC(NCOL,NROW,NLAY),
     &          CR(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY)   
C     ------------------------------------------------------------------
C
      KSHARE1=(NCPPL(1) +1)/2
      ZERO=0.
C1----FIRST SET ALL CELLS AS ACTIVE INTERFACE CELLS
      DO 100 K=1,NLAY
        DO 110 I=1,NROW
          DO 120 J=1,NCOL
            IF(ABS(IBOUND(J,I,K)) .EQ. -IBFLG) IBOUND(J,I,K)=-IBFLG
  120     CONTINUE
  110   CONTINUE
  100 CONTINUE

C NOTE:  THIS IS HARDWIRED FOR THE TOP LAYER OF THE CHILD NOT BEING 
C        CONNECTED TO THE PARENT (ie, TOP OF CHILD IS TOP OF MODEL)
C        ALSO, WE ARE LOOPING THROUGH ALL THE CELLS INSTEAD OF JUST THE
C        INTERFACE.  THIS IS OKAY BECAUSE THE INTERIOR GETS SET TO ZERO

C2A-----SET INTERIOR VERTICAL CONDUCTANCES TO ZERO
      KMAX =MAX(1,NLAY-1)
      IF(IBOTFLG .EQ. 0) KMAX = NLAY
      DO 200 K=1, KMAX
        DO 210 I=MIN(2, NROW), MAX(1, NROW-1)
          DO 220 J=MIN(2, NCOL), MAX(1, NCOL-1)
            IBOUND(J,I,K)=0
            CC(J,I,K)=ZERO
            CR(J,I,K)=ZERO
            IF(K .NE. NLAY)CV(J,I,K)=ZERO
  220     CONTINUE
  210   CONTINUE
  200 CONTINUE
C
C2B-----SET INTERIOR COLUMN CONDUCTANCES TO ZERO
      DO 300 K=1, KMAX
        DO 310 J=2, NCOL-1
          CC(J,1,K)=ZERO
          CC(J,NROW,K)=ZERO   
  310   CONTINUE
  300 CONTINUE
C
C2C-----SET INTERIOR ROW CONDUCTANCES TO ZERO
      DO 320 K=1, KMAX
        DO 330 I=2, NROW-1
          CR(1,I,K)=ZERO
          CR(NCOL,I,K)=ZERO   
  330   CONTINUE
  320 CONTINUE
C
C3------LOOP AROUND EXTERIOR AND SET IBOUND FOR SHARED NODES AND 
C3------CONNECTIONS BETWEEN SHARED NODES FOR SHELL SOLUTION.
C3
C3------LOOP THROUGH LAYERS OF SHARED NODES MOVING ALONG EACH OF THE 
c3------INTERFACES
C3      
      DO 400 KK=1,MAX(1,NPL)
        KSHARE=(NCPPL(KK) +1)/2
        IF(KK .EQ. 1)THEN
          K=KSHARE
        ELSE
          K=K + (NCPPL(KK-1) +1)/2 -1  + KSHARE  
        ENDIF
C
C3A-----BACK FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        IC=1
        DO 410 J=1,NCOL
          IF(K .EQ. KSHARE1)THEN
            IF(MOD(J-1,NCPP) .EQ. 0)THEN
              DO 405 KC=1,NLAY
                IBOUND(J,IC,KC)=IBFLG
  405         CONTINUE
            ENDIF
          ENDIF
          IBOUND(J,IC,K)=IBFLG
  410   CONTINUE
C
C3B-----RIGHT FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        JC=NCOL
        DO 420 I=1,NROW
          IF(K .EQ. KSHARE1)THEN
            IF(MOD(I-1,NCPP) .EQ. 0)THEN 
              DO 415 KC=1,NLAY
                IBOUND(JC,I,KC)=IBFLG
  415         CONTINUE
            ENDIF
          ENDIF 
          IBOUND(JC,I,K)=IBFLG
  420   CONTINUE
C
C3C-----FRONT FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        IC=NROW
        DO 430 J=1,NCOL
          IF(K .EQ. KSHARE1)THEN
            IF(MOD(J-1,NCPP) .EQ. 0)THEN
              DO 425 KC=1,NLAY
                IBOUND(J,IC,KC)=IBFLG
  425         CONTINUE
            ENDIF
          ENDIF
          IBOUND(J,IC,K)=IBFLG
  430   CONTINUE
C
C3D-----LEFT FACE. FILL IN VERTICAL SHARED NODES IF ON THE FIRST LAYER
        JC=1
        DO 440 I=1,NROW
          IF(K .EQ. KSHARE1)THEN
            IF(MOD(I-1,NCPP) .EQ. 0)THEN
              DO 435 KC=1,NLAY
                IBOUND(JC,I,KC)=IBFLG
  435         CONTINUE
            ENDIF
          ENDIF
          IBOUND(JC,I,K)=IBFLG
  440   CONTINUE
C3
  400 CONTINUE
C
C4------INTERIOR BOTTOM FACE IF NEEDED
      IF(IBOTFLG .EQ. 1)THEN
        K=NLAY
C4A-----BOTTOM FACE - MOVE ALONG COLUMNS
        DO 500 I=1,NROW, NCPP
          DO 510 J=1,NCOL
            IBOUND(J,I,K)=IBFLG
  510     CONTINUE
  500   CONTINUE
C
C4B-----BOTTOM FACE - MOVE DOWN ROWS
        DO 520 I=1,NROW
          DO 530 J=1,NCOL,NCPP
            IBOUND(J,I,K)=IBFLG
  530     CONTINUE
  520   CONTINUE
      ENDIF
C5----RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 20JULY2009 SGWF2LGR1INTERIOR
      SUBROUTINE SGWF2LGR1INTERIOR(IGRID,NGRIDS,J,I,LINT)
C     ******************************************************************
C     DETERMINE IF THE CELL INDEX IS INSIDE OF THE PARENT AREA THAT IS
C     COVERED BY A CHILD.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE LGRMODULE,   ONLY:LGRDAT
      INTEGER LINT
C     ------------------------------------------------------------------
C1------INITIALIZE LINT TO ZERO.  
      LINT = 0
C
C2------SEARCH THROUGH ALL SUBGRIDS AND DETERMINE IF CELL IS INSIDE AN 
C2------AREA COVERED BY A CHILD.  IF SO, SET LINT=1 AND EXIT SEARCH. 
C NOTE: WILL NOT WORK FOR IRREGULARLY SHAPED AREAS
      DO LG = IGRID+1,NGRIDS
        IF (I .GT. LGRDAT(LG)%NPRBEG .AND. I .LT. LGRDAT(LG)%NPREND 
     &     .AND. 
     &     J .GT. LGRDAT(LG)%NPCBEG .AND. J .LT. LGRDAT(LG)%NPCEND) THEN
          LINT = 1
          EXIT
        ENDIF
      END DO
C
C3------RETURN
      RETURN
      END
