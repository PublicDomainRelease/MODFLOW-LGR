      MODULE GWFBFHMODULE
         INTEGER,          SAVE, POINTER ::ISCHILD,IBOTFLG,IBFLG,NCPP 
         INTEGER,          SAVE, POINTER ::NBNODES,NPBNODES,NTIMES
         INTEGER,          SAVE, POINTER ::NPLBEG,NPRBEG,NPCBEG
         INTEGER,          SAVE, POINTER ::NPLEND,NPREND,NPCEND
         INTEGER,          SAVE, POINTER ::IUBC
         CHARACTER(LEN=17),SAVE, POINTER ::BTEXT
         INTEGER,          SAVE, POINTER, DIMENSION(:) ::KLAY
         INTEGER,          SAVE, POINTER, DIMENSION(:) ::IROW
         INTEGER,          SAVE, POINTER, DIMENSION(:) ::JCOL 
         INTEGER,          SAVE, POINTER, DIMENSION(:) ::KPLAY 
         INTEGER,          SAVE, POINTER, DIMENSION(:) ::IPROW 
         INTEGER,          SAVE, POINTER, DIMENSION(:) ::JPCOL 
         INTEGER,          SAVE, POINTER, DIMENSION(:) ::NPINDX
         REAL,             SAVE, POINTER, DIMENSION(:) ::BFLUX
         REAL,             SAVE, POINTER, DIMENSION(:) ::BFLUXCHK
         REAL,             SAVE, POINTER, DIMENSION(:) ::BHEAD
         REAL,             SAVE, POINTER, DIMENSION(:) ::BHEADCHK
         REAL,             SAVE, POINTER, DIMENSION(:) ::VCB
        TYPE BFHTYPE
         INTEGER,           POINTER ::ISCHILD,IBOTFLG,IBFLG,NCPP 
         INTEGER,           POINTER ::NBNODES,NPBNODES,NTIMES
         INTEGER,           POINTER ::NPLBEG,NPRBEG,NPCBEG
         INTEGER,           POINTER ::NPLEND,NPREND,NPCEND
         INTEGER,           POINTER ::IUBC
         CHARACTER(LEN=17), POINTER ::BTEXT
         INTEGER,           POINTER, DIMENSION(:) ::KLAY
         INTEGER,           POINTER, DIMENSION(:) ::IROW
         INTEGER,           POINTER, DIMENSION(:) ::JCOL
         INTEGER,           POINTER, DIMENSION(:) ::KPLAY 
         INTEGER,           POINTER, DIMENSION(:) ::IPROW 
         INTEGER,           POINTER, DIMENSION(:) ::JPCOL 
         INTEGER,           POINTER, DIMENSION(:) ::NPINDX
         REAL,              POINTER, DIMENSION(:) ::BFLUX
         REAL,              POINTER, DIMENSION(:) ::BFLUXCHK
         REAL,              POINTER, DIMENSION(:) ::BHEAD
         REAL,              POINTER, DIMENSION(:) ::BHEADCHK
         REAL,              POINTER, DIMENSION(:) ::VCB
        END TYPE
        TYPE(BFHTYPE), SAVE  ::BFHDAT(10)
      END MODULE GWFBFHMODULE

C-----VERSION 1.0 15FEBRUARY2006 GWF2BFH1AR      
      SUBROUTINE GWF2BFH1AR(IN,ILGR,IGRID)
C     ******************************************************************
C     ALLOCATE SPACE FOR BOUNDARY FLOW AND HEAD PACKAGE AND READ DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT
      USE GWFBFHMODULE,ONLY:ISCHILD,IBOTFLG,IBFLG,NBNODES,NPBNODES,
     1                      NTIMES,NPLBEG,NPRBEG,NPCBEG,NPLEND,NPREND,
     2                      NPCEND,IUBC,NCPP,BTEXT,KLAY,IROW,JCOL,KPLAY,
     3                      IPROW,JPCOL,NPINDX,BFLUX,BHEAD,BFLUXCHK,
     4                      BHEADCHK,VCB 
C
      LOGICAL             ::LOP
      CHARACTER*200 LINE
      CHARACTER(LEN=17)   ::CTEXTH,PTEXTF
      DATA CTEXTH /'  CHILD HEAD B.C.'/
      DATA PTEXTF /' PARENT FLUX B.C.'/
C     ------------------------------------------------------------------
      ALLOCATE(ISCHILD,IBOTFLG,IBFLG,NBNODES,NPBNODES,NTIMES,NPLBEG,
     &         NPRBEG,NPCBEG,NPLEND,NPREND,NPCEND,IUBC,NCPP,BTEXT,
     &         VCB(4))
      
C1------PRINT A MESSAGE IDENTIFYING BFH PACKAGE
      WRITE(IOUT,500)IN
  500 FORMAT(1X,/1X,'BFH -- BOUNDARY FLOW AND HEAD PACKAGE',
     &    ', VERSION 1.0, 02/15/2006',/8X,'INPUT READ FROM UNIT ',I4)

C2------CHECK IF LGR IS ACTIVE, IF SO STOP.
      IF(ILGR .NE. 0)THEN 
        WRITE(IOUT,*)'THE BFH PACKAGE CANNOT BE USED WHEN RUNNING LGR' 
        WRITE(IOUT,*)'RUN LGR FIRST TO PRODUCE THE FILES NEEDED BY BFH'
        CALL USTOP(' ')
      ENDIF 
C
C3------READ IN HEADER INFO - BTEXT,NTIMES,NBODES,AND ISCHILD AND PRINT COMMENTS
C3------NOTE THAT WE ARE SKIPPING NLAY,NROW,NCOL BECAUSE WE HAVE IT ALREADY
      IC=1
      CALL URDCOM(IN,IOUT,LINE)
      CALL URWORD(LINE,IC,ISTART1,ISTOP1,1,N,R,0,0)
      CALL URWORD(LINE,IC,ISTART2,ISTOP2,1,N,R,0,0)
      CALL URWORD(LINE,IC,ISTART3,ISTOP3,1,N,R,0,0)
      BTEXT=LINE(ISTART1:ISTOP1+1)//LINE(ISTART2:ISTOP2+1)
     &      //LINE(ISTART3:ISTOP3+1)  
      BTEXT=TRIM(ADJUSTL(BTEXT))
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,ISCHILD,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,0,N,R,0,0)   !skip nlay
      CALL URWORD(LINE,IC,ISTART,ISTOP,0,N,R,0,0)   !skip nrow
      CALL URWORD(LINE,IC,ISTART,ISTOP,0,N,R,0,0)   !skip ncol
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,NTIMES,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,NBNODES,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,NPBNODES,R,0,0)
      IF(ISCHILD .GE. 0) CALL URWORD(LINE,IC,ISTART,ISTOP,2,NCPP,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,IBOTFLG,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,IBFLG,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,IUBC,R,0,0)
      IF(ISCHILD .LE. 0) THEN
        CALL URDCOM(IN,IOUT,LINE)
        READ (LINE,*)  NPLBEG,NPRBEG,NPCBEG
        CALL URDCOM(IN,IOUT,LINE)
        READ (LINE,*)  NPLEND,NPREND,NPCEND  
      ENDIF
C
C3A-----PRINT COMMENTS
      IF(ISCHILD .EQ. -1 .AND. BTEXT .EQ. TRIM(ADJUSTL(PTEXTF)))THEN
        WRITE(IOUT,410) BTEXT,NBNODES
      ELSEIF(ISCHILD .EQ. 1 .AND. BTEXT .EQ. TRIM(ADJUSTL(CTEXTH)))THEN
        WRITE(IOUT,411) BTEXT,NBNODES
      ELSE
        WRITE(IOUT,413) 
        STOP
      ENDIF
C       
C3B-----CHECK TO SEE IF COMPLEMENTARY BOUNDARY CONDITIONS WERE SAVED.
C3B-----IF SO, CHECK TO SEE IF FILE IS OPENED.  IF SO, THE RUN CHECK.
C3B-----IF B.C.'s WERE SAVED, BUT FILE IS NOT OPENED, DO NOT RUN CHECK.
      IF(IUBC .NE. 0) THEN
        INQUIRE(IUBC,OPENED=LOP)
        IF(LOP)THEN
          IF(ISCHILD .GT. 0) WRITE(IOUT,420) IUBC
          IF(ISCHILD .LT. 0) WRITE(IOUT,421) IUBC
        ELSE
          IUBC=0
        ENDIF
      ENDIF
C
  410 FORMAT (1X,A,/,'RUNNING PARENT MODEL WITH ',I5, ' INTERIOR FLUX ',
     &               'BOUNDARY NODES')
  411 FORMAT (1X,A,/,'RUNNING CHILD MODEL WITH ',I5,' SPECIFIED HEAD ',
     &               'BOUNDARY NODES')
  413 FORMAT (1X,'INVALID INPUT IN BFH FILE:',/, 
     &           'ISCHILD AND BTEXT ARE NOT COMPATIBLE')
C
  420 FORMAT (1X,'CHECKING AGAINST FLUX BOUNDARY CONDITIONS ON UNIT',I5)
  421 FORMAT (1X,'CHECKING AGAINST HEAD BOUNDARY CONDITIONS ON UNIT',I5)

C
C4------ALLOCATE BOUNDARY CONDITION ARRARYS. 
      ALLOCATE(KLAY(NBNODES),IROW(NBNODES),JCOL(NBNODES))
      ALLOCATE(BFLUX(NPBNODES))
      IF(ISCHILD .GE. 0)THEN
        ALLOCATE(BHEAD(NBNODES),KPLAY(NBNODES),IPROW(NBNODES),
     &           JPCOL(NBNODES),NPINDX(NBNODES))  
        IF(IUBC .GT. 0)ALLOCATE(BFLUXCHK(NPBNODES))
      ELSEIF(IUBC .GT. 0)THEN
        ALLOCATE(BHEADCHK(NBNODES))
      ENDIF
C
C6-----SAVE POINTER DATA TO ARRARYS
      CALL SGWF2BFH1PSV(IGRID)
C7
      RETURN
      END 
C***********************************************************************
      SUBROUTINE GWF2BFH1DA(IGRID)
C     ******************************************************************
C     DEALLOCATE BFH DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE
C     ------------------------------------------------------------------
C
      DEALLOCATE(BFHDAT(IGRID)%IBOTFLG)
      DEALLOCATE(BFHDAT(IGRID)%IBFLG)
      DEALLOCATE(BFHDAT(IGRID)%NBNODES)
      DEALLOCATE(BFHDAT(IGRID)%NCPP)
      DEALLOCATE(BFHDAT(IGRID)%NPBNODES)
      DEALLOCATE(BFHDAT(IGRID)%NTIMES)
      DEALLOCATE(BFHDAT(IGRID)%NPLBEG)
      DEALLOCATE(BFHDAT(IGRID)%NPRBEG)
      DEALLOCATE(BFHDAT(IGRID)%NPCBEG)
      DEALLOCATE(BFHDAT(IGRID)%NPLEND)
      DEALLOCATE(BFHDAT(IGRID)%NPREND)
      DEALLOCATE(BFHDAT(IGRID)%NPCEND)
      DEALLOCATE(BFHDAT(IGRID)%BTEXT)
      DEALLOCATE(BFHDAT(IGRID)%KLAY)
      DEALLOCATE(BFHDAT(IGRID)%IROW)
      DEALLOCATE(BFHDAT(IGRID)%JCOL)
      DEALLOCATE(BFHDAT(IGRID)%BFLUX)
      DEALLOCATE(BFHDAT(IGRID)%VCB)
      IF(BFHDAT(IGRID)%ISCHILD .GE. 0)THEN
        DEALLOCATE(BFHDAT(IGRID)%BHEAD)
        DEALLOCATE(BFHDAT(IGRID)%KPLAY)
        DEALLOCATE(BFHDAT(IGRID)%IPROW)
        DEALLOCATE(BFHDAT(IGRID)%JPCOL)
        DEALLOCATE(BFHDAT(IGRID)%NPINDX)
        IF(BFHDAT(IGRID)%IUBC .GT. 0)THEN
          DEALLOCATE(BFHDAT(IGRID)%BFLUXCHK)
        ENDIF
      ELSEIF(BFHDAT(IGRID)%IUBC .GT. 0)THEN
        DEALLOCATE(BFHDAT(IGRID)%BHEADCHK)
      ENDIF
      DEALLOCATE(BFHDAT(IGRID)%IUBC)
      DEALLOCATE(BFHDAT(IGRID)%ISCHILD)
C
      RETURN
      END 
C***********************************************************************
      SUBROUTINE SGWF2BFH1PNT(IGRID)
C     ******************************************************************
C     CHANGE POINTERS FOR BFH DATA TO A DIFFERENT GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE
C     ------------------------------------------------------------------
C
      ISCHILD=>BFHDAT(IGRID)%ISCHILD
      IBOTFLG=>BFHDAT(IGRID)%IBOTFLG
      IBFLG=>BFHDAT(IGRID)%IBFLG
      NCPP=>BFHDAT(IGRID)%NCPP
      NBNODES=>BFHDAT(IGRID)%NBNODES
      NPBNODES=>BFHDAT(IGRID)%NPBNODES
      NTIMES=>BFHDAT(IGRID)%NTIMES
      NPLBEG=>BFHDAT(IGRID)%NPLBEG
      NPCBEG=>BFHDAT(IGRID)%NPRBEG
      NPRBEG=>BFHDAT(IGRID)%NPCBEG
      NPLEND=>BFHDAT(IGRID)%NPLEND
      NPREND=>BFHDAT(IGRID)%NPREND
      NPCEND=>BFHDAT(IGRID)%NPCEND
      IUBC=>BFHDAT(IGRID)%IUBC
      BTEXT=>BFHDAT(IGRID)%BTEXT
      KLAY=>BFHDAT(IGRID)%KLAY
      IROW=>BFHDAT(IGRID)%IROW
      JCOL=>BFHDAT(IGRID)%JCOL
      KPLAY=>BFHDAT(IGRID)%KPLAY
      IPROW=>BFHDAT(IGRID)%IPROW
      JPCOL=>BFHDAT(IGRID)%JPCOL
      NPINDX=>BFHDAT(IGRID)%NPINDX
      BFLUX=>BFHDAT(IGRID)%BFLUX
      BFLUXCHK=>BFHDAT(IGRID)%BFLUXCHK
      BHEAD=>BFHDAT(IGRID)%BHEAD
      BHEADCHK=>BFHDAT(IGRID)%BHEADCHK
      VCB=>BFHDAT(IGRID)%VCB
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE SGWF2BFH1PSV(IGRID)
C     ******************************************************************
C     SAVE POINTERS ARRAYS FOR BFH DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE
C     ------------------------------------------------------------------
C
      BFHDAT(IGRID)%ISCHILD=>ISCHILD
      BFHDAT(IGRID)%IBOTFLG=>IBOTFLG
      BFHDAT(IGRID)%IBFLG=>IBFLG
      BFHDAT(IGRID)%NCPP=>NCPP
      BFHDAT(IGRID)%NBNODES=>NBNODES
      BFHDAT(IGRID)%NPBNODES=>NPBNODES
      BFHDAT(IGRID)%NTIMES=>NTIMES
      BFHDAT(IGRID)%NPLBEG=>NPLBEG 
      BFHDAT(IGRID)%NPRBEG=>NPCBEG
      BFHDAT(IGRID)%NPCBEG=>NPRBEG
      BFHDAT(IGRID)%NPLEND=>NPLEND
      BFHDAT(IGRID)%NPREND=>NPREND
      BFHDAT(IGRID)%NPCEND=>NPCEND
      BFHDAT(IGRID)%IUBC=>IUBC
      BFHDAT(IGRID)%BTEXT=>BTEXT
      BFHDAT(IGRID)%KLAY=>KLAY 
      BFHDAT(IGRID)%IROW=>IROW
      BFHDAT(IGRID)%JCOL=>JCOL
      BFHDAT(IGRID)%KPLAY=>KPLAY 
      BFHDAT(IGRID)%IPROW=>IPROW
      BFHDAT(IGRID)%JPCOL=>JPCOL
      BFHDAT(IGRID)%NPINDX=>NPINDX
      BFHDAT(IGRID)%BFLUX=>BFLUX
      BFHDAT(IGRID)%BFLUXCHK=>BFLUXCHK
      BFHDAT(IGRID)%BHEAD=>BHEAD
      BFHDAT(IGRID)%BHEADCHK=>BHEADCHK
      BFHDAT(IGRID)%VCB=>VCB
C
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY GWF2BFH1RP
      SUBROUTINE GWF2BFH1RP(IN,KPER,IUBCF,IULPF,IUHUF,IGRID)
C     ******************************************************************
C     READ IN THE LAYER, ROW, AND COLUMN DATA FOR WHERE THE BOUNDARY 
C     CONDITIONS WILL BE APPLIED. ADJUST STORAGE COEFFICIENTS IF THE  
C     SIMULATION IS TRANSIENT.
C     THIS ROUTINE ALSO ZEROS OUT THE INTERIOR OF THE PARENT GRID WHERE 
C     THE CHILD WILL BE.
C     IF COMPLIMENTARY BOUNDARY CONDITIONS WERE SAVED, READ IN THE  
C     HEADER INFO.
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NLAY,ITRSS,IBOUND,HNEW,CC,CR,CV
      USE GWFBASMODULE,ONLY:HNOFLO
      USE GWFBFHMODULE,ONLY:ISCHILD,NBNODES,NPLBEG,NPRBEG,NPCBEG,NPLEND,
     &                      NPREND,NPCEND,KLAY,IROW,JCOL,KPLAY,IPROW,
     &                      JPCOL,IBOTFLG,IBFLG,NPINDX,IUBC
      CHARACTER*200 LINE

      DOUBLE PRECISION  HNF
C     ------------------------------------------------------------------
C
C1-----IF NOT ON THE FIRST STRESS PERIOD, THEN RETURN
      IF(KPER .GT. 1)RETURN

      CALL SGWF2BFH1PNT(IGRID)
      ZERO = 0.
      HNF = HNOFLO
C
C2-3----IF ON THE FIRST STRESS PERIOD, READ IN THE LAY, ROW, AND COL OF 
C2-3----THE INTERFACE CELLS WHERE BOUNDARY CONDITIONS WILL BE APPLIED.
C2A-----IF IT IS A CHILD GRID READ THE INDEX THAT MAPS CHILD->PARENT 
C3A-----IF IT IS A PARENT GRID, THEN ZERO OUT INTERIOR. 
C
      I=1
      IF(ISCHILD .GE. 0)THEN
        DO N=1,NBNODES
          READ(IN,*) KLAY(N),IROW(N),JCOL(N),KPLAY(N),IPROW(N),
     &               JPCOL(N),NPINDX(N)
        ENDDO
C
C2C-----IF TRANSIENT, THEN ADJUST STORAGES
        IF(ITRSS .NE. 0) CALL SGWF2BFH1FMCS(IUBCF,IULPF,IUHUF,IGRID)
C2------END CHILD GRID READING AND STORAGE ADJUSTMENTS
C
      ELSE
        DO N=1,NBNODES
          READ(IN,*) KLAY(N),IROW(N),JCOL(N)
        ENDDO
C
C3A-----ZERO OUT THE INTERIOR BECAUSE THIS IS A PARENT SIMULATION
        IF(NPLBEG .EQ. NPLEND)THEN
          KBEG=NPLBEG
          KEND=KBEG
        ELSE
          KBEG=1
          KEND=NPLEND
        ENDIF
        DO K = KBEG,KEND
          DO I = NPRBEG,NPREND
            DO J= NPCBEG,NPCEND
              IF(I .EQ. NPRBEG .OR. I .EQ. NPREND .OR. 
     &           J .EQ. NPCBEG .OR. J .EQ. NPCEND)THEN
                IBOUND(J,I,K) = IBFLG 
C pick up bottom only if refinement is more than 1 layer and refinement
              ELSEIF(K .EQ. KEND .AND. IBOTFLG .EQ. 1)THEN
                IBOUND(J,I,K) = IBFLG
              ELSE
                IBOUND(J,I,K)=0
                HNEW(J,I,K) = HNF
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
C
C3B-----IF TRANSIENT, THEN ADJUST STORAGES
        IF(ITRSS .NE. 0) CALL SGWF2BFH1FMPS(IUBCF,IULPF,IUHUF,IGRID) 
C-------END PARENT GRID READING AND ADJUSTMENTS
      ENDIF  
C
C4------IF COMPLIMENTARY BOUNDARY CONDITION FILE WAS SAVED THEN READ
C4------(AND IGNORE) THE HEADER INFO
      IF(IUBC .NE. 0)THEN
        CALL URDCOM(IUBC,0,LINE)
        IF(ISCHILD .LT. 0)THEN
          CALL URDCOM(IUBC,0,LINE)
          CALL URDCOM(IUBC,0,LINE)
        ENDIF
        DO N=1,NBNODES
          READ(IUBC,*) DUM
        ENDDO  
      ENDIF
C5
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1FMCS      
      SUBROUTINE SGWF2BFH1FMCS(IUBCF,IULPF,IUHUF,IGRID)
C     ******************************************************************
C     ADJUST CHILD STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY.
C     MODIFIED FROM GWF2LGR1FMCS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LAYHDS
      USE GWFBFHMODULE,ONLY:IBOTFLG,IBFLG   
C     ------------------------------------------------------------------
C1
C1------ADJUST THE STORAGE COEFFICIENT 
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
C1C
C1C-----SC1 AND SC2 ARE MULTIPED BY 1/2**(# OF EDGES)  
C1C------> 1/2*1/2=1/4 WHICH IS WHAT IS NEEDED ALONG THE CORNERS 
C1C-----MULTIPLY BY 1/2 AGAIN FOR BOTTOM. 
              IF(K .EQ. NLAY .AND. IBOTFLG .EQ. 1) KEDGE=1
              IF(IUBCF .NE. 0) CALL SGWF2BFH1BCFSA(0,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IULPF .NE. 0) CALL SGWF2BFH1LPFSA(0,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IUHUF .NE. 0) CALL SGWF2BFH1HUFSA(0,J,I,K,
     1                              JEDGE,IEDGE,KEDGE)
            ENDIF
          ENDDO  
        ENDDO   
      ENDDO   
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1FMPS      
      SUBROUTINE SGWF2BFH1FMPS(IUBCF,IULPF,IUHUF,IGRID)
C     ******************************************************************
C     ADJUST PARENT STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY.
C     MODIFIED FROM GWF2LGR1FMPS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LAYHDS  
      USE GWFBFHMODULE,ONLY:IBFLG   
C     ------------------------------------------------------------------
C1
C1------ADJUST THE STORAGE COEFFICIENT 
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
              IF(IUBCF .NE. 0) CALL SGWF2BFH1BCFSA(1,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IULPF .NE. 0) CALL SGWF2BFH1LPFSA(1,J,I,K,
     1                              JEDGE,IEDGE,KEDGE,LAYHDS(K))
              IF(IUHUF .NE. 0) CALL SGWF2BFH1HUFSA(1,J,I,K,
     1                              JEDGE,IEDGE,KEDGE)
            ENDIF
          ENDDO  
        ENDDO   
      ENDDO   

      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY GWF2BFH1AD
      SUBROUTINE GWF2BFH1AD(IN,IGRID)
C     ******************************************************************
C     READ IN BOUNDARY CONDITION DATA FOR THE CURRENT TIME STEP
C     IF THIS IS A CHILD GRID, THEN APPLY SPECIFIED HEADS FOR THE   
C     CURRENT TIME STEP.      
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:HNEW
      USE GWFBFHMODULE,ONLY:ISCHILD,KLAY,IROW,JCOL,NBNODES,NPBNODES,
     1                      IUBC,BFLUX,BFLUXCHK,BHEAD,BHEADCHK
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      CALL SGWF2BFH1PNT(IGRID)

C1
C1------FIRST READ (AND IGNORE) TIME INFO.
      CALL URDCOM(IN,0,LINE)
C
C1------READ IN THE BOUNDARY CONDITION DATA FOR CHILD AND PARENT GRIDS.  
C1------APPLY BOUNDARY HEADS TO HNEW.
C1------CHECK IF COMPLEMENTARY BOUNDARY CONDITIONS ARE BEING CHECKED.  
C1------IF SO, READ THOSE TOO.
      IF(ISCHILD .GE. 0)THEN
        DO N=1,NBNODES
          READ(IN,*) BHEAD(N)
          HNEW(JCOL(N),IROW(N),KLAY(N)) = BHEAD(N)
        ENDDO
C2------READ COMPLEMENTARY BOUNDARY CONDITION DATA
        IF(IUBC .GT. 0)THEN
          CALL URDCOM(IUBC,0,LINE)
          DO N=1,NPBNODES
            READ(IUBC,*) BFLUXCHK(N) 
          ENDDO
        ENDIF

      ELSE
C3------PARENT GRID DATA
        DO N=1,NBNODES
          READ(IN,*) BFLUX(N)
        ENDDO
C4------READ COMPLEMENTARY BOUNDARY CONDITION DATA
        IF(IUBC .GT. 0)THEN
          CALL URDCOM(IUBC,0,LINE)
          DO N=1,NBNODES
            READ(IUBC,*) BHEADCHK(N)
          ENDDO
        ENDIF
      ENDIF
C5
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY GWF2BFH1FM
      SUBROUTINE GWF2BFH1FM(KPER,KSTP,KITER,IUBCF,IGRID)
C     ******************************************************************
C     ADJUST CONDUCTANCES ALONG INTERFACING BOUNDARY.
C     ADD SPECIFIED FLUX CONTRIBUTION TO RHS (PARENT GRID ONLY)
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE,ONLY:ISCHILD
C     ------------------------------------------------------------------
      CALL SGWF2BFH1PNT(IGRID)
C1
C1------ADJUST CONDUCTANCES DEPENDING ON WHETHER A PARENT OR CHILD GRID
      IF(ISCHILD .GE. 0)THEN
        CALL SGWF2BFH1FMCBC(KPER,KSTP,KITER,IUBCF)
      ELSE
        CALL SGWF2BFH1FMPBC(KPER,KSTP,KITER,IUBCF)
C2------ACCUMULATE FLUX TO RHS
        CALL SGWF2BFH1FMPBF()
      ENDIF
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1FMCBC
      SUBROUTINE SGWF2BFH1FMCBC(KPER,KSTP,KITER,IUBCF)
C     ******************************************************************
C     ADJUST CHILD CONDUCTANCES ALONG INTERFACING BOUNDARY.
C     MODIFIED FROM GWF2LGR1FMCBC
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NLAY,NROW,NCOL,IBOUND,LAYHDT,CR,CC,CV
      USE GWFBFHMODULE,ONLY:IBOTFLG,IBFLG
C     ------------------------------------------------------------------
C1
C1------HALVE THE CONDUCTANCES IF ON THE FIRST ITERATION OR IF THERE
C1------IS AN UNCONFINED LAYER (FOR UNCONFINED, CONDUCTANCES ARE 
C1------RECALCULATED IN THE FLOW PACKAGES).

      DO K=1,NLAY
        IF (LAYHDT(K) .EQ. 0 .AND. (KITER .NE. 1 
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
C1A-----CHECK NEIGHBOR IN THE +COL DIRECTION.  IF AN INTERFACE CELL,
C1A-----THEN HALVE CR AND SET ICR=1.  CHECK +/- ROW DIRECTION TO SEE
C1A-----IF THIS CELL IS ALONG AND EDGE.  IF SO, SET JEDGE=1.  ICR AND
C1A-----JEDGE ARE USED LATER TO ADJUST CONDUCTANCES OF EDGE CELLS.
              IF(IBOUND(J,I,K) .EQ. IBFLG)THEN
                IF(JP1 .LE. NCOL)THEN
                  IF(IBOUND(JP1,I,K) .EQ. IBFLG)THEN
                    CR(J,I,K)=0.5*CR(J,I,K)
                    ICR=1
                  ENDIF
                ENDIF
                IF(IM1 .GE. 1)THEN
                ELSEIF(IBOUND(J,I,K) .EQ. IBFLG)THEN
                  JEDGE=1
                ENDIF
                IF(IP1 .LE. NROW)THEN
                ELSEIF(IBOUND(J,I,K) .EQ. IBFLG)THEN
                  JEDGE=1
                ENDIF
C1B
                IF(IP1 .LE. NROW)THEN
                  IF(IBOUND(J,IP1,K) .EQ. IBFLG)THEN
                    CC(J,I,K)=0.5*CC(J,I,K)
                    ICC=1
                  ENDIF
                ENDIF
                IF(JM1 .GE. 1)THEN
                ELSEIF(IBOUND(J,I,K) .EQ. IBFLG)THEN
                  IEDGE=1
                ENDIF
                IF(JP1 .LE. NCOL)THEN
                ELSEIF(IBOUND(J,I,K) .EQ. IBFLG)THEN
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
                  ELSEIF((KITER .EQ. 1 .AND. KSTP .EQ. 1 .AND. 
     &                    KPER .EQ. 1) .OR. IUBCF .EQ. 0)THEN
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
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1FMPBC      
      SUBROUTINE SGWF2BFH1FMPBC(KPER,KSTP,KITER,IUBCF)
C
C     ******************************************************************
C     ADJUST CONDUCTANCES ON BOUNDARIES OF PARENT MESH INTERFACE 
C     MODIFIED FROM GWF2LGR1FMCBC
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LAYHDT,CR,CC,CV
      USE GWFBFHMODULE,ONLY:IBFLG
C
C     ------------------------------------------------------------------
C
C1------FIND IF THERE IS AN UNCONFINED LAYER.
C1------IF ON THE FIRST SOLVER ITERATION, OR IF THERE IS AN UNCONFINED 
C1------LAYER, LOOP THROUGH IBOUND AND HALVE THE CONDUCTANCES FOR 
C1------INTERFACE CELLS.  IF AT AN EDGE CELL, THEN THREE-QUARTER THE 
C1------CONDUCTANCES.
      DO K=1,NLAY
        IF (LAYHDT(K) .EQ. 0 .AND. (KITER .NE. 1 .OR. KSTP .NE. 1 
     &                              .OR. KPER .NE. 1))THEN
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
              IF(IBOUND(J,I,K).EQ.IBFLG)THEN
C1A
C1A-----CHECK NEIGHBOR IN THE +COL DIRECTION.  IF AN INTERFACE CELL,
C1A-----THEN HALVE CR AND SET ICR=1.  CHECK +/- ROW DIRECTION TO SEE
C1A-----IF THIS CELL IS ALONG AND EDGE.  IF SO, SET JEDGE=1.  ICR AND
C1A-----JEDGE ARE USED LATER TO ADJUST CONDUCTANCES OF EDGE CELLS.
                IF(JP1 .LE. NCOL)THEN
                  IF(IBOUND(JP1,I,K).EQ.IBFLG)THEN
                    CR(J,I,K)=0.5*CR(J,I,K)
                    ICR=1
                  ENDIF
                ENDIF
                IF(IM1 .GE. 1)THEN
                  IF(IBOUND(J,IM1,K).NE.IBFLG)JEDGE=1
                ENDIF
                IF(IP1 .LE. NROW)THEN
                  IF(IBOUND(J,IP1,K).NE.IBFLG)JEDGE=1
                ENDIF
C1B
                IF(IP1 .LE. NROW)THEN
                  IF(IBOUND(J,IP1,K).EQ.IBFLG)THEN
                    CC(J,I,K)=0.5*CC(J,I,K)
                    ICC=1
                  ENDIF
                ENDIF
                IF(JM1 .GE. 1)THEN
                  IF(IBOUND(JM1,I,K).NE.IBFLG)IEDGE=1
                ENDIF 
                IF(JP1 .LE. NCOL)THEN
                  IF(IBOUND(JP1,I,K).NE.IBFLG)IEDGE=1
                ENDIF 
C1C-----NOTE:  HARDWIRED FOR TOP LAYER BEING UNREFINED 
                IF(KP1 .LE. NLAY)THEN
                  IF(IBOUND(J,I,KP1).NE.IBFLG)KEDGE=1 
                ENDIF
C1D
C1D-----SKIP VERTICAL ADJUSTMENTS IF ONLY 1 LAYER MODEL
C1D-----CR AND CC ARE MULTIPED BY 3/2 (1.5) BECAUSE THEY WERE ALREADY 
C1D-----HALVED --> 1/2*3/2=3/4 WHICH IS WHAT IS NEEDED ALONG THE EDGES
C1D-----HALVE VERTICAL CONDUCTANCES ON THE FIRST ITERATION OR IF BCF
C1D-----IS NOT ACTIVE (IUBCF=0)...(BCF DOESN'T REFORMULATE CV)
                IF(NLAY .NE. 1)THEN
                  IF(KEDGE .EQ. 1)THEN
                    IF(JEDGE .EQ. 1 .AND. ICR .EQ. 1)
     &                  CR(J,I,K)=1.5*CR(J,I,K)
                    IF(IEDGE .EQ. 1 .AND. ICC .EQ. 1)
     &                  CC(J,I,K)=1.5*CC(J,I,K)
                  ELSEIF((KITER .EQ. 1 .AND. KSTP .EQ. 1 .AND. 
     &                    KPER .EQ. 1) .OR. IUBCF .EQ. 0)THEN
                    IF(JEDGE+IEDGE .EQ. 1) CV(J,I,K)=0.5*CV(J,I,K)  
                    IF(JEDGE+IEDGE .EQ. 2) CV(J,I,K)=0.75*CV(J,I,K)
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
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1FMPBF      
      SUBROUTINE SGWF2BFH1FMPBF()
C
C     ******************************************************************
C     ACCUMULATE BOUNDARY FLUX CONTRIBUTION ON RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:RHS
      USE GWFBFHMODULE,ONLY:NBNODES,JCOL,IROW,KLAY,BFLUX
C     ------------------------------------------------------------------
C
C1-----LOOP THROUGH BOUNDARY CELLS AND SUBTRACT BOUNDARY FLUX FROM RHS
      DO N=1,NBNODES
        RHS(JCOL(N),IROW(N),KLAY(N))=RHS(JCOL(N),IROW(N),KLAY(N))
     &                              - BFLUX(N)
      ENDDO     
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1CBF      
      SUBROUTINE SGWF2BFH1CBF(KKPER,IUBCF,IULPF,IUHUF,NPBNODES,BFLUX)
C     ******************************************************************
C     CALCULATE THE CHILD INTERFACE FLUXES FOR CHECKING AGAINST LGR 
C     BOUNDARY FLUXES.  ALSO USED TO ADJUST GLOBAL BUDGET FLUX FROM 
C     CONSTANT HEAD BOUNDARIES.
C     ADAPTED FROM GWF2LGR1FMBF
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,LBOTM,BOTM,LAYHDS,
     1                      ISSFLG,HNEW,CR,CC,CV
      USE GWFBASMODULE,ONLY:DELT
      USE GWFBFHMODULE,ONLY:NBNODES,JCOL,IROW,KLAY,NPINDX
      DOUBLE PRECISION HD, CD, HDIFF, DZERO,CHCH1, CHCH2, CHCH3, CHCH4, 
     1                 CHCH5, CHCH6
      DIMENSION BFLUX(NPBNODES)
C     ------------------------------------------------------------------
C1------INTIALIZE PARENT FLUX ARRAY
      ISS=ISSFLG(KKPER)
      ONE=1.
      IF(ISS .EQ. 0) TLED=ONE/DELT         
      ZERO=0.
      DZERO=0.D0
      DO N=1,NPBNODES
        BFLUX(N)=ZERO
      ENDDO

C2------LOOP THROUGH ALL INTERFACE CELLS AND CALCULATE FLOW INTO MODEL 
      DO N=1,NBNODES
        K=KLAY(N)
        I=IROW(N)
        J=JCOL(N)
        NP=NPINDX(N)
        LAYFLG=LAYHDS(K)
C
        CHCH1=DZERO
        CHCH2=DZERO
        CHCH3=DZERO
        CHCH4=DZERO
        CHCH5=DZERO
        CHCH6=DZERO
C
C3------CALCULATE FLOW THROUGH THE LEFT FACE.
C3------COMMENTS A-B APPEAR ONLY IN THE SECTION HEADED BY COMMENT 5,
C3------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 6-10.
C
C3A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C3A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, OR TO AN
C3A-----ADJACENT NO-FLOW CELL. 
        IF(J.EQ.1) GO TO 30
        IF(IBOUND(J-1,I,K).EQ.0) GO TO 30 
C
C3B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
        HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
        CD=CR(J-1,I,K)
        CHCH1=HDIFF*CD
C
C4------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30   IF(J.EQ.NCOL) GO TO 60
        IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
        HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
        CD=CR(J,I,K)
        CHCH2=HDIFF*CD
C
C5------CALCULATE FLOW THROUGH THE BACK FACE.
   60   IF(I.EQ.1) GO TO 90
        IF(IBOUND(J,I-1,K).EQ.0) GO TO 90
        HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
        CD=CC(J,I-1,K)
        CHCH3=HDIFF*CD
C
C6------CALCULATE FLOW THROUGH THE FRONT FACE.
   90   IF(I.EQ.NROW) GO TO 120
        IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
        HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
        CD=CC(J,I,K)
        CHCH4=HDIFF*CD
C
C7------CALCULATE FLOW THROUGH THE UPPER FACE.
  120   IF(K.EQ.1) GO TO 150
        IF(IBOUND(J,I,K-1).EQ.0) GO TO 150
        HD=HNEW(J,I,K)
        IF(LAYFLG .EQ. 0) GO TO 122
        TMP=HD
        IF(TMP.LT.BOTM(J,I,LBOTM(K)-1)) HD=BOTM(J,I,LBOTM(K)-1)
  122   HDIFF=HD-HNEW(J,I,K-1)
        CD=CV(J,I,K-1)
        CHCH5=HDIFF*CD
C
C8------CALCULATE FLOW THROUGH THE LOWER FACE.
  150   IF(K.EQ.NLAY) GO TO 180
        IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
        HD=HNEW(J,I,K+1)
        IF(LAYHDS(K+1) .EQ. 0) GO TO 152
        TMP=HD
        IF(TMP.LT.BOTM(J,I,LBOTM(K+1)-1)) HD=BOTM(J,I,LBOTM(K+1)-1)
  152   HDIFF=HNEW(J,I,K)-HD
        CD=CV(J,I,K)
        CHCH6=HDIFF*CD
C
C9------SUM THE FLOWS THROUGH SIX FACES OF THE INTERFACE CELL, AND
C9------STORE SUM IN RATE.   
 180    RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
C
C10-----IF TRANSIENT, ACCOUNT FOR FLUX TO/FROM STORAGE 
        IF(ISS .EQ. 0)THEN
          IF(IUBCF .NE. 0) CALL SGWF2BFH1BCFSF(J,I,K,TLED,LAYFLG,
     1                                         STRG)
          IF(IULPF .NE. 0) CALL SGWF2BFH1LPFSF(J,I,K,TLED,LAYFLG,
     1                                         STRG)
          IF(IUHUF .NE. 0) CALL SGWF2BFH1HUFSF(J,I,K,TLED,LAYFLG,
     1                                         STRG)
C11----ADD STORAGE CONTRIBUTION TO RATE
C11----NOTE: RATE ACCUMULATED ALL OUTFLOWS FROM THE CELL.  A NEGATIVE
C------      vALUE FOR STRG IS WATER GOING INTO STORAGE, WHICH IS LIKE
C------      AN OUTFLOW.  A POSITIVE VALUE IS WATER LEAVING STORAGE AND
C------      CONTRIBUTING TO THE CELL.
          RATE=RATE-STRG

        ENDIF

        BFLUX(NP) = BFLUX(NP) + RATE
C
      ENDDO
C12
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY GWF2BFH1BD
      SUBROUTINE GWF2BFH1BD(KSTP,KPER,IUBCF,IULPF,IUHUF,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR PARENT FLUX AND CHILD HEAD B.C.'s
C     THIS ROUTINE IS ADAPTED FROM GWF2LGR1BD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,DELT
      USE GWFBFHMODULE,ONLY:ISCHILD,BFLUX,NPBNODES
      CHARACTER(LEN=16):: TEXTP
      DOUBLE PRECISION RATIN,RATOUT,QQ,DZERO
      DATA TEXTP /'PARENT FLUX B.C.'/
C     ------------------------------------------------------------------
C
      CALL SGWF2BFH1PNT(IGRID)
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS 
      DZERO=0.D0
      RATIN=DZERO
      RATOUT=DZERO
      VBNM(MSUM)=TEXTP
C
C2------IF CHILD GRID THEN FIND FLUX THROUGH INTERPOLATED HEAD B.C.
C2------AND RETURN (BUDGETS THROUGH SPECIFIED HEADS ARE NOT SEPARATED)
      IF(ISCHILD .GE. 0)THEN 
        CALL SGWF2BFH1CBF(KPER,IUBCF,IULPF,IUHUF,NPBNODES,BFLUX)
        RETURN
      ENDIF
C
C3------LOOP THROUGH FLUX B.C. CALCULATING FLOW.
C3------SEE IF FLOW IS POSITIVE OR NEGATIVE.
C3------FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
C3------FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
      DO N=1,NPBNODES
        Q=BFLUX(N)
        QQ=Q
        IF(Q .GT. 0) RATIN=RATIN+QQ
        IF(Q .LT. 0) RATOUT=RATOUT-QQ
      ENDDO
C
C4------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
C
C6------INCREMENT BUDGET TERM COUNTER(MSUM) 
      MSUM=MSUM+1
C
C7------RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY GWF2BFH1OT
      SUBROUTINE GWF2BFH1OT(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE BOUNDARY HEAD OR FLUX DIFFERENCES AGAINST THOSE SAVED
C     FROM THE LGR AND REPORT WHERE LARGEST DISCREPANCY IS FOUND.  
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,HNEW
      USE GWFBASMODULE,ONLY:IBUDFL
      USE GWFBFHMODULE,ONLY:ISCHILD,IUBC,NBNODES,NPBNODES,KLAY,IROW,
     1                      JCOL,KPLAY,IPROW,JPCOL,NPINDX,BFLUX,
     2                      BFLUXCHK,BHEADCHK
C     ------------------------------------------------------------------
C
C1------IF BUDGET IS BEING SAVED THEN PRINT FLUX FROM BFH SPECIFIED
C1------HEAD BOUNDARIES.
        IF(ISCHILD .GE. 0) CALL SGWF2BFH1CBD(KPER,KSTP,IBUDFL,NPBNODES,
     &                                      BFLUX)
C 
C2------CHECK IF COMPLEMENTARY BOUNDARY CONDITION FILES WERE SAVED 
C2------AND IF BUDGET IS BEING SAVED.  IF SO, THEN LOOP THROUGH
C2------BOUNDARY CONDITIONS AND FIND MAXIUMUM DIFFERENCE
      IF(IUBC .NE. 0 .AND. IBUDFL .NE. 0)THEN
        SUMNEW = 0.
        SUMOLD = 0.
        DIFFMAX = 0.
        DIFFMEAN = 0.
        NODEMAX = 0
        IF(ISCHILD .GE. 0)THEN 
          DO N=1,NPBNODES
            SUMNEW = SUMNEW + BFLUX(N)
            SUMOLD = SUMOLD + BFLUXCHK(N) 
            DIFF = BFLUX(N) - BFLUXCHK(N) 
            DIFFMEAN=DIFFMEAN + ABS(DIFF)
            IF(ABS(DIFF) .GT. ABS(DIFFMAX))THEN
              DIFFMAX = DIFF
              NODEMAX = N
            ENDIF
          ENDDO
C
C2A-----FIND PARENT LAYER, ROW, COLUMN ASSOCIATED WITH MAX DIFFERENCE
          DO N=1,NBNODES
            IF(NODEMAX .EQ. NPINDX(N))THEN
              NPMAX=N 
              EXIT
            ENDIF
          ENDDO
C
C2B-----WRITE OUTPUT
          WRITE(IOUT,300) SUMNEW,SUMOLD,DIFFMEAN/NPBNODES,DIFFMAX,
     &                    KPLAY(NPMAX),IPROW(NPMAX),JPCOL(NPMAX),
     &                    BFLUX(NODEMAX),BFLUXCHK(NODEMAX)
C2
C2C-----PARENT SIMULATION: FIND MAX HEAD DIFFERENCE
        ELSE
          DO N=1,NBNODES
            DIFF = HNEW(JCOL(N),IROW(N),KLAY(N)) - BHEADCHK(N)
            DIFFMEAN=DIFFMEAN + ABS(DIFF)
            IF(ABS(DIFF) .GT. ABS(DIFFMAX))THEN
              DIFFMAX = DIFF
              NODEMAX = N
              HDMAX=HNEW(JCOL(N),IROW(N),KLAY(N))
            ENDIF
          ENDDO
C2D
C2D-----WRITE OUTPUT
          WRITE(IOUT,400) DIFFMEAN/NBNODES,DIFFMAX,KLAY(NODEMAX),
     &                    IROW(NODEMAX),JCOL(NODEMAX),HDMAX,
     &                    BHEADCHK(NODEMAX)         
        ENDIF

      ENDIF            
C
 300  FORMAT(1X,/,'BFH: BOUNDARY FLUX COMPARISON',
     &       1X,/,29('-'),
     &       1X,/,'NEW TOTAL BOUNDARY FLUX = ',G16.9,
     &       1X,/,'OLD TOTAL BOUNDARY FLUX = ',G16.9,
     &       1X,/,'AVERAGE ABSOLUTE FLUX DIFFERENCE = ',G16.9,
     &       1X,/,'MAXIMUM ABSOLUTE FLUX DIFFERENCE OF ',G16.9,
     &       1X,/,'OCCURS AT PARENT LAYER ',I0,' ROW ',I0,' COLUMN ',I0,
     &       1X,/,'NEW FLUX AT THIS NODE = ',G16.9,
     &       1X,/,'OLD FLUX AT THIS NODE = ',G16.9,/)

 400  FORMAT(1X,/,'BFH: BOUNDARY HEAD COMPARISON',
     &       1X,/,29('-'),
     &       1X,/,'AVERAGE ABSOLUTE HEAD DIFFERENCE = ',G16.9,
     &       1X,/,'MAXIMUM ABSOLUTE HEAD DIFFERENCE OF ',G16.9,
     &       1X,/,'OCCURS AT PARENT LAYER ',I0,' ROW ',I0,' COLUMN ',I0,
     &       1X,/,'NEW HEAD AT THIS NODE = ',G16.9,
     &       1X,/,'OLD HEAD AT THIS NODE = ',G16.9,/)
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1CBD
      SUBROUTINE SGWF2BFH1CBD(KPER,KSTP,IBUDFL,NPBNODES,BFLUX)
C     ******************************************************************
C     CALCULATE FLUXES FROM THE CHILD SPECIFIED HEAD BOUNDARIES
C     MODFIFIED FROM GWF2LGR1CBD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT
      USE GWFBASMODULE,ONLY:DELT
      USE GWFBFHMODULE,ONLY:VCB
      DIMENSION BFLUX(NPBNODES)
      CHARACTER(LEN=16):: VCBNM 
      CHARACTER(LEN=17):: VAL1,VAL2
      DATA VCBNM /' CHILD HEAD B.C.'/
C     ------------------------------------------------------------------
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS AND SET VALUES FOR PRINTING
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
C
C2------LOOP THROUGH FLUX B.C. CALCULATING FLOW.
C2------SEE IF FLOW IS POSITIVE OR NEGATIVE.
C2------FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATOUT.
C2------FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATIN.
      DO N=1,NPBNODES  
        Q=BFLUX(N)
        QQ=Q
        IF(Q .GT. 0) RATIN=RATIN+QQ
        IF(Q .LT. 0) RATOUT=RATOUT-QQ
      ENDDO
C
C3------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VCB(3)=RIN
      VCB(4)=ROUT
      VCB(1)=VCB(1)+RIN*DELT
      VCB(2)=VCB(2)+ROUT*DELT
C
C4------PRINT RATES TO OUTPUT FILE IF REQUESTED
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

  260 FORMAT('1',/2X,'VOLUMETRIC BUDGET FOR BFH SPECIFIED HEADS AT'
     1,' TIME STEP',I3,' IN STRESS PERIOD',I4/2X,78('-'))
  265 FORMAT(1X,/5X,'CUMULATIVE VOLUMES',6X,'L**3',7X
     1,'RATES FOR THIS TIME STEP',6X,'L**3/T'/5X,18('-'),17X,24('-'))
  270 FORMAT(1X,/12X,'TOTAL IN =',A,14X,'TOTAL IN =',A)
  280 FORMAT(1X,/11X,'TOTAL OUT =',A,13X,'TOTAL OUT =',A,/)
C6
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1BCFSA
      SUBROUTINE SGWF2BFH1BCFSA(ISPARENT,J,I,K,JEDGE,IEDGE,KEDGE,
     1                          IFLGSC2)
C     ******************************************************************
C     ADJUST BCF STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY 
C     MODFIFIED FROM SGWF2LGR1BCFSA
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
C-----VERSION 1.0 15FEBRUARY SGWF2BFH1LPFSA
      SUBROUTINE SGWF2BFH1LPFSA(ISPARENT,J,I,K,JEDGE,IEDGE,KEDGE,
     1                          IFLGSC2)
C     ******************************************************************
C     ADJUST LPF STORAGE COEFFICIENTS ALONG INTERFACING BOUNDARY 
C     MODFIFIED FROM SGWF2LGR1LPFSA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLPFMODULE,ONLY:SC1,SC2
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
C-----VERSION 1.0 15FEBRUARY2006 SGWF2BFH1HUFSA
      SUBROUTINE SGWF2BFH1HUFSA(ISPARENT,J,I,K,JEDGE,IEDGE,KEDGE)
C     ******************************************************************
C     ADJUST HUF STORAGE COEFFICIENT ALONG INTERFACING BOUNDARY 
C     MODFIFIED FROM SGWF2BFH1LPFSA
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
C-----VERSION 1.0 15FEBRUARY2006 SGWF2BFH1BCFSF
      SUBROUTINE SGWF2BFH1BCFSF(J,I,K,TLED,LAYFLG,STRG)
C     ******************************************************************
C     CALCULATE FLOW IN/OUT OF STORAGE FOR THE BCF PACKAGE 
C     MODIFIED FROM SGWF2LGR1BCFSF
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
C-----VERSION 1.0 15FEBRUARY2006 SGWF2BFH1LPFSF
      SUBROUTINE SGWF2BFH1LPFSF(J,I,K,TLED,LAYFLG,STRG)
C     ******************************************************************
C     CALCULATE FLOW IN/OUT OF STORAGE FOR THE LPF PACKAGE 
C     MODIFIED FROM SGWF2LGR1LPFSF
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
C-----VERSION 1.0 15FEBRUARY2006 SGWF2BFH1HUFSF
      SUBROUTINE SGWF2BFH1HUFSF(J,I,K,TLED,LAYFLG,STRG)
C     ******************************************************************
C     CALCULATE FLOW IN/OUT OF STORAGE FOR THE HUF PACKAGE 
C     MODIFIED FROM SGWF2LGR1HUFSF
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
C2B-------------Compute SC2 Component
        CALL SGWF2HUF7SC2(1,J,I,K,TOP,BOT,HN,HO,TLED,CHCOF,STRG,
     &           HUFTHK,NCOL,NROW,NHUF,DELR(J)*DELC(I),IOUT)
C
      ENDIF
C3
      RETURN
      END
