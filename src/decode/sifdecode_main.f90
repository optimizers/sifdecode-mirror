C     ( Last modified on 7 Dec 2001 at 09:05:00 )
      PROGRAM       SIFDEC
C
C  This is the main program for running the SIF decoder for the GALAHAD and 
C  CUTEr optimization packages. It calls the driver routine SDLANC which does 
C  all the work. The purpose of this main program is to open and close all 
C  files, and to care for the proper filenames when possible.
C
C  Nick Gould, for CGT Productions.
C  December 7th, 1990.
C
C
      INTEGER       IINGPS, IOUTDA, IINFN , IOUTFN, IOUTRA, IINGR
      INTEGER       IOUTGR, IOUT  , IOUTEX, NEWL  , I
      INTEGER       IINEX , IPRINT, INFORM, IALGOR, IOUTFF, IOUTFD
      INTEGER       IAUTO , IOUTGF, IOUTGD, IOUTEM, IOUTEA, IAD0
      LOGICAL       NONAME, SINGLE
      EXTERNAL      SDLANC
C
C  ASSIGN THE STANDARD OUTPUT UNIT NUMBERS.
C
      PARAMETER   ( IOUT = 6 )
      CHARACTER * 10 PRB
      CHARACTER * 10 PBNAME
      CHARACTER * 24 PRBDAT, PRBFN , PRBRA , PRBOUT, PRBGR , PRBET
      CHARACTER * 24 PRBEX, PRBFF , PRBFD , PRBGF , PRBGD , PRBEA
C
C  ASSIGN THE REMAINING I/O UNIT NUMBERS.
C
      PARAMETER   ( IOUTDA = 55, IINGPS = 61 )
      PARAMETER   ( IOUTFN = 52, IOUTRA = 53, IOUTGR = 54 )
      PARAMETER   ( IOUTEX = 57, IOUTFD = 59, IOUTEA = 66 )
      PARAMETER   ( IOUTGD = 63, IOUTEM = 67 )
      PARAMETER   ( IOUTFF = 0 , IOUTGF = 0 )
C     PARAMETER   ( IOUTFF = 58, IOUTGD = 63 )
      PARAMETER   ( IINFN  = IINGPS,  IINGR = IINGPS, IINEX = IINGPS )
      PARAMETER   ( NONAME = .FALSE. )
C
C  READ PROBLEM'S NAME, BUILD DEFAULT FILE NAMES AND ASSIGN
C  THE ACTUAL VALUES USED.
C
      READ ( 5, 1000 ) PRB
C      WRITE( IOUT, 2000 ) PRB
      DO 10 I = 1, 10
         IF ( PRB( I: I ) .EQ. ' ' ) THEN
           NEWL = I - 1
           GO TO 20
         END IF
   10 CONTINUE
      NEWL = 10
   20 CONTINUE
C
C  specify the method to be used (1=SBMIN,2=AUGLG,3=BARIA).
C
      READ( 5, 1020 ) IALGOR
C
C  specify whether the problem should be described(<0=DEBUG,0=NO,>0=YES)
C
      READ( 5, 1030 ) IPRINT
C
C  read the actual problem name and use it for initial output
C
      READ( 5, 1000 ) PBNAME
      WRITE( IOUT, 2000 ) PBNAME
C
C  specify whether the derivatives are supplied or are to be computed
C  using automatic differentiation
C
      READ( 5, 1020 ) IAUTO
C
C  specify whether AD01 or AD02 should be used to perform the
C  automatic differentiation
C
      READ( 5, 1020 ) IAD0
C
C  Specify the precision of the output files (single=0,double=1)
C
      READ( 5, 1020 ) I
      SINGLE = I .EQ. 0
C
C  Open output files (if needed)
C
      PRBDAT = '                       '
      PRBOUT = '                       '
      PRBFN  = '                       '
      PRBRA  = '                       '
      PRBGR  = '                       '
      PRBET  = '                       '
      PRBEX  = '                       '
      PRBDAT = PRB( 1 : NEWL ) // '.SIF'
      PRBOUT = 'OUTSDIF.d'
      PRBFN  = 'ELFUN.f'
      PRBFF  = 'ELFUNF.f'
      PRBFD  = 'ELFUND.f'
      PRBRA  = 'RANGE.f'
      PRBGR  = 'GROUP.f'
      PRBGF  = 'GROUPF.f'
      PRBGD  = 'GROUPD.f'
      PRBET  = 'SETTYP.f'
      PRBEX  = 'EXTER.f'
      PRBEA  = 'EXTERA.f'
C
C  OPEN THE RELEVANT FILES - UNIX SYSTEMS.
C
      OPEN ( IINGPS, FILE = PRBDAT, FORM = 'FORMATTED',
     *       STATUS = 'UNKNOWN' )
      REWIND IINGPS
      OPEN ( IOUTDA, FILE = PRBOUT, FORM = 'FORMATTED',
     *       STATUS = 'UNKNOWN' )
      REWIND IOUTDA
      OPEN ( IOUTRA, FILE = PRBRA,  FORM = 'FORMATTED',
     *       STATUS = 'UNKNOWN' )
      REWIND IOUTRA
      OPEN ( IOUTEX, FILE = PRBEX,  FORM = 'FORMATTED',
     *       STATUS = 'UNKNOWN' )
      REWIND IOUTEX
      IF ( IAUTO .EQ. 0 ) THEN
        OPEN ( IOUTFN, FILE = PRBFN,  FORM = 'FORMATTED',
     *         STATUS = 'UNKNOWN' )
        REWIND IOUTFN
        OPEN ( IOUTGR, FILE = PRBGR,  FORM = 'FORMATTED',
     *         STATUS = 'UNKNOWN' )
        REWIND IOUTGR
      ELSE
        IF ( IOUTFF .GT. 0 ) THEN
          OPEN ( IOUTFF, FILE = PRBFF,  FORM = 'FORMATTED',
     *           STATUS = 'UNKNOWN' )
          REWIND IOUTFF
        END IF
        OPEN ( IOUTFD, FILE = PRBFD,  FORM = 'FORMATTED',
     *         STATUS = 'UNKNOWN' )
        REWIND IOUTFD
        IF ( IOUTGF .GT. 0 ) THEN
          OPEN ( IOUTGF, FILE = PRBGF,  FORM = 'FORMATTED',
     *           STATUS = 'UNKNOWN' )
          REWIND IOUTGF
        END IF
        OPEN ( IOUTGD, FILE = PRBGD,  FORM = 'FORMATTED',
     *         STATUS = 'UNKNOWN' )
        REWIND IOUTGD 
        OPEN ( IOUTEA, FILE = PRBEA,  FORM = 'FORMATTED',
     *         STATUS = 'UNKNOWN' )
        REWIND IOUTEA
      END IF
      OPEN( UNIT = IOUTEM )
C
C     DO THE WORK
C
      CALL SDLANC( IINGPS, IOUTDA, IINFN , IOUTFN, IOUTFF, IOUTFD, 
     *             IOUTRA, IINGR , IOUTGR, IOUTGF, IOUTGD, 
     *             IINEX , IOUTEX, IOUTEM, IOUTEA, IPRINT, IOUT  , 
     *             NONAME, IALGOR, IAUTO , IAD0  , SINGLE, INFORM )
C
C   CLOSE THE OPENED FILES
C
      CLOSE( IINGPS )
      IF ( INFORM .EQ. 0 ) THEN
        CLOSE( IOUTDA )
        CLOSE( IOUTRA )
        CLOSE( IOUTEX )
        IF ( IAUTO .EQ. 0 ) THEN
          CLOSE( IOUTFN )
          CLOSE( IOUTGR )
        ELSE
          IF ( IOUTFF .GT. 0 ) CLOSE( IOUTFF )
          CLOSE( IOUTFD )
          IF ( IOUTGF .GT. 0 ) CLOSE( IOUTGF )
          CLOSE( IOUTGD )
          CLOSE( IOUTEA )
        END IF
C
C  IF AN ERROR HAS BEEN DISCOVERED, DELETE THE OUTPUT FILES.
C
      ELSE
        CLOSE( IOUTDA, STATUS = 'DELETE' )
        CLOSE( IOUTRA, STATUS = 'DELETE' )
        IF ( IAUTO .EQ. 0 ) THEN
          CLOSE( IOUTFN, STATUS = 'DELETE' )
          CLOSE( IOUTGR, STATUS = 'DELETE' )
          CLOSE( IOUTEX, STATUS = 'DELETE' )
        ELSE
          IF ( IOUTFF .GT. 0 ) CLOSE( IOUTFF, STATUS = 'DELETE' )
          CLOSE( IOUTFD, STATUS = 'DELETE' )
          IF ( IOUTGF .GT. 0 ) CLOSE( IOUTGF, STATUS = 'DELETE' )
          CLOSE( IOUTGD, STATUS = 'DELETE' )
          CLOSE( IOUTEA, STATUS = 'DELETE' )
        END IF
      END IF
      CLOSE( IOUTEM, STATUS = 'DELETE' )
      STOP
 1000 FORMAT( A10 )
 1020 FORMAT( I2 )
 1030 FORMAT( I6 )
 2000 FORMAT( /, ' Problem name: ', A10 )

C
C END OF PROGRAM SIFDEC
C

      END
