! THIS VERSION: SIFDECODE 1.0 - 20/10/2012 AT 13:00 GMT.

!-*-*-*-*-*-*-*-*-*-*-*- S I F D E C O D E   M O D U L E -*-*-*-*-*-*-*-*-*-*-

!  Copyright reserved, Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released as part of CUTE, December 1990
!   Became separate package SifDec, April 2004
!   Updated fortran 2003 version released October 2012

!  For full documentation, see 
!   http://galahad.rl.ac.uk/galahad-www/specs.html

    MODULE SIFDECODE

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: SIFDECODE_sdlanc

!--------------------
!   P r e c i s i o n
!--------------------

      INTEGER, PARAMETER :: sp = KIND( 1.0E+0 )
      INTEGER, PARAMETER :: dp = KIND( 1.0D+0 )
      INTEGER, PARAMETER :: wp = dp

!----------------------
!   P a r a m e t e r s
!----------------------

      REAL ( KIND = wp ), PARAMETER :: zero = 0.0_wp
      REAL ( KIND = wp ), PARAMETER :: one = 1.0_wp

    CONTAINS

      SUBROUTINE SIFDECODE_sdlanc( IINGPS, IOUTDA, IINFN , IOUTFN, IOUTFF, &
                         IOUTFD, IOUTRA, IINGR , IOUTGR, IOUTGF, IOUTGD, &
                         IINEX , IOUTEX, IOUTEM, IOUTEA, IPRINT, IOUT  , &
                         NONAME, IALGOR, IAUTO , IAD0  , SINGLE, INFORM)

!  DECODE A SIF FILE AND CONVERT THE DATA INTO A FORM SUITABLE FOR
!  INPUT TO SBMIN, AUGLG OR BARIA.

!  NICK GOULD, FOR CGT PRODUCTIONS.
!  DECEMBER 7TH, 1990.

      INTEGER          IINGPS, IINFN , IINGR , INFORM, NMAX  , NGMAX
      INTEGER          NOBMAX, ONLY1 , NIMAX , LIWK  , LWK   , NCONST
      INTEGER          IOUTDA, IOUTRA, IOUTFN, IOUTGR, LENGTH
      INTEGER          I , IG, ISG   , IINEX , IOUTEX, IOUT  , LA, LB
      INTEGER          NSMAX , NBMAX , NETMAX, NOMAX , NLMAX , NELMAX
      INTEGER          IPRINT, NELNUM, NELING, NEGMAX, NEPVMX, NGPVMX
      INTEGER          NINDEX, MAXINS, MAXLEV, MAXARA, NARRAY, NOBJGR
      INTEGER          IALGOR, NGRMAX, NRLNDX, NEPMAX, NGPMAX, NEVMAX
      INTEGER          NINMAX, NUMAX , NSETVC, LSTADG, LSTADA, LELVAR
      INTEGER          LSTAEV, LNTVAR, LBNDS , LINTRE, LICNA , NREAL
      INTEGER          NLINOB, NNLNOB, NLINEQ, NNLNEQ, NLININ, NNLNIN
      INTEGER          NFREE , NFIXED, NLOWER, NUPPER, NBOTH , NSLACK
      INTEGER          N , NG, NBND  , NELTYP, NLVARS, NOBJ  , NRANGE
      INTEGER          NNZA  , NGRTYP, NSTART, NLISGP, NNLVRS, NOBBND
      INTEGER          IOUTFF, IOUTFD, IOUTGF, IOUTGD, IOUTEM, IAUTO 
      INTEGER          IOUTEA, IAD0
      DOUBLE PRECISION BIG   , BLO   , BUP
      LOGICAL          DEBUG , NONAME, SINGLE, ONEOBJ, GOTLIN
      CHARACTER * 10   NAMEOF, NAMERH, NAMERA, NAMEBN, NAMEST, NAMEOB
      CHARACTER * 72   LINEEX
      PARAMETER      ( BIG = 1.0D+20 )

!  ---------------------------------------------------------------------

!  Parameters whose value might be changed by the user:

!  The following parameters define the sizes of problem
!  dependent arrays. These may be changed by the user to
!  suit a particular problem or system configuration.

!  The TOOLS will issue error messages if any of these sizes
!  is too small, telling which parameter to increase.

!  ---------------------------------------------------------------------

!#{sizing}

!  ---------------------------------------------------------------------


!  End of parameters which might be changed by the user.

!  ---------------------------------------------------------------------

!  DEPENDENCIES ON THE MAXIMUM NUMBER OF NONTRIVIAL GROUP TYPES.
!  NGPMAX IS THE TOTAL NUMBER OF GROUP PARAMETERS.

      PARAMETER      ( NGPMAX = NGRMAX )

!  DEPENDENCIES ON THE MAXIMUM NUMBER OF GROUPS

      PARAMETER      ( NOMAX = NGMAX )
      PARAMETER      ( NUMAX = NGMAX )
      PARAMETER      ( LSTADG = NGMAX )
      PARAMETER      ( LSTADA = NGMAX )
      PARAMETER      ( LB = NGMAX )
      PARAMETER      ( LBNDS = NMAX + NGMAX )

!  DEPENDENCIES ON THE MAXIMUM TOTAL NUMBER OF REAL
!  PARAMETERS ASSOCIATED WITH GROUPS.

      PARAMETER      ( LWK = NGPVMX )

!  DEPENDENCIES ON THE MAXIMUM NUMBER OF NONLINEAR ELEMENT TYPES.
!  NETMAX, NIMAX AND NEPMAX ARE THE TOTAL NUMBER OF ELEMENTAL AND
!  INTERNAL VARIABLES AND PARAMETERS RESPECTIVELY.

      PARAMETER      ( NETMAX = 5 * NLMAX )
      PARAMETER      ( NIMAX = 5 * NLMAX )
      PARAMETER      ( NEPMAX = 3 * NLMAX )

!  DEPENDENCIES ON THE MAXIMUM NUMBER OF NONLINEAR ELEMENTS.

      PARAMETER      ( NEGMAX = NELMAX )
      PARAMETER      ( LSTAEV = NELMAX )
      PARAMETER      ( LNTVAR = NELMAX + 1 )
      PARAMETER      ( LINTRE = NELMAX )
      PARAMETER      ( LIWK = NELMAX + NGMAX )

!  DEPENDENCIES ON THE MAXIMUM TOTAL NUMBER OF ELEMENTAL VARIABLES.

      PARAMETER      ( LELVAR = NEVMAX )

!  DEPENDENCIES ON THE MAXIMUM NUMBER OF NONZEROS IN LINEAR ELEMENTS.

      PARAMETER      ( LICNA = LA     )

!  MAXIMUM NUMBER OF STATEMENTS IN A DO-LOOP.

      PARAMETER      ( MAXINS = 200    )

!  MAXIMUM NESTING OF DO-LOOPS

      PARAMETER      ( MAXLEV = 3      )

!  MAXIMUM NUMBER OF ARRAY INSTRUCTIONS.

      PARAMETER      ( MAXARA = 150    )

!  MAXIMUM SIZE OF DICTIONARY.

      PARAMETER      ( LENGTH = NMAX + NGMAX + NELMAX + NINMAX + 1000 )

!  ARRAY DEFINITIONS.

      INTEGER          INLIST( LENGTH ), ISTAEV( NELMAX )
      INTEGER          ISTATE( NGMAX ), ITABLE ( LENGTH )
      INTEGER          IELV  ( NLMAX  ), IINV  ( NLMAX )
      INTEGER          ITYPEE( NELMAX ), IELING( NEGMAX, 2 )
      INTEGER          IEPA  ( NLMAX  ), IGPA  ( NGRMAX )
      INTEGER          IDROWS( 2, NGMAX ), ITYPEG( NGMAX )
      INTEGER          IELVAR( LELVAR ), INTVAR( LNTVAR )
      INTEGER          ISTADA( LSTADA ), ICNA  ( LICNA  )
      INTEGER          ISTEP ( NELMAX ), ISTGP( NGMAX ), IWK( LIWK )
      INTEGER          INDVAL( NINDEX ), INSTR( 5, MAXINS, MAXLEV )
      INTEGER          NINSTR( MAXLEV ), IARRAY( 5, 3, MAXARA )
      INTEGER          ICOORD( LA, 2 ), ISTADG( NGMAX ), IJUMP( NLMAX )
      INTEGER          ITYPEV( NMAX )
      DOUBLE PRECISION GPTEMP( NGPVMX )
      DOUBLE PRECISION EPVALU( NEPVMX ), GPVALU( NGPVMX ), DFAULT( NMAX)
      DOUBLE PRECISION A( LA ), BND( 2, NMAX, NBMAX ), REALVL( NRLNDX )
      DOUBLE PRECISION BNDFLT( 2, NBMAX ), CSTART( NGMAX, NSMAX )
      DOUBLE PRECISION RSCALE( NGMAX ), CSCALE( NMAX ), WK( LWK )
      DOUBLE PRECISION RDROWS( 2, NGMAX ), VSTART( NMAX, NSMAX )
      DOUBLE PRECISION RVALUE( MAXARA, 3 ), VARRAY( 2, MAXARA )
      DOUBLE PRECISION FBOUND( 2, NOBMAX ), WEIGHT( NEGMAX )
      DOUBLE PRECISION ABYROW( LA ), B( LB ), BL( LBNDS ), BU( LBNDS )
      DOUBLE PRECISION X( NMAX ), U( NUMAX ), ESCALE( NEGMAX )
      DOUBLE PRECISION VSCALE( NMAX ), GSCALE( NGMAX ), CLMULT( NGMAX )
      LOGICAL          INTREP( LINTRE ), LDEFND( NLMAX )
      LOGICAL          SETVEC( NSETVC ), GXEQX( NGMAX )
      CHARACTER * 1    S( 2 )
      CHARACTER * 2    FARRAY( MAXARA )
      CHARACTER * 4    ARE( 2 )
      CHARACTER * 8    PNAME
      CHARACTER * 10   NAMIIN( NINDEX ), NAMRIN( NRLNDX )
      CHARACTER * 10   LONAME( NINMAX ), BNAMES( NBMAX  )
      CHARACTER * 10   GNAMES( NGMAX  ), VNAMES( NMAX   )
      CHARACTER * 10   ETYPES( NLMAX  ), GTYPES( NGRMAX )
      CHARACTER * 10   LNAMES( NELMAX ), OBNAME( NOBMAX )
      CHARACTER * 10   EPNAME( NEPMAX ), GPNAME( NGPMAX )
      CHARACTER * 10   ONAMES( NOMAX  ), ENAMES( NETMAX )
      CHARACTER * 10   EXNAME( NINMAX ), SNAMES( NSMAX )
      CHARACTER * 10   ANAMES( NGRMAX ), INAMES( NIMAX  )
      CHARACTER * 10   MINAME( NINMAX ), RENAME( NINMAX )
      CHARACTER * 10   INNAME( NINMAX )
      CHARACTER * 10   ARRAY( 3, MAXARA ), CARRAY( 2, MAXARA )
      CHARACTER * 12   KEY   ( LENGTH )
      CHARACTER * 160  NULINE
      EXTERNAL         GPSMPS, INLANC, PRINTP, MAKEFN, MAKEGR, ONLY1
      DATA S / ' ', 's' /, ARE / ' is ', 'are ' /
      DATA ONEOBJ / .FALSE. /
      DEBUG = IPRINT .LT. 0
      IF ( SINGLE ) THEN
         WRITE( IOUT, 2050 )
      ELSE
         WRITE( IOUT, 2060 )
      END IF
      IF ( IPRINT .NE. 0 ) IPRINT = 9

!  READ THE GPS MPS DATA.

      CALL GPSMPS( LA    , NMAX  , NGMAX , NOMAX , NLMAX , NELMAX,             &
                   NIMAX , NETMAX, NEVMAX, NGRMAX, NSMAX , NEPMAX,             &
                   NGPMAX, NBMAX , NOBMAX, NNZA  , LENGTH, N , NG,             &
                   NOBJ  , NCONST, NRANGE, NBND  , NSTART, NELTYP,             &
                   NGRTYP, NLVARS, NNLVRS, NLISGP, LIWK  ,                     &
                   NELNUM, NELING, NARRAY, NINDEX, NEGMAX, NEPVMX,             &
                   NGPVMX, MAXINS, MAXLEV, MAXARA, NRLNDX, NOBBND,             &
                   PNAME , ICOORD, IELING, INLIST, ITABLE, ISTATE,             &
                   IELV  , IINV  , ITYPEE, IDROWS, IELVAR, ISTADG,             &
                   ITYPEG, IEPA  , IGPA  , IWK   , ISTEP , ISTAEV,             &
                   ISTGP , INDVAL, INSTR , NINSTR, IARRAY, ITYPEV,             &
                   A, BND, VSTART, CSTART, RSCALE, CSCALE, RDROWS,             &
                   REALVL, DFAULT, RVALUE, VARRAY, EPVALU, BNDFLT,             &
                   GPVALU, GPTEMP, FARRAY, FBOUND, WEIGHT, NAMIIN,             &
                   NAMRIN, GNAMES, VNAMES, BNAMES, ETYPES, INAMES,             &
                   LNAMES, ONAMES, ENAMES, SNAMES, ANAMES, GTYPES,             &
                   EPNAME, GPNAME, OBNAME, ARRAY , CARRAY, KEY   ,             &
                   SINGLE, IINGPS, IOUT  , INFORM, DEBUG )
      IF ( INFORM .NE. 0 ) THEN
         WRITE( IOUT, 2010 ) INFORM
         RETURN
      END IF

!  ASSIGN THE GROUPS TO CONSTRAINT TYPES AND OBJECTIVES.

      NLINOB = 0
      NNLNOB = 0
      NLINEQ = 0
      NNLNEQ = 0
      NLININ = 0
      NNLNIN = 0
      DO 100 IG = 1, NG
         ISG = ISTATE( IG )
         IF ( ISG .GT. 0 ) THEN
            ISG = MOD( ISG - 1, 4 )
            IF ( ISG .EQ. 0 ) NLINOB = NLINOB + 1
            IF ( ISG .EQ. 1 ) NLINEQ = NLINEQ + 1
            IF ( ISG .GE. 2 ) NLININ = NLININ + 1
         ELSE
            ISG = MOD( ISG + 1, 4 )
            IF ( ISG .EQ.   0 ) NNLNOB = NNLNOB + 1
            IF ( ISG .EQ. - 1 ) NNLNEQ = NNLNEQ + 1
            IF ( ISG .LE. - 2 ) NNLNIN = NNLNIN + 1
         END IF
  100 CONTINUE

!  SELECT RHS, RANGES AND BOUNDS.

      IF ( NCONST .GT. 0 ) NAMERH = VNAMES( NLVARS + 1 )
      IF ( NRANGE .GT. 0 ) NAMERA = VNAMES( NLVARS + NCONST + 1 )
      IF ( NBND   .GT. 0 ) NAMEBN = BNAMES( 1 )
      IF ( NSTART .GT. 0 ) NAMEST = SNAMES( 1 )
      IF ( NOBJ   .GT. 0 .AND. ONEOBJ ) NAMEOF = ONAMES( 1 )
      IF ( NOBBND .GT. 0 ) NAMEOB = OBNAME( 1 )
      IF ( IPRINT .NE. 0 ) WRITE( IOUT, 2070 ) NCONST, NRANGE, NBND,           &
           NSTART, NOBJ, NOBBND

!  CONVERT TO INPUT FOR ONE OF THE LANCELOT PROGRAMS.

      CALL INLANC( N     , NLVARS, NG    , NELNUM, NOBJ  , LENGTH,             &
                   LSTADG, LELVAR, LSTAEV, LNTVAR, LICNA , LSTADA,             &
                   LA, LB, LBNDS , LINTRE, LIWK  , LWK   , NMAX  ,             &
                   NGMAX , NBMAX , NSMAX , NLMAX , NELMAX, NEGMAX,             &
                   NOBMAX, NGRMAX, NGPVMX, NEPVMX, NOMAX , NLISGP,             &
                   NBND  , NNZA  , NCONST, NSTART, NRANGE, NOBJGR,             &
                   NOBBND, NELTYP, NGRTYP, PNAME , ONEOBJ,                     &
                   NAMEOB, NAMERH, NAMERA, NAMEBN, NAMEST, NAMEOF,             &
                   ISTADG, IELVAR, ISTAEV, INTVAR, ICNA  , ISTADA,             &
                   ICOORD, INLIST, ITABLE, ISTATE,                             &
                   IDROWS, IELV  , IINV  , IGPA  , IELING( 1, 1 ),             &
                   ISTEP , ISTGP , ITYPEE, ITYPEG, ITYPEV, IWK   ,             &
                   A, BND, VSTART, CSTART, RSCALE, CSCALE,                     &
                   RDROWS, DFAULT, WEIGHT, BNDFLT, WK    ,                     &
                   GPVALU, EPVALU, FBOUND, ABYROW, B , BL, BU, X ,             &
                   CLMULT, ESCALE, GSCALE, VSCALE, INTREP, GXEQX,              &
                   KEY   , GNAMES, VNAMES, BNAMES, SNAMES, ONAMES,             &
                   ETYPES, GTYPES, OBNAME, IALGOR, IAUTO,                      &
                   IOUT  , IOUTDA, SINGLE, INFORM, DEBUG )
      IF ( INFORM .NE. 0 ) THEN
         WRITE( IOUT, 2020 ) INFORM
         RETURN
      END IF


!  ASSIGN THE VARIABLES TO BOUND TYPES.

      NFREE = 0
      NFIXED = 0
      NLOWER = 0
      NUPPER = 0
      NBOTH = 0
      IF ( IALGOR .LE. 2 ) THEN
         NSLACK = NLININ + NNLNIN
      ELSE
         NSLACK = 0
      END IF
      NREAL = N - NSLACK
      DO 110 I = 1, NREAL
         BLO = BL( I )
         BUP = BU( I )
         IF ( BLO .LE. - BIG .AND. BUP .GE. BIG ) NFREE = NFREE  + 1
         IF ( BLO .LE. - BIG .AND. BUP .LT. BIG ) NUPPER = NUPPER + 1
         IF ( BLO .GT. - BIG .AND. BUP .GE. BIG ) NLOWER = NLOWER + 1
         IF ( BLO .GT. - BIG .AND. BUP .LT. BIG ) THEN
            IF ( BLO .EQ. BUP ) THEN
                NFIXED = NFIXED + 1
            ELSE
                NBOTH = NBOTH  + 1
            END IF
         END IF
  110 CONTINUE

!  PRINT PROBLEM SUMMARY.

      IF ( NLINOB .GT. 0 ) WRITE( IOUT, 2100 ) NLINOB, S( ONLY1( NLINOB ) )
      IF ( NNLNOB .GT. 0 ) WRITE( IOUT, 2110 ) NNLNOB, S( ONLY1( NNLNOB ) )
      IF ( NLINEQ + NLININ + NNLNEQ + NNLNIN .GT. 0 ) WRITE( IOUT, 2000)
      IF ( NLINEQ .GT. 0 ) WRITE( IOUT, 2120 ) ARE( ONLY1( NLINEQ ) ),         &
                NLINEQ, S( ONLY1( NLINEQ ) )
      IF ( NLININ .GT. 0 ) WRITE( IOUT, 2130 ) ARE( ONLY1( NLININ ) ),         &
                NLININ, S( ONLY1( NLININ ) )
      IF ( NNLNEQ .GT. 0 ) WRITE( IOUT, 2140 ) ARE( ONLY1( NNLNEQ ) ),         &
                NNLNEQ, S( ONLY1( NNLNEQ ) )
      IF ( NNLNIN .GT. 0 ) WRITE( IOUT, 2150 ) ARE( ONLY1( NNLNIN ) ),         &
                NNLNIN, S( ONLY1( NNLNIN ) )
      WRITE( IOUT, 2000 )
      IF ( NFREE  .GT. 0 ) WRITE( IOUT, 2200 ) ARE( ONLY1( NFREE  ) ),         &
                NFREE , S( ONLY1( NFREE  ) )
      IF ( NUPPER .GT. 0 ) WRITE( IOUT, 2210 ) ARE( ONLY1( NUPPER ) ),         &
                NUPPER, S( ONLY1( NUPPER ) )
      IF ( NLOWER .GT. 0 ) WRITE( IOUT, 2220 ) ARE( ONLY1( NLOWER ) ),         &
                NLOWER, S( ONLY1( NLOWER ) )
      IF ( NBOTH  .GT. 0 ) WRITE( IOUT, 2230 ) ARE( ONLY1( NBOTH  ) ),         &
                NBOTH,  S( ONLY1( NBOTH  ) )
      IF ( NFIXED .GT. 0 ) WRITE( IOUT, 2240 ) ARE( ONLY1( NFIXED ) ),         &
                NFIXED, S( ONLY1( NFIXED ) )
      IF ( NSLACK .GT. 0 ) WRITE( IOUT, 2250 ) ARE( ONLY1( NSLACK ) ),         &
                NSLACK, S( ONLY1( NSLACK ) )
      WRITE( IOUTDA, 2080 ) PNAME, NFREE , NFIXED, NLOWER, NUPPER, NBOTH,      &
                NSLACK, NLINOB, NNLNOB, NLINEQ, NNLNEQ, NLININ, NNLNIN

!  PRINT DETAILS OF THE PROBLEM.

      CALL PRINTP( NMAX, NGMAX, NLMAX, NELMAX, NETMAX,                         &
                   NEVMAX, NEPMAX, NGRMAX, NEGMAX, NEPVMX,                     &
                   NGPVMX, NGPMAX, LSTADA, LICNA, LIWK,                        &
                   N, NG, NLVARS, NELNUM,                                      &
                   ISTATE, ISTADG, IELVAR, ITYPEG, ITYPEE,                     &
                   IELV, IINV, IEPA, IGPA,                                     &
                   ISTADA, ICNA, ISTGP, ISTEP, ISTAEV,                         &
                   IELING, ITYPEV, IWK, ABYROW, B, BL, BU, X,                  &
                   EPVALU, GPVALU, GSCALE, ESCALE, VSCALE,                     &
                   PNAME, VNAMES, GNAMES,                                      &
                   LNAMES, ETYPES, ENAMES, ANAMES, EPNAME, GPNAME, GTYPES,     &
                   IOUT, IPRINT )
      IF ( NONAME ) PNAME = '        '

!  MAKE SUBROUTINES ELFUN AND RANGE.

      IF ( IAUTO .EQ. 0 ) THEN
         CALL MAKEFN( IINFN , IOUT  , IOUTFN, IOUTRA, INFORM,                  &
                      NLMAX , NIMAX , NETMAX, NINMAX, NUMAX ,                  &
                      NELNUM, NELTYP, PNAME , ENAMES, INAMES, RENAME,          &
                      INNAME, LONAME, MINAME, EXNAME, ETYPES, LDEFND,          &
                      LENGTH, ITABLE, KEY   , IELV  , IINV  , INLIST,          &
                      EPNAME, IEPA  , NEPMAX, DEBUG , IJUMP ,                  &
                      U     , SETVEC, NSETVC, SINGLE, NULINE, GOTLIN,          &
                      IPRINT )
         IF ( INFORM .NE. 0 ) THEN
            WRITE( IOUT, 2030 ) INFORM
            RETURN
         END IF

!  MAKE SUBROUTINES ELFUNF, ELFUND AND RANGE

      ELSE
         CALL MAFNAD( IINFN , IOUT  , IOUTFF, IOUTFD, IOUTRA,                  &
                      IOUTEM, INFORM, NLMAX , NIMAX , NETMAX,                  &
                      NINMAX, NUMAX , NELNUM, NELTYP, PNAME , ENAMES,          &
                      INAMES, RENAME, INNAME, LONAME, MINAME, EXNAME,          &
                      ETYPES, LDEFND, LENGTH, ITABLE, KEY   , IELV  ,          &
                      IINV  , INLIST, EPNAME, IEPA  , NEPMAX, DEBUG ,          &
                      IJUMP , U     , SETVEC, NSETVC, SINGLE, &
                      NULINE, GOTLIN, IAUTO , IAD0  , IPRINT )
         IF ( INFORM .NE. 0 ) THEN
            WRITE( IOUT, 2090 ) INFORM
            RETURN
         END IF
      END IF

!  MAKE SUBROUTINE GROUP AND OBTAIN GROUP INFORMATION.

      IF ( IAUTO .EQ. 0 ) THEN
         CALL MAKEGR( IINGR , IOUT  , IOUTGR, INFORM, NGRTYP,                  &
                      NGRMAX, NLMAX , NINMAX, PNAME , ANAMES,                  &
                      RENAME, INNAME, LONAME, MINAME, EXNAME, GTYPES,          &
                      LDEFND, GPNAME, IGPA  , NGPMAX, DEBUG , LENGTH,          &
                      ITABLE, KEY   , INLIST, SINGLE, NULINE, GOTLIN,          &
                      IPRINT )
         IF ( INFORM .NE. 0 ) THEN
            WRITE( IOUT, 2040 ) INFORM
            RETURN
         END IF

!  MAKE SUBROUTINES GROUPF AND GROUPD

      ELSE
         CALL MAGRAD( IINGR , IOUT  , IOUTGF, IOUTGD, IOUTEM, INFORM,          &
                      NGRTYP, NGRMAX, NLMAX , NINMAX,                          &
                      PNAME , ANAMES, RENAME, INNAME, LONAME, MINAME,          &
                      EXNAME, GTYPES, LDEFND, GPNAME, IGPA  , NGPMAX,          &
                      DEBUG , LENGTH, ITABLE, KEY   , INLIST, SINGLE,          &
                      NULINE, GOTLIN, IAUTO , IAD0  , IPRINT )
         IF ( INFORM .NE. 0 ) THEN
            WRITE( IOUT, 2160 ) INFORM
            RETURN
         END IF
      END IF

!  FINALLY, READ ANY ADDITIONAL PROGRAMS.

  500 CONTINUE
      IF ( GOTLIN ) THEN
         LINEEX( 1: 72 ) = NULINE( 1: 72 )
         GOTLIN = .FALSE.
      ELSE
         READ( UNIT = IINEX, FMT = 1000, END = 600, ERR = 600 ) LINEEX
      END IF

!  SKIP BLANK LINES.

      DO 510 I = 1, 72
         IF ( LINEEX( I: I ) .NE. ' ' ) THEN
            WRITE( IOUTEX, 1000 ) LINEEX
            GO TO 500
         END IF
  510 CONTINUE
      GO TO 500
  600 CONTINUE

!  IF REQUIRED, TRANSLATE ANY EXTERNAL FILE TO ACCEPT AUTOMATIC 
!  DIFFERENTIATION CONSTRUCTS

      IF ( IAUTO .EQ. 1 .OR. IAUTO .EQ. 2 )                                    &
        CALL TRANS( IOUT, IOUTEX, IOUTEA, IOUTEM, SINGLE, IAUTO,               &
                    IAD0, NAMRIN, NRLNDX, NAMIIN, NINDEX )
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 1000 FORMAT( A72 )
 2000 FORMAT( ' ' )
 2010 FORMAT( /, ' Return from GPSMPS, INFORM = ', I3 )
 2020 FORMAT( /, ' Return from INLANC, INFORM = ', I3 )
 2030 FORMAT( /, ' Return from MAKEFN, INFORM = ', I3 )
 2040 FORMAT( /, ' Return from MAKEGR, INFORM = ', I3 )
 2050 FORMAT( /, ' Single precision version will be formed. ', / )
 2060 FORMAT( /, ' Double precision version will be formed. ', / )
 2070 FORMAT( /, '  NCONST  NRANGE    NBND  NSTART    NOBJ  NOBBND ', /, 6I8, /)
 2080 FORMAT( A8, 12I8 )
 2090 FORMAT( /, ' Return from MAFNAD, INFORM = ', I3 )
 2100 FORMAT( ' The objective function uses ', I8, ' linear group', A1 )
 2110 FORMAT( ' The objective function uses ', I8, ' nonlinear group', A1 )
 2120 FORMAT( ' There ', A4, I8, ' linear equality constraint', A1 )
 2130 FORMAT( ' There ', A4, I8, ' linear inequality constraint', A1 )
 2140 FORMAT( ' There ', A4, I8, ' nonlinear equality constraint', A1 )
 2150 FORMAT( ' There ', A4, I8, ' nonlinear inequality constraint', A1 )
 2160 FORMAT( /, ' Return from MAGRAD, INFORM = ', I3 )
 2200 FORMAT( ' There ', A4, I8, ' free variable', A1 )
 2210 FORMAT( ' There ', A4, I8, ' variable', A1,                              &
                ' bounded only from above ' )
 2220 FORMAT( ' There ', A4, I8, ' variable', A1,                              &
                ' bounded only from below ' )
 2230 FORMAT( ' There ', A4, I8,                                               &
              ' variable', A1, ' bounded from below and above ' )
 2240 FORMAT( ' There ', A4, I8, ' fixed variable', A1 )
 2250 FORMAT( ' There ', A4, I8, ' slack variable', A1 )

!  END OF SDLANC.

      END

!  THIS VERSION: 13/04/2012 AT 13:45:00 GMT
! ** Correction report.
! ** Correction -6. 13/04/12: QMATRIX added as alias for QUADOBJ
! ** Correction -5. 07/09/00: Check for non-useful transformations added
! ** Correction -4. 07/09/00: Error return for too small NIMAX added
! ** Correction -3. 21/02/00: Code to handle incomplete/missing data added
! ** Correction -2. 21/02/00: QSECTION added as alias for QUADOBJ
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
! ** Correction 0. 20/12/99: Array holding variable types introduced.
! ** Correction 1. 30/11/93: 3 lines interchanged **
! ** Correction 2. 26/02/01: 4 dummy arguments removed from SVAR1 **
! ** Correction 3. 26/02/01: 1 dummy argument removed from SBOUND **
! ** Correction 4. 26/02/01: 1 dummy argument removed from SQHESS **
! ** Correction 5. 26/02/01: 2 dummy arguments removed from SETYPE **
! ** Correction 6. 26/02/01: 2 dummy arguments removed from SGTYPE **
! ** Correction 7. 26/02/01: 2 dummy arguments removed from SOBBND **
! ** Correction 8. 26/02/01: 1 dummy argument removed from PROCAA **
! ** Correction 9. 27/02/01: Character debug output format increased **
! ** Correction 10. 16/04/02: No double assignment of internals for quad terms
! ** Correction 11  30/06/04: Assigned goto statements replaced
! ** End of Correction report.

      SUBROUTINE GPSMPS( LA    , NMAX  , NGMAX , NOMAX , NLMAX , NELMAX,  &
                         NIMAX , NETMAX, NEVMAX, NGRMAX, NSMAX , NEPMAX,  &
                         NGPMAX, NBMAX , NOBMAX, NNZA  , LENGTH, N , NG,  &
                         NOBJ  , NCONST, NRANGE, NBND  , NSTART, NELTYP,  &
                         NGRTYP, NLVARS, NNLVRS, NLISGP, LIWK  ,          &
                         NELNUM, NELING, NARRAY, NINDEX, NEGMAX, NEPVMX,  &
                         NGPVMX, MAXINS, MAXLEV, MAXARA, NRLNDX, NOBBND,  &
                         PNAME , ICOORD, IELING, INLIST, ITABLE, ISTATE,  &
                         IELV  , IINV  , ITYPEE, IDROWS, IELVAR, ISTADG,  &
                         ITYPEG, IEPA  , IGPA  , IWK   , ISTEP , ISTAEV,  &
                         ISTGP , INDVAL, INSTR , NINSTR, IARRAY, ITYPEV,  &
                         A, BND, VSTART, CSTART, RSCALE, CSCALE, RDROWS,  &
                         REALVL, DFAULT, RVALUE, VARRAY, EPVALU, BNDFLT,  &
                         GPVALU, GPTEMP, FARRAY, FBOUND, WEIGHT, NAMIIN,  &
                         NAMRIN, GNAMES, VNAMES, BNAMES, ETYPES, INAMES,  &
                         LNAMES, ONAMES, ENAMES, SNAMES, ANAMES, GTYPES,  &
                         EPNAME, GPNAME, OBNAME, ARRAY , CARRAY, KEY   ,  &
                         SINGLE, INPUT , IOUT  , INFORM, DEBUG )
      INTEGER          LA    , NMAX  , NGMAX , NOMAX , NLMAX , NELMAX
      INTEGER          NEVMAX, NGRMAX, NSMAX , NOBMAX, NOBBND, NRLNDX
      INTEGER          NEPMAX, NGPMAX, NBMAX , NNZA  , LENGTH, N , NG
      INTEGER          NCONST, NRANGE, NBND  , NSTART, NELTYP, NGRTYP
      INTEGER          NLVARS, NNLVRS, NELNUM, NELING, NARRAY, NINDEX
      INTEGER          NEGMAX, NEPVMX, NGPVMX, MAXINS, MAXLEV, MAXARA
      INTEGER          NLISGP, INPUT , NETMAX, IOUT  , INFORM, NIMAX
      INTEGER          NOBJ  , LIWK
      LOGICAL          SINGLE, DEBUG
      CHARACTER * 8    PNAME
      INTEGER          INLIST( LENGTH ), ISTAEV( NELMAX )
      INTEGER          ISTATE( NGMAX ), ITABLE ( LENGTH )
      INTEGER          IELV  ( NLMAX  ), IINV  ( NLMAX )
      INTEGER          ITYPEE( NELMAX ), IELING( NEGMAX, 2 )
      INTEGER          IEPA  ( NLMAX  ), IGPA  ( NGRMAX )
      INTEGER          IDROWS( 2, NGMAX ), ITYPEG( NGMAX )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
      INTEGER          IELVAR( NEVMAX ), ITYPEV( NMAX )
      INTEGER          ISTEP ( NELMAX ), ISTGP( NGMAX ), IWK( LIWK )
      INTEGER          INDVAL( NINDEX ), INSTR( 5, MAXINS, MAXLEV )
      INTEGER          NINSTR( MAXLEV ), IARRAY( 5, 3, MAXARA )
      INTEGER          ICOORD( LA, 2 ) , ISTADG( NGMAX )
      DOUBLE PRECISION GPTEMP( NGPVMX )
      DOUBLE PRECISION EPVALU( NEPVMX ), GPVALU( NGPVMX ), DFAULT( NMAX)
      DOUBLE PRECISION A( LA ), BND( 2, NMAX, NBMAX ), REALVL( NRLNDX )
      DOUBLE PRECISION BNDFLT( 2, NBMAX ), CSTART( NGMAX, NSMAX )
      DOUBLE PRECISION RSCALE( NGMAX ), CSCALE( NMAX )
      DOUBLE PRECISION RDROWS( 2, NGMAX ), VSTART( NMAX, NSMAX )
      DOUBLE PRECISION RVALUE( MAXARA, 3 ), VARRAY( 2, MAXARA )
      DOUBLE PRECISION FBOUND( 2, NOBMAX ), WEIGHT( NEGMAX )
      CHARACTER * 2    FARRAY( MAXARA )
      CHARACTER * 10   NAMIIN( NINDEX ), NAMRIN( NRLNDX )
      CHARACTER * 10   BNAMES( NBMAX  )
      CHARACTER * 10   GNAMES( NGMAX  ), VNAMES( NMAX   )
      CHARACTER * 10   ETYPES( NLMAX  ), INAMES( NIMAX  )
      CHARACTER * 10   LNAMES( NELMAX ), OBNAME( NOBMAX )
      CHARACTER * 10   EPNAME( NEPMAX ), GPNAME( NGPMAX )
      CHARACTER * 10   ONAMES( NOMAX  ), ENAMES( NETMAX )
      CHARACTER * 10   SNAMES( NSMAX )
      CHARACTER * 10   ANAMES( NGRMAX ), GTYPES( NGRMAX )
      CHARACTER * 10   ARRAY( 3, MAXARA ), CARRAY( 2, MAXARA )
      CHARACTER * 12   KEY   ( LENGTH )

!  .....................................................................

!  READ A GPS MPS DATA FILE.
!  -------------------------

!  NICK GOULD 12/08/1989
!  FOR CGT PRODUCTIONS.

!  MPS INDICATOR CARDS.
!  --------------------

!  DEFINITION   PURPOSE.
!  ----------   --------
!  NAME         PROBLEM NAME.
!  ROWS         NAMES OF ROWS (ALIAS GROUP NAMES).
!  COLUMNS      NAMES OF COLUMNS (ALIAS VARIABLE NAMES).
!  RHS          RIGHT-HAND-SIDES (ALIAS CONSTANT TERMS IN GROUPS).
!  RHS'         ALIAS FOR RHS.
!  RANGES       ADDITIONAL BOUNDS ON ROWS.
!  BOUNDS       BOUNDS ON COLUMNS.
!  ENDATA       END OF INPUT DATA.

!  ADDITIONAL INDICATOR CARDS.
!  ---------------------------

!  DEFINITION   PURPOSE.
!  ----------   --------
!  GROUPS       ALIAS FOR ROWS.
!  CONSTRAINTS  ALIAS FOR ROWS.
!  VARIABLES    ALIAS FOR COLUMNS.
!  CONSTANTS    ALIAS FOR RHS.
!  START_POINT  ESTIMATE OF MINIMIZER.
!  HESSIAN      QUADRATIC TERMS
!  QUADRATIC    ALIAS FOR HESSIAN
!  QUADS        ALIAS FOR HESSIAN
!  QUADOBJ      ALIAS FOR HESSIAN
!  QSECTION     ALIAS FOR HESSIAN
!  QMATRIX      ALIAS FOR HESSIAN
!  ELEMENT_TYPE TYPES OF NONLINEAR ELEMENTS.
!  ELEMENT_USES DEFINITIONS OF NONLINEAR ELEMENTS.
!  GROUP_TYPE   TYPES OF NONTRIVIAL GROUPS.
!  GROUP_USES   DEFINITIONS OF GROUPS.

!  DATA CARD DESCRIPTION.
!  ----------------------

!  SEE 'A PROPOSAL FOR A STANDARD DATA INPUT FORMAT FOR LARGE-SCALE
!       NONLINEAR PROGRAMMING PROBLEMS', SECTION 2,
!       A. R. CONN, N. I. M. GOULD AND PH. L. TOINT,
!       REPORT CS-89-61, DEPT OF COMPUTER SCIENCE, U. OF WATERLOO,
!       WATERLOO, ONTARIO, N2L3G1, CANADA.

!  -------------------------------------------------------------------
!  RETURNS WITH NEGATIVE VALUES OF INFORM INDICATE THAT INSUFFICIENT
!  ARRAY SPACE HAS BEEN ALLOWED, AS FOLLOWS:

!    INFORM = - 1  WHEN LENGTH NOT LARGE ENOUGH
!    INFORM = - 2  WHEN NNZA .GT. LA
!    INFORM = - 3  WHEN NELTYP .GE. NLMAX
!    INFORM = - 4  WHEN NGRTYP .GE. NGRMAX
!    INFORM = - 5  WHEN NOBJ .GT. NOMAX
!    INFORM = - 6  WHEN NG .GT. NGMAX
!    INFORM = - 7  WHEN N .GT. NMAX
!    INFORM = - 8  WHEN NSTART .GT. NSMAX
!    INFORM = - 9  WHEN NELNUM .GT. NELMAX
!    INFORM = - 10 WHEN NELING .GT. NEGMAX
!    INFORM = - 11 WHEN NINSTR( 1 OR 2 OR 3 ) .GT. MAXINS
!    INFORM = - 12 WHEN NARRAY .GT. MAXARA
!    INFORM = - 13 WHEN NBND .GT. NBMAX
!    INFORM = - 14 WHEN NELN .GT. NETMAX
!    INFORM = - 15 WHEN NLISEV .GT. NEVMAX
!    INFORM = - 16 WHEN NINN .GT. NIMAX
!    INFORM = - 17 WHEN NLISEP .GT. NEPVMX
!    INFORM = - 18 WHEN NLISGP .GT. NGPVMX
!    INFORM = - 19 WHEN NEPN .GT. NEPMAX
!    INFORM = - 20 WHEN NGPN .GT. NGPMAX
!    INFORM = - 21 WHEN NUSEIN .GT. NINDEX
!    INFORM = - 22 WHEN NUSERE .GT. NRLNDX
!    INFORM = - 23 WHEN NOBBND .GT. NOBMAX

!  .....................................................................

      INTEGER          I, IP, IS, INTYPE, INTYPO, J, K, K1, K2, L
      INTEGER          IFREE, IFIELD, NOVALS, NVAR, NCOL, NELN, NINN
      INTEGER          MBLANK, MNAME, MROWS, MGROUP, MCNSTR, MOBBND
      INTEGER          MVARS, MCONST, MRHS, MRHSP, MRANGE, MBOUND, MCOLS
      INTEGER          MSTART, METYPE, MGTYPE, MEUSES, MGUSES, MENDAT
      INTEGER          MFREE, MFIXED, MAXNUL, NLINES, ILINES, MXRECL
      INTEGER          NEPN, NGPN, NGRUPE, NELMNT, NUSEIN, NUSERE
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      INTEGER          MQHESS, MQUADO, MQUADR, MQUADS, IPTYPE, ISTYPE
! ** Correction -2. 21/02/00: QSECTION added as alias for QUADOBJ
! ** Correction -6. 13/04/12: QMATRIX added as alias for QUADOBJ
      INTEGER          MQSECT, MQMATR
      INTEGER          NLISEP, NLISEV, L1, L2, L3, LEVEL2, NDTYPE
      INTEGER          LEVEL3, LEV1, LEV2, LEV3, LEV1S, LEV2S, LEV3S
      INTEGER          LEV1E, LEV2E, LEV3E, LEV1I, LEV2I, LEV3I, LEVL3A
      INTEGER          LEVEL, IJUMP, NINCRS, LINENO, LOOP( 4 )
      DOUBLE PRECISION BIG, ONE, ZERO, VALUE4, VALUE6
      LOGICAL          DEFNAM, INCARD, INREP, DEFAUT, DOLOOP, STRTGU
      LOGICAL          ENDBND, ENDST, SETANA, ENDELT, ENDGRT, DELSET
      LOGICAL          ENDELU, GRP1ST, GRPYET, VARYET, FIXED, DGRSET
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      LOGICAL          QGROUP, QSQR, QPROD
      CHARACTER * 2    FIELD1, COLFIE
      CHARACTER * 10   FIELD2, FIELD3, FIELD5, GRUPE, ELMNT
      CHARACTER * 10   DETYPE, DGTYPE
      CHARACTER * 12   FIELD
      EXTERNAL         SGRP1, SGRP2, SVAR1, SVAR2, SBOUND, SSTART
      EXTERNAL         SETYPE, SEUSES, SGTYPE, SGUSES, SOBBND, GETRIN
      EXTERNAL         GETVAL, GETLIN, GETIIN, PROCAI, PROCAD, PROCAA
      EXTERNAL         HASHA , HASHB , HASHC, REORDA
      INTRINSIC        ABS

!  PARAMETER DEFINITIONS.

      PARAMETER        ( MXRECL = 160 )
      CHARACTER * 160  NULINE, BLNKLN
      PARAMETER        ( NINCRS = 23 )
      CHARACTER * 6    INCRSE( NINCRS )
      PARAMETER        ( MBLANK =  1, MFIXED =  2, MFREE = 3  )
      PARAMETER        ( MNAME =  4, MROWS =  5 )
      PARAMETER        ( MGROUP =  6, MCNSTR =  7, MCOLS =  8 )
      PARAMETER        ( MVARS =  9, MCONST = 10, MRHS = 11 )
      PARAMETER        ( MRHSP = 12, MRANGE = 13, MBOUND = 14 )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      PARAMETER        ( MSTART = 15, MQHESS = 16, MQUADR = 17 )
! ** Correction -2. 21/02/00: QSECTION added as alias for QUADOBJ
      PARAMETER        ( MQUADS = 18, MQUADO = 19, MQSECT = 20 )
! ** Correction -6. 13/04/12: QMATRIX added as alias for QUADOBJ
      PARAMETER        ( MQMATR = 21 )
      PARAMETER        ( METYPE = 22, MEUSES = 23, MGTYPE = 24 )
      PARAMETER        ( MGUSES = 25, MOBBND = 26, MENDAT = 27 )
! ** Correction  10. 16/04/02: 3 lines added
      CHARACTER * 10   CQSQR, CQPROD
      PARAMETER      ( CQSQR = '123456789S' )
      PARAMETER      ( CQPROD = '123456789P' )
      INTEGER          LENIND( MENDAT )
      CHARACTER * 12   INDIC8( MENDAT ), HEADER
      PARAMETER        ( MAXNUL = 20 )
      CHARACTER * 65   NULINA( MAXNUL )
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0, BIG = 1.0D+20 )
! ** Correction 11. 18/10/06: 1 line added
      LOGICAL          AddDoLoop

!  DATA DECLARATIONS.

      DATA INCRSE / 'LENGTH', 'LA    ', 'NLMAX ', 'NGRMAX', 'NOMAX ',  &
                    'NGMAX ', 'NMAX  ', 'NSMAX ', 'NELMAX', 'NEGMAX',  &
                    'MAXINS', 'MAXARA', 'NBMAX ', 'NETMAX', 'NEVMAX',  &
                    'NIMAX ', 'NEPVMX', 'NGPVMX', 'NEPMAX', 'NGPMAX',  &
                    'NINDEX', 'NRLNDX', 'NOBMAX' /
      DATA INDIC8( MBLANK ) / '            ' /, LENIND( MBLANK ) / 0  /
      DATA INDIC8( MFIXED ) / 'FIXED FORMAT' /, LENIND( MFIXED ) / 12 /
      DATA INDIC8( MFREE  ) / 'FREE FORMAT ' /, LENIND( MFREE  ) / 11 /
      DATA INDIC8( MNAME  ) / 'NAME        ' /, LENIND( MNAME  ) / 4  /
      DATA INDIC8( MROWS  ) / 'ROWS        ' /, LENIND( MROWS  ) / 4  /
      DATA INDIC8( MGROUP ) / 'GROUPS      ' /, LENIND( MGROUP ) / 6  /
      DATA INDIC8( MCNSTR ) / 'CONSTRAINTS ' /, LENIND( MCNSTR ) / 11 /
      DATA INDIC8( MCOLS  ) / 'COLUMNS     ' /, LENIND( MCOLS  ) / 7  /
      DATA INDIC8( MVARS  ) / 'VARIABLES   ' /, LENIND( MVARS  ) / 9  /
      DATA INDIC8( MCONST ) / 'CONSTANTS   ' /, LENIND( MCONST ) / 9  /
      DATA INDIC8( MRHS   ) / 'RHS         ' /, LENIND( MRHS   ) / 3  /
      DATA INDIC8( MRHSP  ) / 'RHS''       ' /, LENIND( MRHSP  ) / 4  /
      DATA INDIC8( MRANGE ) / 'RANGES      ' /, LENIND( MRANGE ) / 6  /
      DATA INDIC8( MBOUND ) / 'BOUNDS      ' /, LENIND( MBOUND ) / 6  /
      DATA INDIC8( MSTART ) / 'START POINT ' /, LENIND( MSTART ) / 11 /
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      DATA INDIC8( MQHESS ) / 'HESSIAN     ' /, LENIND( MQHESS ) / 7  /
      DATA INDIC8( MQUADR ) / 'QUADRATIC   ' /, LENIND( MQUADR ) / 9  /
      DATA INDIC8( MQUADS ) / 'QUADS       ' /, LENIND( MQUADS ) / 5  /
      DATA INDIC8( MQUADO ) / 'QUADOBJ     ' /, LENIND( MQUADO ) / 7  /
! ** Correction -2. 21/02/00: QSECTION added as alias for QUADOBJ
      DATA INDIC8( MQSECT ) / 'QSECTION    ' /, LENIND( MQSECT ) / 8  /
! ** Correction -6. 13/04/12: QMATRIX added as alias for QUADOBJ
      DATA INDIC8( MQMATR ) / 'QMATRIX    '  /, LENIND( MQMATR ) / 7  /
      DATA INDIC8( METYPE ) / 'ELEMENT TYPE' /, LENIND( METYPE ) / 12 /
      DATA INDIC8( MEUSES ) / 'ELEMENT USES' /, LENIND( MEUSES ) / 12 /
      DATA INDIC8( MGTYPE ) / 'GROUP TYPE  ' /, LENIND( MGTYPE ) / 10 /
      DATA INDIC8( MGUSES ) / 'GROUP USES  ' /, LENIND( MGUSES ) / 10 /
      DATA INDIC8( MOBBND ) / 'OBJECT BOUND' /, LENIND( MOBBND ) / 12 /
      DATA INDIC8( MENDAT ) / 'ENDATA      ' /, LENIND( MENDAT ) / 6  /

!  SET INITIAL VALUES FOR INTEGER VARIABLES.

      INTYPE = 1
      INTYPO = 1
      INFORM = 0
      LINENO = 0
      NVAR = 0
      NNZA = 0
      NG = 0
      NBND = 0
      NSTART = 0
      NOBJ = 0
      NELTYP = 0
      NGRTYP = 0
      NELNUM = 0
      NGRUPE = 0
      NLISEV = 0
      NLISEP = 0
      NLISGP = 0
      NELING = 0
      NUSEIN = 0
      NUSERE = 0
      NOBBND = 0
      NDTYPE = 0
      NELN = 0
      NINN = 0
      NEPN = 0
      NLVARS = - 1
      NNLVRS = - 1
      NCONST = - 1
      NRANGE = - 1
      LEVEL = 0
      ILINES = 0
      NLINES = 0
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      IPTYPE = 0
      ISTYPE = 0

!  SET INITIAL VALUES FOR LOGICAL VARIABLES.

      DEFNAM = .FALSE.
      DOLOOP = .FALSE.
      ENDBND = .FALSE.
      ENDST = .FALSE.
      ENDELT = .FALSE.
      ENDELU = .FALSE.
      ENDGRT = .FALSE.
      STRTGU = .FALSE.
      GRPYET = .FALSE.
      VARYET = .FALSE.
      DELSET = .FALSE.
      DGRSET = .FALSE.
      GRP1ST = .TRUE.
      FIXED = .TRUE.
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      QGROUP = .FALSE.
      QSQR = .FALSE.
      QPROD = .FALSE.

!  SET INITIAL VALUES FOR REAL VARIABLES.

      VALUE4 = 0.0D+0
      VALUE6 = 0.0D+0

!  SET UP ITABLE DATA.

      CALL HASHA ( LENGTH, ITABLE )

!  INITIALIZE ROW DATA.

      DO 10 I = 1, NGMAX
         RSCALE( I ) = ONE
   10 CONTINUE

!  INITIALIZE COLUMN DATA.

      DO 20 I = 1, NMAX
! ** Correction 0. 20/12/99: Array holding variable types introduced.
         ITYPEV( I ) = 0
         CSCALE( I ) = ONE
         BND( 1, I, 1 ) = ZERO
         BND( 2, I, 1 ) = BIG
   20 CONTINUE

!  INITIALIZE DICTIONARY DATA.

      DO 30 I = 1, LENGTH
         INLIST( I ) = 0
   30 CONTINUE

!  SET A BLANK LINE.

      DO 40 I = 1, MXRECL
         BLNKLN( I: I ) = ' '
   40 CONTINUE

!  START OF MAIN LOOP.

  100 CONTINUE
      IF ( ILINES + 1 .GT. NLINES ) THEN

!  READ NEXT LINE FROM THE INPUT FILE.

         LINENO = LINENO + 1
         NULINE = BLNKLN
         IF ( FIXED ) THEN
! ** Correction -3. 21/02/00: Code to handle incomplete/missing data added
            READ ( INPUT, 1000, END = 810, ERR = 810 ) NULINE
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2990 )  &
                 LINENO, NULINE
         ELSE
! ** Correction -3. 21/02/00: Code to handle incomplete/missing data added
            READ ( INPUT, 1010, END = 810, ERR = 810 ) NULINE
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2970 )  &
                 LINENO, NULINE

!  IF THE CARD IS IN FREE FORMAT, TRANSLATE IT INTO FIXED FORMAT.

            CALL  FREEFM( NULINE, MXRECL, MENDAT, INDIC8, LENIND,  &
                          NULINA, MAXNUL, NLINES, .TRUE., INFORM, IOUT )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( NLINES .GT. 0 ) THEN

!  IF THERE ARE NON-BLANK LINES ON THE FREE FORMAT CARD, READ THE FIRST.

               ILINES = 1
               NULINE = BLNKLN
               NULINE = NULINA( ILINES )
               IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
                    LINENO, ILINES, NULINE
            ELSE

!  THERE ARE ONLY BLANK LINES ON THE FREE FORMAT CARD.

               GO TO 100
            END IF
         END IF
      ELSE

!  READ NEXT LINE FROM THE LAST ENCOUNTERED FREE FORMAT CARD.

         ILINES = ILINES + 1
         NULINE = BLNKLN
         NULINE = NULINA( ILINES )
         IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
              LINENO, ILINES, NULINE
      END IF

!  CONSIDER THE HEADER PART OF THE CARD.

      HEADER = NULINE( 1: 12 )

!  IGNORE BLANK LINES.

      IF ( HEADER .EQ. INDIC8( MBLANK ) ) THEN
         IF (  NULINE( 13: 14 ) .EQ. '  ' .AND.&
               NULINE( 15: 24 ) .EQ. '          ' .AND.&
               NULINE( 40: 49 ) .EQ. '          ' ) GO TO 100
      END IF
      IF ( NULINE( 1: 1 ) .NE. ' ' ) THEN

!  IGNORE COMMENT CARDS.

         IF ( NULINE( 1: 1 ) .EQ. '*' ) GO TO 100

!  CHECK IF WE HAVE ENTERED FIXED-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFIXED ) ) THEN
            FIXED = .TRUE.
            GO TO 100
         END IF

!  CHECK IF WE HAVE ENTERED FREE-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFREE ) ) THEN
            FIXED = .FALSE.
            GO TO 100
         END IF

!  CHECK THAT THE FIRST ENCOUNTERED INDICATOR CARD IS THE NAME CARD.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( HEADER .NE. INDIC8( MNAME ) ) THEN
               INFORM = 1
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2010 )
               GO TO 800
            ELSE

!  INDICATOR CARD IS NAME.
!  -----------------------

               DEFNAM = .TRUE.
               PNAME = NULINE( 15: 22 )
               GO TO 100
            END IF
         END IF
         INCARD = .TRUE.

!  AN INDICATOR CARD HAS BEEN FOUND.

         IF ( .NOT. GRP1ST ) INTYPE = MROWS
         DO 110 I = INTYPE, MENDAT
            IF ( HEADER .EQ. INDIC8( I ) ) THEN
               INTYPE = I
               GO TO 120
            END IF
  110    CONTINUE

!  THE INDICATOR CARD IS NOT RECOGNISED.

         INFORM = 2
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2020 )
         GO TO 800
  120    CONTINUE
         IF ( INTYPE .EQ. MGROUP .OR. INTYPE .EQ. MCNSTR) INTYPE = MROWS
         IF ( INTYPE .EQ. MRHS .OR. INTYPE .EQ. MRHSP ) INTYPE = MCONST
         IF ( INTYPE .EQ. MVARS ) INTYPE = MCOLS
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
! ** Correction -2. 21/02/00: QSECTION added as alias for QUADOBJ
! ** Correction -6. 13/04/12: QMATRIX added as alias for QUADOBJ
         IF ( INTYPE .EQ. MQUADR .OR. INTYPE .EQ. MQUADS .OR. &
              INTYPE .EQ. MQUADO .OR. INTYPE .EQ. MQSECT .OR.&
              INTYPE .EQ. MQMATR ) &
            INTYPE = MQHESS

!  ENSURE THAT THE GROUPS AND VARIABLES SECTIONS DO NOT GET MIXED UP.

         IF ( .NOT. GRP1ST .AND. VARYET .AND. INTYPE .EQ. MCOLS ) THEN
            INFORM = 21
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2210 )
            GO TO 800
         END IF
         IF ( INTYPE .EQ. MROWS ) GRPYET = .TRUE.
         IF ( INTYPE .EQ. MCOLS ) VARYET = .TRUE.
         IF ( VARYET .AND. .NOT. GRPYET ) GRP1ST = .FALSE.

!  ENSURE THAT PREVIOUSLY STARTED DO-LOOPS HAVE BEEN FINISHED.

         IF ( INTYPE .NE. INTYPO .AND. DOLOOP ) THEN
            INFORM = 38
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2380 )
            GO TO 800
         END IF
         INTYPO = INTYPE

!  ALL OF THE LINEAR VARIABLES HAVE BEEN SPECIFIED.

         IF ( INTYPE .GE. MCONST ) THEN
            IF ( NLVARS .LT. 0 ) THEN
               NLVARS = NVAR
               N = NLVARS
            END IF
         END IF

!  THE RIGHT-HAND-SIDE VECTORS HAVE BEEN COMPLETED.

         IF ( INTYPE .GE. MRANGE ) THEN
            IF ( NCONST .LT. 0 ) NCONST = NVAR - NLVARS
            IF ( NCONST .EQ. 0 ) THEN
               NCONST = 1
               NVAR = NVAR + 1
            END IF
         END IF

!  THE RANGE VECTORS HAVE BEEN COMPLETED.

         IF ( INTYPE .GE. MBOUND ) THEN
            IF ( NRANGE .LT. 0 ) NRANGE = NVAR - NCONST - NLVARS
         END IF

!  THE BOUND VECTORS HAVE BEEN COMPLETED.

         IF ( INTYPE .GE. MSTART ) THEN
            IF ( .NOT. ENDBND ) THEN
               ENDBND = .TRUE.
               IF ( NBND .EQ. 0 ) NBND = 1
            END IF
         END IF

!  THE STARTING VECTORS HAVE BEEN COMPLETED.

! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
         IF ( INTYPE .GE. MQHESS ) THEN
            IF ( .NOT. ENDST ) THEN
               ENDST = .TRUE.
               IF ( NSTART .EQ. 0 ) NSTART = 1
            END IF
         END IF
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.

!  THE QUADRATIC HESSIAN HAS BEEN COMPLETED.

         IF ( INTYPE .GE. METYPE ) THEN
         END IF

!  THE ELEMENT TYPES HAVE ALL BEEN SPECIFIED.

         IF ( INTYPE .GE. MEUSES ) THEN
            IF ( .NOT. ENDELT ) THEN
               ENDELT = .TRUE.

!  IF THE LAST ELEMENT HAS NO EXPLICIT INTERNAL REPRESENTATION,
!  USE ITS ELEMENTAL REPRESENTATION.

               IF ( NELTYP .GT. 0 ) THEN
! ** Correction  10. 16/04/02: 2 lines added
                  IF ( ETYPES( NELTYP ) .NE. CQSQR .AND.&
                       ETYPES( NELTYP ) .NE. CQPROD ) THEN
                  IF ( .NOT. INREP ) THEN
                     DO 150 K = IELV( NELTYP ), NELN
                        NINN = NINN + 1
! ** Correction -4. 07/09/00: Error return for too small NIMAX added
                        IF ( NINN .GT. NIMAX ) THEN
                           INFORM = - 16
                           GO TO 700
                        END IF
                        INAMES( NINN ) = ENAMES( K )
  150                CONTINUE
! ** Correction -5a. 07/09/00: Check for non-useful transformations added
                  ELSE
                     IF ( NINN - IINV( NELTYP ) .GE.&
                          NELN - IELV( NELTYP ) ) THEN
                        INFORM = 76
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2760 )
                        GO TO 800
                     END IF
                  END IF
! ** Correction  10. 16/04/02: 1 line added
                  END IF
                  IF ( NELTYP .GE. NLMAX ) THEN
                     INFORM = - 3
                     GO TO 700
                  END IF
               END IF
               IELV( NELTYP + 1 ) = NELN + 1
               IINV( NELTYP + 1 ) = NINN + 1
               IEPA( NELTYP + 1 ) = NEPN + 1
            END IF
         END IF

!  THE NONLINEAR ELEMENTS HAVE ALL BEEN SPECIFIED.

         IF ( INTYPE .GE. MGTYPE ) THEN

!  CHECK IF THERE ARE ANY NONLINEAR VARIABLES.

            IF ( NNLVRS .LT. 0 ) NNLVRS = N - NLVARS
            IF ( .NOT. ENDELU ) THEN
               ENDELU = .TRUE.
               IF ( NELNUM .GT. 0 ) THEN

!  CHECK THAT THE NONLINEAR ELEMENTS HAVE BEEN COMPLETELY SPECIFIED.
!  FIRST CHECK THE PARAMETER VALUES HAVE BEEN SET.

                  DO 153 J = 1, NELNUM
                     K = ITYPEE( J )
                     IP = IEPA( K ) - 1
                     K1 = IEPA( K + 1 ) - IEPA( K )
                     K2 = ISTEP( J ) - 1
                     DO 151 I = 1, K1
                        IF ( EPVALU( K2 + I ) .EQ. BIG ) THEN
                           INFORM = 28
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2280 )  &
                              LNAMES( J ), EPNAME( IP + I )
                        END IF
  151                CONTINUE

!  NOW CHECK THE ELEMENTAL VARIABLES HAVE BEEN SET.

                     IS = IELV( K ) - 1
                     K1 = IELV( K + 1 ) - IELV( K )
                     K2 = ISTAEV( J ) - 1
                     DO 152 I = 1, K1
                        IF ( IELVAR( K2 + I ) .EQ. 0 ) THEN
                           INFORM = 16
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2160 )  &
                              LNAMES( J ), ENAMES( IS + I )
                        END IF
  152                CONTINUE
  153             CONTINUE
               END IF
               IF ( INFORM .NE. 0 ) RETURN
            END IF
            ISTEP( NELNUM + 1 ) = NLISEP + 1
            ISTAEV( NELNUM + 1 ) = NLISEV + 1
         END IF

!  THE GROUP TYPES HAVE ALL BEEN SPECIFIED.

         IF ( INTYPE .GE. MGUSES .AND. NGRTYP .GT. 0 ) THEN

!  CHECK THAT THE ARGUMENT FOR THE LAST GROUP-TYPE HAS BEEN SET.

            IF ( .NOT. ENDGRT ) THEN
               ENDGRT = .TRUE.
               IF ( .NOT. SETANA ) THEN
                  INFORM = 25
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2250 )
                  RETURN
               END IF
               IF ( NGRTYP .GE. NGRMAX ) THEN
                  INFORM = - 4
                  GO TO 700
               END IF
               IGPA( NGRTYP + 1 ) = NGPN + 1
            END IF
         END IF
         IF ( INTYPE .EQ. MENDAT ) THEN

!  CHECK THAT THE GROUPS HAVE BEEN COMPLETELY SPECIFIED BY
!  CHECKING THAT THE PARAMETER VALUES HAVE BEEN SET.

            NLISGP = 0
            DO 157 J = 1, NG
               K = ITYPEG( J )
               IF ( K .LT. 0 ) THEN
                  K = - K - 1
                  ITYPEG( J ) = K
                  ISTGP( J ) = NLISGP + 1
                  IF ( K .NE. 0 ) THEN
                    K1 = IGPA( K + 1 ) - IGPA( K )
                    IF ( K1 .GT. 0 ) THEN
                       IP = IGPA( K ) - 1
                       K2 = ISTGP( J ) - 1
                       INFORM = 34
                       DO 155 I = 1, K1
                          NLISGP = NLISGP + 1
                          GPVALU( NLISGP ) = BIG
                          IF ( IOUT .GT. 0 ) WRITE( IOUT, 2340 )  &
                             GNAMES( J ), GPNAME( IP + I )
  155                 CONTINUE
                    END IF 
                  END IF 
               ELSE IF ( K .EQ. 0 ) THEN
                  ISTGP( J ) = NLISGP + 1
               ELSE
                  ISTGP( J ) = NLISGP + 1
                  IP = IGPA( K ) - 1
                  K1 = IGPA( K + 1 ) - IGPA( K )
                  K2 = ISTGP( J ) - 1
                  DO 156 I = 1, K1
                     NLISGP = NLISGP + 1
                     GPVALU( NLISGP ) = GPTEMP( K2 + I )
                     IF ( GPVALU( NLISGP ) .EQ. BIG ) THEN
                        INFORM = 34
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2340 )  &
                            GNAMES( J ), GPNAME( IP + I )
                     END IF
  156             CONTINUE
               END IF
  157       CONTINUE
            ISTGP( NG + 1 ) = NLISGP + 1
            IF ( INFORM .NE. 0 ) RETURN

!  SORT THE LIST OF ELEMENTS FOR EACH GROUP, SO THAT THE
!  ELEMENTS FOR GROUP I PRECEDE THOSE FOR GROUP I + 1, I = 1, NG - 1.

            IF ( NELING .GT. 0 ) THEN
               CALL REORDA( NG, NELING, IELING( 1, 1 ), IELING( 1, 2 ),  &
                            WEIGHT, ISTADG, IWK )
            ELSE
               DO 158 I = 1, NG + 1
                  ISTADG( I ) = 1
  158          CONTINUE
            END IF
         END IF

!  INDICATOR CARD IS ENDATA.
!  -------------------------

         IF ( INTYPE .EQ. MENDAT ) GO TO 900
         GO TO 100
      END IF

!  A DATA CARD HAS BEEN FOUND.

      INCARD = .FALSE.

!  READ THE CHARACTER FIELDS 1, 2, 3 AND 5 FROM THE NEW DATA LINE.

      FIELD1 = NULINE(  2:  3 )
      FIELD2 = NULINE(  5: 14 )
      FIELD3 = NULINE( 15: 24 )
      FIELD5 = NULINE( 40: 49 )

!  START OF A DO-LOOP.
! ===================

      IF ( FIELD1 .EQ. 'DO' ) THEN
         IF ( LEVEL .GE. 3 ) THEN
            INFORM = 13
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2130 )
            GO TO 800
         END IF

!  THIS IS THE FIRST LEVEL OF THE LOOP.

         IF ( LEVEL .EQ. 0 ) THEN
            DOLOOP = .TRUE.
            NARRAY = 0
            NINSTR( 1 ) = 0
            NINSTR( 2 ) = 0
            NINSTR( 3 ) = 0

!  THIS IS THE SECOND OR THIRD LEVEL OF THE LOOP.

         ELSE
            NINSTR( LEVEL ) = NINSTR( LEVEL ) + 1
            IF ( NINSTR( LEVEL ) .GT. MAXINS ) THEN
               INFORM = - 11
               GO TO 700
            END IF
            IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4010 )  &
               LEVEL, NINSTR( LEVEL )
            INSTR( 1, NINSTR( LEVEL ), LEVEL ) = 1
         END IF

!  RECORD THE LOCATION OF THE DO-LOOP VARIABLE IN THE ARRAY INLIST.

         FIELD = FIELD2( 1 : 10 ) // 'II'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) GO TO 700
            IFREE = - IFREE
         ELSE
            NUSEIN = NUSEIN + 1
            IF ( NUSEIN .GT. NINDEX ) THEN
               INFORM = - 21
               GO TO 700
            END IF
            INLIST( IFREE ) = NUSEIN
            NAMIIN( NUSEIN ) = FIELD( 1: 7 )
         END IF
         IF ( LEVEL .EQ. 0 ) THEN
            LOOP( 1 ) = INLIST( IFREE )
         ELSE
            INSTR( 2, NINSTR( LEVEL ), LEVEL ) = INLIST( IFREE )
         END IF

!  RECORD THE STARTING VALUE OF THE DO-LOOP VARIABLE.

         FIELD = FIELD3( 1 : 10 ) // 'II'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 3
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1: 10 )
            GO TO 800
         END IF
         IF ( LEVEL .EQ. 0 ) THEN
            LOOP( 2 ) = INLIST( IFIELD )
         ELSE
            INSTR( 3, NINSTR( LEVEL ), LEVEL ) = INLIST( IFIELD )
         END IF

!  RECORD THE FINISHING VALUE OF THE DO-LOOP VARIABLE AND
!  SET THE INCREMENTAL VALUE OF THE DO-LOOP VARIABLE TO 1.

         FIELD = FIELD5( 1 : 10 ) // 'II'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 3
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
            GO TO 800
         END IF
         IF ( LEVEL .EQ. 0 ) THEN
            LOOP( 3 ) = INLIST( IFIELD )
            LOOP( 4 ) = - 1
         ELSE
            INSTR( 4, NINSTR( LEVEL ), LEVEL ) = INLIST( IFIELD )
            INSTR( 5, NINSTR( LEVEL ), LEVEL ) = - 1
         END IF
         LEVEL = LEVEL + 1
         GO TO 100
      END IF

!  A DO-LOOP VARIABLE IS TO HAVE A NON-TRIVIAL INCREMENT.

      IF ( FIELD1 .EQ. 'DI' ) THEN

!  RECORD THE LOCATION OF THE DO-LOOP VARIABLE IN THE ARRAY INLIST.

! ** Correction 11. 18/10/06: In IF ( A .OR. B ), the Fortran Standard
!    may evaluate A and B in any order and may evaluate B even if A is
!    .TRUE. This causes a failure on some compilers since
!    NINSTR(LEVEL-1) was accessed even when LEVEL=1. Thanks to
!    Jeff Renfro for pointing this out.
         AddDoLoop = .FALSE.
         IF ( LEVEL .EQ. 1 ) THEN
            AddDoLoop = (FIELD2( 1: 10 ) .EQ. NAMIIN( LOOP( 1 ) ))
         ELSE IF( LEVEL .GT. 1 ) THEN
            AddDoLoop = (FIELD2( 1: 10 ) .EQ. NAMIIN(
     .                   INSTR( 2, NINSTR( LEVEL - 1 ), LEVEL - 1 ) ))
         ENDIF
         IF ( AddDoLoop ) THEN


!         IF ( ( LEVEL .EQ. 1 .AND.  FIELD2( 1: 10 ) .EQ.
!     *        NAMIIN( LOOP( 1 ) ) ) .OR.( LEVEL .GT. 1 .AND.
!     *        FIELD2( 1: 10 ) .EQ. NAMIIN(
!     *        INSTR( 2, NINSTR( LEVEL - 1 ), LEVEL - 1 ) ) ) ) THEN
            IF ( DEBUG .AND. IOUT .GT. 0 .AND. LEVEL .GT. 1 )  &
               WRITE( IOUT, 4030 ) LEVEL - 1, NINSTR( LEVEL - 1 )
            FIELD = FIELD3( 1 : 10 ) // 'II'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
               GO TO 800
            END IF
            IF ( LEVEL .EQ. 1 ) THEN
               LOOP( 4 ) = INLIST( IFIELD )
            ELSE
               INSTR( 5, NINSTR( LEVEL - 1 ), LEVEL - 1 ) =&
                  INLIST( IFIELD )
            END IF
         END IF
         GO TO 100
      END IF

!  END OF ONE OR MORE DO-LOOPS.
! ============================

! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!     IF ( FIELD1 .EQ. 'OD' .OR. FIELD1 .EQ. 'ND' ) THEN
      IF ( FIELD1 .NE. 'OD' .AND. FIELD1 .NE. 'ND' ) GO TO 341

!  TERMINATE THE CURRENT LEVEL OF LOOP.

         IF ( FIELD1 .EQ. 'OD' ) THEN
            NINSTR( LEVEL ) = NINSTR( LEVEL ) + 1
            IF ( NINSTR( LEVEL ) .GT. MAXINS ) THEN
               INFORM = - 11
               GO TO 700
            END IF
            INSTR( 1, NINSTR( LEVEL ), LEVEL ) = 2
            IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4020 )  &
               LEVEL, NINSTR( LEVEL )
            LEVEL = LEVEL - 1
         ELSE
            DO 210 I = LEVEL, 1, - 1
               NINSTR( LEVEL ) = NINSTR( LEVEL ) + 1
               IF ( NINSTR( LEVEL ) .GT. MAXINS ) THEN
                  INFORM = - 11
                  GO TO 700
               END IF
               INSTR( 1, NINSTR( LEVEL ), LEVEL ) = 2
               IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4020 )  &
                  LEVEL, NINSTR( LEVEL )
               LEVEL = LEVEL - 1
  210       CONTINUE
         END IF

!  EXECUTE DO-LOOP INSTRUCTIONS.
! =============================

! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!        IF ( LEVEL .EQ. 0 ) THEN
         IF ( LEVEL .NE. 0 ) GO TO 339

!  EXECUTE LEVEL-1 DO-LOOP INSTRUCTIONS.

            LEV1S = INDVAL( LOOP( 2 ) )
            LEV1E = INDVAL( LOOP( 3 ) )
            IF ( LOOP( 4 ) .LE. 0 ) THEN
               LEV1I = 1
            ELSE
               LEV1I = INDVAL( LOOP( 4 ) )
            END IF

!  MOCK DO-LOOP.

            LEV1 = LEV1S
  220       CONTINUE
! ** Correction 11  30/06/04: Assigned goto statements replaced. 2 lines replaced
!           IF ( ( LEV1I .GT. 0 .AND. LEV1 .LE. LEV1E ) .OR.
!    *           ( LEV1I .LT. 0 .AND. LEV1 .GE. LEV1E ) ) THEN
            IF ( .NOT. ( ( LEV1I .GT. 0 .AND. LEV1 .LE. LEV1E ) .OR.&
                 ( LEV1I .LT. 0 .AND. LEV1 .GE. LEV1E ) ) ) GO TO 337
               L1 = 0
               L2 = 0
               L3 = 0

!  SET THE LOOP VALUE.

               INDVAL( LOOP( 1 ) ) = LEV1
               IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 5000 ) 1,  &
                  NAMIIN( LOOP( 1 ) ), LEV1

!  EXECUTE THE REMAINING LIST OF LEVEL-1 INSTRUCTIONS.

  230          CONTINUE
               L1 = L1 + 1

!  SEE IF THE LEVEL-1 LOOP IS TO BE TERMINATED.

               IF ( INSTR( 1, L1, 1 ) .EQ. 2 ) GO TO 300

!  SEE IF A LEVEL-2 LOOP IS TO BE STARTED.

!              IF ( INSTR( 1, L1, 1 ) .EQ. 1 ) THEN
               IF ( INSTR( 1, L1, 1 ) .NE. 1 ) GO TO 295

!  EXECUTE LEVEL-2 DO-LOOP INSTRUCTIONS.

                  LEV2S = INDVAL( INSTR( 3, L1, 1 ) )
                  LEV2E = INDVAL( INSTR( 4, L1, 1 ) )
                  IF ( INSTR( 5, L1, 1 ) .LE. 0 ) THEN
                     LEV2I = 1
                  ELSE
                     LEV2I = INDVAL( INSTR( 5, L1, 1 ) )
                  END IF
                  LEVEL2 = L2
                  LEVL3A = L3

!  MOCK DO-LOOP.

                  LEV2 = LEV2S
  240             CONTINUE
                  L2 = LEVEL2
! ** Correction 1. 30/11/93: 3 lines interchanged **
! ** Correction 11  30/06/04: Assigned goto statements replaced. 2 lines replaced
!                 IF ( ( LEV2I .GT. 0 .AND. LEV2 .LE. LEV2E ) .OR.
!    *                 ( LEV2I .LT. 0 .AND. LEV2 .GE. LEV2E ) ) THEN
                  IF ( .NOT. ( LEV2I .GT. 0 .AND. LEV2 .LE. LEV2E ) .OR.&
                     ( LEV2I .LT. 0 .AND. LEV2 .GE. LEV2E ) ) GO TO 292
                     L3 = LEVL3A
! ** Correction 1. 30/11/93: end of correction **

!  SET THE LOOP VALUE.

                     INDVAL( INSTR( 2, L1, 1 ) ) = LEV2
                     IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 5000 )  &
                        2, NAMIIN( INSTR( 2, L1, 1 ) ), LEV2

!  EXECUTE THE REMAINING LIST OF LEVEL-2 INSTRUCTIONS.

  250                CONTINUE
                     L2 = L2 + 1

!  SEE IF THE LEVEL-2 LOOP IS TO BE TERMINATED.

                     IF ( INSTR( 1, L2, 2 ) .EQ. 2 ) GO TO 290

!  SEE IF A LEVEL-3 LOOP IS TO BE STARTED.

! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!                    IF ( INSTR( 1, L2, 2 ) .EQ. 1 ) THEN
                     IF ( INSTR( 1, L2, 2 ) .NE. 1 ) GO TO 283

!  EXECUTE LEVEL-3 DO-LOOP INSTRUCTIONS.

                        LEV3S = INDVAL( INSTR( 3, L2, 2 ) )
                        LEV3E = INDVAL( INSTR( 4, L2, 2 ) )
                        IF ( INSTR( 5, L2, 2 ) .LE. 0 ) THEN
                           LEV3I = 1
                        ELSE
                           LEV3I = INDVAL( INSTR( 5, L2, 2 ) )
                        END IF
                        LEVEL3 = L3

!  MOCK DO-LOOP.

                        LEV3 = LEV3S
  260                   CONTINUE
                        L3 = LEVEL3
! ** Correction 11  30/06/04: Assigned goto statements replaced. 2 lines replaced
!                       IF ( ( LEV3I .GT. 0 .AND. LEV3 .LE. LEV3E ) .OR.
!    *                     ( LEV3I .LT. 0 .AND. LEV3 .GE. LEV3E ) ) THEN
                        IF ( .NOT.&
                           ( LEV3I .GT. 0 .AND. LEV3 .LE. LEV3E ) .OR.&
                           ( LEV3I .LT. 0 .AND. LEV3 .GE. LEV3E ) ) &
                             GO TO 281

!  SET THE LOOP VALUE.

                           INDVAL( INSTR( 2, L2, 2 ) ) = LEV3
                           IF ( DEBUG .AND. IOUT .GT. 0 )  &
                              WRITE( IOUT, 5000 ) 3,  &
                                 NAMIIN( INSTR( 2, L2, 2 ) ), LEV3

!  EXECUTE THE REMAINING LIST OF LEVEL-3 INSTRUCTIONS.

  270                      CONTINUE
                           L3 = L3 + 1

!  SEE IF THE LEVEL-3 LOOP IS TO BE TERMINATED.

                           IF ( INSTR( 1, L3, 3 ) .EQ. 2 ) GO TO 280

!  EXECUTE LEVEL-3 INDEX INSTRUCTIONS.

                           IF ( INSTR( 1, L3, 3 ) .GE. 21 .AND.&
                                INSTR( 1, L3, 3 ) .LE. 50 ) THEN
                              CALL GETIIN( NINDEX, INDVAL, NRLNDX,  &
                                           REALVL, INSTR( 1, L3, 3 ) )
                              IF ( DEBUG .AND. IOUT .GT. 0 )  &
                                 WRITE( IOUT, 5010 ) 3, L3,  &
                                 NAMIIN( INSTR( 2, L3, 3 ) ),  &
                                 INDVAL( INSTR( 2, L3, 3 ) )
                           END IF
                           IF ( INSTR( 1, L3, 3 ) .GE. 51 .AND.&
                                INSTR( 1, L3, 3 ) .LE. 99 ) THEN
                              CALL GETRIN( NINDEX, NRLNDX, INDVAL,  &
                                           REALVL, RVALUE( L3, 3 ),  &
                                           INSTR( 1, L3, 3 ), INFORM )
                              IF ( INFORM .GT. 0 ) GO TO 800
                              IF ( DEBUG .AND. IOUT .GT. 0 )  &
                                 WRITE( IOUT, 5020 ) 3, L3,  &
                                 NAMRIN( INSTR( 2, L3, 3 ) ),  &
                                 REALVL( INSTR( 2, L3, 3 ) )
                           END IF
                           IF ( INSTR( 1, L3, 3 ) .GE. 100 ) THEN
                              NARRAY = INSTR( 2, L3, 3 )
                              CALL GETLIN( NINDEX, NRLNDX, INDVAL,  &
                                           IARRAY( 1, 1, NARRAY ),  &
                                           VARRAY( 1, NARRAY ),  &
                                           ARRAY( 1, NARRAY ),  &
                                           CARRAY( 1, NARRAY ),  &
                                           FARRAY( NARRAY ), REALVL,  &
                                           NAMIIN, NOVALS,  &
                                           INSTR( 1, L3, 3 ), FIELD1,  &
                                           FIELD2, FIELD3, VALUE4,  &
                                           FIELD5, VALUE6, IOUT, INFORM,  &
                                           LENGTH, KEY, ITABLE, INLIST )
                              IF ( INFORM .GT. 0 ) GO TO 800
                              IF ( INFORM .LT. 0 ) GO TO 700
                              IF ( DEBUG .AND. IOUT .GT. 0 )  &
                                 WRITE( IOUT, 5060 )  &
                                     3, L3, FIELD1, FIELD2,  &
                                     FIELD3, VALUE4, FIELD5, VALUE6
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!                             ASSIGN 270 TO IJUMP
                              IJUMP = 3
                              GO TO 400
                           END IF
                           GO TO 270
  280                      CONTINUE
                           LEV3 = LEV3 + LEV3I
                           GO TO 260
!                       ELSE

!  THE DO-LOOP IS NOT EXECUTED. FIND THE NEXT RELEVANT INSTRUCTION.

  281                      CONTINUE
                           L3 = L3 + 1
                           IF ( INSTR( 1, L3, 3 ) .NE. 2 ) GO TO 281
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!                       END IF

!  END OF LEVEL-3 DO-LOOP.

! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!                    END IF
  283                CONTINUE

!  EXECUTE LEVEL-2 INDEX INSTRUCTIONS.

                     IF ( INSTR( 1, L2, 2 ) .GE. 21 .AND.&
                          INSTR( 1, L2, 2 ) .LE. 50 ) THEN
                        CALL GETIIN( NINDEX, INDVAL, NRLNDX, REALVL,  &
                                     INSTR( 1, L2, 2 ) )
                        IF ( DEBUG .AND. IOUT .GT. 0 )  &
                           WRITE( IOUT, 5010 ) 2, L2,  &
                              NAMIIN( INSTR( 2, L2, 2 ) ),  &
                              INDVAL( INSTR( 2, L2, 2 ) )
                     END IF
                     IF ( INSTR( 1, L2, 2 ) .GE. 51 .AND.&
                          INSTR( 1, L2, 2 ) .LE. 99 ) THEN
                        CALL GETRIN( NINDEX, NRLNDX, INDVAL,  &
                                     REALVL, RVALUE( L2, 2 ),  &
                                     INSTR( 1, L2, 2 ), INFORM )
                        IF ( INFORM .GT. 0 ) GO TO 800
                        IF ( DEBUG .AND. IOUT .GT. 0 )  &
                           WRITE( IOUT, 5020 ) 2, L2,  &
                           NAMRIN( INSTR( 2, L2, 2 ) ),  &
                           REALVL( INSTR( 2, L2, 2 ) )
                     END IF
                     IF ( INSTR( 1, L2, 2 ) .GE. 100 ) THEN
                        NARRAY = INSTR( 2, L2, 2 )
                        CALL GETLIN( NINDEX, NRLNDX, INDVAL,  &
                                     IARRAY( 1, 1, NARRAY ),  &
                                     VARRAY( 1, NARRAY ),  &
                                     ARRAY( 1, NARRAY ),  &
                                     CARRAY( 1, NARRAY ),  &
                                     FARRAY( NARRAY ), REALVL,  &
                                     NAMIIN, NOVALS,  &
                                     INSTR( 1, L2, 2 ), FIELD1,  &
                                     FIELD2, FIELD3, VALUE4,  &
                                     FIELD5, VALUE6, IOUT, INFORM,  &
                                     LENGTH, KEY, ITABLE, INLIST )
                        IF ( INFORM .GT. 0 ) GO TO 800
                        IF ( INFORM .LT. 0 ) GO TO 700
                        IF ( DEBUG .AND. IOUT .GT. 0 )  &
                           WRITE( IOUT, 5060 )  &
                              2, L2, FIELD1, FIELD2, FIELD3,  &
                              VALUE4, FIELD5, VALUE6
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!                       ASSIGN 250 TO IJUMP
                        IJUMP = 2
                        GO TO 400
                     END IF
                     GO TO 250
  290                CONTINUE
                     LEV2 = LEV2 + LEV2I
                     GO TO 240
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!                 ELSE

!  THE DO-LOOP IS NOT EXECUTED. FIND THE NEXT RELEVANT INSTRUCTION.

  292                CONTINUE
                     L2 = L2 + 1
                     IF ( INSTR( 1, L2, 2 ) .NE. 2 ) GO TO 292
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!                 END IF
  293             CONTINUE
                  LEVEL2 = L2

!  END OF LEVEL-2 DO-LOOP.

! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!              END IF
  295          CONTINUE

!  EXECUTE LEVEL-1 INDEX INSTRUCTIONS.

               IF ( INSTR( 1, L1, 1 ) .GE. 21 .AND.&
                    INSTR( 1, L1, 1 ) .LE. 50 ) THEN
                  CALL GETIIN( NINDEX, INDVAL, NRLNDX, REALVL,  &
                               INSTR( 1, L1, 1 ) )
                  IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 5010 )  &
                     1, L1, NAMIIN( INSTR( 2, L1, 1 ) ),  &
                     INDVAL( INSTR( 2, L1, 1 ) )
               END IF
               IF ( INSTR( 1, L1, 1 ) .GE. 51 .AND.&
                    INSTR( 1, L1, 1 ) .LE. 99 ) THEN
                  CALL GETRIN( NINDEX, NRLNDX, INDVAL,  &
                               REALVL, RVALUE( L1, 1 ),  &
                               INSTR( 1, L1, 1 ), INFORM )
                  IF ( INFORM .GT. 0 ) GO TO 800
                  IF ( DEBUG .AND. IOUT .GT. 0 )  &
                     WRITE( IOUT, 5020 ) 1, L1,  &
                     NAMRIN( INSTR( 2, L1, 1 ) ),  &
                     REALVL( INSTR( 2, L1, 1 ) )
               END IF
               IF ( INSTR( 1, L1, 1 ) .GE. 100 ) THEN
                  NARRAY = INSTR( 2, L1, 1 )
                  CALL GETLIN( NINDEX, NRLNDX, INDVAL,  &
                               IARRAY( 1, 1, NARRAY ),  &
                               VARRAY( 1, NARRAY ), ARRAY( 1, NARRAY ),  &
                               CARRAY( 1, NARRAY ),  &
                               FARRAY( NARRAY ), REALVL,  &
                               NAMIIN, NOVALS,  &
                               INSTR( 1, L1, 1 ), FIELD1,  &
                               FIELD2, FIELD3, VALUE4,  &
                               FIELD5, VALUE6, IOUT, INFORM,  &
                               LENGTH, KEY, ITABLE, INLIST )
                  IF ( INFORM .GT. 0 ) GO TO 800
                  IF ( INFORM .LT. 0 ) GO TO 700
                  IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 5060 )  &
                                   1, L1, FIELD1, FIELD2, FIELD3,  &
                                   VALUE4, FIELD5, VALUE6
!                 ASSIGN 230 TO IJUMP
                  IJUMP = 1
                  GO TO 400
               END IF
               GO TO 230
  300          CONTINUE
               LEV1 = LEV1 + LEV1I
               GO TO 220
  337       CONTINUE   
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!           END IF

!  END OF LEVEL-1 DO-LOOP.

            DOLOOP = .FALSE.
  339    CONTINUE
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!        END IF
         GO TO 100
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
!     END IF
  341 CONTINUE

!  CONSTRUCT A LIST OF DO-LOOP INSTRUCTIONS: 1) ARITHMETIC INSTRUCTIONS.
! =====================================================================

      IF ( DOLOOP ) THEN
         NINSTR( LEVEL ) = NINSTR( LEVEL ) + 1
         IF ( NINSTR( LEVEL ) .GT. MAXINS ) THEN
            INFORM = - 11
            GO TO 700
         END IF

!  AN ARITHMETIC INSTRUCTION IS TO BE PROCESSED.

         IF ( FIELD1 .EQ. 'IE' .OR. FIELD1 .EQ. 'IA' .OR.&
              FIELD1 .EQ. 'IS' .OR. FIELD1 .EQ. 'IM' .OR.&
              FIELD1 .EQ. 'ID' .OR. FIELD1 .EQ. 'IR' .OR.&
              FIELD1 .EQ. 'I=' .OR. FIELD1 .EQ. 'I+' .OR.&
              FIELD1 .EQ. 'I-' .OR. FIELD1 .EQ. 'I*' .OR.&
              FIELD1 .EQ. 'I/' .OR. FIELD1 .EQ. 'RE' .OR.&
              FIELD1 .EQ. 'RA' .OR. FIELD1 .EQ. 'RS' .OR.&
              FIELD1 .EQ. 'RM' .OR. FIELD1 .EQ. 'RD' .OR.&
              FIELD1 .EQ. 'RI' .OR. FIELD1 .EQ. 'RF' .OR.&
              FIELD1 .EQ. 'R=' .OR. FIELD1 .EQ. 'R+' .OR.&
              FIELD1 .EQ. 'R-' .OR. FIELD1 .EQ. 'R*' .OR.&
              FIELD1 .EQ. 'R/' .OR. FIELD1 .EQ. 'R(' ) THEN
            CALL PROCAI( NINDEX, NRLNDX, LENGTH, NUSEIN, NUSERE,  &
                         INFORM, IOUT, LEVEL, NINSTR( LEVEL ),  &
                         DEBUG, RVALUE( NINSTR( LEVEL ), LEVEL ),  &
                         INLIST, ITABLE, NAMIIN, NAMRIN,  &
                         INSTR( 1, NINSTR( LEVEL ), LEVEL ), KEY,  &
                         FIELD1, FIELD2, FIELD3, FIELD5,  &
                         NULINE( 25: 36 ) )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( INFORM .LT. 0 ) GO TO 700
         ELSE

!  CONSTRUCT A LIST OF DO-LOOP INSTRUCTIONS: 2) ARRAY DEFINITIONS.
! ===============================================================

            IF ( FIELD1( 1: 1 ) .NE. 'X' .AND. FIELD1( 1: 1 ) .NE. 'Z'&
                 .AND. FIELD1( 1: 1 ) .NE. 'A' ) THEN
               INFORM = 6
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2060 )
               GO TO 800
            END IF
            NARRAY = NARRAY + 1
            IF ( NARRAY .GT. MAXARA ) THEN
               INFORM = - 12
               GO TO 700
            END IF
            CALL PROCAD( NINDEX, NRLNDX, LEVEL, NINSTR( LEVEL ),  &
                         NUSERE, LENGTH, NARRAY, INTYPE, INFORM, IOUT,  &
                         DEBUG, GRP1ST,  &
                         FIELD1, FIELD2, FIELD3, FIELD5,  &
                         NULINE( 25 : 36 ), NULINE( 50 : 61 ),  &
                         INLIST,  &
                         INSTR( 1, NINSTR( LEVEL ), LEVEL ), ITABLE,  &
                         IARRAY( 1, 1, NARRAY ), VARRAY( 1, NARRAY ),  &
                         FARRAY( NARRAY ), NAMIIN, NAMRIN,  &
                         ARRAY( 1, NARRAY ), CARRAY( 1, NARRAY ), KEY )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( INFORM .LT. 0 ) GO TO 700

!  THE ARRAY DEFINITION IS COMPLETE.

         END IF
         GO TO 100
      ELSE

!  EXECUTE A NON-DO-LOOP INSTRUCTION.
! ==================================

!  THE INSTRUCTION IS AN ARRAY INSTRUCTION.

         IF ( FIELD1 .EQ. 'IE' .OR. FIELD1 .EQ. 'IA' .OR.&
              FIELD1 .EQ. 'IS' .OR. FIELD1 .EQ. 'IM' .OR.&
              FIELD1 .EQ. 'ID' .OR. FIELD1 .EQ. 'IR' .OR.&
              FIELD1 .EQ. 'I=' .OR. FIELD1 .EQ. 'I+' .OR.&
              FIELD1 .EQ. 'I-' .OR. FIELD1 .EQ. 'I*' .OR.&
              FIELD1 .EQ. 'I/' .OR. FIELD1 .EQ. 'RE' .OR.&
              FIELD1 .EQ. 'RA' .OR. FIELD1 .EQ. 'RS' .OR.&
              FIELD1 .EQ. 'RM' .OR. FIELD1 .EQ. 'RD' .OR.&
              FIELD1 .EQ. 'RI' .OR. FIELD1 .EQ. 'RF' .OR.&
              FIELD1 .EQ. 'R=' .OR. FIELD1 .EQ. 'R+' .OR.&
              FIELD1 .EQ. 'R-' .OR. FIELD1 .EQ. 'R*' .OR.&
              FIELD1 .EQ. 'R/' .OR. FIELD1 .EQ. 'R(' ) THEN

!  1) AN ARITHMETIC INSTRUCTION. DECODE THE INSTRUCTION.

            CALL PROCAI( NINDEX, NRLNDX, LENGTH, NUSEIN, NUSERE,  &
                         INFORM, IOUT, 0, 1, DEBUG, RVALUE( 1, 1 ),  &
                         INLIST, ITABLE, NAMIIN, NAMRIN,  &
                         INSTR( 1, 1, 1 ), KEY,  &
                         FIELD1, FIELD2, FIELD3, FIELD5,  &
                         NULINE( 25: 36 ) )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( INFORM .LT. 0 ) GO TO 700

!  EXECUTE THE INSTRUCTION.

            IF ( FIELD1( 1 : 1 ) .EQ. 'I' ) THEN
               CALL GETIIN( NINDEX, INDVAL, NRLNDX, REALVL,  &
                            INSTR( 1, 1, 1 ) )
               IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 5010 ) 0, 1,  &
                  NAMIIN( INSTR( 2, 1, 1 ) ), INDVAL( INSTR( 2, 1, 1 ) )
            ELSE
               CALL GETRIN( NINDEX, NRLNDX, INDVAL, REALVL,  &
                            RVALUE( 1, 1 ), INSTR( 1, 1, 1 ), INFORM )
               IF ( INFORM .GT. 0 ) GO TO 800
               IF ( DEBUG .AND. IOUT .GT. 0 )  &
                  WRITE( IOUT, 5020 ) 0, 1, NAMRIN( INSTR( 2, 1, 1 ) ),  &
                  REALVL( INSTR( 2, 1, 1 ) )
            END IF
            GO TO 100
         ELSE
            IF ( FIELD1( 1 : 1 ) .EQ. 'X' .OR. FIELD1( 1: 1 ) .EQ. 'Z'&
                 .OR. FIELD1( 1 : 1 ) .EQ. 'A' ) THEN

!  2) AN ARRAY DEFINITION. DECODE THE INSTRUCTION.

               CALL PROCAD( NINDEX, NRLNDX, 0, 1, NUSERE,  &
                            LENGTH, 1, INTYPE, INFORM, IOUT,  &
                            DEBUG, GRP1ST,  &
                            FIELD1, FIELD2, FIELD3, FIELD5,  &
                            NULINE( 25 : 36 ), NULINE( 50 : 61 ),  &
                            INLIST, INSTR( 1, 1, 1 ), ITABLE,  &
                            IARRAY( 1, 1, 1 ), VARRAY( 1, 1 ),  &
                            FARRAY( 1 ), NAMIIN, NAMRIN,  &
                            ARRAY( 1, 1 ), CARRAY( 1, 1 ), KEY )
               IF ( INFORM .GT. 0 ) GO TO 800
               IF ( INFORM .LT. 0 ) GO TO 700

!  EXECUTE THE INSTRUCTION.

               CALL GETLIN( NINDEX, NRLNDX, INDVAL,  &
                            IARRAY( 1, 1, 1 ), VARRAY( 1, 1 ),  &
                            ARRAY( 1, 1 ), CARRAY( 1, 1 ),  &
                            FARRAY( 1 ), REALVL, NAMIIN, NOVALS,  &
                            INSTR( 1, 1, 1 ), FIELD1,  &
                            FIELD2, FIELD3, VALUE4,  &
                            FIELD5, VALUE6, IOUT, INFORM,  &
                            LENGTH, KEY, ITABLE, INLIST )
               IF ( INFORM .GT. 0 ) GO TO 800
               IF ( INFORM .LT. 0 ) GO TO 700
               IF ( DEBUG .AND. IOUT .GT. 0 )  &
                  WRITE( IOUT, 5060 ) 0, 1, FIELD1, FIELD2,  &
                                  FIELD3, VALUE4, FIELD5, VALUE6
               GO TO 400
            ELSE

!  THE INSTRUCTION IS NOT AN ARRAY INSTRUCTION.
!  CHECK TO SEE IF THERE IS ARE ANY NUMERICAL VALUES TO BE READ.

               NOVALS = 0
               IF ( ( FIELD3 .NE. '          ' .AND.&
                      NULINE( 15: 15 ) .NE. '$' )
!    *                .OR. FIELD1( 1: 1 ) .EQ. 'D'
!    *                .OR. FIELD2 .EQ. '''DEFAULT'' '
!    *                .OR. FIELD2 .EQ. '''default'' '
!    *                .OR. FIELD3 .EQ. '''DEFAULT'' '
!    *                .OR. FIELD3 .EQ. '''default'' '&
                      .OR. INTYPE .EQ. MOBBND ) THEN
                  NOVALS = NOVALS + 1
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
                  IF (   INTYPE .LE. MQHESS .OR. INTYPE .EQ. MOBBND .OR.&
                     ( ( INTYPE .EQ. MEUSES .OR. INTYPE .EQ. MGUSES )  &
                         .AND. FIELD1 .EQ. 'P ' ) .OR. ( INTYPE .EQ.&
                               MGUSES .AND. FIELD1 .EQ. 'E ' ) ) THEN
                     IF ( INTYPE .EQ. MGUSES .AND. FIELD1 .EQ. 'E '.AND.&
                       NULINE( 25: 36 ) .EQ. '            ' ) THEN
                       VALUE4 = ONE
                     ELSE
                        CALL GETVAL( NULINE( 25: 36 ), VALUE4 )
                     END IF
                     IF ( INTYPE .EQ. MRANGE ) VALUE4 = ABS( VALUE4 )
                  END IF
                  IF ( FIELD5 .NE. '          ' .AND.&
                       NULINE( 40: 40 ) .NE. '$' ) THEN
                     NOVALS = NOVALS + 1
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
                     IF (  INTYPE .LE. MQHESS .OR.&
                       ( ( INTYPE .EQ. MEUSES .OR. INTYPE .EQ. MGUSES )  &
                            .AND. FIELD1 .EQ. 'P ' ) .OR. ( INTYPE .EQ.&
                                 MGUSES .AND. FIELD1 .EQ. 'E ' ) ) THEN
                        IF ( INTYPE .EQ. MGUSES .AND.&
                             FIELD1 .EQ. 'E '   .AND.&
                             NULINE( 50: 61 ) .EQ. '            ' ) THEN
                          VALUE6 = ONE
                        ELSE
                           CALL GETVAL( NULINE( 50: 61 ), VALUE6 )
                        END IF
                        IF ( INTYPE .EQ. MRANGE ) VALUE6 = ABS( VALUE6 )
                        IF ( INTYPE .LT. MCONST ) THEN
                           IF ( VALUE6 .EQ. ZERO ) NOVALS = 1
                        END IF
                     END IF

!  REMOVE FIELDS WITH NUMERICAL VALUES OF ZERO.

                  END IF
                  IF ( INTYPE .LT. MCONST ) THEN
                     IF ( VALUE4 .EQ. ZERO ) THEN
                        IF ( NOVALS .EQ. 2 ) THEN
                           VALUE4 = VALUE6
                           FIELD3 = FIELD5
                        END IF
                       NOVALS = NOVALS - 1
                     END IF
                  END IF
                  IF ( FIELD3 .EQ. '''SCALE''   ' .OR.&
                       FIELD3 .EQ. ' ''SCALE''  ' ) THEN
                     NOVALS = 0
                     VALUE4 = ABS( VALUE4 )
                  END IF
               END IF
               IF ( DEBUG .AND. IOUT .GT. 0 )  &
                  WRITE( IOUT, 5060 ) 0, 1, FIELD1, FIELD2,  &
                                  FIELD3, VALUE4, FIELD5, VALUE6
            END IF
         END IF
      END IF
  400 CONTINUE

!  EXECUTE REAL PARAMETER ARRAY CARD.

      IF ( FIELD1 .EQ. 'AE' .OR. FIELD1 .EQ. 'AA' .OR.&
           FIELD1 .EQ. 'AS' .OR. FIELD1 .EQ. 'AM' .OR.&
           FIELD1 .EQ. 'AD' .OR. FIELD1 .EQ. 'AI' .OR.&
           FIELD1 .EQ. 'AF' .OR. FIELD1 .EQ. 'A=' .OR.&
           FIELD1 .EQ. 'A+' .OR. FIELD1 .EQ. 'A-' .OR.&
           FIELD1 .EQ. 'A*' .OR. FIELD1 .EQ. 'A/' .OR.&
           FIELD1 .EQ. 'A(' ) THEN
         CALL    PROCAA( NINDEX, LENGTH, NUSERE, INFORM, IOUT,
! ** Correction 8. 26/02/01: 1 dummy argument removed from PROCAA **&
                         NRLNDX, INLIST, ITABLE,  &
                         NAMRIN, KEY, INDVAL, REALVL,  &
                         FIELD1, FIELD2, FIELD3, FIELD5, VALUE4 )
         IF ( INFORM .GT. 0 ) GO TO 800
         IF ( INFORM .LT. 0 ) GO TO 700
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF

!  BRANCH DEPENDING ON THE CURRENT INDICATOR CARD.
! ===============================================

! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
! ** Correction -2. 21/02/00: QSECTION added as alias for QUADOBJ
! ** Correction -6. 13/04/12: QMATRIX added as alias for QUADOBJ

      GO TO ( 100, 100, 100, 100, 420, 420, 420, 430, 430, 430,  &
              430, 430, 430, 440, 450, 455, 455, 455, 455, 455, &
              455, 460, 470, 480, 490, 500, 900 ), INTYPE

!  INDICATOR CARD IS GROUPS/ROWS/CONSTRAINTS.
!  ------------------------------------------

  420 CONTINUE
      IF ( GRP1ST ) THEN
         CALL    SGRP1 ( NG, NGMAX, NOMAX, LENGTH, NOBJ, NOVALS,  &
                         INLIST, IDROWS, ISTATE, ITYPEG, ITABLE,  &
                         RDROWS, RSCALE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         GNAMES, ONAMES, KEY, IOUT, INFORM )
      ELSE
         CALL    SGRP2 ( NG, NGMAX, NOMAX, LA, LENGTH,  &
                         NNZA, NOBJ, NOVALS, INLIST, IDROWS,  &
                         ISTATE, ITYPEG, ITABLE, ICOORD,  &
                         A, RDROWS, RSCALE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         GNAMES, ONAMES, KEY, IOUT, INFORM )
      END IF
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS COLUMNS/VARIABLES, CONSTANTS/RHS/RHS' OR RANGES.
!  ------------------------------------------------------------------

  430 CONTINUE
      IF ( INTYPE .EQ. MCOLS  ) COLFIE = 'VA'
      IF ( INTYPE .EQ. MCONST ) COLFIE = 'CO'
      IF ( INTYPE .EQ. MRANGE ) COLFIE = 'RA'
      IF ( GRP1ST .OR. INTYPE .NE. MCOLS ) THEN
         CALL    SVAR2 ( NMAX, NGMAX, LA, LENGTH, NNZA, NVAR, NOVALS,  &
                         NRLNDX, INTYPE .EQ. MCOLS, COLFIE,
! ** Correction 0. 20/12/99: Array holding variable types introduced.&
                         ICOORD, ISTATE, ITYPEV, INLIST, ITABLE,  &
                         A, CSCALE, REALVL, DFAULT,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         VNAMES, KEY, IOUT, INFORM )
      ELSE
         CALL    SVAR1 ( NMAX, LENGTH, NVAR, COLFIE,
! ** Correction 0. 20/12/99: Array holding variable types introduced.&
                         ITYPEV, INLIST, ITABLE, CSCALE,
! ** Correction 2. 26/02/01: 4 dummy arguments removed from SVAR1 **&
                         FIELD2, FIELD3, VALUE4, &
                         VNAMES, KEY, INFORM )
      END IF
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS BOUNDS.
!  -------------------------

  440 CONTINUE
      CALL       SBOUND( NMAX, NBMAX, LENGTH, NLVARS, NBND, NCOL,  &
                         NRLNDX, DEFAUT, INLIST, ITABLE, BND, REALVL,
! ** Correction 3. 26/02/01: 1 dummy argument removed from SBOUND **&
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, &
                         BNAMES, BNDFLT, KEY, IOUT, INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS START POINT.
!  ------------------------------

  450 CONTINUE
      CALL       SSTART( NMAX, NGMAX, NSMAX, LENGTH, NLVARS, NG, NSTART,  &
                         NCOL, NRLNDX, DEFAUT, INLIST, ITABLE, VSTART,  &
                         CSTART, REALVL, FIELD1, FIELD2, FIELD3, VALUE4,  &
                         FIELD5, VALUE6, SNAMES, NOVALS, KEY,  &
                         IOUT, INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.

!  INDICATOR CARD IS QHESS.
!  ------------------------

  455 CONTINUE
      CALL       SQHESS( NEGMAX, NGMAX, NLMAX, NELMAX, NEVMAX, NETMAX, &
                         NOMAX, NIMAX, LENGTH, NG, NOBJ, NGRUPE, NOVALS, &
                         NELN, NINN, NEPN, NELTYP, NLISEP, NLISEV,  &
                         NELNUM, NELING, QGROUP, QSQR, QPROD, IELV, &
                         IINV, IEPA, ITYPEG, IELING, ISTAEV, IELVAR,
! ** Correction 4. 26/02/01: 1 dummy argument removed from SQHESS **&
                         INLIST, ITABLE, ISTATE, ITYPEE, ISTEP,  &
                         IPTYPE, ISTYPE, INREP,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         ENAMES, GNAMES, ETYPES, ONAMES, INAMES, &
                         LNAMES, WEIGHT, KEY, IOUT, INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS ELEMENT TYPE.
!  -------------------------------

  460 CONTINUE
      CALL       SETYPE( NLMAX,  NIMAX, NETMAX, NEPMAX, LENGTH,  &
                         NOVALS, NELN, NINN, NEPN, NELTYP,  &
                         INREP, IELV, IINV, IEPA, INLIST, ITABLE,
! ** Correction 5. 26/02/01: 2 dummy arguments removed from SETYPE **&
                         FIELD1, FIELD2, FIELD3, FIELD5,  &
                         ENAMES, INAMES, EPNAME, ETYPES, KEY,  &
                         IOUT, INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS ELEMENT USES.
!  --------------------------------

  470 CONTINUE
      CALL       SEUSES( NLMAX, NELMAX, NETMAX, NEVMAX, NLISEV, NLISEP,  &
                         NOVALS, NEPMAX, NEPVMX, LENGTH, NELNUM, NELMNT,  &
                         NMAX, N, ELMNT, IELV, IEPA, ITYPEE, IELVAR,  &
                         INLIST, ITABLE, ISTAEV, ISTEP, DELSET, DETYPE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         EPVALU, ENAMES, LNAMES, EPNAME, VNAMES,  &
                         KEY, IOUT, INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS GROUP TYPE.
!  -----------------------------

  480 CONTINUE
      CALL       SGTYPE( NGRMAX, NGPMAX, NOVALS, LENGTH, NGRTYP, NGPN,  &
                         SETANA, INLIST, IGPA, ITABLE,
! ** Correction 6. 26/02/01: 2 dummy arguments removed from SGTYPE **&
                         FIELD1, FIELD2, FIELD3, FIELD5, &
                         ANAMES, GTYPES, GPNAME, KEY, IOUT, INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS GROUP USES.
!  -----------------------------

  490 CONTINUE
      CALL       SGUSES( NEGMAX, NGPMAX, NGRMAX, NGMAX, NGPVMX,  &
                         LENGTH, NG, NGRUPE, NLISGP, NOVALS, NELING,  &
                         NDTYPE, STRTGU, GRUPE, IGPA, ITYPEG, IELING,  &
                         INLIST, ITABLE, ISTGP, ISTATE, DGRSET, DGTYPE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         GPTEMP, GNAMES, GPNAME, WEIGHT,  &
                         KEY, IOUT, INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700

!  INDICATOR CARD IS OBJECT BOUND.
!  -------------------------------

  500 CONTINUE
      CALL       SOBBND( NOBBND, NOBMAX, NRLNDX, LENGTH,  &
                         INLIST, ITABLE, FBOUND, REALVL,
! ** Correction 7. 26/02/01: 2 dummy arguments removed from SOBBND **&
                         FIELD1, FIELD2, VALUE4, FIELD5,  &
                         OBNAME, KEY   , SINGLE, IOUT  , INFORM )
      IF ( INFORM .EQ. 0 ) THEN
! ** Correction 11  30/06/04: Assigned goto statements replaced. 1 line replaced
         IF ( DOLOOP ) GO TO 600
         GO TO 100
      END IF
      IF ( INFORM .GT. 0 ) GO TO 800
      GO TO 700
! ** Correction 11  30/06/04: Assigned goto statements replaced. 11 lines added

!  BRANCH BACK INTO DO LOOPS AT APPROPRIATE POINT

  600 CONTINUE
      IF ( IJUMP .EQ. 1 ) THEN
        GO TO 230
      ELSE IF ( IJUMP .EQ. 2 ) THEN
        GO TO 250
      ELSE
        GO TO 270
      END IF

!  INSUFFICIENT SPACE.

  700 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2000 ) INCRSE( - INFORM )
      RETURN

!  DATA CARD INCONSISTENCY.

  800 CONTINUE
      IF ( IOUT .GT. 0 ) THEN
         IF ( DOLOOP ) THEN
            WRITE( IOUT, 2960 ) LINENO, FIELD1, FIELD2, FIELD3,  &
                                VALUE4, FIELD5, VALUE6
         ELSE
            WRITE( IOUT, 2990 ) LINENO, NULINE
         END IF
      END IF
      GO TO 960
! ** Correction -3. 21/02/00: Code to handle incomplete/missing data added

!  MISSING/INCOMPLETE DATA CARD.

  810 CONTINUE
      IF ( .NOT. DEFNAM ) THEN
         INFORM = 74
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2740 )
      ELSE
         INFORM = 75
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2750 )
      END IF
      GO TO 960

!  SUCCESSFUL RETURN.

  900 CONTINUE
      INFORM = 0
      IF ( DEBUG .AND. IOUT .GT. 0 ) THEN
         WRITE( IOUT, 3000 ) ( GNAMES( J ), J = 1, NG )
         WRITE( IOUT, 3010 ) ( VNAMES( J ), J = 1, N )
         WRITE( IOUT, 3020 )  &
             ( ICOORD( J, 1 ), ICOORD( J, 2 ), A( J ), J = 1, NNZA )
         WRITE( IOUT, 3030 )  &
          ( ( I, J, BND( 1, J, I ), BND( 2, J, I ), J = 1, NLVARS ),  &
              I =1, NBND )
         WRITE( IOUT, 3100 )  &
          ( ( I, J, VSTART( J, I ), J = 1, NLVARS ), I = 1, NSTART )
         WRITE( IOUT, 3040 ) ( RSCALE( J ), J = 1, NG )
         WRITE( IOUT, 3050 ) ( CSCALE( J ), J = 1, N )
         IF ( NELTYP .GT. 0 )  &
            WRITE( IOUT, 3060 ) ( ETYPES( I ), IELV( I + 1 ) - IELV( I),  &
                                  IINV( I + 1 ) - IINV( I ),  &
                                  IEPA( I + 1 ) - IEPA( I ),  &
                                  I = 1, NELTYP )
         IF ( NGRTYP .GT. 0 )  &
            WRITE( IOUT, 3110 ) ( GTYPES( I ), ANAMES( I ),  &
                                  IGPA( I + 1 ) - IGPA( I ),  &
                                  I = 1, NGRTYP )
         WRITE( IOUT, 3070 )
         DO 950 I = 1, NG
            K1 = ISTADG( I )
            K2 = ISTADG( I + 1 ) - 1
            IS = ITYPEG( I )
            IF ( K1 .LE. K2 ) THEN
               IF ( IS .EQ. 0 ) THEN
                  DO 930 K = K1, K2
                     L = IELING( K, 1 )
                     WRITE( IOUT, 3080 ) GNAMES( I ),  &
                     'TRIVIAL   ', LNAMES( L ),  &
                     ETYPES( ITYPEE( L ) ), ( VNAMES( IELVAR( J ) ),  &
                     J = ISTAEV( L ), ISTAEV( L + 1 ) - 1 )
  930             CONTINUE
               ELSE
                  DO 940 K = K1, K2
                     L = IELING( K, 1 )
                     WRITE( IOUT, 3080 ) GNAMES( I ),  &
                     GTYPES( IS ), LNAMES( L ),  &
                     ETYPES( ITYPEE( L ) ), ( VNAMES( IELVAR( J ) ),  &
                     J = ISTAEV( L ), ISTAEV( L + 1 ) - 1 )
  940             CONTINUE
               END IF
            ELSE
               IF ( IS .EQ. 0 ) THEN
                  WRITE( IOUT, 3090 ) GNAMES( I ), 'TRIVIAL   '
               ELSE
                  WRITE( IOUT, 3090 ) GNAMES( I ), GTYPES( IS )
               END IF
            END IF
  950    CONTINUE
      END IF
  960 CONTINUE
      IF ( DEBUG .AND. IOUT .GT. 0 ) THEN
         DO 980 I = 1, NUSEIN
            WRITE( IOUT, 4000 ) I, NAMIIN( I ), INDVAL( I )
  980    CONTINUE
         DO 990 I = 1, NUSERE
            WRITE( IOUT, 4100 ) I, NAMRIN( I ), REALVL( I )
  990    CONTINUE
      END IF
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 1000 FORMAT( A65 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from GPSMPS - insufficient space.',  &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from GPSMPS - first card is not NAME ' )
 2020 FORMAT( ' ** Exit from GPSMPS - indicator card not recognised ' )
 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2060 FORMAT( ' ** Exit from GPSMPS - non array defn. within do-loop ' )
 2130 FORMAT( ' ** Exit from GPSMPS - do loop level greater than 3 ' )
 2160 FORMAT( ' ** Exit from GPSMPS - element ', A10, ' variable ',  &
              A10, ' not set ' )
 2210 FORMAT( ' ** Exit from GPSMPS -',  &
              ' groups and variables sections mixed')
 2250 FORMAT( ' ** Exit from GPSMPS - no group-type arg. given ' )
 2280 FORMAT( ' ** Exit from GPSMPS - element ', A10, ' parameter ',  &
              A10, ' not set ' )
 2340 FORMAT( ' ** Exit from GPSMPS - group ', A10, ' parameter ',  &
              A10, ' not set ' )
 2380 FORMAT( ' ** Exit from GPSMPS - do loop not completed ' )
! ** Correction -3. 21/02/00: Code to handle incomplete/missing data added
 2740 FORMAT( ' ** Exit from GPSMPS - data file empty ' )
 2750 FORMAT( ' ** Exit from GPSMPS - data file incomplete.',  &
              ' No ENDATA card ' )
! ** Correction -5b. 07/09/00: Check for non-useful transformations added
 2760 FORMAT( ' ** Exit from GPSMPS - #internal vars >= #elementals' )
 2960 FORMAT( /, ' From within do loop ending on line ', I5,  &
              ', current line is ', /,  &
              2X, A2, 1X, A10, A10, 1P, D12.4, 3X, A10, D12.4 )
 2970 FORMAT( ' Line ', I5, 4X, A160 )
 2980 FORMAT( ' Line ', I5, '.', I2, 1X, A65 )
 2990 FORMAT( ' Line ', I5, 4X, A65 )
 3000 FORMAT( /, ' Row names ', /, ' --------- ', /, 8( 1X, A8 ) )
 3010 FORMAT( /, ' Column names ', /, ' ------------', /, 8( 1X, A8 ) )
 3020 FORMAT( /, 3('  Col   Row    Value  '),  &
              /, 3('  ---   ---    -----  '),  &
              /, ( 3( 2I5, 1P, D12.4 ) ) )
 3030 FORMAT( /, 2(' No. var.  Lower bnd   upper bnd '),  &
              /, 2(' --- ----  ---------   --------- '),  &
              /,  ( 2( I3, I6, 1P, 2E12.4 ) ) )
 3040 FORMAT( /, ' Row scaling ', /, ( 1P, D12.4 ) )
 3050 FORMAT( /, ' Column scaling ', /, ( 1P, D12.4 ) )
! ** Correction 9. 27/02/01: Character debug output format increased **
 3060 FORMAT( /, '    Element type No. el. vars. No. in. vars.',  &
                 ' No. parameters ',  &
              /, '    ------------ ------------- ------------- ',  &
                 ' -------------- ', /, ( 5X, A10, I12, 2( 2X, I12 ) ) )
 3070 FORMAT( /, ' Group      Gr. type    Element   El. type',  &
                 '     Variables ',  &
              /, ' -----      --------    -------   --------',  &
                 '     --------- ' )
 3080 FORMAT( 1X, A10, 1X, A10, 2X, A10, A10, 4X, 5A10,  &
              /, ( 48X, 5A10 ) )
 3090 FORMAT( 1X, A10, 1X, A10, 2X, '   -    ' )
 3100 FORMAT( /, 2(' No. var.  Start point '),  &
              /, 2(' --- ----  ----------- '),  &
              /,  ( 2( I3, I6, 1P, D12.4 ) ) )
 3110 FORMAT( /, '    Group type   Argument   No. parameters',  &
              /, '    ----------   --------   -------------- ',  &
              /, ( 5X, 2A10, I14 ) )
 4000 FORMAT( ' Int. par. num. ', I5, ' Name = ', A10, ' Value = ', I12)
 4010 FORMAT( ' Level-', I1, ' Instruction ', I4, ' Starting do-loop ' )
 4020 FORMAT( ' Level-', I1, ' Instruction ', I4, ' Ending do-loop ' )
 4030 FORMAT( ' Level-', I1, ' Instruction ', I4,  &
              ' Incrementing do-loop ' )
 4100 FORMAT( ' Real par. num. ', I5, ' Name = ', A10,' Value = ',  &
                1P, D12.4)
 5000 FORMAT( /, ' Level-', I1, ' loop index ', A10, ' = ', I12 )
 5010 FORMAT( ' Level-', I1, ' instruction ', I3,  &
              ' Index ', A10, ' = ', I12 )
 5020 FORMAT( ' Level-', I1, ' instruction ', I3,  &
              ' Index ', A10, ' = ', 1P, D12.4 )
 5060 FORMAT( ' Level-', I1, ' instruction ', I3, ' Set line ', /,  &
              '    FIELD1 = ', A12, ' FIELD2 = ', A10, ' FIELD3 = ',  &
              A10, /, '    VALUE4 = ', 1P, D12.4, ' FIELD5 = ', A10,  &
              ' VALUE6 = ', 1P, D12.4 )

!  END OF GPSMPS.

      END

!  THIS VERSION: 27/02/2001 AT 21:30:00 PM.
!     ( Last modified on 15 Mar 2001 at 22:28:00 )
! ** Correction report.
! ** Correction -3. 03/11/00: quadratic elements have no internal representation
! ** Correction -2. 07/09/00: Check for non-useful transformations added
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
! ** Correction 0. 20/12/99: Array holding variable types introduced.
! ** Correction 1. 28/10/92: 1 line added **
! ** Correction 2. 26/02/01: 4 dummy arguments removed from SVAR1 **
! ** Correction 3. 26/02/01: 1 dummy argument removed from SBOUND **
! ** Correction 4. 26/02/01: 1 dummy argument removed from SQHESS **
! ** Correction 5. 26/02/01: 2 dummy arguments removed from SETYPE **
! ** Correction 6. 26/02/01: 2 dummy arguments removed from SGTYPE **
! ** Correction 7. 26/02/01: 2 dummy arguments removed from SOBBND **
! ** Correction 8. 26/02/01: unused BIG and J removed from SQHESS **
! ** Correction 9. 27/02/01: check to see if there are quadratic elements
! ** Correction 10. 04/04/02: Number of group parameters for trivial group set
! ** End of Correction report.
      SUBROUTINE SGRP1 ( NG, NGMAX, NOMAX, LENGTH, NOBJ, NOVALS,  &
                         INLIST, IDROWS, ISTATE, ITYPEG, ITABLE,  &
                         RDROWS, RSCALE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         GNAMES, ONAMES, KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LENGTH
      INTEGER        NG, NGMAX, NOMAX
      INTEGER        NOBJ, NOVALS
      DOUBLE PRECISION VALUE4, VALUE6
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INLIST( LENGTH ), IDROWS( 2, NGMAX )
      INTEGER        ISTATE( NGMAX ), ITYPEG( NGMAX )
      INTEGER        ITABLE ( LENGTH )
      DOUBLE PRECISION           RDROWS( 2, NGMAX ), RSCALE( NGMAX )
      CHARACTER * 10 GNAMES( NGMAX ), ONAMES( NOMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS GROUPS/ROWS/CONSTRAINTS.
!  ------------------------------------------
!  THE GROUPS SECTION APPEARS BEFORE THE VARIABLES SECTION.
!  --------------------------------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFREE, IFIELD, IS, J
      DOUBLE PRECISION             ZERO
      CHARACTER * 12   FIELD
      EXTERNAL         HASHB , HASHC
      PARAMETER      ( ZERO = 0.0D+0 )

!  IGNORE 'MARKER' CARDS.

      IF ( FIELD3 .EQ. '''MARKER''  ' ) RETURN

!  FIND A PLACE TO INSERT THE NEW GROUP NAME IN THE HASH-TABLE.

      CALL HASHB ( LENGTH, 12, FIELD2 // 'GR', KEY, ITABLE, IFREE )
      IF ( IFREE .LE. 0 ) THEN
         IF ( IFREE .EQ. 0 ) THEN
            INFORM = - 1
            RETURN
         END IF

!  THE GROUP HAS APPEARED BEFORE AS THE J-TH IN THE LIST.

         J = INLIST( - IFREE )
      ELSE

!  MARK ANY OBJECTIVE FUNCTION ROWS AS SPECIAL.

         IF ( FIELD1 .EQ. 'N ' .OR. FIELD1 .EQ. ' N' .OR.&
              FIELD1 .EQ. 'DN' .OR. FIELD1 .EQ. 'XN' .OR.&
              FIELD1 .EQ. 'ZN' ) THEN
            NOBJ = NOBJ + 1
            IF( NOBJ .GT. NOMAX ) THEN
               INFORM = - 5
               RETURN
            END IF
            ONAMES ( NOBJ ) = FIELD2
         END IF

!  THE GROUP IS THE NG-TH ENCOUNTERED.

         NG = NG + 1
         IF ( NG .GE. NGMAX ) THEN
            INFORM = - 6
            RETURN
         END IF

!  RECORD THE POSITION OF THE NEW GROUP IN THE TABLE, RECORD ITS
!  NAME AND INITIALISE ITS TYPE AS TRIVIAL.

         J = NG
         INLIST( IFREE ) = NG
         GNAMES( NG ) = FIELD2
         ITYPEG( NG ) = - 1
         IS = 0

!  RECORD THE STATUS, ISTATE, OF THE GROUP. ISTATE( NG ) = :
!  1 THE GROUP IS AN OBJECTIVE FUNCTION TYPE.
!  2 THE GROUP IS AN EQUALITY FUNCTION TYPE.
!  3 THE GROUP IS A LESS-THAN-OR-EQUAL-TO TYPE.
!  4 THE GROUP IS A GREATER-THAN-OR-EQUAL-TO TYPE.
!  5 THE GROUP IS OF D-TYPE AND AN OBJECTIVE FUNCTION TYPE.
!  6 THE GROUP IS OF D-TYPE AND AN EQUALITY FUNCTION TYPE.
!  7 THE GROUP IS OF D-TYPE AND A LESS-THAN-OR-EQUAL-TO TYPE.
!  8 THE GROUP IS OF D-TYPE AND A GREATER-THAN-OR-EQUAL-TO TYPE.

         IF ( FIELD1 .EQ. 'N ' .OR. FIELD1 .EQ. ' N' .OR.&
              FIELD1 .EQ. 'XN' .OR. FIELD1 .EQ. 'ZN' ) IS = 1
         IF ( FIELD1 .EQ. 'E ' .OR. FIELD1 .EQ. ' E' .OR.&
              FIELD1 .EQ. 'XE' .OR. FIELD1 .EQ. 'ZE' ) IS = 2
         IF ( FIELD1 .EQ. 'L ' .OR. FIELD1 .EQ. ' L' .OR.&
              FIELD1 .EQ. 'XL' .OR. FIELD1 .EQ. 'ZL' ) IS = 3
         IF ( FIELD1 .EQ. 'G ' .OR. FIELD1 .EQ. ' G' .OR.&
              FIELD1 .EQ. 'XG' .OR. FIELD1 .EQ. 'ZG' ) IS = 4
         IF ( FIELD1 .EQ. 'DN' ) IS = 5
         IF ( FIELD1 .EQ. 'DE' ) IS = 6
         IF ( FIELD1 .EQ. 'DL' ) IS = 7
         IF ( FIELD1 .EQ. 'DG' ) IS = 8
         IF ( IS .EQ. 0 ) THEN
            INFORM = 10
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
            RETURN
         ENDIF
         ISTATE( NG ) = IS
      END IF

!  INCLUDE GROUP SCALE FACTORS.

      IF ( FIELD3 .EQ. '''SCALE''   ' .OR. FIELD3&
           .EQ. ' ''SCALE''  ' ) RSCALE( J ) = VALUE4

!  MARK 'D'-TYPE GROUPS AND RECORD THEIR MULTIPLICATIVE FACTORS.
!  IDROWS(1, ), IDROWS(2, ) GIVE THE NUMBERS OF THE GROUPS REFERRED
!  TO BY THE NEW D-TYPE GROUP AND RDROWS(1, ) AND RDROWS(2, ) GIVE
!  THE MULTIPLICATIVE FACTORS.

      IF ( FIELD1( 1: 1 ) .EQ. 'D' ) THEN
         IF ( ISTATE( J ) .LE. 4 ) THEN
            INFORM = 22
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2220 )
            RETURN
         END IF
         DO 10 I = 1, 2
            IF ( I .GT. NOVALS ) THEN
               IDROWS( I, J ) = 1
               RDROWS( I, J ) = ZERO
            ELSE

!  CHECK THAT THE GROUPS REFERRED TO WHEN CONSTRUCTING A D-TYPE
!  GROUP ALREADY EXIST.

               IF ( I .EQ. 1 ) THEN
                  FIELD = FIELD3 // 'GR'
               ELSE
                  FIELD = FIELD5 // 'GR'
               END IF
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .GT. 0 ) THEN
                  IDROWS( I, J ) = INLIST( IFIELD )
                  IF ( I .EQ. 1 ) THEN
                     RDROWS( I, J ) = VALUE4
                  ELSE
                     RDROWS( I, J ) = VALUE6
                  END IF
               ELSE
                  INFORM = 4
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2040 ) FIELD( 1: 10 )
                  RETURN
               END IF
            END IF
   10    CONTINUE
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',  &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in GROUPS section ' )
 2220 FORMAT( ' ** Exit from GPSMPS -',  &
              ' conflicting field 1 on GROUPS card')
      END
!  THIS VERSION: 28/10/1992 AT 02:36:30 PM.
      SUBROUTINE SGRP2 ( NG, NGMAX, NOMAX, LA, LENGTH,  &
                         NNZA, NOBJ, NOVALS, INLIST, IDROWS,  &
                         ISTATE, ITYPEG, ITABLE, ICOORD,  &
                         A, RDROWS, RSCALE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         GNAMES, ONAMES, KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LA, LENGTH
      INTEGER        NG, NGMAX, NOMAX
      INTEGER        NNZA, NOBJ, NOVALS
      DOUBLE PRECISION VALUE4, VALUE6
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INLIST( LENGTH ), IDROWS( 2, NGMAX )
      INTEGER        ISTATE( NGMAX ), ITYPEG( NGMAX )
      INTEGER        ITABLE ( LENGTH )
      INTEGER        ICOORD( LA, 2 )
      DOUBLE PRECISION A( LA ), RDROWS( 2, NGMAX ), RSCALE( NGMAX )
      CHARACTER * 10 GNAMES( NGMAX ), ONAMES( NOMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS GROUPS/ROWS/CONSTRAINTS.
!  ------------------------------------------
!  THE GROUPS SECTION APPEARS AFTER THE VARIABLES SECTION.
!  --------------------------------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFREE, IFIELD, IS, J
      DOUBLE PRECISION ZERO
      CHARACTER * 12   FIELD
      EXTERNAL         HASHB , HASHC
      PARAMETER      ( ZERO = 0.0D+0 )

!  IGNORE 'MARKER' CARDS.

      IF ( FIELD3 .EQ. '''MARKER''  ' ) RETURN

!  FIND A PLACE TO INSERT THE NEW GROUP NAME IN THE HASH-TABLE.

      CALL HASHB ( LENGTH, 12, FIELD2 // 'GR', KEY, ITABLE, IFREE )

!  THE NAME ALREADY EXISTS. IT IS THE J-TH NAME IN THE LIST.

      IF ( IFREE .LE. 0 ) THEN
         IF ( IFREE .EQ. 0 ) THEN
            INFORM = - 1
            RETURN
         END IF
         J = INLIST( - IFREE )
      ELSE

!  MARK ANY OBJECTIVE FUNCTION ROWS AS SPECIAL.

         IF ( FIELD1 .EQ. 'N ' .OR. FIELD1 .EQ. ' N' .OR.&
              FIELD1 .EQ. 'DN' .OR. FIELD1 .EQ. 'XN' .OR.&
              FIELD1 .EQ. 'ZN' ) THEN
            NOBJ = NOBJ + 1
            IF( NOBJ .GT. NOMAX ) THEN
               INFORM = - 5
               RETURN
            END IF
            ONAMES ( NOBJ ) = FIELD2
         END IF

!  THE GROUP IS THE NG-TH ENCOUNTERED.

         NG = NG + 1
         J = NG
         IF ( NG .GE. NGMAX ) THEN
            INFORM = - 6
            RETURN
         END IF

!  RECORD THE POSITION OF THE NEW GROUP IN THE TABLE, RECORD ITS
!  NAME AND INITIALISE ITS TYPE AS TRIVIAL.

         INLIST( IFREE ) = NG
         GNAMES( NG ) = FIELD2
         ITYPEG( NG ) = - 1
         IS = 0

!  RECORD THE STATUS, ISTATE, OF THE GROUP. ISTATE( NG ) = :
!  1 THE GROUP IS AN OBJECTIVE FUNCTION TYPE.
!  2 THE GROUP IS AN EQUALITY FUNCTION TYPE.
!  3 THE GROUP IS A LESS-THAN-OR-EQUAL-TO TYPE.
!  4 THE GROUP IS A GREATER-THAN-OR-EQUAL-TO TYPE.
!  5 THE GROUP IS OF D-TYPE AND AN OBJECTIVE FUNCTION TYPE.
!  6 THE GROUP IS OF D-TYPE AND AN EQUALITY FUNCTION TYPE.
!  7 THE GROUP IS OF D-TYPE AND A LESS-THAN-OR-EQUAL-TO TYPE.
!  8 THE GROUP IS OF D-TYPE AND A GREATER-THAN-OR-EQUAL-TO TYPE.

         IF ( FIELD1 .EQ. 'N ' .OR. FIELD1 .EQ. ' N' .OR.&
              FIELD1 .EQ. 'XN' .OR. FIELD1 .EQ. 'ZN' ) IS = 1
         IF ( FIELD1 .EQ. 'E ' .OR. FIELD1 .EQ. ' E' .OR.&
              FIELD1 .EQ. 'XE' .OR. FIELD1 .EQ. 'ZE' ) IS = 2
         IF ( FIELD1 .EQ. 'L ' .OR. FIELD1 .EQ. ' L' .OR.&
              FIELD1 .EQ. 'XL' .OR. FIELD1 .EQ. 'ZL' ) IS = 3
         IF ( FIELD1 .EQ. 'G ' .OR. FIELD1 .EQ. ' G' .OR.&
              FIELD1 .EQ. 'XG' .OR. FIELD1 .EQ. 'ZG' ) IS = 4
         IF ( FIELD1 .EQ. 'DN' ) IS = 5
         IF ( FIELD1 .EQ. 'DE' ) IS = 6
         IF ( FIELD1 .EQ. 'DL' ) IS = 7
         IF ( FIELD1 .EQ. 'DG' ) IS = 8
         IF ( IS .EQ. 0 ) THEN
            INFORM = 10
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
            RETURN
         ENDIF
         ISTATE( NG ) = IS
      END IF

!  INCLUDE GROUP SCALE FACTORS.

      IF ( FIELD3 .EQ. '''SCALE''   ' .OR. FIELD3&
           .EQ. ' ''SCALE''  ' ) THEN
         RSCALE( J ) = VALUE4
         RETURN
      END IF

!  MARK 'D'-TYPE GROUPS AND RECORD THEIR MULTIPLICATIVE FACTORS.
!  IDROWS(1, ), IDROWS(2, ) GIVE THE NUMBERS OF THE GROUPS REFERRED
!  TO BY THE NEW D-TYPE GROUP AND RDROWS(1, ) AND RDROWS(2, ) GIVE
!  THE MULTIPLICATIVE FACTORS.

      IF ( FIELD1( 1: 1 ) .EQ. 'D' ) THEN
         IF ( ISTATE( J ) .LE. 4 ) THEN
            INFORM = 22
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2220 )
            RETURN
         END IF
         DO 10 I = 1, 2
            IF ( I .GT. NOVALS ) THEN
               IDROWS( I, J ) = 1
               RDROWS( I, J ) = ZERO
            ELSE

!  CHECK THAT THE GROUPS REFERRED TO WHEN CONSTRUCTING A D-TYPE
!  GROUP ALREADY EXIST.

               IF ( I .EQ. 1 ) THEN
                  FIELD = FIELD3 // 'GR'
               ELSE
                  FIELD = FIELD5 // 'GR'
               END IF
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .GT. 0 ) THEN
                  IDROWS( I, J ) = INLIST( IFIELD )
                  IF ( I .EQ. 1 ) THEN
                     RDROWS( I, J ) = VALUE4
                  ELSE
                     RDROWS( I, J ) = VALUE6
                  END IF
               ELSE
                  INFORM = 4
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2040 ) FIELD( 1: 10 )
                  RETURN
               END IF
            END IF
   10    CONTINUE
      ELSE
         IF ( NOVALS .GT. 0 ) THEN

!  CHECK THAT DATA HAS NOT BEEN SPECIFIED FOR A 'D'-GROUP.

            IF ( ISTATE( J ) .GE. 5 ) THEN
               INFORM = 8
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2080 )
               RETURN
            END IF

!  ENTRIES FOR THE LINEAR ELEMENT FOR GROUP J ARE TO BE SPECIFIED.
!  FIND THE VARIABLE NUMBERS FOR THE INPUT ENTRIES.

            DO 20 I = 1, NOVALS
               IF ( I .EQ. 1 ) THEN
                  FIELD = FIELD3 // 'VA'
               ELSE
                  FIELD = FIELD5 // 'VA'
               END IF
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .GT. 0 ) THEN

!  THE NNZA-TH NONZERO HAS BEEN SPECIFIED.

                  NNZA = NNZA + 1
                  IF ( NNZA .GT. LA ) THEN
                     INFORM = - 2
                    RETURN
                  END IF

!  THE NONZERO IS FOR GROUP ICOORD( ,1) AND VARIABLE ICOORD( ,2).
!  ITS VALUE IS GIVEN IN A( ).

                  ICOORD( NNZA, 1 ) = J
                  ICOORD( NNZA, 2 ) = INLIST( IFIELD )
                  IF ( I .EQ. 1 ) THEN
                     A( NNZA ) = VALUE4
                  ELSE
                     A( NNZA ) = VALUE6
                  END IF
               ELSE

!  THE VARIABLE NAME IS UNKNOWN.

                  INFORM = 5
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD( 1 : 10 )
                  RETURN
               END IF
   20       CONTINUE
         END IF
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',  &
              ' name is ', A10 )
 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',  &
              ' name is ', A10 )
 2080 FORMAT( ' ** Exit from GPSMPS - ''D'' group/row contains data ' )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in GROUPS section ' )
 2220 FORMAT( ' ** Exit from GPSMPS -',  &
              ' conflicting field 1 on GROUPS card')
      END
!  THIS VERSION: 20/12/1999 AT 18:00:00 PM.
      SUBROUTINE SVAR1 ( NMAX, LENGTH, NVAR, COLFIE,
! ** Correction 0. 20/12/99: Array holding variable types introduced.&
                         ITYPEV, INLIST, ITABLE, CSCALE,
! ** Correction 2. 26/02/01: 4 dummy arguments removed from SVAR1 **&
                         FIELD2, FIELD3, VALUE4, &
                         VNAMES, KEY, INFORM )
      INTEGER        INFORM, LENGTH
      INTEGER        NMAX
      INTEGER        NVAR
      DOUBLE PRECISION VALUE4
      CHARACTER * 2  COLFIE
      CHARACTER * 10 FIELD2, FIELD3
      INTEGER        INLIST( LENGTH ),  ITABLE ( LENGTH )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
      INTEGER        ITYPEV( NMAX )
      DOUBLE PRECISION CSCALE( NMAX )
      CHARACTER * 10 VNAMES( NMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS COLUMNS/VARIABLES, CONSTANTS/RHS/RHS' OR RANGES.
!  ------------------------------------------------------------------
!  THE VARIABLES SECTION BEFORE THE GROUPS SECTION.
!  -------------------------------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          IFREE, J
      EXTERNAL         HASHB

!  IGNORE 'MARKER' CARDS.

      IF ( FIELD3 .EQ. '''MARKER''  ' ) RETURN

!  FIND A PLACE TO INSERT THE NEW VARIABLE NAME IN THE HASH-TABLE
!  IF IT HAS NOT ALREADY BEEN ENTERED.

      CALL HASHB ( LENGTH, 12, FIELD2 // COLFIE, KEY, ITABLE, IFREE )
      IF ( IFREE .LE. 0 ) THEN

!  THE VARIABLE NAME ALREADY EXISTS. IT IS THE J-TH NAMED VARIABLE.

         IF ( IFREE .EQ. 0 ) THEN
            INFORM = - 1
            RETURN
         END IF
         J = INLIST( - IFREE )
      ELSE

!  THE VARIABLE NAME IS NEW. THE VARIABLE IS THE NVAR-TH ENCOUNTERED AND
!  IT OCCURS IN POSITION IFREE IN THE TABLE.

         NVAR = NVAR + 1
         IF ( NVAR .GT. NMAX ) THEN
            INFORM = - 7
            RETURN
         END IF
         J = NVAR
         INLIST( IFREE ) = NVAR
         VNAMES( NVAR ) = FIELD2
      END IF

!  INCLUDE COLUMN SCALE FACTORS IF THEY ARE ALLOWED.

      IF ( FIELD3 .EQ. '''SCALE''   ' .OR.&
           FIELD3 .EQ. ' ''SCALE''  ' ) THEN
         CSCALE( J ) = VALUE4
      END IF
! ** Correction 0. 20/12/99: Array holding variable types introduced.

!  MARK ZERO-ONE AND INTEGER VARIABLES

      IF ( FIELD3 .EQ. '''ZERO-ONE''' ) ITYPEV( J ) = 1
      IF ( FIELD3 .EQ. '''INTEGER'' ' ) ITYPEV( J ) = 2
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

      END
!  THIS VERSION: 20/12/1999 AT 18:00:00 PM.
      SUBROUTINE SVAR2 ( NMAX, NGMAX, LA, LENGTH, NNZA, NVAR, NOVALS,
! ** Correction 0. 20/12/99: Array holding variable types introduced.&
                         NRLNDX, VARSEC, COLFIE, ICOORD, ISTATE, ITYPEV,  &
                         INLIST, ITABLE, A, CSCALE, REALVL, DFAULT,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         VNAMES, KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LA, LENGTH
      INTEGER        NMAX, NGMAX, NNZA, NVAR, NOVALS, NRLNDX
      DOUBLE PRECISION VALUE4, VALUE6
      LOGICAL        VARSEC
      CHARACTER * 2  FIELD1, COLFIE
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        ICOORD( LA, 2 ), ISTATE( NGMAX )
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
      INTEGER        ITYPEV( NMAX )
      DOUBLE PRECISION A( LA ), CSCALE( NMAX )
      DOUBLE PRECISION REALVL( NRLNDX ), DFAULT( NMAX )
      CHARACTER * 10 VNAMES( NMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS COLUMNS/VARIABLES, CONSTANTS/RHS/RHS' OR RANGES.
!  ------------------------------------------------------------------
!  THE VARIABLES SECTION APPEARS AFTER THE GROUPS SECTION.
!  -------------------------------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFREE, IFIELD, J
      DOUBLE PRECISION ZERO, BIGINF
      CHARACTER * 12   FIELD
      EXTERNAL         HASHB , HASHC
      PARAMETER      ( ZERO = 0.0D+0, BIGINF = 1.0D+20 )

!  IGNORE 'MARKER' CARDS.

      IF ( FIELD3 .EQ. '''MARKER''  ' ) RETURN

!  FIND A PLACE TO INSERT THE NEW VARIABLE NAME IN THE HASH-TABLE
!  IF IT HAS NOT ALREADY BEEN ENTERED.

      CALL HASHB ( LENGTH, 12, FIELD2 // COLFIE, KEY, ITABLE, IFREE )

!  THE VARIABLE NAME ALREADY EXISTS. IT IS THE J-TH NAMED VARIABLE.

      IF ( IFREE .LE. 0 ) THEN
         IF ( IFREE .EQ. 0 ) THEN
            INFORM = - 1
            RETURN
         END IF
         J = INLIST( - IFREE )
      ELSE

!  THE VARIABLE NAME IS NEW. THE VARIABLE IS THE NVAR-TH ENCOUNTERED AND
!  IT OCCURS IN POSITION IFREE IN THE TABLE.

         NVAR = NVAR + 1
         IF ( NVAR .GT. NMAX ) THEN
            INFORM = - 7
            RETURN
         END IF
         J = NVAR
         INLIST( IFREE ) = NVAR
         VNAMES( NVAR ) = FIELD2
         IF ( COLFIE .EQ. 'CO' ) DFAULT( NVAR ) = ZERO
         IF ( COLFIE .EQ. 'RA' ) DFAULT( NVAR ) = BIGINF
      END IF

!  INCLUDE COLUMN SCALE FACTORS IF THEY ARE ALLOWED.

      IF ( FIELD3 .EQ. '''SCALE''   ' .OR.&
           FIELD3 .EQ. ' ''SCALE''  ' ) THEN
         IF ( .NOT. VARSEC ) THEN
            INFORM = 7
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2070 )
         ELSE
            CSCALE( J ) = VALUE4
         END IF
         RETURN
      END IF
! ** Correction 0. 20/12/99: Array holding variable types introduced.

!  MARK ZERO-ONE AND INTEGER VARIABLES

      IF ( FIELD3 .EQ. '''ZERO-ONE''' ) THEN
         IF ( .NOT. VARSEC ) THEN
            INFORM = 7
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2060 )
         ELSE
            ITYPEV( J ) = 1
         END IF
         RETURN
      END IF
      IF ( FIELD3 .EQ. '''INTEGER'' ' ) THEN
         IF ( .NOT. VARSEC ) THEN
            INFORM = 7
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2060 )
         ELSE
            ITYPEV( J ) = 2
         END IF
         RETURN
      END IF

!  A NONTRIVIAL DEFAULT VALUE HAS BEEN SPECIFIED FOR A CONSTANT OR
!  RANGE VECTOR.

      IF ( FIELD3 .EQ. '''DEFAULT'' ' .AND. .NOT. VARSEC ) THEN
         IF ( FIELD1 .EQ. 'Z ' ) THEN
            CALL HASHC ( LENGTH, 12,  FIELD5( 1 : 10 ) // 'RI',  &
                         KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD5( 1 : 10 )
               RETURN
            END IF
            VALUE4 = REALVL( INLIST( IFIELD ) )
         END IF
         DFAULT( NVAR ) = VALUE4
      ELSE

!  FIND THE GROUP NUMBERS FOR THE INPUT NONZERO(S).

         IF ( NOVALS .GT. 0 ) THEN
            DO 10 I = 1, NOVALS
               IF ( I .EQ. 1 ) THEN
                  FIELD = FIELD3 // 'GR'
               ELSE
                  FIELD = FIELD5 // 'GR'
               END IF
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .GT. 0 ) THEN

!  CHECK THAT DATA HAS NOT BEEN SPECIFIED FOR A 'D'-GROUP.

                  IF ( ISTATE( INLIST( IFIELD ) ) .GE. 5&
                       .AND. VARSEC ) THEN
                     INFORM = 8
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2080 )
                     RETURN
                  END IF

!  THE NNZA-TH NONZERO HAS BEEN SPECIFIED.

                  NNZA = NNZA + 1
                  IF ( NNZA .GT. LA ) THEN
                     INFORM = - 2
                     RETURN
                  END IF

!  THE NONZERO IS FOR GROUP ICOORD( ,1) AND VARIABLE ICOORD( ,2).
!  ITS VALUE IS GIVEN IN A( ).

                  ICOORD( NNZA, 1 ) = INLIST( IFIELD )
                  ICOORD( NNZA, 2 ) = J
                  IF ( I .EQ. 1 ) THEN
                     A( NNZA ) = VALUE4
                  ELSE
                     A( NNZA ) = VALUE6
                  END IF
               ELSE

!  THE GROUP NAME IS UNKNOWN.

                  INFORM = 4
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2040 ) FIELD( 1: 10 )
                  RETURN
               END IF
   10       CONTINUE
         END IF
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',  &
              ' name is ', A10 )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
 2060 FORMAT( ' ** Exit from GPSMPS - type given for RHS or RANGES ' )
 2070 FORMAT( ' ** Exit from GPSMPS - scale given for RHS or RANGES ' )
 2080 FORMAT( ' ** Exit from GPSMPS - ''D'' group/row contains data ' )
      END
!  THIS VERSION: 28/10/1992 AT 02:36:30 PM.
      SUBROUTINE SBOUND( NMAX, NBMAX, LENGTH, NLVARS, NBND, NCOL,  &
                         NRLNDX, DEFAUT, INLIST, ITABLE, BND, REALVL,
! ** Correction 3. 26/02/01: 1 dummy argument removed from SBOUND **&
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5,  &
                         BNAMES, BNDFLT, KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LENGTH
      INTEGER        NMAX, NBMAX, NLVARS, NBND, NCOL, NRLNDX
      LOGICAL        DEFAUT
      DOUBLE PRECISION VALUE4
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      DOUBLE PRECISION BND( 2, NMAX, NBMAX ), REALVL( NRLNDX )
      DOUBLE PRECISION BNDFLT( 2, NBMAX )
      CHARACTER * 10 BNAMES( NBMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS BOUNDS.
!  -------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFIELD
      DOUBLE PRECISION BIG, ZERO
      EXTERNAL         HASHC
      PARAMETER      ( ZERO = 0.0D+0, BIG = 1.0D+20 )

!  THE FIRST PAIR OF BOUND VECTORS ARE TO BE ASSIGNED.

      IF ( NBND .EQ. 0 ) THEN
         NBND = 1
         IF ( NBND .GT. NBMAX ) THEN
            INFORM = - 13
            RETURN
         END IF
         BNAMES( NBND ) = FIELD2
         DEFAUT = .TRUE.
         BNDFLT( 1, NBND ) = ZERO
         BNDFLT( 2, NBND ) = BIG
         DO 10 I = 1, NLVARS
            BND( 1, I, NBND ) = ZERO
            BND( 2, I, NBND ) = BIG
   10    CONTINUE
      END IF

!  A NEW PAIR OF BOUND VECTORS ARE TO BE ASSIGNED.

      IF ( FIELD2 .NE. BNAMES( NBND ) ) THEN
         NBND = NBND + 1
         IF ( NBND .GT. NBMAX ) THEN
            INFORM = - 13
            RETURN
         END IF
         BNAMES( NBND ) = FIELD2
         DEFAUT = .TRUE.
         BNDFLT( 1, NBND ) = ZERO
         BNDFLT( 2, NBND ) = BIG
         DO 20 I = 1, NLVARS
            BND( 1, I, NBND ) = ZERO
            BND( 2, I, NBND ) = BIG
   20    CONTINUE
      END IF

!  ENSURE THAT DEFAULT VALUES ARE ASSIGNED FIRST.

      IF ( FIELD3 .EQ. '''DEFAULT'' ' ) THEN
         IF ( .NOT. DEFAUT ) THEN
            INFORM = 20
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2200 )
            RETURN
         END IF
         IF ( FIELD1 .EQ. 'ZL' .OR. FIELD1 .EQ. 'ZU' .OR.&
              FIELD1 .EQ. 'ZX' ) THEN
            CALL HASHC ( LENGTH, 12,  FIELD5( 1 : 10 ) // 'RI',  &
                         KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD5( 1 : 10 )
               RETURN
            END IF
            VALUE4 = REALVL( INLIST( IFIELD ) )
         END IF
         IF ( FIELD1 .EQ. 'LO' .OR. FIELD1 .EQ. 'UP' .OR.&
              FIELD1 .EQ. 'FX' .OR. FIELD1 .EQ. 'FR' .OR.&
              FIELD1 .EQ. 'MI' .OR. FIELD1 .EQ. 'PL' .OR.&
              FIELD1 .EQ. 'XL' .OR. FIELD1 .EQ. 'XU' .OR.&
              FIELD1 .EQ. 'XX' .OR. FIELD1 .EQ. 'XR' .OR.&
              FIELD1 .EQ. 'XM' .OR. FIELD1 .EQ. 'XP' .OR.&
              FIELD1 .EQ. 'ZL' .OR. FIELD1 .EQ. 'ZU' .OR.&
              FIELD1 .EQ. 'ZX' ) THEN

!  ASSIGN DEFAULT LOWER BOUNDS FOR VARIABLES.

            IF ( ( FIELD1( 2 : 2 ) .EQ. 'L' .AND.&
                   FIELD1 .NE. 'PL' ) .OR.&
                 FIELD1( 2 : 2 ) .EQ. 'X' .OR.&
                 FIELD1( 2 : 2 ) .EQ. 'R' .OR.&
                 FIELD1( 2 : 2 ) .EQ. 'M' .OR.&
                 FIELD1 .EQ. 'LO' .OR. FIELD1 .EQ. 'MI' ) THEN

!  A FINITE LOWER BOUND IS SPECIFIED.

               IF ( FIELD1( 2 : 2 ) .EQ. 'L' .OR.&
                    FIELD1( 2 : 2 ) .EQ. 'X' .OR.&
                    FIELD1 .EQ. 'LO' ) THEN
                  BNDFLT( 1, NBND ) = VALUE4
                  DO 30 I = 1, NLVARS
                     BND( 1, I, NBND ) = VALUE4
   30             CONTINUE

!  AN INFINITE LOWER BOUND IS SPECIFIED.

               ELSE
                  BNDFLT( 1, NBND ) = - BIG
                  DO 40 I = 1, NLVARS
                     BND( 1, I, NBND ) = - BIG
   40             CONTINUE
                  IF ( FIELD1( 2 : 2 ) .EQ. 'M' .OR.&
                       FIELD1 .EQ. 'MI' ) THEN
                     BNDFLT( 2, NBND ) = ZERO
                     DO 41 I = 1, NLVARS
                        BND( 2, I, NBND ) = ZERO
   41                CONTINUE
                  END IF
               END IF
            END IF

!  ASSIGN DEFAULT UPPER BOUNDS FOR VARIABLES.

            IF ( FIELD1( 2 : 2 ) .EQ. 'U' .OR.&
                 FIELD1( 2 : 2 ) .EQ. 'X' .OR.&
                 FIELD1( 2 : 2 ) .EQ. 'R' .OR.&
                 FIELD1( 2 : 2 ) .EQ. 'P' ) THEN

!  A FINITE UPPER BOUND IS SPECIFIED.

               IF ( FIELD1( 2 : 2 ) .EQ. 'U' .OR.&
                    FIELD1( 2 : 2 ) .EQ. 'X' .OR.&
                    FIELD1 .EQ. 'UP' ) THEN
                  IF ( ( FIELD1( 2 : 2 ) .EQ. 'U' .OR.&
                         FIELD1 .EQ. 'UP' )  &
                       .AND. VALUE4 .EQ. ZERO&
                       .AND. BNDFLT( 1, NBND ) .EQ. ZERO&
                       .AND. BNDFLT( 2, NBND ) .EQ. BIG ) THEN
                     BNDFLT( 1, NBND ) = - BIG
                     DO 51 I = 1, NLVARS
                        BND( 1, I, NBND ) = - BIG
   51                CONTINUE
                  END IF
                  BNDFLT( 2, NBND ) = VALUE4
                  DO 50 I = 1, NLVARS
                     BND( 2, I, NBND ) = VALUE4
   50             CONTINUE

!  AN INFINITE UPPER BOUND IS SPECIFIED.

               ELSE
                  BNDFLT( 2, NBND ) = BIG
                  DO 60 I = 1, NLVARS
                     BND( 2, I, NBND ) = BIG
   60             CONTINUE
               END IF
            END IF

!  FIELD 1 IS NOT RECOGNISED.

         ELSE
            INFORM = 10
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
            RETURN
         END IF
      ELSE

!  AN INDIVIDUAL BOUND IS TO BE ASSIGNED.

         DEFAUT = .FALSE.
         IF ( FIELD1 .EQ. 'LO' .OR. FIELD1 .EQ. 'XL' .OR.&
              FIELD1 .EQ. 'UP' .OR. FIELD1 .EQ. 'XU' .OR.&
              FIELD1 .EQ. 'FX' .OR. FIELD1 .EQ. 'XX' .OR.&
              FIELD1 .EQ. 'FR' .OR. FIELD1 .EQ. 'XR' .OR.&
              FIELD1 .EQ. 'MI' .OR. FIELD1 .EQ. 'XM' .OR.&
              FIELD1 .EQ. 'PL' .OR. FIELD1 .EQ. 'XP' .OR.&
              FIELD1 .EQ. 'ZL' .OR. FIELD1 .EQ. 'ZU' .OR.&
              FIELD1 .EQ. 'ZX' ) THEN

!  FIND WHICH VARIABLE IS BEING ASSIGNED.

            CALL HASHC ( LENGTH, 12, FIELD3//'VA', KEY, ITABLE, IFIELD )
            IF ( IFIELD .GT. 0 ) THEN
               NCOL = INLIST( IFIELD )

!  ASSIGN A LOWER BOUND FOR THIS VARIABLE.

               IF ( FIELD1 .EQ. 'LO' .OR. FIELD1 .EQ. 'XL' .OR.&
                    FIELD1 .EQ. 'FX' .OR. FIELD1 .EQ. 'XX' .OR.&
                    FIELD1 .EQ. 'FR' .OR. FIELD1 .EQ. 'XR' .OR.&
                    FIELD1 .EQ. 'MI' .OR. FIELD1 .EQ. 'XM' .OR.&
                    FIELD1 .EQ. 'ZL' .OR. FIELD1 .EQ. 'ZX' ) THEN

!  A FINITE LOWER BOUND IS SPECIFIED.

                  IF ( FIELD1 .EQ. 'LO' .OR. FIELD1 .EQ. 'XL' .OR.&
                       FIELD1 .EQ. 'ZL' .OR. FIELD1 .EQ. 'ZX' .OR.&
                       FIELD1 .EQ. 'FX' .OR. FIELD1 .EQ. 'XX' ) THEN
                     BND( 1, NCOL, NBND ) = VALUE4

!  AN INFINITE LOWER BOUND IS SPECIFIED.

                  ELSE
                     IF ( ( FIELD1 .EQ. 'MI' .OR. FIELD1 .EQ. 'XM' )  &
                            .AND. BND( 1, NCOL, NBND ) .EQ. ZERO&
                            .AND. BND( 2, NCOL, NBND ) .EQ. BIG )  &
                        BND( 2, NCOL, NBND ) = ZERO
                     BND( 1, NCOL, NBND ) = - BIG
                  END IF
               END IF

!  ASSIGN AN UPPER BOUND FOR THE VARIABLE.

               IF ( FIELD1 .EQ. 'UP' .OR. FIELD1 .EQ. 'XU' .OR.&
                    FIELD1 .EQ. 'FX' .OR. FIELD1 .EQ. 'XX' .OR.&
                    FIELD1 .EQ. 'FR' .OR. FIELD1 .EQ. 'XR' .OR.&
                    FIELD1 .EQ. 'PL' .OR. FIELD1 .EQ. 'XP' .OR.&
                    FIELD1 .EQ. 'ZU' .OR. FIELD1 .EQ. 'ZX' ) THEN

!  A FINITE UPPER BOUND IS SPECIFIED.

                  IF ( FIELD1 .EQ. 'UP' .OR. FIELD1 .EQ. 'XU' .OR.&
                       FIELD1 .EQ. 'ZU' .OR. FIELD1 .EQ. 'ZX' .OR.&
                       FIELD1 .EQ. 'FX' .OR. FIELD1 .EQ. 'XX' ) THEN
                     IF ( ( FIELD1 .EQ. 'UP' .OR. FIELD1 .EQ. 'XU' .OR.&
                            FIELD1 .EQ. 'ZU' ) .AND. VALUE4 .EQ. ZERO&
                            .AND. BND( 1, NCOL, NBND ) .EQ. ZERO&
                            .AND. BND( 2, NCOL, NBND ) .EQ. BIG )  &
                          BND( 1, NCOL, NBND ) = - BIG
                     BND( 2, NCOL, NBND ) = VALUE4


!  AN INFINITE UPPER BOUND IS SPECIFIED.

                  ELSE
                     BND( 2, NCOL, NBND ) = BIG
                  END IF
               END IF
            ELSE
               INFORM = 5
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD3
               RETURN
            END IF

!  FIELD 1 IS NOT RECOGNISED.

         ELSE
            INFORM = 10
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
            RETURN
         END IF
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',  &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in BOUNDS section ' )
 2200 FORMAT( ' ** Exit from GPSMPS - default specified out of order ' )
      END
!  THIS VERSION: 28/10/1992 AT 02:36:30 PM.
      SUBROUTINE SSTART( NMAX, NGMAX, NSMAX, LENGTH, NLVARS, NG,  &
                         NSTART, NCOL, NRLNDX, DEFAUT, INLIST, ITABLE,  &
                         VSTART, CSTART, REALVL,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         SNAMES, NOVALS, KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LENGTH, NOVALS, NG, NGMAX
      INTEGER        NMAX, NSMAX, NLVARS, NSTART, NCOL, NRLNDX
      LOGICAL        DEFAUT
      DOUBLE PRECISION VALUE4, VALUE6
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      DOUBLE PRECISION VSTART( NMAX, NSMAX ), CSTART( NGMAX, NSMAX )
      DOUBLE PRECISION REALVL( NRLNDX )
      CHARACTER * 10 SNAMES( NSMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS START POINT.
!  ------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFIELD
      DOUBLE PRECISION ZERO
      CHARACTER * 10   FIELD
      EXTERNAL         HASHC
      PARAMETER      ( ZERO = 0.0D+0 )

!  THE STARTING VECTOR IS  TO BE ASSIGNED.

      IF ( NSTART .EQ. 0 ) THEN
         NSTART = 1
         IF ( NSTART .GT. NSMAX ) THEN
            INFORM = - 8
            RETURN
         END IF
         SNAMES( NSTART ) = FIELD2
         DEFAUT = .TRUE.
         DO 10 I = 1, NLVARS
            VSTART( I, NSTART ) = ZERO
   10    CONTINUE
         DO 20 I = 1, NG
            CSTART( I, NSTART ) = ZERO
   20    CONTINUE
      END IF

!  A NEW STARTING VECTOR IS TO BE ASSIGNED.

      IF ( FIELD2 .NE. SNAMES( NSTART ) ) THEN
         NSTART = NSTART + 1
         IF ( NSTART .GT. NSMAX ) THEN
            INFORM = - 8
            RETURN
         END IF
         SNAMES( NSTART ) = FIELD2
         DEFAUT = .TRUE.

!  ASSIGN A DEFAULT VALUE OF ZERO TO THE VARIABLES.

         DO 30 I = 1, NLVARS
            VSTART( I, NSTART ) = ZERO
   30    CONTINUE

!  ASSIGN A DEFAULT VALUE OF ZERO TO THE LAGRANGE MULTIPLIERS.

         DO 40 I = 1, NG
            CSTART( I, NSTART ) = ZERO
   40    CONTINUE
      END IF

!  ENSURE THAT DEFAULT VALUES ARE ASSIGNED FIRST.

      IF ( FIELD3 .EQ. '''DEFAULT'' ' ) THEN
         IF ( .NOT. DEFAUT ) THEN
            INFORM = 20
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2200 )
            RETURN
         END IF
         IF ( FIELD1( 1: 1 ) .EQ. 'Z' ) THEN
            CALL HASHC ( LENGTH, 12,  FIELD5( 1 : 10 ) // 'RI',  &
                         KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD5( 1 : 10 )
               RETURN
            END IF
            VALUE4 = REALVL( INLIST( IFIELD ) )
         END IF

!  ASSIGN DEFAULT VALUES TO THE STARTING POINT.

         IF ( FIELD1 .EQ. '  ' .OR. FIELD1 .EQ. 'V ' .OR.&
              FIELD1 .EQ. 'X ' .OR. FIELD1 .EQ. 'Z ' .OR.&
              FIELD1 .EQ. 'XV' .OR. FIELD1 .EQ. 'ZV' ) THEN
            DO 50 I = 1, NLVARS
               VSTART( I, NSTART ) = VALUE4
   50       CONTINUE
         END IF

!  ASSIGN DEFAULT VALUES TO THE LAGRANGE MULTIPLIERS.

         IF ( FIELD1 .EQ. '  ' .OR. FIELD1 .EQ. 'M ' .OR.&
              FIELD1 .EQ. 'X ' .OR. FIELD1 .EQ. 'Z ' .OR.&
              FIELD1 .EQ. 'XM' .OR. FIELD1 .EQ. 'ZM' ) THEN
            DO 60 I = 1, NG
               CSTART( I, NSTART ) = VALUE4
   60       CONTINUE
         END IF
      ELSE

!  AN INDIVIDUAL STARTING VALUE IS TO BE ASSIGNED.

         IF ( FIELD1 .EQ. 'X ' .OR. FIELD1 .EQ. '  ' .OR.&
              FIELD1 .EQ. 'Z ' ) THEN
            DEFAUT = .FALSE.

!  FIND WHICH VALUE IS, OR VALUES ARE, BEING ASSIGNED.

            DO 70 I = 1, NOVALS
               IF ( I .EQ. 1 ) THEN
                  FIELD = FIELD3
               ELSE
                  FIELD = FIELD5
               END IF

!  SEE IF THE NAME BELONGS TO A VARIABLE.

               CALL HASHC ( LENGTH, 12, FIELD // 'VA',  &
                            KEY, ITABLE, IFIELD )
               IF ( IFIELD .GT. 0 ) THEN
                  NCOL = INLIST( IFIELD )

!  ASSIGN THE STARTING VALUE FOR THIS VARIABLE.

                  IF ( I .EQ. 1 ) THEN
                     VSTART( NCOL, NSTART ) = VALUE4
                  ELSE
                     VSTART( NCOL, NSTART ) = VALUE6
                  END IF
               ELSE

!  SEE IF THE NAME BELONGS TO A GROUP.

                  CALL HASHC ( LENGTH, 12, FIELD // 'GR',  &
                               KEY, ITABLE, IFIELD )
                  IF ( IFIELD .GT. 0 ) THEN
                     NCOL = INLIST( IFIELD )

!  ASSIGN THE STARTING VALUE FOR THE LAGRANGE MULTIPLIER FOR THIS GROUP.

                     IF ( I .EQ. 1 ) THEN
                        CSTART( NCOL, NSTART ) = VALUE4
                     ELSE
                        CSTART( NCOL, NSTART ) = VALUE6
                     END IF
                  ELSE
                     INFORM = 5
                     IF ( I .EQ. 1 ) THEN
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD3
                     ELSE
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD5
                     END IF
                     RETURN
                  END IF
               END IF
   70       CONTINUE
         ELSE

!  AN INDIVIDUAL STARTING VALUE FOR A VARIABLE IS TO BE ASSIGNED.

            IF ( FIELD1 .EQ. 'V ' .OR. FIELD1 .EQ. 'XV' .OR.&
                 FIELD1 .EQ. 'ZV' ) THEN
               DEFAUT = .FALSE.

!  FIND WHICH VALUE IS, OR VALUES ARE, BEING ASSIGNED.

               DO 80 I = 1, NOVALS
                  IF ( I .EQ. 1 ) THEN
                     FIELD = FIELD3
                  ELSE
                     FIELD = FIELD5
                  END IF

!  SEE IF THE NAME BELONGS TO A VARIABLE.

                  CALL HASHC ( LENGTH, 12, FIELD // 'VA',  &
                               KEY, ITABLE, IFIELD )
                  IF ( IFIELD .GT. 0 ) THEN
                     NCOL = INLIST( IFIELD )

!  ASSIGN THE STARTING VALUE FOR THIS VARIABLE.

                     IF ( I .EQ. 1 ) THEN
                        VSTART( NCOL, NSTART ) = VALUE4
                     ELSE
                        VSTART( NCOL, NSTART ) = VALUE6
                     END IF
                  ELSE
                     INFORM = 4
                     IF ( I .EQ. 1 ) THEN
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2040 ) FIELD3
                     ELSE
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2040 ) FIELD5
                     END IF
                     RETURN
                  END IF
   80          CONTINUE
            ELSE

!  AN INDIVIDUAL STARTING LAGRANGE MULTIPLIER VALUE IS TO BE ASSIGNED.

               IF ( FIELD1 .EQ. 'M ' .OR. FIELD1 .EQ. 'XM' .OR.&
                    FIELD1 .EQ. 'ZM' ) THEN
                  DEFAUT = .FALSE.

!  FIND WHICH VALUE IS, OR VALUES ARE, BEING ASSIGNED.

                  DO 90 I = 1, NOVALS
                     IF ( I .EQ. 1 ) THEN
                        FIELD = FIELD3
                     ELSE
                        FIELD = FIELD5
                     END IF

!  SEE IF THE NAME BELONGS TO A GROUP.

                     CALL HASHC ( LENGTH, 12, FIELD // 'GR',  &
                                  KEY, ITABLE, IFIELD )
                     IF ( IFIELD .GT. 0 ) THEN
                        NCOL = INLIST( IFIELD )

!  ASSIGN THE STARTING VALUE FOR THE LAGRANGE MULTIPLIER FOR THIS GROUP.

                        IF ( I .EQ. 1 ) THEN
                           CSTART( NCOL, NSTART ) = VALUE4
                        ELSE
                           CSTART( NCOL, NSTART ) = VALUE6
                        END IF
                     ELSE
                        INFORM = 5
                        IF ( I .EQ. 1 ) THEN
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD3
                         ELSE
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD5
                        END IF
                        RETURN
                     END IF
   90             CONTINUE

!  FIELD 1 IS NOT RECOGNISED.

               ELSE
                  INFORM = 10
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
                  RETURN
               END IF
            END IF
         END IF
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',  &
              ' name is ', A10 )
 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',  &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in START POINT section ' )
 2200 FORMAT( ' ** Exit from GPSMPS - default specified out of order ' )
      END
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
!  THIS VERSION: 20/12/1999 AT 18:00:00 PM.
      SUBROUTINE SQHESS( NEGMAX, NGMAX, NLMAX, NELMAX, NEVMAX, NETMAX, &
                         NOMAX, NIMAX, LENGTH, NG, NOBJ, NGRUPE, NOVALS, &
                         NELN, NINN, NEPN, NELTYP, NLISEP, NLISEV,  &
                         NELNUM, NELING, QGROUP, QSQR, QPROD, IELV, &
                         IINV, IEPA, ITYPEG, IELING, ISTAEV, IELVAR,
! ** Correction 4. 26/02/01: 1 dummy argument removed from SQHESS **&
                         INLIST, ITABLE, ISTATE, ITYPEE, ISTEP,  &
                         IPTYPE, ISTYPE, INREP, &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         ENAMES, GNAMES, ETYPES, ONAMES, INAMES, &
                         LNAMES, WEIGHT, KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LENGTH, IPTYPE, ISTYPE, NIMAX
      INTEGER        NEGMAX, NGMAX, NOBJ, NELN, NINN, NEPN, NELTYP
      INTEGER        NG, NELING, NOVALS, NGRUPE, NLISEP, NLISEV
      INTEGER        NLMAX, NELMAX, NEVMAX, NETMAX, NOMAX, NELNUM
      DOUBLE PRECISION VALUE4, VALUE6
      LOGICAL        QGROUP, QSQR, QPROD, INREP
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      INTEGER        IELV  ( NLMAX ), IINV  ( NLMAX ), IEPA( NLMAX )
      INTEGER        IELING( NEGMAX, 2 )
      INTEGER        ITYPEG( NGMAX ), ITYPEE( NELMAX ), ISTEP( NELMAX )
      INTEGER        ISTATE( NGMAX )
      INTEGER        IELVAR( NEVMAX ), ISTAEV( NELMAX )
      DOUBLE PRECISION WEIGHT( NEGMAX )
      CHARACTER * 10 GNAMES( NGMAX ), ONAMES( NOMAX ), INAMES( NIMAX )
      CHARACTER * 10 ETYPES( NLMAX ), ENAMES( NETMAX ), LNAMES( NELMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS QUADRATIC.
!  ----------------------------

!  NICK GOULD 18/12/1999
!  FOR CGT PRODUCTIONS.

! ** Correction 8. 26/02/01: unused BIG and J removed from SQHESS **
      INTEGER          I, IFIELD, K, NEVARS
      INTEGER          NCOL1, NCOL2, NTERMS, IFREE
      DOUBLE PRECISION VALUE
      CHARACTER * 12   FIELD
      CHARACTER * 10   CQGROU, CQSQR, CQPROD
      EXTERNAL         HASHB , HASHC
! ** Correction 8a. 26/02/01: 1 line removed **
      PARAMETER      ( CQGROU = '123456789G' )
      PARAMETER      ( CQSQR = '123456789S' )
      PARAMETER      ( CQPROD = '123456789P' )

!  FIND THE FIRST VARIABLE.

      CALL HASHC ( LENGTH, 12, FIELD2 // 'VA', KEY, ITABLE, IFIELD )
      IF ( IFIELD .GT. 0 ) THEN
         NCOL1 = INLIST( IFIELD )
      ELSE
         INFORM = 5
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD2
         RETURN
      END IF

!  FIND THE SECOND VARIABLE.

      IF ( FIELD1 .EQ. 'Z ' ) THEN
         NTERMS = 1
      ELSE
         NTERMS = NOVALS
      END IF


      DO 110 I = 1, NTERMS
         IF ( I .EQ. 1 ) THEN
            FIELD = FIELD3 // 'VA'
            VALUE = VALUE4
         ELSE
            FIELD = FIELD5 // 'VA'
            VALUE = VALUE6
         END IF
         IF ( VALUE .NE. 0.0D+0 ) THEN
            CALL HASHC( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .GT. 0 ) THEN
               NCOL2 = INLIST( IFIELD )
            ELSE
               INFORM = 5
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2050 ) FIELD( 1 : 10 )
               RETURN
            END IF
            IF ( .NOT. QGROUP ) THEN

!  THIS IS THE FIRST HESSIAN TERM. MAKE IT A NEW GROUP.
!  FIND A PLACE TO INSERT THE NEW GROUP NAME IN THE HASH-TABLE.

               CALL HASHB ( LENGTH, 12, CQGROU // 'GR', KEY, &
                            ITABLE, IFREE )
               IF ( IFREE .LE. 0 ) THEN
                  INFORM = - 1
                  RETURN
               ELSE

!  MARK THIS AS AN OBJECTIVE FUNCTION GROUP.

                  NOBJ = NOBJ + 1
                  IF( NOBJ .GT. NOMAX ) THEN
                     INFORM = - 5
                     RETURN
                  END IF
                  ONAMES ( NOBJ ) =  CQGROU

!  THE GROUP IS THE NG-TH ENCOUNTERED.

                  NG = NG + 1
                  IF ( NG .GE. NGMAX ) THEN
                     INFORM = - 6
                     RETURN
                  END IF

!  RECORD THE POSITION OF THE NEW GROUP IN THE TABLE, RECORD ITS
!  NAME AND INITIALISE ITS TYPE AS TRIVIAL.

                  NGRUPE = NG
! ** Correction 8b. 26/02/01: 1 line removed **
                  INLIST( IFREE ) = NG
                  GNAMES( NG ) = CQGROU
                  ITYPEG( NG ) = 0
                  ISTATE( NG ) = 1
                  QGROUP = .TRUE.
               END IF
            END IF
            IF ( NCOL1 .EQ. NCOL2 ) THEN

!  CHECK IF THIS IS THE FIRST OCCURENCE OF A DIAGONAL TERM

               IF ( .NOT. QSQR ) THEN

!  CHECK IF THIS IS THE FIRST ELEMENT TYPE.

                  IF ( NELTYP .EQ. 0 ) THEN
                     ISTYPE = 1
                     NELN = 1
                     NINN = 0
                     NEPN = 0
                     NELTYP = 1
                  ELSE
                     ISTYPE = 2
                     NELTYP = NELTYP + 1
                     IF ( NELTYP .GT. NLMAX ) THEN
                        INFORM = - 3
                        RETURN
                     END IF
                     NELN = NELN + 1
                     IF ( NELN .GT. NETMAX ) THEN
                        INFORM = - 14
                        RETURN
                     END IF
                  END IF

!  INPUT THE NAMES OF THE ELEMENT TYPE.

                  CALL HASHB ( LENGTH, 12, CQSQR // 'ET', KEY, &
                               ITABLE, IFREE )
                  IF ( IFREE .LE. 0 ) THEN
                     IF ( IFREE .EQ. 0 ) THEN
                        INFORM = - 1
                        RETURN
                     END IF
                     INFORM = 18
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2180 )
                     RETURN
                  END IF
                  INLIST( IFREE ) = NELTYP
                  IELV( NELTYP ) = NELN
                  IINV( NELTYP ) = NINN + 1
                  IEPA( NELTYP ) = NEPN + 1
                  ETYPES( NELTYP ) = CQSQR

!  INSERT THE ELEMENT VARIABLE.

                  ENAMES( NELN ) = 'X         '
                  NINN = NINN + 1
                  IF ( NINN .GT. NIMAX ) THEN
                     INFORM = - 16
                     RETURN
                  END IF
                  INAMES( NINN ) = ENAMES( NELN )
! ** Correction -3. 03/11/00: quadratic elements have no internal representation
                  INREP = .FALSE.
                  QSQR = .TRUE.
               END IF

!  THE NEW ELEMENT IS THE NELNUM-TH NONLINEAR ELEMENT.

               NELNUM = NELNUM + 1
               IF ( NELNUM .GT. NELMAX ) THEN
                  INFORM = - 9
                  RETURN
               END IF

!  INSERT THE NAME INTO THE TABLE.

               WRITE( UNIT = FIELD, FMT = "( '%', I9, 'EL' )" ) &
                      123456789 - NELNUM
               CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )

!  RECORD THE ELEMENTS POSITION IN THE TABLE ALONG WITH ITS NAME.

               INLIST( IFREE ) = NELNUM
               LNAMES( NELNUM ) = FIELD( 1 : 10 )

!  DETERMINE THE NUMBER OF THE ELEMENT TYPE, K, THE STARTING
!  ADDRESSES FOR THE PARAMETERS AND VARIABLES FOR THE ELEMENT
!  AND THE NUMBER OF PARAMETERS AND VARIABLES INVOLVED.

               K = ISTYPE
               ITYPEE( NELNUM ) = K
               ISTEP ( NELNUM ) = NLISEP + 1
               ISTAEV( NELNUM ) = NLISEV + 1
               NEVARS = 1
               IF ( NLISEV + NEVARS .GT. NEVMAX ) THEN
                  INFORM = - 15
                  RETURN
               END IF
               IELVAR( NLISEV + 1 ) = NCOL1
               NLISEV = NLISEV + NEVARS

!  ASSIGN THE ELEMENT AS NELING IN GROUP NGRUPE

               NELING = NELING + 1
               IF ( NELING .GT. NEGMAX ) THEN
                  INFORM = - 10
                  RETURN
               END IF

!  THE ELEMENT IS IELING( ,1) AND THE GROUP IS GIVEN BY IELING( ,2).

               IELING( NELING, 1 ) = NELNUM
               IELING( NELING, 2 ) = NGRUPE

!  THE ELEMENT IS WEIGHTED BY THE CONSTANT WEIGHT().

               WEIGHT( NELING ) = VALUE
            ELSE
               IF ( .NOT. QPROD ) THEN

!  CHECK IF THIS IS THE FIRST ELEMENT TYPE.

                  IF ( NELTYP .EQ. 0 ) THEN
                     IPTYPE = 1
                     NELN = 1
                     NINN = 0
                     NEPN = 0
                     NELTYP = 1
                  ELSE
                     IPTYPE = 2
                     NELTYP = NELTYP + 1
                     IF ( NELTYP .GT. NLMAX ) THEN
                        INFORM = - 3
                        RETURN
                     END IF
                     NELN = NELN + 1
                     IF ( NELN .GT. NETMAX ) THEN
                        INFORM = - 14
                        RETURN
                     END IF
                  END IF

!  INPUT THE NAMES OF THE ELEMENT TYPE.

                  CALL HASHB ( LENGTH, 12, CQPROD // 'ET', KEY, &
                               ITABLE, IFREE )
                  IF ( IFREE .LE. 0 ) THEN
                     IF ( IFREE .EQ. 0 ) THEN
                        INFORM = - 1
                        RETURN
                     END IF
                     INFORM = 18
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2180 )
                     RETURN
                  END IF
                  INLIST( IFREE ) = NELTYP
                  IELV( NELTYP ) = NELN
                  IINV( NELTYP ) = NINN + 1
                  IEPA( NELTYP ) = NEPN + 1
                  ETYPES( NELTYP ) = CQPROD

!  INSERT THE ELEMENT VARIABLE.

                  ENAMES( NELN ) = 'X         '
                  NINN = NINN + 1
                  IF ( NINN .GT. NIMAX ) THEN
                     INFORM = - 16
                     RETURN
                  END IF
                  INAMES( NINN ) = ENAMES( NELN )
                  NELN = NELN + 1
                  IF ( NELN .GT. NETMAX ) THEN
                     INFORM = - 14
                     RETURN
                  END IF
                  ENAMES( NELN ) = 'Y         '
                  NINN = NINN + 1
                  IF ( NINN .GT. NIMAX ) THEN
                     INFORM = - 16
                     RETURN
                  END IF
                  INAMES( NINN ) = ENAMES( NELN )
! ** Correction -3. 03/11/00: quadratic elements have no internal representation
                  INREP = .FALSE.
                  QPROD = .TRUE.
               END IF

!  THE NEW ELEMENT IS THE NELNUM-TH NONLINEAR ELEMENT.

               NELNUM = NELNUM + 1
               IF ( NELNUM .GT. NELMAX ) THEN
                  INFORM = - 9
                  RETURN
               END IF

!  INSERT THE NAME INTO THE TABLE.

               WRITE( UNIT = FIELD, FMT = "( '%', I9, 'EL' )" ) &
                      123456789 - NELNUM
               CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )

!  RECORD THE ELEMENTS POSITION IN THE TABLE ALONG WITH ITS NAME.

               INLIST( IFREE ) = NELNUM
               LNAMES( NELNUM ) = FIELD( 1 : 10 )

!  DETERMINE THE NUMBER OF THE ELEMENT TYPE, K, THE STARTING
!  ADDRESSES FOR THE PARAMETERS AND VARIABLES FOR THE ELEMENT
!  AND THE NUMBER OF PARAMETERS AND VARIABLES INVOLVED.

               K = IPTYPE
               ITYPEE( NELNUM ) = K
               ISTEP ( NELNUM ) = NLISEP + 1
               ISTAEV( NELNUM ) = NLISEV + 1
               NEVARS = 2
               IF ( NLISEV + NEVARS .GT. NEVMAX ) THEN
                  INFORM = - 15
                  RETURN
               END IF
               IELVAR( NLISEV + 1 ) = NCOL1
               IELVAR( NLISEV + 2 ) = NCOL2
               NLISEV = NLISEV + NEVARS

!  ASSIGN THE ELEMENT AS NELING IN GROUP NGRUPE

               NELING = NELING + 1
               IF ( NELING .GT. NEGMAX ) THEN
                  INFORM = - 10
                  RETURN
               END IF

!  THE ELEMENT IS IELING( ,1) AND THE GROUP IS GIVEN BY IELING( ,2).

               IELING( NELING, 1 ) = NELNUM
               IELING( NELING, 2 ) = NGRUPE

!  THE ELEMENT IS WEIGHTED BY THE CONSTANT WEIGHT().

               WEIGHT( NELING ) = VALUE
            END IF
         END IF 
  110 CONTINUE   
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',  &
              ' name is ', A10 )
 2180 FORMAT( ' ** Exit from GPSMPS - duplicate element-type name ' )

!  END OF SUBROUTINE SQHESS.

      END
!  THIS VERSION: 28/10/1992 AT 02:36:30 PM.
      SUBROUTINE SETYPE( NLMAX, NIMAX, NETMAX, NEPMAX, LENGTH,  &
                         NOVALS, NELN, NINN, NEPN, NELTYP,  &
                         INREP, IELV, IINV, IEPA, INLIST, ITABLE,
! ** Correction 5. 26/02/01: 2 dummy arguments removed from SETYPE **&
                         FIELD1, FIELD2, FIELD3, FIELD5, &
                         ENAMES, INAMES, EPNAME, ETYPES, KEY,  &
                         IOUT, INFORM )
      INTEGER        IOUT, INFORM, LENGTH
      INTEGER        NLMAX, NIMAX, NETMAX, NEPMAX
      INTEGER        NOVALS, NELN, NINN, NELTYP, NEPN
      LOGICAL        INREP
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      INTEGER        IELV  ( NLMAX ), IINV  ( NLMAX ), IEPA( NLMAX )
      CHARACTER * 10 ETYPES( NLMAX ), INAMES( NIMAX ), ENAMES( NETMAX )
      CHARACTER * 10 EPNAME( NEPMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS ELEMENT TYPE.
!  -------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFREE, K
      EXTERNAL         HASHB
! ** Correction 8b. 27/02/01: check to see if there are quadratic elements
      CHARACTER * 10   CQSQR, CQPROD
      PARAMETER      ( CQSQR = '123456789S' )
      PARAMETER      ( CQPROD = '123456789P' )

!  CHECK IF THIS IS THE FIRST ELEMENT TYPE.

      IF ( NELTYP .EQ. 0 ) THEN
         CALL HASHB ( LENGTH, 12, FIELD2 // 'ET', KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               RETURN
            END IF
            INFORM = 18
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2180 )
            RETURN
         END IF
         NELTYP = 1
         NELN = 0
         NINN = 0
         NEPN = 0
         INREP = .FALSE.
         INLIST( IFREE ) = NELTYP
         IELV( NELTYP ) = NELN + 1
         IINV( NELTYP ) = NINN + 1
         IEPA( NELTYP ) = NEPN + 1
         ETYPES( NELTYP ) = FIELD2
      END IF

!  CHECK IF THE COLUMN IS NEW.

      IF ( FIELD2 .NE. ETYPES( NELTYP ) ) THEN
! ** Correction 8. 27/02/01: check to see if there are quadratic elements
         IF ( ETYPES( NELTYP ) .NE. CQSQR .AND.&
              ETYPES( NELTYP ) .NE. CQPROD ) THEN

!  IF THE PREVIOUS ELEMENT HAS NO EXPLICIT INTERNAL REPRESENTATION,
!  USE ITS ELEMENTAL REPRESENTATION.

            IF ( .NOT. INREP ) THEN
               DO 10 K = IELV( NELTYP ), NELN
                  NINN = NINN + 1
                  IF ( NINN .GT. NIMAX ) THEN
                     INFORM = - 16
                     RETURN
                  END IF
                  INAMES( NINN ) = ENAMES( K )
   10          CONTINUE
! ** Correction -2a. 07/09/00: Check for non-useful transformations added
            ELSE
              IF ( NINN - IINV( NELTYP ) .GE.&
                   NELN - IELV( NELTYP ) ) THEN
                 INFORM = 76
                 IF ( IOUT .GT. 0 ) WRITE( IOUT, 2760 )
                 RETURN
              END IF
            END IF
         END IF

!  RECORD THE NAME AND STARTING ADDRESS OF THE NEW ELEMENT TYPE.

         CALL HASHB ( LENGTH, 12, FIELD2 // 'ET', KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               RETURN
            END IF
            INFORM = 18
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2180 )
            RETURN
         END IF
         NELTYP = NELTYP + 1
         INREP = .FALSE.
         IF ( NELTYP .GT. NLMAX ) THEN
            INFORM = - 3
            RETURN
         END IF
         INLIST( IFREE ) = NELTYP
         IELV( NELTYP ) = NELN + 1
         IINV( NELTYP ) = NINN + 1
         IEPA( NELTYP ) = NEPN + 1
         ETYPES( NELTYP ) = FIELD2
      END IF

!  INPUT THE NAME OF AN INTERNAL VARIABLE.

      IF ( FIELD1 .EQ. 'IV' ) THEN
         IF ( NOVALS .GT. 0 ) THEN
            INREP = .TRUE.
            DO 30 I = 1, NOVALS

!  CHECK THE NAME HAS NOT ALREADY BEEN USED IN THE CURRENT ELEMENT.

                DO 20 K = IINV( NELTYP ), NINN
                   IF ( ( I .EQ. 1 .AND. FIELD3 .EQ. INAMES( K ) )  &
                        .OR. ( I .EQ. 2 .AND. FIELD5 .EQ. INAMES( K ) )  &
                      ) THEN
                      INFORM = 12
                      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2120 )
                      RETURN
                   END IF
   20           CONTINUE

!  THE NAME IS NEW. RECORD IT IN THE ARRAY INAMES.

               NINN = NINN + 1
               IF ( NINN .GT. NIMAX ) THEN
                  INFORM = - 16
                  RETURN
               END IF
               IF ( I .EQ. 1 ) THEN
                  INAMES( NINN ) = FIELD3
               ELSE
                  INAMES( NINN ) = FIELD5
               END IF
   30       CONTINUE
         END IF
      ELSE

!  INPUT THE NAME OF AN ELEMENTAL VARIABLE.

         IF ( FIELD1 .EQ. 'EV' ) THEN
            IF ( NOVALS .GT. 0 ) THEN
               DO 50 I = 1, NOVALS

!  CHECK THE NAME HAS NOT ALREADY BEEN USED IN THE CURRENT ELEMENT.

                  DO 40 K = IELV( NELTYP ), NELN
                      IF ( ( I .EQ. 1 .AND. FIELD3 .EQ. ENAMES( K ) )  &
                      .OR. ( I .EQ. 2 .AND. FIELD5 .EQ. ENAMES( K ) )  &
                      ) THEN
                        INFORM = 11
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2110 )
                        RETURN
                     END IF
   40             CONTINUE

!  THE NAME IS NEW. RECORD IT IN THE ARRAY ENAMES.

                  NELN = NELN + 1
                  IF ( NELN .GT. NETMAX ) THEN
                     INFORM = - 14
                     RETURN
                  END IF
                  IF ( I .EQ. 1 ) THEN
                     ENAMES( NELN ) = FIELD3
                  ELSE
                     ENAMES( NELN ) = FIELD5
                  END IF
   50          CONTINUE
            END IF
         ELSE

!  INPUT THE NAME OF AN ELEMENT PARAMETER.

            IF ( FIELD1 .EQ. 'EP' ) THEN
               IF ( NOVALS .GT. 0 ) THEN
                  DO 70 I = 1, NOVALS

!  CHECK THE NAME HAS NOT ALREADY BEEN USED IN THE CURRENT ELEMENT.

                     DO 60 K = IEPA( NELTYP ), NEPN
                         IF ( ( I .EQ. 1 .AND. FIELD3 .EQ. EPNAME( K ) )  &
                         .OR. ( I .EQ. 2 .AND. FIELD5 .EQ. EPNAME( K ) )  &
                         ) THEN
                           INFORM = 23
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2230 )
                           RETURN
                        END IF
   60                CONTINUE

!  THE NAME IS NEW. RECORD IT IN THE ARRAY EPNAME.

                     NEPN = NEPN + 1
                     IF ( NEPN .GT. NEPMAX ) THEN
                        INFORM = - 19
                        RETURN
                     END IF
                     IF ( I .EQ. 1 ) THEN
                        EPNAME( NEPN ) = FIELD3
                     ELSE
                        EPNAME( NEPN ) = FIELD5
                     END IF
   70             CONTINUE
               END IF

!  FIELD1 NOT RECOGNISED.

            ELSE
               INFORM = 10
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
               RETURN
            END IF
         END IF
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in ELEMENT TYPE section ' )
 2110 FORMAT( ' ** Exit from GPSMPS - duplicate elemental var. name ' )
 2120 FORMAT( ' ** Exit from GPSMPS - duplicate internal var. name ' )
 2180 FORMAT( ' ** Exit from GPSMPS - duplicate element-type name ' )
 2230 FORMAT( ' ** Exit from GPSMPS - duplicate elemental param. name ')
! ** Correction -2b. 07/09/00: Check for non-useful transformations added
 2760 FORMAT( ' ** Exit from GPSMPS - #internal vars >= #elementals' )
      END
!  THIS VERSION: 28/10/1992 AT 02:36:30 PM.
      SUBROUTINE SEUSES( NLMAX, NELMAX, NETMAX, NEVMAX, NLISEV, NLISEP,  &
                         NOVALS, NEPMAX, NEPVMX, LENGTH, NELNUM, NELMNT,  &
                         NMAX, N, ELMNT, IELV, IEPA, ITYPEE, IELVAR,  &
                         INLIST, ITABLE, ISTAEV, ISTEP, DELSET, DETYPE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         EPVALU, ENAMES, LNAMES, EPNAME, VNAMES,  &
                         KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LENGTH
      INTEGER        NMAX, NLMAX, NELMAX, NETMAX, NEVMAX
      INTEGER        NELNUM, NLISEP, NLISEV
      INTEGER        NOVALS, NEPVMX, NEPMAX, NELMNT, N
      DOUBLE PRECISION VALUE4, VALUE6
      LOGICAL        DELSET
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5, ELMNT, DETYPE
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      INTEGER        IELV( NLMAX ), IEPA( NLMAX )
      INTEGER        IELVAR( NEVMAX ), ITYPEE( NELMAX )
      INTEGER        ISTAEV( NELMAX ), ISTEP( NELMAX )
      DOUBLE PRECISION EPVALU( NEPVMX )
      CHARACTER * 10 ENAMES( NETMAX ), LNAMES( NELMAX )
      CHARACTER * 10 EPNAME( NEPMAX ), VNAMES( NMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS ELEMENT USES.
!  -------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFREE, IFIELD, IP, IS, J, K, MLISEP, MLISEV
      INTEGER          NEPARS, NEVARS
      DOUBLE PRECISION BIG
      CHARACTER * 12 FIELD
      EXTERNAL         HASHB , HASHC
      PARAMETER      ( BIG = 1.0D+20 )

      INTEGER LENFIELD

!  THE CURRENT CARD DEFINES A DEFAULT TYPE.

      IF ( FIELD2 .EQ. '''DEFAULT'' ' ) THEN
         IF ( DELSET ) THEN
            INFORM = 26
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2260 )
         END IF
         DELSET = .TRUE.
         DETYPE = FIELD3
      ELSE

!  IF THE ELEMENT NAMED IN FIELD2 IS NOT THAT OF THE PREVIOUS CARD,
!  DETERMINE THE CHARACTERISTICS OF THE ELEMENT.

         IF ( ELMNT .NE. FIELD2 ) THEN

!  LOOK THE NAME UP IN THE DICTIONARY TO SEE IF IT ALREADY EXISTS.

            CALL HASHB ( LENGTH, 12, FIELD2 // 'EL', KEY,  &
                         ITABLE, IFREE )

!  IF THE ELEMENT NAME IS RECOGNISED, RECOVER ITS CHARACTERISTICS.

            IF ( IFREE .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  RETURN
               END IF
               NELMNT = INLIST( - IFREE )
            ELSE

!  THE NEW ELEMENT IS THE NELNUM-TH NONLINEAR ELEMENT.

               NELNUM = NELNUM + 1
               IF ( NELNUM .GT. NELMAX ) THEN
                  INFORM = - 9
                  RETURN
               END IF

!  RECORD THE ELEMENTS POSITION IN THE TABLE ALONG WITH ITS NAME.

               INLIST( IFREE ) = NELNUM
               LNAMES( NELNUM ) = FIELD2
               NELMNT = NELNUM
            END IF

!  RECORD THE NONLINEAR ELEMENT'S NAME.

            ELMNT = FIELD2

!  IF THE ELEMENT HAS NOT YET BEEN ALLOCATED A TYPE, SET IT.

            IF ( FIELD1 .EQ. 'T ' .OR. FIELD1 .EQ. 'XT'&
                 .OR. IFREE .GT. 0 ) THEN

!  RECORD THE ELEMENT TYPE.

               IF ( FIELD1 .EQ. 'T ' .OR. FIELD1 .EQ. 'XT' ) THEN
                  FIELD = FIELD3 // 'ET'
               ELSE
                  IF ( DELSET ) THEN
                     FIELD = DETYPE // 'ET'
                  ELSE

!  THE ELEMENT NAME IS NEW. CHECK THAT IF A DEFAULT ELEMENT TYPE
!  IS REQUIRED, A DEFAULT HAS BEEN SET.

                     INFORM = 41
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2410 )
                     RETURN
                  END IF
               END IF

!  IF THE GROUP IS NON-TRIVIAL, DETERMINE ITS CHARACTERISTICS.

               LENFIELD = LEN( FIELD )
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .LE. 0 ) THEN
                  INFORM = 9
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2090 ) FIELD3
                  RETURN
               END IF

!  DETERMINE THE NUMBER OF THE ELEMENT TYPE, K, THE STARTING
!  ADDRESSES FOR THE PARAMETERS AND VARIABLES FOR THE ELEMENT
!  AND THE NUMBER OF PARAMETERS AND VARIABLES INVOLVED.

               K = INLIST( IFIELD )
               ITYPEE( NELNUM ) = K
               ISTEP ( NELNUM ) = NLISEP + 1
               ISTAEV( NELNUM ) = NLISEV + 1
               NEPARS = IEPA( K + 1 ) - IEPA( K )
               NEVARS = IELV( K + 1 ) - IELV( K )
               IF ( NLISEV + NEVARS .GT. NEVMAX ) THEN
                  INFORM = - 15
                  RETURN
               END IF
               IF ( NLISEP + NEPARS .GT. NEPVMX ) THEN
                  INFORM = - 17
                  RETURN
               END IF

!  INITIALIZE THE SET OF PROBLEM VARIABLES.

               DO 10 I = 1, NEVARS
                  IELVAR( NLISEV + I ) = 0
   10          CONTINUE

!  INITIALIZE THE SET OF PARAMETER VALUES.

               DO 20 I = 1, NEPARS
                  EPVALU( NLISEP + I ) = BIG
   20          CONTINUE

!  FIND THE STARTING ADDRESSES FOR THE LISTS OF THE ELEMENTS
!  PARAMETERS AND VARIABLES.

               NLISEP = NLISEP + NEPARS
               NLISEV = NLISEV + NEVARS
               IF ( FIELD1 .EQ. 'T ' .OR. FIELD1 .EQ. 'XT' ) RETURN
            END IF
         END IF

!  CHECK THAT THE CARDS ARE IN THE CORRECT ORDER.

         IF ( FIELD1 .EQ. 'T ' .OR. FIELD1 .EQ. 'XT' ) THEN
            INFORM = 27
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2270 )
            RETURN
         END IF

!  DETERMINE THE NUMBER OF THE ELEMENT TYPE, K, THE STARTING
!  ADDRESSES FOR THE PARAMETERS AND VARIABLES FOR THE ELEMENT
!  AND THE NUMBER OF PARAMETERS AND VARIABLES INVOLVED.

         K = ITYPEE( NELMNT )
         NEPARS = IEPA( K + 1 ) - IEPA( K )
         NEVARS = IELV( K + 1 ) - IELV( K )
         MLISEP = ISTEP( NELMNT ) - 1
         MLISEV = ISTAEV( NELMNT ) - 1
         IP = IEPA( K ) - 1
         IS = IELV( K ) - 1

!  THE CARD CONTAINS NAMES OF ELEMENTAL VARIABLES.

         IF ( FIELD1 .EQ. 'V ' .OR. FIELD1 .EQ. 'ZV' ) THEN

!  THE ELEMENTAL VARIABLE IS DEFINED IN FIELD3.

            DO 110 I = 1, NEVARS
               IF ( FIELD3 .EQ. ENAMES( IS + I ) ) GO TO 120
  110       CONTINUE

!  THE ELEMENTAL VARIABLE NAME IS NOT RECOGNISED.

            INFORM = 15
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2150 )
            RETURN
  120       CONTINUE

!  CHECK THAT THE VARIABLE HAS NOT ALREADY BEEN SET.

            IF ( IELVAR( MLISEV + I ) .NE. 0 ) THEN
               INFORM = 30
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2300 )
               RETURN
            END IF

!  SEARCH THE TABLE FOR THE NAME OF THE INPUT VARIABLE.

            CALL HASHB ( LENGTH, 12, FIELD5//'VA', KEY, ITABLE, IFREE )
            IF ( IFREE .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  RETURN
               END IF

!  THE VARIABLE HAS APPEARED BEFORE. STORE ITS NUMBER.

               IELVAR( MLISEV + I ) = INLIST( - IFREE )
            ELSE

!  THE VARIABLE IS COMPLETELY NEW (AND THUS NONLINEAR).
!  IT WILL BE RECORDED AS VARIABLE N.

               N = N + 1
               IF ( N .GT. NMAX ) THEN
                  INFORM = - 7
                  RETURN
               END IF

!  RECORD THE POSITION OF THE NEW GROUP IN THE TABLE, RECORD ITS
!  NAME, INITIALISE ITS TYPE AS TRIVIAL AND RECORD ITS STATUS AS
!  AN EQUALITY GROUP.

               INLIST( IFREE ) = N
               VNAMES( N ) = FIELD5
               IELVAR( MLISEV + I ) = N
            END IF
         ELSE

!  THE CARD CONTAINS NAMES AND VALUES OF ELEMENTAL PARAMETERS.

            IF ( FIELD1 .EQ. 'P ' .OR. FIELD1 .EQ. 'XP' .OR.&
                 FIELD1 .EQ. 'ZP' ) THEN
               IF ( NOVALS .GT. 0 ) THEN
                  DO 230 J = 1, NOVALS

!  CHECK THE NAME HAS NOT ALREADY BEEN USED IN THE CURRENT ELEMENT.
!  THE PARAMETER NAME OCCURS IN FIELD3 OR FIELD5.

                     DO 210 I = 1, NEPARS
                        IF (&
                        ( J .EQ. 1 .AND. FIELD3 .EQ. EPNAME( IP + I ) )  &
                   .OR. ( J .EQ. 2 .AND. FIELD5 .EQ. EPNAME( IP + I ) )  &
                             ) GO TO 220
  210                CONTINUE

!  THE ELEMENTAL PARAMETER NAME IS NOT RECOGNISED.

                     INFORM = 28
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2280 )  &
                        LNAMES( NELNUM ), EPNAME( IP + I )
                     RETURN

!  THE ELEMENTAL PARAMETER NAME IS THE I-TH PARAMETER IN THE LIST.

  220                CONTINUE

!  CHECK THAT THE VALUE HAS NOT ALREADY BEEN SET.

                     IF ( EPVALU( MLISEP + I ) .LT. BIG ) THEN
                        INFORM = 29
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2290 )
                        RETURN
                     END IF

!  READ THE ASSOCIATED VALUE FROM FIELD4 OR FIELD 6.

                     IF ( J .EQ. 1 ) THEN
                        EPVALU( MLISEP + I ) = VALUE4
                     ELSE
                        EPVALU( MLISEP + I ) = VALUE6
                     END IF
  230             CONTINUE
               END IF

!  FIELD1 NOT RECOGNISED.

            ELSE
               INFORM = 10
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
               RETURN
            END IF
         END IF
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2090 FORMAT( ' ** Exit from GPSMPS - element type not recognised:',  &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in ELEMENT USES section ' )
 2150 FORMAT( ' ** Exit from GPSMPS - element variable unrecognised ' )
 2260 FORMAT( ' ** Exit from GPSMPS - duplicate default element type ' )
 2270 FORMAT( ' ** Exit from GPSMPS - type for element already set ' )
 2280 FORMAT( ' ** Exit from GPSMPS - element ', A10, ' parameter ',  &
              A10, ' unrecognised ' )
 2290 FORMAT( ' ** Exit from GPSMPS - element parameter already set ' )
 2300 FORMAT( ' ** Exit from GPSMPS - element variable already set ' )
 2410 FORMAT( ' ** Exit from GPSMPS - element type unrecognised ' )
      END
!  THIS VERSION: 28/10/1992 AT 02:36:30 PM.
      SUBROUTINE SGTYPE( NGRMAX, NGPMAX, NOVALS, LENGTH, NGRTYP, NGPN,  &
                         SETANA, INLIST, IGPA, ITABLE,
! ** Correction 6. 26/02/01: 2 dummy arguments removed from SGTYPE **&
                         FIELD1, FIELD2, FIELD3, FIELD5, &
                         ANAMES, GTYPES, GPNAME, KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, NOVALS, LENGTH
      INTEGER        NGRMAX, NGPMAX
      INTEGER        NGRTYP, NGPN
      LOGICAL        SETANA
! ** Correction 6. 26/02/01: 2 dummy arguments removed from SGTYPE **
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        ITABLE ( LENGTH ), IGPA( NGRMAX )
      INTEGER        INLIST( LENGTH )
      CHARACTER * 10 ANAMES( NGRMAX ), GTYPES( NGRMAX ), GPNAME( NGPMAX)
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS GROUP TYPE.
!  -----------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFREE, K
      EXTERNAL         HASHB

!  CHECK IF THIS IS THE FIRST GROUP TYPE.

      IF ( NGRTYP .EQ. 0 ) THEN
         CALL HASHB ( LENGTH, 12, FIELD2 // 'GT', KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               RETURN
            END IF
            INFORM = 17
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2170 )
            RETURN
         END IF
         NGRTYP = 1
         NGPN = 0
         SETANA = .FALSE.
         INLIST( IFREE ) = NGRTYP
         IGPA( NGRTYP ) = NGPN + 1
         GTYPES( NGRTYP ) = FIELD2
      END IF

!  CHECK IF THE GROUP-TYPE IS NEW.

      IF ( FIELD2 .NE. GTYPES( NGRTYP ) ) THEN

!  CHECK THAT THE ARGUMENT FOR THE PREVIOUS GROUP-TYPE HAS BEEN SET.

         IF ( .NOT. SETANA ) THEN
            INFORM = 25
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2250 )
            RETURN
         END IF

!  RECORD THE NAME AND STARTING ADDRESS OF THE NEW GROUP TYPE.

         CALL HASHB ( LENGTH, 12, FIELD2 // 'GT', KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               RETURN
            END IF
            INFORM = 17
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2170 )
            RETURN
         END IF
         NGRTYP = NGRTYP + 1
         SETANA = .FALSE.
         IF ( NGRTYP .GT. NGRMAX ) THEN
            INFORM = - 4
            RETURN
         END IF
         INLIST( IFREE ) = NGRTYP
         IGPA( NGRTYP ) = NGPN + 1
         GTYPES( NGRTYP ) = FIELD2
      END IF

!  INPUT THE NAME OF THE GROUP-TYPE ARGUMENT.

      IF ( FIELD1 .EQ. 'GV' ) THEN
         SETANA = .TRUE.
         ANAMES( NGRTYP ) = FIELD3
      ELSE

!  INPUT THE NAME OF AN GROUP PARAMETER.

         IF ( FIELD1 .EQ. 'GP' ) THEN
            IF ( NOVALS .GT. 0 ) THEN
               DO 20 I = 1, NOVALS

!  CHECK THE NAME HAS NOT ALREADY BEEN USED IN THE CURRENT GROUP.

                  DO 10 K = IGPA( NGRTYP ), NGPN
                      IF ( ( I .EQ. 1 .AND. FIELD3 .EQ. GPNAME( K ) )  &
                      .OR. ( I .EQ. 2 .AND. FIELD5 .EQ. GPNAME( K ) )  &
                      ) THEN
                        INFORM = 24
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2240 )
                        RETURN
                     END IF
   10             CONTINUE

!  THE NAME IS NEW. RECORD IT IN THE ARRAY GPNAME.

                  NGPN = NGPN + 1
                  IF ( NGPN .GT. NGPMAX ) THEN
                     INFORM = - 20
                     RETURN
                  END IF
                  IF ( I .EQ. 1 ) THEN
                     GPNAME( NGPN ) = FIELD3
                  ELSE
                     GPNAME( NGPN ) = FIELD5
                  END IF
   20          CONTINUE
            END IF

!  FIELD1 NOT RECOGNISED.

         ELSE
            INFORM = 10
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
            RETURN
         END IF
      END IF

!  NON-EXECUTABLE STATEMENTS.

 2170 FORMAT( ' ** Exit from GPSMPS - duplicate group-type name ' )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in GROUP TYPE section ' )
 2240 FORMAT( ' ** Exit from GPSMPS - duplicate group param. name ' )
 2250 FORMAT( ' ** Exit from GPSMPS - no group-type arg. given ' )
      END
!  THIS VERSION: 20/12/1999 AT 18:00:00 PM.
      SUBROUTINE SGUSES( NEGMAX, NGPMAX, NGRMAX, NGMAX, NGPVMX,  &
                         LENGTH, NG, NGRUPE, NLISGP, NOVALS, NELING,  &
                         NDTYPE, STRTGU, GRUPE, IGPA, ITYPEG, IELING,  &
                         INLIST, ITABLE, ISTGP, ISTATE, DGRSET, DGTYPE,  &
                         FIELD1, FIELD2, FIELD3, VALUE4, FIELD5, VALUE6,  &
                         GPTEMP, GNAMES, GPNAME, WEIGHT,  &
                         KEY, IOUT, INFORM )
      INTEGER        IOUT, INFORM, LENGTH
      INTEGER        NEGMAX, NGPMAX, NGRMAX, NGMAX, NGPVMX
      INTEGER        NG, NLISGP, NELING, NOVALS, NGRUPE, NDTYPE
      DOUBLE PRECISION VALUE4, VALUE6
      LOGICAL        DGRSET, STRTGU
      CHARACTER * 2  FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5, GRUPE, DGTYPE
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      INTEGER        IGPA( NGRMAX ), IELING( NEGMAX, 2 )
      INTEGER        ITYPEG( NGMAX )
      INTEGER        ISTGP( NGMAX ), ISTATE( NGMAX )
      DOUBLE PRECISION GPTEMP( NGPVMX ), WEIGHT( NEGMAX )
      CHARACTER * 10 GPNAME( NGPMAX ), GNAMES( NGMAX )
      CHARACTER * 12 KEY( LENGTH )

!  INDICATOR CARD IS GROUP USES.
!  -----------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

      INTEGER          I, IFIELD, IP, J, K, MLISGP, NGPARS
      DOUBLE PRECISION BIG
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      CHARACTER * 12   FIELD
      CHARACTER * 10   CQGROU
      EXTERNAL         HASHB , HASHC
      INTRINSIC        ABS
      PARAMETER      ( BIG = 1.0D+20 )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      PARAMETER      ( CQGROU = '123456789G' )

!  THE CURRENT CARD DEFINES A DEFAULT TYPE.

      IF ( FIELD2 .EQ. '''DEFAULT'' ' ) THEN
         IF ( DGRSET ) THEN
            INFORM = 42
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2420 )
            RETURN
         END IF
         DGRSET = .TRUE.
         DGTYPE = FIELD3

!  FIND THE NUMBER ALLOCATED TO THE GROUP TYPE.

         CALL HASHC ( LENGTH, 12, DGTYPE // 'GT', KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 19
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2190 ) FIELD3
            RETURN
         END IF

!  RESET THE DEFAULTS FOR EACH OF THE GROUPS ALLOCATED IN PREVIOUS
!  SECTIONS.

         NDTYPE = INLIST( IFIELD )
         DO 10 I = 1, NG
            IF ( ITYPEG( I ) .EQ. - 1 ) ITYPEG( I ) = - NDTYPE - 1
   10    CONTINUE
         RETURN
      END IF

!  IF THE GROUP NAMED IN FIELD2 IS NOT THAT OF THE PREVIOUS CARD,
!  DETERMINE THE CHARACTERISTICS OF THE GROUP.

      IF ( .NOT. STRTGU .OR. GRUPE .NE. FIELD2 ) THEN
         STRTGU = .TRUE.

!  LOOK THE NAME UP IN THE DICTIONARY TO SEE IF IT EXISTS.

         CALL HASHC ( LENGTH, 12, FIELD2 // 'GR', KEY,  &
                      ITABLE, IFIELD )
         IF ( IFIELD .GT. 0 ) THEN
            NGRUPE = INLIST( IFIELD )
            ISTATE( NGRUPE ) = - ABS( ISTATE( NGRUPE ) )
         ELSE

!  THE GROUP NAME IS UNKNOWN.

            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2040 ) FIELD2
            INFORM = 4
            RETURN
         END IF

!  RECORD THE GROUP'S NAME.

         GRUPE = FIELD2

!  RECORD THE GROUP TYPE.

         IF ( FIELD1 .EQ. 'T ' .OR. FIELD1 .EQ. 'XT' ) THEN
            FIELD = FIELD3 // 'GT'

!  IF THE GROUP IS NON-TRIVIAL, DETERMINE ITS CHARACTERISTICS.

            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 19
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2190 ) FIELD3
               RETURN
            END IF

!  DETERMINE THE NUMBER OF THE GROUP TYPE, K, THE STARTING
!  ADDRESSES FOR THE PARAMETERS FOR THE GROUP AND THE
!  NUMBER OF PARAMETERS INVOLVED.

            K = INLIST( IFIELD )
! ** Correction 10. 1 line replaced by 5
            IF ( K .EQ. 0 ) THEN
              NGPARS = 0
            ELSE
              NGPARS = IGPA( K + 1 ) - IGPA( K )
            END IF
            ITYPEG( NGRUPE ) = K
            ISTGP ( NGRUPE ) = NLISGP + 1
            IF ( NLISGP + NGPARS .GT. NGPVMX ) THEN
               INFORM = - 18
               RETURN
            END IF

!  INITIALIZE THE SET OF PARAMETER VALUES.

            DO 50 I = 1, NGPARS
               GPTEMP( NLISGP + I ) = BIG
   50       CONTINUE

!  FIND THE STARTING ADDRESSES FOR THE LISTS OF THE GROUP PARAMETERS.

            NLISGP = NLISGP + NGPARS
            RETURN
         ELSE

!  THE GROUP IS NEW AND OF DEFAULT TYPE. DETERMINE THE STARTING
!  ADDRESSES FOR THE PARAMETERS FOR THE GROUP AND THE
!  NUMBER OF PARAMETERS INVOLVED.

            K = NDTYPE
! ** Correction 10. 1 line replaced by 5
            IF ( K .EQ. 0 ) THEN
              NGPARS = 0
            ELSE
              NGPARS = IGPA( K + 1 ) - IGPA( K )
            END IF
            ITYPEG( NGRUPE ) = K
            ISTGP ( NGRUPE ) = NLISGP + 1
            IF ( NLISGP + NGPARS .GT. NGPVMX ) THEN
               INFORM = - 18
               RETURN
            END IF

!  INITIALIZE THE SET OF PARAMETER VALUES.

            DO 55 I = 1, NGPARS
               GPTEMP( NLISGP + I ) = BIG
   55       CONTINUE

!  FIND THE STARTING ADDRESSES FOR THE LISTS OF THE GROUP PARAMETERS.

            NLISGP = NLISGP + NGPARS
         END IF
      END IF

!  CHECK THAT THE CARDS ARE IN THE CORRECT ORDER.

      IF ( FIELD1 .EQ. 'T ' .OR. FIELD1 .EQ. 'XT' ) THEN
         INFORM = 31
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2310 )
         RETURN
      END IF

!  THE CARD CONTAINS NAMES OF NONLINEAR ELEMENTS

      IF ( FIELD1 .EQ. 'E ' .OR. FIELD1 .EQ. 'XE' .OR.&
           FIELD1 .EQ. 'ZE' ) THEN
         IF ( NOVALS .GT. 0 ) THEN

!  CHECK THE NAME HAS NOT ALREADY BEEN USED IN THE CURRENT ELEMENT.
!  THE PARAMETER NAME OCCURS IN FIELD3 OR FIELD5.

            DO 110 I = 1, NOVALS
               IF ( I .EQ. 1 ) THEN
                  FIELD = FIELD3 // 'EL'
               ELSE
                  FIELD = FIELD5 // 'EL'
               END IF
               CALL HASHC( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD)
               IF ( IFIELD .GT. 0 ) THEN

!  THE NELING-TH ELEMENT HAS BEEN ASSIGNED.

                  NELING = NELING + 1
                  IF ( NELING .GT. NEGMAX ) THEN
                     INFORM = - 10
                     RETURN
                  END IF

!  THE ELEMENT IS IELING( ,1) AND THE GROUP IS GIVEN BY IELING( ,2).

                  IELING( NELING, 1 ) = INLIST( IFIELD )
                  IELING( NELING, 2 ) = NGRUPE

!  THE ELEMENT IS WEIGHTED BY THE CONSTANT WEIGHT().

                  IF ( I .EQ. 1 ) THEN
                     WEIGHT( NELING ) = VALUE4
                  ELSE
                     WEIGHT( NELING ) = VALUE6
                  END IF

!  THE ELEMENT NAME IS UNKNOWN.

               ELSE
                  INFORM = 43
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2430 )
                  RETURN
               END IF
  110       CONTINUE
         END IF
      ELSE

!  THE CARD CONTAINS NAMES AND VALUES OF ELEMENTAL PARAMETERS.

         IF ( FIELD1 .EQ. 'P ' .OR. FIELD1 .EQ. 'XP' .OR.&
              FIELD1 .EQ. 'ZP' ) THEN

!  DETERMINE THE NUMBER OF THE GROUP TYPE, K, THE STARTING
!  ADDRESSES FOR THE PARAMETERS FOR THE GROUP AND THE
!  NUMBER OF PARAMETERS INVOLVED.

            IF ( ITYPEG( NGRUPE ) .LT. 0 ) &
              ITYPEG( NGRUPE ) = - ITYPEG( NGRUPE ) - 1
            ITYPEG( NGRUPE ) = ABS( ITYPEG( NGRUPE ) )
            K = ITYPEG( NGRUPE )
            NGPARS = IGPA( K + 1 ) - IGPA( K )
            IP = IGPA( K ) - 1
            MLISGP = ISTGP( NGRUPE ) - 1
            IF ( NOVALS .GT. 0 ) THEN
               DO 230 J = 1, NOVALS

!  CHECK THE NAME HAS NOT ALREADY BEEN USED IN THE CURRENT ELEMENT.
!  THE PARAMETER NAME OCCURS IN FIELD3 OR FIELD5.

                  DO 210 I = 1, NGPARS
                     IF (&
                     ( J .EQ. 1 .AND. FIELD3 .EQ. GPNAME( IP + I ) )  &
                .OR. ( J .EQ. 2 .AND. FIELD5 .EQ. GPNAME( IP + I ) )  &
                          ) GO TO 220
  210             CONTINUE

!  THE GROUP PARAMETER NAME IS NOT RECOGNISED.

                  INFORM = 33
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2330 )
                  RETURN

!  THE GROUP PARAMETER NAME IS THE I-TH PARAMETER IN THE LIST.

  220             CONTINUE

!  CHECK THAT THE VALUE HAS NOT ALREADY BEEN SET.

                  IF ( GPTEMP( MLISGP + I ) .LT. BIG ) THEN
                     INFORM = 32
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2320 )
                     RETURN
                  END IF

!  READ THE ASSOCIATED VALUE FROM FIELD4 OR FIELD 6.

                  IF ( J .EQ. 1 ) THEN
                     GPTEMP( MLISGP + I ) = VALUE4
                  ELSE
                     GPTEMP( MLISGP + I ) = VALUE6
                  END IF
  230          CONTINUE
            END IF

!  FIELD1 NOT RECOGNISED.

         ELSE
            INFORM = 10
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2100 ) FIELD1
            RETURN
         END IF
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',  &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,  &
              '  not recognised in GROUP USES section' )
 2190 FORMAT( ' ** Exit from GPSMPS - group type not recognised:',  &
              ' name is ', A10 )
 2310 FORMAT( ' ** Exit from GPSMPS - type for group already set ' )
 2320 FORMAT( ' ** Exit from GPSMPS - group parameter already set ' )
 2330 FORMAT( ' ** Exit from GPSMPS - group parameter unrecognised ' )
 2420 FORMAT( ' ** Exit from GPSMPS - default group type already set ' )
 2430 FORMAT( ' ** Exit from GPSMPS - element name not recognised ' )
      END
!  THIS VERSION: 28/10/1992 AT 02:36:30 PM.
      SUBROUTINE SOBBND( NOBBND, NOBMAX, NRLNDX, LENGTH,  &
                         INLIST, ITABLE, FBOUND, REALVL,
! ** Correction 7. 26/02/01: 2 dummy arguments removed from SOBBND **&
                         FIELD1, FIELD2, VALUE4, FIELD5,  &
                         OBNAME, KEY   , SINGLE, IOUT  , INFORM )
      INTEGER          IOUT, INFORM, LENGTH, NOBBND, NOBMAX, NRLNDX
      DOUBLE PRECISION VALUE4
      LOGICAL          SINGLE
      CHARACTER * 2    FIELD1
      CHARACTER * 10   FIELD2, FIELD5
      INTEGER          INLIST( LENGTH ), ITABLE ( LENGTH )
      DOUBLE PRECISION FBOUND( 2, NOBMAX ), REALVL( NRLNDX )
      CHARACTER * 10   OBNAME( NOBMAX )
      CHARACTER * 12   KEY( LENGTH )

!  INDICATOR CARD IS OBJECT BOUND.
!  -------------------------------

!  NICK GOULD 12/01/1990
!  FOR CGT PRODUCTIONS.

      INTEGER          IFREE, IFIELD, J
      REAL             SMACHR
      DOUBLE PRECISION DMACHR, BIG
      EXTERNAL         HASHB , HASHC, DMACHR
      INTRINSIC        DBLE
      IF ( SINGLE ) THEN
         BIG = 9.0D-1 * DBLE( SMACHR( 5 ) )
      ELSE
         BIG = 9.0D-1 * DMACHR( 5 )
      END IF

!  FIND A PLACE TO INSERT THE OBJECTIVE BOUND NAME IN THE HASH-TABLE.

      CALL HASHB ( LENGTH, 12, FIELD2 // 'OB', KEY, ITABLE, IFREE )

!  THE NAME ALREADY EXISTS. IT IS THE J-TH NAME IN THE LIST.

      IF ( IFREE .LE. 0 ) THEN
         IF ( IFREE .EQ. 0 ) THEN
            INFORM = - 1
            RETURN
         END IF
         J = INLIST( - IFREE )
      ELSE

!  THE OBJECTIVE FUNCTION BOUND IS THE NOBBND-TH SPECIFIED.

         NOBBND = NOBBND + 1
         IF( NOBBND .GT. NOBMAX ) THEN
            INFORM = - 23
            RETURN
         END IF
         J = NOBBND

!  RECORD THE DEFAULT BOUNDS.

         FBOUND( 1, NOBBND ) = - BIG
         FBOUND( 2, NOBBND ) = BIG

!  RECORD THE POSITION OF THE NEW BOUND IN THE TABLE AND RECORD ITS

         INLIST( IFREE ) = NOBBND
         OBNAME( NOBBND ) = FIELD2
      END IF

!  RECORD THE BOUND GIVEN.

      IF ( FIELD1( 1: 1 ) .EQ. 'Z' ) THEN
         CALL HASHC ( LENGTH, 12,  FIELD5( 1 : 10 ) // 'RI',  &
                      KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 3
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD5( 1 : 10 )
            RETURN
         END IF
         VALUE4 = REALVL( INLIST( IFIELD ) )
      END IF
      IF ( FIELD1 .EQ. 'XL' .OR. FIELD1 .EQ. 'ZL' .OR.&
           FIELD1 .EQ. 'LO' ) FBOUND( 1, J ) = VALUE4
      IF ( FIELD1 .EQ. 'XU' .OR. FIELD1 .EQ. 'ZU' .OR.&
           FIELD1 .EQ. 'UP' ) FBOUND( 1, J ) = VALUE4
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
      END

!  THIS VERSION: 26/02/2001 AT 09:30:00 AM.
!     ( Last modified on 15 Mar 2001 at 22:28:00 )
! ** Correction report.
! ** Correction -4. 20/04/12: QMATRIX added as alias for QUADOBJ
! ** Correction -3. 21/02/00: QSECTION added as alias for QUADOBJ
! ** Correction -2. 21/02/00: Code to process ZERO-ONE and INTEGER cards added.
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
! ** Correction 1. 26/02/01: 1 dummy argument removed from PROCAA **
! ** End of Correction report.
      SUBROUTINE PROCAI( NINDEX, NRLNDX, LENGTH, NUSEIN, NUSERE,  &
                         INFORM, IOUT, LEVEL, NINSTR,  &
                         DEBUG, RVALUE, INLIST, ITABLE,  &
                         NAMIIN, NAMRIN, INSTR, KEY,  &
                         FIELD1, FIELD2, FIELD3, FIELD5, FIELD4 )
      INTEGER       NINDEX, NRLNDX, LENGTH, NUSEIN, NUSERE
      INTEGER       INFORM, IOUT, LEVEL, NINSTR
      LOGICAL       DEBUG
      DOUBLE PRECISION RVALUE
      CHARACTER *  2 FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      CHARACTER * 12 FIELD4
      INTEGER       INSTR( 5 ), INLIST( LENGTH ), ITABLE ( LENGTH )
      CHARACTER * 10 NAMIIN( NINDEX ), NAMRIN( NRLNDX )
      CHARACTER * 12 KEY( LENGTH )

!  CONSTRUCT A LIST OF DO-LOOP INTEGER AND REAL
!  ARITHMETIC INSTRUCTIONS.

!  NICK GOULD, 8/09/89
!  FOR CGT PRODUCTIONS.

      INTEGER        NFUNCT, I, IFIELD, IFREE
      CHARACTER * 12 FIELD
      EXTERNAL       HASHB , HASHC, GETINT, GETVAL
      PARAMETER      ( NFUNCT = 14 )
      CHARACTER * 6  FUNCTN( NFUNCT )
      DATA  FUNCTN / 'ABS   ', 'SQRT  ', 'EXP   ', 'LOG   ', 'LOG10 ',  &
                     'SIN   ', 'COS   ', 'TAN   ', 'ARCSIN', 'ARCCOS',  &
                     'ARCTAN', 'HYPSIN', 'HYPCOS', 'HYPTAN' /

!  DECIDE WHAT SORT OF INSTRUCTION IS TO BE PERFORMED.
!  INTEGER INSTRUCTIONS.

      IF ( FIELD1 .EQ. 'IE' .OR. FIELD1 .EQ. 'IA' .OR.&
           FIELD1 .EQ. 'IS' .OR. FIELD1 .EQ. 'IM' .OR.&
           FIELD1 .EQ. 'ID' .OR. FIELD1 .EQ. 'IR' .OR.&
           FIELD1 .EQ. 'I=' .OR. FIELD1 .EQ. 'I+' .OR.&
           FIELD1 .EQ. 'I-' .OR. FIELD1 .EQ. 'I*' .OR.&
           FIELD1 .EQ. 'I/' ) THEN
         IF ( FIELD1 .EQ. 'IE' ) INSTR( 1 ) = 21
         IF ( FIELD1 .EQ. 'IA' ) INSTR( 1 ) = 22
         IF ( FIELD1 .EQ. 'IS' ) INSTR( 1 ) = 23
         IF ( FIELD1 .EQ. 'IM' ) INSTR( 1 ) = 24
         IF ( FIELD1 .EQ. 'ID' ) INSTR( 1 ) = 25
         IF ( FIELD1 .EQ. 'IR' ) INSTR( 1 ) = 26
         IF ( FIELD1 .EQ. 'I=' ) INSTR( 1 ) = 31
         IF ( FIELD1 .EQ. 'I+' ) INSTR( 1 ) = 32
         IF ( FIELD1 .EQ. 'I-' ) INSTR( 1 ) = 33
         IF ( FIELD1 .EQ. 'I*' ) INSTR( 1 ) = 34
         IF ( FIELD1 .EQ. 'I/' ) INSTR( 1 ) = 35
         IF ( FIELD1 .EQ. 'IE' .OR. FIELD1 .EQ. 'IA' .OR.&
              FIELD1 .EQ. 'IS' .OR. FIELD1 .EQ. 'IM' .OR.&
              FIELD1 .EQ. 'ID' ) THEN

!  READ THE INTEGER VALUE, IVALUE, FROM FIELD 4.

            CALL GETINT( FIELD4, INSTR( 4 ) )
         ELSE

!  OBTAIN THE INTEGER VALUE, IVALUE, AS THE VALUE OF THE INDEX IN
!  FIELD 5, FIRST ENSURING THAT THE INDEX EXISTS.

            IF ( FIELD1 .NE. 'I=' .AND. FIELD1 .NE. 'IR' ) THEN
               FIELD = FIELD5( 1 : 10 ) // 'II'
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .LE. 0 ) THEN
                  INFORM = 3
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 4 ) = INLIST( IFIELD )
            END IF
         END IF

!  IF A DEFINITION IS TO BE MADE FROM A PREVIOUSLY DEFINED INDEX,
!  ENSURE THAT THE INDEX EXISTS.

         IF ( FIELD1 .NE. 'IE' ) THEN
            IF ( FIELD1 .NE. 'IR' ) THEN
               FIELD = FIELD3( 1 : 10 ) // 'II'
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .LE. 0 ) THEN
                  INFORM = 3
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 3 ) = INLIST( IFIELD )
            ELSE
               FIELD = FIELD3( 1 : 10 ) // 'RI'
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .LE. 0 ) THEN
                  INFORM = 3
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 3 ) = INLIST( IFIELD )
            END IF
         END IF

!  RECORD THE ADDRESS OF THE INDEX WHICH IS TO BE SET.

         FIELD = FIELD2( 1 : 10 ) // 'II'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               RETURN
            END IF
            IFREE = - IFREE
         ELSE
            NUSEIN = NUSEIN + 1
            IF ( NUSEIN .GT. NINDEX ) THEN
               INFORM = - 21
               RETURN
            END IF
            INLIST( IFREE ) = NUSEIN
            NAMIIN( NUSEIN ) = FIELD( 1 : 10 )
         END IF
         INSTR( 2 ) = INLIST( IFREE )

!  PRINT DETAILS OF THE INSTRUCTION.

         IF ( DEBUG .AND. IOUT .GT. 0 ) THEN
            IF ( INSTR( 1 ) .EQ. 21 )  &
               WRITE( IOUT, 4030 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) .EQ. 22 )  &
               WRITE( IOUT, 4040 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) .EQ. 23 )  &
               WRITE( IOUT, 4041 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) .EQ. 24 )  &
               WRITE( IOUT, 4050 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) .EQ. 25 )  &
               WRITE( IOUT, 4051 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), INSTR( 4 ), NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 26 )  &
               WRITE( IOUT, 4055 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 31 )  &
               WRITE( IOUT, 4059 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 32 )  &
               WRITE( IOUT, 4060 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ),  &
               NAMIIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) .EQ. 33 )  &
               WRITE( IOUT, 4061 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 4 ) ),  &
               NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 34 )  &
               WRITE( IOUT, 4070 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ),  &
               NAMIIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) .EQ. 35 )  &
               WRITE( IOUT, 4071 ) LEVEL, NINSTR,  &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ),  &
               NAMIIN( INSTR( 4 ) )
         END IF
      ELSE

!  REAL INSTRUCTIONS.

         IF ( FIELD1 .EQ. 'RE' ) INSTR( 1 ) = 51
         IF ( FIELD1 .EQ. 'RA' ) INSTR( 1 ) = 52
         IF ( FIELD1 .EQ. 'RS' ) INSTR( 1 ) = 53
         IF ( FIELD1 .EQ. 'RM' ) INSTR( 1 ) = 54
         IF ( FIELD1 .EQ. 'RD' ) INSTR( 1 ) = 55
         IF ( FIELD1 .EQ. 'RI' ) INSTR( 1 ) = 56
         IF ( FIELD1 .EQ. 'RF' ) INSTR( 1 ) = 57
         IF ( FIELD1 .EQ. 'R=' ) INSTR( 1 ) = 61
         IF ( FIELD1 .EQ. 'R+' ) INSTR( 1 ) = 62
         IF ( FIELD1 .EQ. 'R-' ) INSTR( 1 ) = 63
         IF ( FIELD1 .EQ. 'R*' ) INSTR( 1 ) = 64
         IF ( FIELD1 .EQ. 'R/' ) INSTR( 1 ) = 65
         IF ( FIELD1 .EQ. 'R(' ) INSTR( 1 ) = 67
         IF ( FIELD1 .EQ. 'RE' .OR. FIELD1 .EQ. 'RA' .OR.&
              FIELD1 .EQ. 'RS' .OR. FIELD1 .EQ. 'RM' .OR.&
              FIELD1 .EQ. 'RD' .OR. FIELD1 .EQ. 'RF' ) THEN

!  READ THE REAL VALUE, RVALUE, FROM FIELD 4.

            CALL GETVAL( FIELD4, RVALUE )
         END IF
         IF ( FIELD1 .EQ. 'R+' .OR. FIELD1 .EQ. 'R-' .OR.&
              FIELD1 .EQ. 'R*' .OR. FIELD1 .EQ. 'R/' .OR.&
              FIELD1 .EQ. 'R(' ) THEN

!  OBTAIN THE REAL VALUE, RVALUE, AS THE VALUE ASSOCIATED WITH THE REAL
!  INDEX IN FIELD 5, FIRST ENSURING THAT THE INDEX EXISTS.

            FIELD = FIELD5( 1 : 10 ) // 'RI'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            INSTR( 4 ) = INLIST( IFIELD )
         END IF

!  IF A DEFINITION IS TO BE MADE FROM A PREVIOUSLY DEFINED INDEX,
!  ENSURE THAT THE INDEX EXISTS.

         IF ( FIELD1 .EQ. 'RI' ) THEN
            FIELD = FIELD3( 1 : 10 ) // 'II'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            INSTR( 3 ) = INLIST( IFIELD )
         ELSE
            IF ( FIELD1 .NE. 'RF' .AND. FIELD1 .NE. 'R(' .AND.&
                 FIELD1 .NE. 'RE' ) THEN
               FIELD = FIELD3( 1 : 10 ) // 'RI'
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .LE. 0 ) THEN
                  INFORM = 3
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 3 ) = INLIST( IFIELD )
            ELSE

!  THE VALUE IS TO BE OBTAINED USING A SPECIAL FUNCTION. DETERMINE
!  WHICH ONE.

               IF ( FIELD1 .NE. 'RE' ) THEN
                  DO 10 I = 1, NFUNCT
                     IF ( FIELD3( 1 : 10 ) .EQ. FUNCTN( I ) ) GO TO 20
   10             CONTINUE
                  INFORM = 39
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2390 ) FIELD3( 1 : 10)
                  RETURN
   20             CONTINUE
                  INSTR( 3 ) = I
               END IF
            END IF
         END IF

!  RECORD THE ADDRESS OF THE INDEX WHICH IS TO BE SET.

         FIELD = FIELD2( 1 : 10 ) // 'RI'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               RETURN
            END IF
            IFREE = - IFREE
         ELSE
            NUSERE = NUSERE + 1
            IF ( NUSERE .GT. NRLNDX ) THEN
               INFORM = - 22
               RETURN
            END IF
            INLIST( IFREE ) = NUSERE
            NAMRIN( NUSERE ) = FIELD( 1 : 10 )
         END IF
         INSTR( 2 ) = INLIST( IFREE )

!  PRINT DETAILS OF THE INSTRUCTION.

         IF ( DEBUG .AND. IOUT .GT. 0 ) THEN
            IF ( INSTR( 1 ) .EQ. 51 )  &
               WRITE( IOUT, 4130 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), RVALUE
            IF ( INSTR( 1 ) .EQ. 52 )  &
               WRITE( IOUT, 4140 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ), RVALUE
            IF ( INSTR( 1 ) .EQ. 53 )  &
               WRITE( IOUT, 4141 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ), RVALUE
            IF ( INSTR( 1 ) .EQ. 54 )  &
               WRITE( IOUT, 4150 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ), RVALUE
            IF ( INSTR( 1 ) .EQ. 55 )  &
               WRITE( IOUT, 4151 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), RVALUE, NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 56 )  &
               WRITE( IOUT, 4180 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 57 )  &
               WRITE( IOUT, 4110 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), FUNCTN( INSTR( 3 ) ), RVALUE
            IF ( INSTR( 1 ) .EQ. 61 )  &
               WRITE( IOUT, 4159 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 62 )  &
               WRITE( IOUT, 4160 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ),  &
               NAMRIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) .EQ. 63 )  &
               WRITE( IOUT, 4161 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 4 ) ),  &
               NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) .EQ. 64 )  &
               WRITE( IOUT, 4170 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ),  &
               NAMRIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) .EQ. 65 )  &
               WRITE( IOUT, 4171 ) LEVEL, NINSTR,  &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ),  &
               NAMRIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) .EQ. 67 )  &
               WRITE( IOUT, 4120 ) LEVEL, NINSTR, NAMRIN( INSTR( 2 ) ),  &
               FUNCTN( INSTR( 3 ) ), NAMRIN( INSTR( 4 ) )
         END IF
      END IF
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2390 FORMAT( ' ** Exit from GPSMPS - specified function name ', A10,  &
              ' not recognised ' )
 4030 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to the value ', I6 )
 4040 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by adding ', A10, ' to the value ', I6 )
 4041 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by subtracting ', A10, ' from the value ', I6 )
 4050 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by multiplying ', A10, ' by the value ', I6 )
 4051 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by dividing the value ', I6, ' by ', A10 )
 4055 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to the integer equivalent of ', A10 )
 4059 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to ', A10 )
 4060 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by adding ', A10, ' to ', A10 )
 4061 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by subtracting ', A10, ' from ', A10 )
 4070 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by multiplying ', A10, ' and ', A10 )
 4071 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by dividing ', A10, ' by ', A10 )
 4110 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to the value ', A6, '(', 1P, D12.4, ')' )
 4120 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to the value ', A6, '(', A10, ')' )
 4130 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to the value ', 1P, D12.4 )
 4140 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by adding ', A10, ' to the value ', 1P, D12.4 )
 4141 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by subtracting ', A10, ' from the value ', 1P, D12.4 )
 4150 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by multiplying ', A10, ' by the value ', 1P, D12.4 )
 4151 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by dividing the value ', 1P, D12.4, ' by ', A10 )
 4159 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to ', A10 )
 4160 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by adding ', A10, ' to ', A10 )
 4161 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by subtracting ', A10, ' from ', A10 )
 4170 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by multiplying ', A10, ' and ', A10 )
 4171 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' by dividing ', A10, ' by ', A10 )
 4180 FORMAT( ' Level ', I2, ' instruction ', I4, ' set ', A10,  &
              ' to the fl. pt. value of ', A10 )

!  END OF PROCAI.

      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE PROCAD( NINDEX, NRLNDX, LEVEL, NINSTR, NUSERE,  &
                         LENGTH, NARRAY, INTYPE, INFORM, IOUT,  &
                         DEBUG, GRP1ST,  &
                         FIELD1, FIELD2, FIELD3, FIELD5,  &
                         FIELD4, FIELD6,  &
                         INLIST, INSTR, ITABLE, IARRAY,  &
                         VARRAY, FARRAY, NAMIIN, NAMRIN,  &
                         ARRAY, CARRAY, KEY )
      INTEGER        NINDEX, NRLNDX, LEVEL, LENGTH, INFORM, IOUT
      INTEGER        NINSTR, NUSERE, INTYPE, NARRAY
      LOGICAL        DEBUG, GRP1ST
      CHARACTER *  2 FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      CHARACTER * 12 FIELD4, FIELD6
      INTEGER        INSTR( 5 ), IARRAY( 5, 3 )
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      DOUBLE PRECISION VARRAY( 2 )
      CHARACTER *  2 FARRAY
      CHARACTER * 10 NAMIIN( NINDEX ), NAMRIN( NRLNDX )
      CHARACTER * 10 ARRAY( 3 ), CARRAY( 2 )
      CHARACTER * 12 KEY( LENGTH )

!  CONSTRUCT A LIST OF DO-LOOP ARRAY DEFINITIONS.

!  NICK GOULD, 8/09/89
!  FOR CGT PRODUCTIONS.

      INTEGER        I, KINDAR, IFREE, MFREE, MFIXED
      INTEGER        MBLANK, MNAME, MROWS, MGROUP, MCNSTR, MCOLS, MVARS
      INTEGER        MCONST, MRHS, MRHSP, MRANGE, MBOUND, MSTART
      INTEGER        METYPE, MGTYPE, MEUSES, MGUSES, MOBBND, MENDAT
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
! ** Correction -3. 21/02/00: QSECTION added as alias for QUADOBJ
! ** Correction -4. 20/04/12: QMATRIX added as alias for QUADOBJ
      INTEGER        MQHESS, MQUADO, MQUADR, MQUADS, MQSECT, MQMATR
      EXTERNAL       INTFIE, GETVAL, HASHB

!  PARAMETER DEFINITIONS.

      PARAMETER        ( MBLANK =  1, MFIXED =  2, MFREE = 3  )
      PARAMETER        ( MNAME =  4, MROWS =  5 )
      PARAMETER        ( MGROUP =  6, MCNSTR =  7, MCOLS =  8 )
      PARAMETER        ( MVARS =  9, MCONST = 10, MRHS = 11 )
      PARAMETER        ( MRHSP = 12, MRANGE = 13, MBOUND = 14 )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      PARAMETER        ( MSTART = 15, MQHESS = 16, MQUADR = 17 )
! ** Correction -3. 21/02/00: QSECTION added as alias for QUADOBJ
      PARAMETER        ( MQUADS = 18, MQUADO = 19, MQSECT = 20 )
! ** Correction -4. 20/04/12: QMATRIX added as alias for QUADOBJ
      PARAMETER        ( MQMATR = 21 )
      PARAMETER        ( METYPE = 22, MEUSES = 23, MGTYPE = 24 )
      PARAMETER        ( MGUSES = 25, MOBBND = 26, MENDAT = 27 )

!  DETERMINE HOW MUCH INFORMATION MUST BE SAVED BY DETERMINING
!  THE KIND OF ARRAY DEFINITION BEING MADE.

      KINDAR = - 1

!  REAL INDEX ARRAY DEFINITIONS.

      IF ( FIELD1 .EQ. 'AE' .OR. FIELD1 .EQ. 'AA' .OR.&
           FIELD1 .EQ. 'AS' .OR. FIELD1 .EQ. 'AM' .OR.&
           FIELD1 .EQ. 'AD' .OR. FIELD1 .EQ. 'AI' .OR.&
           FIELD1 .EQ. 'A=' .OR. FIELD1 .EQ. 'A+' .OR.&
           FIELD1 .EQ. 'A-' .OR. FIELD1 .EQ. 'A*' .OR.&
           FIELD1 .EQ. 'A/' .OR. FIELD1 .EQ. 'AF' .OR.&
           FIELD1 .EQ. 'A(' ) THEN
         IF ( FIELD1 .EQ. 'AE' ) KINDAR = 107
         IF ( FIELD1 .EQ. 'AF' ) KINDAR = 108
         IF ( FIELD1 .EQ. 'A(' ) KINDAR = 105
         IF ( FIELD1 .EQ. 'AA' ) KINDAR = 101
         IF ( FIELD1 .EQ. 'AS' ) KINDAR = 101
         IF ( FIELD1 .EQ. 'AM' ) KINDAR = 101
         IF ( FIELD1 .EQ. 'AD' ) KINDAR = 101
         IF ( FIELD1 .EQ. 'A=' ) KINDAR = 103
         IF ( FIELD1 .EQ. 'A+' ) KINDAR = 104
         IF ( FIELD1 .EQ. 'A-' ) KINDAR = 104
         IF ( FIELD1 .EQ. 'A*' ) KINDAR = 104
         IF ( FIELD1 .EQ. 'A/' ) KINDAR = 104
         IF ( FIELD1 .EQ. 'AI' ) KINDAR = 106
      ELSE

!  GROUPS SECTION.

         IF ( INTYPE .EQ. MROWS  .OR. INTYPE .EQ. MGROUP .OR.&
              INTYPE .EQ. MCNSTR ) THEN
            IF ( FIELD1( 2: 2 ) .EQ. 'N' .OR.&
                 FIELD1( 2: 2 ) .EQ. 'G' .OR.&
                 FIELD1( 2: 2 ) .EQ. 'L' .OR.&
                 FIELD1( 2: 2 ) .EQ. 'E' ) THEN
               IF ( GRP1ST ) THEN
                  IF ( FIELD3( 1: 7 ) .EQ. '''SCALE''' ) THEN
                     IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                        KINDAR = 115
                     ELSE
                        KINDAR = 108
                     END IF
                  ELSE
                     KINDAR = 100
                  END IF
               ELSE
                  IF ( FIELD3( 1: 7 ) .EQ. '''SCALE''' ) THEN
                     IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                        KINDAR = 115
                     ELSE
                        KINDAR = 108
                     END IF
                  ELSE
                     IF ( FIELD3( 1: 10 ) .EQ. '          ' ) THEN
                        KINDAR = 100
                     ELSE
                        IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                           KINDAR = 113
                        ELSE
                           IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                              KINDAR = 101
                           ELSE
                              KINDAR = 102
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF

!  VARIABLES SECTION.

         IF ( INTYPE .EQ. MCOLS  .OR. INTYPE .EQ. MVARS ) THEN
            IF ( GRP1ST ) THEN
               IF ( FIELD3( 1: 7 ) .EQ. '''SCALE''' ) THEN
                  IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                     KINDAR = 115
                  ELSE
                     KINDAR = 108
                  END IF
! ** Correction -2a. 21/02/00: Code to process ZERO-ONE and INTEGER cards added.
               ELSE IF ( FIELD3 .EQ. '''ZERO-ONE''' .OR. &
                         FIELD3 .EQ. '''INTEGER'' ' ) THEN
                  KINDAR = 106
               ELSE
                  IF ( FIELD3( 1: 10 ) .EQ. '          ' ) THEN
                     KINDAR = 100
                  ELSE
                     IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                        KINDAR = 113
                     ELSE
                        IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                           KINDAR = 101
                        ELSE
                           KINDAR = 102
                        END IF
                     END IF
                  END IF
               END IF
            ELSE
               IF ( FIELD3( 1: 7 ) .EQ. '''SCALE''' ) THEN
                  IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                     KINDAR = 115
                  ELSE
                     KINDAR = 108
                  END IF
! ** Correction -2b. 21/02/00: Code to process ZERO-ONE and INTEGER cards added.
               ELSE IF ( FIELD3 .EQ. '''ZERO-ONE''' .OR. &
                         FIELD3 .EQ. '''INTEGER'' ' ) THEN
                  KINDAR = 106
               ELSE
                  KINDAR = 100
               END IF
            END IF
         END IF

!  CONSTANTS SECTION.

         IF ( INTYPE .EQ. MCONST .OR. INTYPE .EQ. MRHS  .OR.&
              INTYPE .EQ. MRHSP  ) THEN
            IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
               KINDAR = 116
            ELSE
               IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                  KINDAR = 111
               ELSE
                  KINDAR = 112
               END IF
            END IF
         END IF

!  RANGES SECTION.

         IF ( INTYPE .EQ. MRANGE ) THEN
            IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
               KINDAR = 116
            ELSE
               IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                  KINDAR = 111
               ELSE
                  KINDAR = 112
               END IF
            END IF
         END IF

!  BOUNDS SECTION.

         IF ( INTYPE .EQ. MBOUND ) THEN
            IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
               KINDAR = 116
            ELSE
               IF ( FIELD1( 2: 2 ) .EQ. 'R' .OR.&
                    FIELD1( 2: 2 ) .EQ. 'M' .OR.&
                    FIELD1( 2: 2 ) .EQ. 'P' ) KINDAR = 110
               IF ( FIELD1( 2: 2 ) .EQ. 'L' .OR.&
                    FIELD1( 2: 2 ) .EQ. 'U' .OR.&
                    FIELD1( 2: 2 ) .EQ. 'X' ) KINDAR = 111
            END IF
         END IF

!  START POINT SECTION.

         IF ( INTYPE .EQ. MSTART ) THEN
            IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
               KINDAR = 116
            ELSE
               IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                  KINDAR = 111
               ELSE
                  KINDAR = 112
               END IF
            END IF
         END IF
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.

!  HESSIAN SECTION.

! ** Correction -3. 21/02/00: QSECTION added as alias for QUADOBJ
! ** Correction -4. 20/04/12: QMATRIX added as alias for QUADOBJ
         IF ( INTYPE .EQ. MQUADR .OR. INTYPE .EQ. MQUADS .OR. &
              INTYPE .EQ. MQUADO .OR. INTYPE .EQ. MQSECT .OR.&
              INTYPE .EQ. MQHESS .OR. INTYPE .EQ. MQMATR) THEN
            IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
               KINDAR = 113
            ELSE
               IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                  KINDAR = 101
               ELSE
                  KINDAR = 102
               END IF
            END IF
         END IF

!  ELEMENT USES SECTION.

         IF ( INTYPE .EQ. MEUSES ) THEN
            IF ( FIELD1( 2: 2 ) .EQ. 'T' ) KINDAR = 106
            IF ( FIELD1( 2: 2 ) .EQ. 'V' ) KINDAR = 105
            IF ( FIELD1( 2: 2 ) .EQ. 'P' ) THEN
               IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                  KINDAR = 115
               ELSE
                  IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                     KINDAR = 108
                  ELSE
                     KINDAR = 109
                  END IF
               END IF
            END IF
         END IF

!  GROUP USES SECTION.

         IF ( INTYPE .EQ. MGUSES ) THEN
            IF ( FIELD1( 2: 2 ) .EQ. 'T' ) KINDAR = 106
            IF ( FIELD1( 2: 2 ) .EQ. 'E' ) THEN
               IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                  KINDAR = 113
               ELSE
                  IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                     KINDAR = 101
                  ELSE
                     KINDAR = 102
                     IF ( FIELD6( 1: 12 ) .EQ. '            ' )  &
                          FIELD6( 1: 3 ) = '1.0'
                  END IF
                  IF ( FIELD4( 1: 12 ) .EQ. '            ' )  &
                       FIELD4( 1: 3 ) = '1.0'
               END IF
            END IF
            IF ( FIELD1( 2: 2 ) .EQ. 'P' ) THEN
               IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
                  KINDAR = 115
               ELSE
                  IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                     KINDAR = 108
                  ELSE
                     KINDAR = 109
                  END IF
               END IF
            END IF
         END IF

!  RANGES SECTION.

         IF ( INTYPE .EQ. MOBBND ) THEN
            IF ( FIELD1( 1: 1 )  .EQ. 'Z' ) THEN
               KINDAR = 116
            ELSE
               IF ( FIELD5( 1: 10 ) .EQ. '          ' ) THEN
                  KINDAR = 111
               ELSE
                  KINDAR = 112
               END IF
            END IF
         END IF
      END IF

!  CHECK THAT THE TYPE OF ARRAY DEFINITION HAS BEEN RECOGNISED.

      IF ( KINDAR .LT. 0 ) THEN
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2140 )
         INFORM = 14
         RETURN
      ELSE
         FARRAY = FIELD1
         INSTR( 1 ) = KINDAR
         INSTR( 2 ) = NARRAY
      END IF

!  AN ARRAY NAME OCCURS IN FIELD 2. INTERPRET THE CONTENTS OF THIS
!  FIELD.

      IF ( ( KINDAR .GE. 100 .AND. KINDAR .LE. 109 ) .OR.&
           ( KINDAR .GE. 113 .AND. KINDAR .LE. 115 ) ) THEN
         CALL INTFIE( LENGTH, 12, KEY, ITABLE, INLIST,  &
                      FIELD2, ARRAY( 1 ),  &
                      IARRAY( 1, 1 ), IOUT, INFORM )
         IF ( INFORM .NE. 0 ) RETURN
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4080 ) LEVEL,  &
                          NINSTR, ARRAY( 1 ),  &
                         ( NAMIIN( IARRAY( 2 + I, 1 ) ),  &
                            I = 1, IARRAY( 2, 1 ) )

!  IF THE ARRAY NAME IS JUST A SCALAR NAME, RECORD ITS NAME.

         IF ( FIELD1 .EQ. 'AE' .OR. FIELD1 .EQ. 'AA' .OR.&
              FIELD1 .EQ. 'AS' .OR. FIELD1 .EQ. 'AM' .OR.&
              FIELD1 .EQ. 'AD' .OR. FIELD1 .EQ. 'AI' .OR.&
              FIELD1 .EQ. 'A=' .OR. FIELD1 .EQ. 'A+' .OR.&
              FIELD1 .EQ. 'A-' .OR. FIELD1 .EQ. 'A*' .OR.&
              FIELD1 .EQ. 'A/' .OR. FIELD1 .EQ. 'AF' .OR.&
              FIELD1 .EQ. 'A(' ) THEN
            IF ( IARRAY( 2, 1 ) .EQ. 0 ) THEN
               CALL HASHB ( LENGTH, 12, ARRAY( 1 ) // 'RI',  &
                            KEY, ITABLE, IFREE )
               IF ( IFREE .LE. 0 ) THEN
                  IF ( IFREE .EQ. 0 ) THEN
                     INFORM = - 1
                     RETURN
                  END IF
               ELSE
                  NUSERE = NUSERE + 1
                  IF ( NUSERE .GT. NRLNDX ) THEN
                     INFORM = - 22
                     RETURN
                  END IF
                  INLIST( IFREE ) = NUSERE
                  NAMRIN( NUSERE ) = ARRAY( 1 )
               END IF
            END IF
         END IF
      END IF

!  AN ARRAY NAME OCCURS IN FIELD 3. INTERPRET THE CONTENTS OF THIS
!  FIELD.

      IF ( ( KINDAR .GE. 101 .AND. KINDAR .LE. 104 ) .OR.&
           ( KINDAR .GE. 110 .AND. KINDAR .LE. 112 ) .OR.&
             KINDAR .EQ. 113 .OR.  KINDAR .EQ. 116 ) THEN
         CALL INTFIE( LENGTH, 12, KEY, ITABLE, INLIST,  &
                      FIELD3, ARRAY( 2 ),  &
                      IARRAY( 1, 2 ), IOUT, INFORM )
         IF ( INFORM .NE. 0 ) RETURN
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4090 ) LEVEL,  &
                          NINSTR, ARRAY( 2 ),  &
                         ( NAMIIN( IARRAY( 2 + I, 2 ) ),  &
                            I = 1, IARRAY( 2, 2 ) )
      END IF

!  AN ARRAY NAME OCCURS IN FIELD 5. INTERPRET THE CONTENTS OF THIS
!  FIELD.

      IF ( KINDAR .EQ. 102 .OR.  KINDAR .EQ. 104 .OR.&
           KINDAR .EQ. 105 .OR.  KINDAR .EQ. 112 .OR.&
         ( KINDAR .GE. 113 .AND. KINDAR .LE. 116 ) ) THEN
         CALL INTFIE( LENGTH, 12, KEY, ITABLE, INLIST,  &
                      FIELD5, ARRAY( 3 ),  &
                      IARRAY( 1, 3 ), IOUT, INFORM )
         IF ( INFORM .NE. 0 ) RETURN
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4100 ) LEVEL,  &
                          NINSTR, ARRAY( 3 ),  &
                         ( NAMIIN( IARRAY( 2 + I, 3 ) ),  &
                           I = 1, IARRAY( 2, 3 ) )
      END IF

!  AN NAME OCCURS IN FIELD 2.

      IF ( ( KINDAR .GE. 110 .AND. KINDAR .LE. 112 ) .OR.&
             KINDAR .EQ. 116 ) THEN
         CARRAY( 1 ) = FIELD2
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4110 ) LEVEL,  &
                          NINSTR, CARRAY( 1 )
      END IF

!  AN NAME OCCURS IN FIELD 3.

      IF ( KINDAR .EQ. 105 .OR. KINDAR .EQ. 106 .OR.&
           KINDAR .EQ. 108 .OR. KINDAR .EQ. 109 .OR.&
           KINDAR .EQ. 115 ) THEN
         CARRAY( 1 ) = FIELD3
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4120 ) LEVEL,  &
                          NINSTR, CARRAY( 1 )
      END IF

!  AN NAME OCCURS IN FIELD 5.

      IF ( KINDAR .EQ. 109 ) THEN
         CARRAY( 2 ) = FIELD5
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4130 ) LEVEL,  &
                          NINSTR, CARRAY( 2 )
      END IF

!  A NUMERICAL VALUE OCCURS IN FIELD 4.

      IF ( KINDAR .EQ. 101 .OR. KINDAR .EQ. 102 .OR.&
           KINDAR .EQ. 107 .OR. KINDAR .EQ. 108 .OR.&
           KINDAR .EQ. 109 .OR. KINDAR .EQ. 111 .OR.&
           KINDAR .EQ. 112 ) THEN
         CALL GETVAL( FIELD4, VARRAY( 1 ) )
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4140 ) LEVEL,  &
                          NINSTR, VARRAY( 1 )
      END IF

!  A NUMERICAL VALUE OCCURS IN FIELD 6.

      IF ( KINDAR .EQ. 102 .OR. KINDAR .EQ. 109 .OR.&
           KINDAR .EQ. 112 ) THEN
         CALL GETVAL( FIELD6, VARRAY( 2 ) )
         IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 4150 ) LEVEL,  &
                          NINSTR, VARRAY( 2 )
      END IF
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2140 FORMAT( ' ** Exit from GPSMPS - type of array defn. unrecognised')
 4080 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 2 array ',  &
                A10, ' indices ', 3( A10, 1X ) )
 4090 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 3 array ',  &
                A10, ' indices ', 3( A10, 1X ) )
 4100 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 5 array ',  &
                A10, ' indices ', 3( A10, 1X ) )
 4110 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 2 name ', A10)
 4120 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 3 name ', A10)
 4130 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 5 name ', A10)
 4140 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 4 value ',  &
              1P, D12.4 )
 4150 FORMAT( ' Level ', I2, ' instruction ', I4, ' field 6 value ',  &
              1P, D12.4 )

!  END OF PROCAD.

      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE PROCAA( NINDEX, LENGTH, NUSERE, INFORM, IOUT, NRLNDX,
! ** Correction 8. 26/02/01: 1 dummy argument removed from PROCAA **&
                         INLIST, ITABLE,  &
                         NAMRIN, KEY, INDVAL, REALVL,  &
                         FIELD1, FIELD2, FIELD3, FIELD5, RVALUE )
      INTEGER        NINDEX, LENGTH, NUSERE, INFORM, IOUT
      INTEGER        NRLNDX
      DOUBLE PRECISION RVALUE
      CHARACTER *  2 FIELD1
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INLIST( LENGTH ), ITABLE ( LENGTH )
      INTEGER        INDVAL( NINDEX )
      DOUBLE PRECISION REALVL( NRLNDX )
      CHARACTER * 10 NAMRIN( NRLNDX )
      CHARACTER * 12 KEY( LENGTH )

!  CONSTRUCT AND EXECUTE A LIST OF DO-LOOP REAL ARRAY
!  ARITHMETIC INSTRUCTIONS.

!  NICK GOULD, 8/09/89
!  FOR CGT PRODUCTIONS.

      INTEGER        I, IFIELD, IFREE, INSTR2, INSTR3, INSTR4, NFUNCT
      CHARACTER * 12 FIELD
      INTRINSIC      FLOAT
      EXTERNAL       RINTRN, HASHB , HASHC
      PARAMETER      ( NFUNCT = 14 )
      CHARACTER * 6  FUNCTN( NFUNCT )
      DATA  FUNCTN / 'ABS   ', 'SQRT  ', 'EXP   ', 'LOG   ', 'LOG10 ',  &
                     'SIN   ', 'COS   ', 'TAN   ', 'ARCSIN', 'ARCCOS',  &
                     'ARCTAN', 'HYPSIN', 'HYPCOS', 'HYPTAN' /
      IF ( FIELD1 .EQ. 'A+' .OR. FIELD1 .EQ. 'A-' .OR.&
           FIELD1 .EQ. 'A*' .OR. FIELD1 .EQ. 'A/' .OR.&
           FIELD1 .EQ. 'A(' ) THEN

!  OBTAIN THE REAL VALUE, RVALUE, AS THE VALUE ASSOCIATED WITH THE REAL
!  INDEX IN FIELD 5, FIRST ENSURING THAT THE INDEX EXISTS.

         FIELD = FIELD5( 1 : 10 ) // 'RI'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 3
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
            RETURN
         END IF
         INSTR4 = INLIST( IFIELD )
      END IF

!  IF A DEFINITION IS TO BE MADE FROM A PREVIOUSLY DEFINED INDEX,
!  ENSURE THAT THE INDEX EXISTS.

      IF ( FIELD1 .EQ. 'AI' ) THEN
         FIELD = FIELD3( 1 : 10 ) // 'II'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 3
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
            RETURN
         END IF
         INSTR3 = INLIST( IFIELD )
      ELSE
         IF ( FIELD1 .NE. 'AF' .AND. FIELD1 .NE. 'A(' .AND.&
              FIELD1 .NE. 'AE' ) THEN
            FIELD = FIELD3( 1 : 10 ) // 'RI'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            INSTR3 = INLIST( IFIELD )
         ELSE

!  THE VALUE IS TO BE OBTAINED USING A SPECIAL FUNCTION. DETERMINE
!  WHICH ONE.

            IF ( FIELD1 .NE. 'AE' ) THEN
               DO 10 I = 1, NFUNCT
                  IF ( FIELD3( 1 : 10 ) .EQ. FUNCTN( I ) ) GO TO 20
   10          CONTINUE
               INFORM = 39
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2390 ) FIELD3( 1 : 10 )
               RETURN
   20          CONTINUE
               INSTR3 = I
            END IF
         END IF
      END IF

!  RECORD THE ADDRESS OF THE INDEX WHICH IS TO BE SET.

      FIELD = FIELD2( 1 : 10 ) // 'RI'
      CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
      IF ( IFREE .LE. 0 ) THEN
         IF ( IFREE .EQ. 0 ) THEN
            INFORM = - 1
            RETURN
         END IF
         IFREE = - IFREE
      ELSE
         NUSERE = NUSERE + 1
         IF ( NUSERE .GT. NRLNDX ) THEN
            INFORM = - 22
            RETURN
         END IF
         INLIST( IFREE ) = NUSERE
         NAMRIN( NUSERE ) = FIELD( 1 : 10 )
      END IF
      INSTR2 = INLIST( IFREE )
      IF ( FIELD1 .EQ. 'AE' ) REALVL( INSTR2 ) = RVALUE
      IF ( FIELD1 .EQ. 'AA' ) REALVL( INSTR2 ) =&
                              RVALUE + REALVL( INSTR3 )
      IF ( FIELD1 .EQ. 'AS' ) REALVL( INSTR2 ) =&
                              RVALUE - REALVL( INSTR3 )
      IF ( FIELD1 .EQ. 'AM' ) REALVL( INSTR2 ) =&
                              RVALUE * REALVL( INSTR3 )
      IF ( FIELD1 .EQ. 'AD' ) REALVL( INSTR2 ) =&
                              RVALUE / REALVL( INSTR3 )
      IF ( FIELD1 .EQ. 'AI' ) REALVL( INSTR2 ) =&
                              FLOAT( INDVAL( INSTR3 ) )
      IF ( FIELD1 .EQ. 'AF' ) CALL RINTRN( REALVL( INSTR2 ),  &
                                           RVALUE, INSTR3, INFORM )
      IF ( FIELD1 .EQ. 'A=' ) REALVL( INSTR2 ) = REALVL( INSTR3 )
      IF ( FIELD1 .EQ. 'A+' ) REALVL( INSTR2 ) =&
                              REALVL( INSTR3 ) + REALVL( INSTR4 )
      IF ( FIELD1 .EQ. 'A-' ) REALVL( INSTR2 ) =&
                              REALVL( INSTR3 ) - REALVL( INSTR4 )
      IF ( FIELD1 .EQ. 'A*' ) REALVL( INSTR2 ) =&
                              REALVL( INSTR3 ) * REALVL( INSTR4 )
      IF ( FIELD1 .EQ. 'A/' ) REALVL( INSTR2 ) =&
                              REALVL( INSTR3 ) / REALVL( INSTR4 )
      IF ( FIELD1 .EQ. 'A(' ) CALL RINTRN( REALVL( INSTR2 ),  &
                              REALVL( INSTR4 ), INSTR3, INFORM )
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2390 FORMAT( ' ** Exit from GPSMPS - specified function name ', A10,  &
              ' not recognised ' )

!  END OF PROCAA.

      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE GETFIE( NINDEX, INDVAL, IARRAY, ARRAY, FIELD, INFORM )
      INTEGER       NINDEX, INFORM
      INTEGER       INDVAL( NINDEX )
      INTEGER       IARRAY( 5 )
      CHARACTER * 10 ARRAY, FIELD

!  CONSTRUCT AN EXPANDED ARRAY NAME FROM ITS CONSTITUENT PARTS.

!  NICK GOULD 02/08/89
!  FOR CGT PRODUCTIONS.

      INTEGER       I, INDCES, IVALUE, J, NDIGIT
      CHARACTER * 9 FIELD9
      INTRINSIC     ABS, FLOAT, LOG10
      J = IARRAY( 1 )
      FIELD = ARRAY( 1: J )
      J = J + 1
      INDCES = IARRAY( 2 )
      DO 10 I = 1, INDCES
         IVALUE = INDVAL( IARRAY( 2 + I ) )
         IF ( IVALUE .EQ. 0 ) THEN
            NDIGIT = 1
         ELSE
            NDIGIT = LOG10( ABS( FLOAT( IVALUE ) ) ) + 1
            IF ( IVALUE .LT. 0 ) NDIGIT = NDIGIT + 1
         END IF
         IF ( ( I .LT. INDCES .AND. J + NDIGIT .GT. 10 ) .OR.&
              ( I .EQ. INDCES .AND. J + NDIGIT .GT. 11 ) ) THEN
            INFORM = 35
            RETURN
         END IF
         WRITE( UNIT = FIELD9, FMT = 2000 ) IVALUE
         FIELD( J: J + NDIGIT - 1 ) = FIELD9( 10 - NDIGIT: 9 )
         J = J + NDIGIT
         IF ( I .LT. INDCES ) THEN
            FIELD( J: J ) = ','
            J = J + 1
         END IF
  10  CONTINUE
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2000 FORMAT( I9 )
      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE GETIIN( NINDEX, INDVAL, NRLNDX, REALVL, INSTR )
      INTEGER NINDEX, NRLNDX
      INTEGER INDVAL( NINDEX ), INSTR( 4 )
      DOUBLE PRECISION REALVL( NRLNDX )

!  EXECUTE INTEGER INDEX INSTRUCTIONS.

!  NICK GOULD 02/08/89
!  FOR CGT PRODUCTIONS.

      IF ( INSTR( 1 ) .EQ. 21 ) INDVAL( INSTR( 2 ) ) = INSTR( 4 )
      IF ( INSTR( 1 ) .EQ. 22 ) INDVAL( INSTR( 2 ) ) =&
         INSTR( 4 ) + INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) .EQ. 23 ) INDVAL( INSTR( 2 ) ) =&
         INSTR( 4 ) - INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) .EQ. 24 ) INDVAL( INSTR( 2 ) ) =&
         INSTR( 4 ) * INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) .EQ. 25 ) INDVAL( INSTR( 2 ) ) =&
         INSTR( 4 ) / INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) .EQ. 26 ) INDVAL( INSTR( 2 ) ) =&
         REALVL( INSTR( 3 ) )
      IF ( INSTR( 1 ) .EQ. 31 ) INDVAL( INSTR( 2 ) ) =&
         INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) .EQ. 32 ) INDVAL( INSTR( 2 ) ) =&
         INDVAL( INSTR( 3 ) ) + INDVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) .EQ. 33 ) INDVAL( INSTR( 2 ) ) =&
         INDVAL( INSTR( 3 ) ) - INDVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) .EQ. 34 ) INDVAL( INSTR( 2 ) ) =&
         INDVAL( INSTR( 3 ) ) * INDVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) .EQ. 35 ) INDVAL( INSTR( 2 ) ) =&
         INDVAL( INSTR( 3 ) ) / INDVAL( INSTR( 4 ) )
      RETURN

!  NON-EXECUTABLE STATEMENTS.

      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE GETRIN( NINDEX, NRLNDX, INDVAL, REALVL,  &
                         RVALUE, INSTR, INFORM )
      INTEGER NINDEX, NRLNDX, INFORM
      DOUBLE PRECISION    RVALUE
      INTEGER INSTR( 4 ), INDVAL( NINDEX )
      DOUBLE PRECISION    REALVL( NRLNDX )

!  EXECUTE REAL INDEX INSTRUCTIONS.

!  NICK GOULD 02/08/89
!  FOR CGT PRODUCTIONS.

      INTEGER I
      INTRINSIC FLOAT
      EXTERNAL RINTRN
      INFORM = 0
      I = INSTR( 1 ) - 50
      GO TO ( 110, 120, 130, 140, 150, 160, 170, 300, 300, 300,  &
              210, 220, 230, 240, 250, 300, 270, 300, 300 ), I
  110 CONTINUE
      REALVL( INSTR( 2 ) ) = RVALUE
      RETURN
  120 CONTINUE
      REALVL( INSTR( 2 ) ) = RVALUE + REALVL( INSTR( 3 ) )
      RETURN
  130 CONTINUE
      REALVL( INSTR( 2 ) ) = RVALUE - REALVL( INSTR( 3 ) )
      RETURN
  140 CONTINUE
      REALVL( INSTR( 2 ) ) = RVALUE * REALVL( INSTR( 3 ) )
      RETURN
  150 CONTINUE
      REALVL( INSTR( 2 ) ) = RVALUE / REALVL( INSTR( 3 ) )
      RETURN
  160 CONTINUE
      REALVL( INSTR( 2 ) ) = FLOAT( INDVAL( INSTR( 3 ) ) )
      RETURN
  170 CONTINUE
      CALL RINTRN( REALVL( INSTR( 2 ) ), RVALUE, INSTR( 3 ), INFORM )
      RETURN
  210 CONTINUE
      REALVL( INSTR( 2 ) ) = REALVL( INSTR( 3 ) )
      RETURN
  220 CONTINUE
      REALVL( INSTR( 2 ) ) = REALVL( INSTR( 3 ) ) + REALVL( INSTR( 4 ) )
      RETURN
  230 CONTINUE
      REALVL( INSTR( 2 ) ) = REALVL( INSTR( 3 ) ) - REALVL( INSTR( 4 ) )
      RETURN
  240 CONTINUE
      REALVL( INSTR( 2 ) ) = REALVL( INSTR( 3 ) ) * REALVL( INSTR( 4 ) )
      RETURN
  250 CONTINUE
      REALVL( INSTR( 2 ) ) = REALVL( INSTR( 3 ) ) / REALVL( INSTR( 4 ) )
      RETURN
  270 CONTINUE
      CALL RINTRN( REALVL( INSTR( 2 ) ),  &
                   REALVL( INSTR( 4 ) ), INSTR( 3 ), INFORM )
      RETURN
  300 CONTINUE
      RETURN

!  NON-EXECUTABLE STATEMENTS.

      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE RINTRN( EVALUE, VALUE, IVALUE, INFORM )
      INTEGER IVALUE, INFORM
      DOUBLE PRECISION VALUE, EVALUE

!  RETURNS THE VALUE OF THE APPROPRIATE INTRINSIC FUNCTION.

!  NICK GOULD 10/11/89
!  FOR CGT PRODUCTIONS.

      INTRINSIC ABS, SQRT, EXP, LOG, LOG10, SIN, COS, TAN, ASIN, ACOS,  &
                ATAN, SINH, COSH, TANH, DBLE
      DOUBLE PRECISION DVALUE
      DVALUE = DBLE( VALUE )
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120,  &
              130, 140 ), IVALUE
   10 CONTINUE
      EVALUE = ABS( DVALUE )
      RETURN
   20 CONTINUE
      IF ( VALUE .LT. 0.0 ) THEN
         INFORM = 40
         WRITE( 6, 2400 ) VALUE, 'SQRT  '
      ELSE
         EVALUE = SQRT( DVALUE )
      END IF
      RETURN
   30 CONTINUE
      EVALUE = EXP( DVALUE )
      RETURN
   40 CONTINUE
      IF ( VALUE .LE. 0.0 ) THEN
         INFORM = 40
         WRITE( 6, 2400 ) VALUE, 'LOG   '
      ELSE
         EVALUE = LOG( DVALUE )
      END IF
      RETURN
   50 CONTINUE
      IF ( VALUE .LE. 0.0 ) THEN
         INFORM = 40
         WRITE( 6, 2400 ) VALUE, 'LOG10 '
      ELSE
         EVALUE = LOG10( DVALUE )
      END IF
      RETURN
   60 CONTINUE
      EVALUE = SIN( DVALUE )
      RETURN
   70 CONTINUE
      EVALUE = COS( DVALUE )
      RETURN
   80 CONTINUE
      EVALUE = TAN( DVALUE )
      RETURN
   90 CONTINUE
      IF ( ABS( VALUE ) .GT. 1.0 ) THEN
         INFORM = 40
         WRITE( 6, 2400 ) VALUE, 'ASIN  '
      ELSE
         EVALUE = ASIN( DVALUE )
      END IF
      RETURN
  100 CONTINUE
      IF ( ABS( VALUE ) .GT. 1.0 ) THEN
         INFORM = 40
         WRITE( 6, 2400 ) VALUE, 'ACOS  '
      ELSE
         EVALUE = ACOS( DVALUE )
      END IF
      RETURN
  110 CONTINUE
      EVALUE = ATAN( DVALUE )
      RETURN
  120 CONTINUE
      EVALUE = SINH( DVALUE )
      RETURN
  130 CONTINUE
      EVALUE = COSH( DVALUE )
      RETURN
  140 CONTINUE
      EVALUE = TANH( DVALUE )
      RETURN
 2400 FORMAT( ' ** Exit from GPSMPS - argument value ', 1P, D9.1,  &
              ' is illegal for function ', A6 )
      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE GETINT( FIELD, IVALUE )
      INTEGER        IVALUE
      CHARACTER * 12 FIELD

!  READ THE INTEGER NUMBER IVALUE STORED IN THE CHARACTER FIELD.

!  NICK GOULD 02/08/89
!  FOR CGT PRODUCTIONS.

      INTEGER        I, J
      CHARACTER * 12 FIELD2

!  RIGHT-SHIFT THE FIELD, ELIMINATING BLANKS.

      FIELD2 = '            '
            J = 12
      DO 10 I = 12, 1, - 1
         IF ( FIELD( I : I ) .EQ. ' ' ) GO TO 10
         FIELD2( J : J ) = FIELD( I : I )
         J = J - 1
   10 CONTINUE
      READ( UNIT = FIELD2, FMT = 2000 ) IVALUE
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2000 FORMAT( I12 )
      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE GETLIN( NINDEX, NRLNDX, INDVAL, IARRAY, VARRAY,  &
                         ARRAY, CARRAY, FARRAY, REALVL, NAMIIN, NOVALS,  &
                         KINDAR, FIELD1, FIELD2, FIELD3, VALUE4,  &
                         FIELD5, VALUE6, IOUT, INFORM,  &
                         LENGTH, KEY, ITABLE, INLIST )
      INTEGER        NINDEX, NRLNDX, KINDAR, NOVALS, IOUT, INFORM
      INTEGER        LENGTH
      DOUBLE PRECISION VALUE4, VALUE6
      CHARACTER *  2 FIELD1, FARRAY
      CHARACTER * 10 FIELD2, FIELD3, FIELD5
      INTEGER        INDVAL( NINDEX ), IARRAY( 5, 3 )
      INTEGER        INLIST ( LENGTH ), ITABLE ( LENGTH )
      DOUBLE PRECISION VARRAY( 2 ), REALVL( NRLNDX )
      CHARACTER *  7 NAMIIN( NINDEX )
      CHARACTER * 10 ARRAY( 3 ), CARRAY( 2 )
      CHARACTER * 12 KEY   ( LENGTH )

!  TRANSLATE THE CONTENTS OF AN ARRAY CARD TO ITS SCALAR VALUES.
!  THE EXPECTED CONTENTS ARE AT THE CONTROL OF THE PARAMETER KINDAR
!  AS FOLLOWS: (N=NAME ,A=ARRAY NAME, V=NUMERICAL VALUE,
!               R=REAL INDEX ARRAY VALUE)

!  KINDAR       FIELD2    FIELD3    FIELD4     FIELD5     FIELD6
!  ------       ------    ------    ------     ------     ------
!   100            A
!   101            A         A         V
!   102            A         A         V          A          V
!   103            A         A
!   104            A         A                    A
!   105            A         N                    A
!   106            A         N
!   107            A                   V
!   108            A         N         V
!   109            A         N         V          N          V
!   110            N         A
!   111            N         A         V
!   112            N         A         V          A          V
!   113            A         A          < ------- R
!   114            A                    < ------- R
!   115            A         N          < ------- R
!   116            N         A          < ------- R

!  NICK GOULD 02/08/89
!  FOR CGT PRODUCTIONS.

      INTEGER  I, IFIELD
      CHARACTER * 12 FIELD
      EXTERNAL GETFIE, HASHC
      FIELD1 = FARRAY
      NOVALS = 0

!  AN ARRAY NAME OCCURS IN FIELD 2. INTERPRET THE CONTENTS OF THIS
!  FIELD.

      IF ( ( KINDAR .GE. 100 .AND. KINDAR .LE. 109 ) .OR.&
           ( KINDAR .GE. 113 .AND. KINDAR .LE. 115 ) ) THEN
         CALL GETFIE( NINDEX, INDVAL, IARRAY( 1, 1 ),  &
                      ARRAY( 1 ), FIELD2, INFORM )
         IF ( INFORM .NE. 0 ) THEN
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2350 )  &
               ARRAY( 1 )( 1: IARRAY( 1, 1 ) ),  &
               ( NAMIIN( IARRAY( 2 + I, 1 ) ),  &
                 INDVAL( IARRAY( 2 + I, 1 ) ), I = 1, IARRAY( 2, 1 ) )
            INFORM = 35
            RETURN
         END IF
      ELSE
         FIELD2 = '          '
      END IF

!  AN ARRAY NAME OCCURS IN FIELD 3. INTERPRET THE CONTENTS OF THIS
!  FIELD.

      IF ( ( KINDAR .GE. 101 .AND. KINDAR .LE. 104 ) .OR.&
           ( KINDAR .GE. 110 .AND. KINDAR .LE. 112 ) .OR.&
             KINDAR .EQ. 113 .OR.  KINDAR .EQ. 116 ) THEN
         CALL GETFIE( NINDEX, INDVAL, IARRAY( 1, 2 ),  &
                      ARRAY( 2 ), FIELD3, INFORM )
         IF ( INFORM .NE. 0 ) THEN
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2350 )  &
               ARRAY( 2 )( 1: IARRAY( 1, 2 ) ),  &
               ( NAMIIN( IARRAY( 2 + I, 2 ) ),  &
                 INDVAL( IARRAY( 2 + I, 2 ) ), I = 1, IARRAY( 2, 2 ) )
            INFORM = 35
            RETURN
         END IF
      ELSE
         FIELD3 = '          '
      END IF
      IF ( KINDAR .EQ. 103 ) NOVALS = 1

!  AN ARRAY NAME OCCURS IN FIELD 5. INTERPRET THE CONTENTS OF THIS
!  FIELD.

      IF ( KINDAR .EQ. 102 .OR.  KINDAR .EQ. 104 .OR.&
           KINDAR .EQ. 105 .OR.  KINDAR .EQ. 112 .OR.&
         ( KINDAR .GE. 113 .AND. KINDAR .LE. 116 ) ) THEN
         CALL GETFIE( NINDEX, INDVAL, IARRAY( 1, 3 ),  &
                      ARRAY( 3 ), FIELD5, INFORM )
         IF ( INFORM .NE. 0 ) THEN
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2350 )  &
               ARRAY( 3 )( 1: IARRAY( 1, 3 ) ),  &
               ( NAMIIN( IARRAY( 2 + I, 3 ) ),  &
                 INDVAL( IARRAY( 2 + I, 3 ) ), I = 1, IARRAY( 2, 3 ) )
            INFORM = 35
            RETURN
         END IF
      ELSE
         FIELD5 = '          '
      END IF
      IF ( KINDAR .EQ. 104 ) NOVALS = 2

!  AN NAME OCCURS IN FIELD 2.

      IF ( ( KINDAR .GE. 110 .AND. KINDAR .LE. 112 ) .OR.&
             KINDAR .EQ. 116 ) THEN
         FIELD2 = CARRAY( 1 )
      END IF

!  AN NAME OCCURS IN FIELD 3.

      IF ( KINDAR .EQ. 105 .OR. KINDAR .EQ. 106 .OR.&
           KINDAR .EQ. 108 .OR. KINDAR .EQ. 109 .OR.&
           KINDAR .EQ. 115 ) THEN
         FIELD3 = CARRAY( 1 )
      END IF

!  AN NAME OCCURS IN FIELD 5.

      IF ( KINDAR .EQ. 109 ) THEN
         FIELD5 = CARRAY( 2 )
      END IF

!  A NUMERICAL VALUE OCCURS IN FIELD 4.

      IF ( KINDAR .EQ. 101 .OR. KINDAR .EQ. 102 .OR.&
           KINDAR .EQ. 107 .OR. KINDAR .EQ. 108 .OR.&
           KINDAR .EQ. 109 .OR. KINDAR .EQ. 111 .OR.&
           KINDAR .EQ. 112 ) THEN
         VALUE4 = VARRAY( 1 )
         NOVALS = 1
      ELSE

!  A REAL INDEX VALUE IS TO BE PLACES IN FIELD 4.

         IF ( KINDAR .EQ. 113 .OR. KINDAR .EQ. 114 .OR.&
              KINDAR .EQ. 115 .OR. KINDAR .EQ. 116 ) THEN
            FIELD = FIELD5( 1 : 10 ) // 'RI'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            VALUE4 = REALVL( INLIST( IFIELD ) )
            NOVALS = 1
         ELSE
            VALUE4 = 0.0D+0
         END IF
      END IF

!  A NUMERICAL VALUE OCCURS IN FIELD 6.

      IF ( KINDAR .EQ. 102 .OR. KINDAR .EQ. 109 .OR.&
           KINDAR .EQ. 112 ) THEN
         VALUE6 = VARRAY( 2 )
         NOVALS = 2
      ELSE
         VALUE6 = 0.0D+0
      END IF
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2350 FORMAT( ' ** Exit from GPSMPS - expanded array name > 10 chars.',  &
              ' Array name = ', A9, /, ( '    Index ', A10,  &
              ' has the value ', I6, : ) )
      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE GETVAL( FIELD, VALUE )
      DOUBLE PRECISION VALUE
      CHARACTER * 12 FIELD

!  READ THE REAL NUMBER VALUE STORED IN THE CHARACTER FIELD.

!  NICK GOULD 02/08/89
!  FOR CGT PRODUCTIONS.

      READ( UNIT = FIELD, FMT = 2000 ) VALUE
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2000 FORMAT( BN, F12.0 )
      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE INTFIE( LENGTH, NCHAR, KEY, ITABLE, INLIST,  &
                         FIELDA, ARRAY, IARRAY, IOUT, INFORM )
      INTEGER        LENGTH, NCHAR, IOUT, INFORM
      CHARACTER * 10 FIELDA, ARRAY
      CHARACTER * 12 KEY   ( LENGTH )
      INTEGER        IARRAY( 5 )
      INTEGER       INLIST ( LENGTH )
      INTEGER       ITABLE ( LENGTH )

!  INTERPRET THE CONTENTS OF FIELDA.

!  NICK GOULD 02/08/89
!  FOR CGT PRODUCTIONS.

      INTEGER        I, IFIELD, INDCES, J
      CHARACTER * 12 FIELD
      EXTERNAL       HASHC

!  FIRST FIND THE ARRAY NAME BY SEARCHING THE FIELD FOR THE STRING '('.

      DO 10 I = 1, 10
         IF ( FIELDA( I: I ) .EQ. '(' ) GO TO 20
   10 CONTINUE
!     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2370 ) FIELDA
!     INFORM = 37
      IARRAY( 1 ) = 10
      IARRAY( 2 ) = 0
      ARRAY = FIELDA
      INFORM = 0
      RETURN

!  THE STRING '(' OCCURS IN POSITION I.

   20 CONTINUE
      J = I - 1
      IARRAY( 1 ) = J
      ARRAY = FIELDA( 1: J )
      INDCES = 0

!  NOW FIND THE ARRAY INDICES. SEARCH FOR ONE OF THE STRINGS ')' OR ','.

   30 CONTINUE
         J = I + 1
         DO 40 I = J, 10
            IF ( FIELDA( I: I ) .EQ. ',' .OR.&
                 FIELDA( I: I ) .EQ. ')' ) GO TO 50
   40    CONTINUE
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2370 ) FIELDA
         INFORM = 37
         RETURN

!  THE STRING ',' OR ')' OCCURS IN POSITION J.

   50    CONTINUE
         IF ( I .NE. J ) THEN
            INDCES = INDCES + 1
            IF ( INDCES .GT. 3 ) THEN
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2360 )
               INFORM = 36
               RETURN
            END IF
            FIELD( 1: 12 ) = '          II'
            FIELD( 1: I - J ) = FIELDA( J: I - 1 )

!  CHECK THAT THE ARRAY INDEX EXISTS AND DETERMINE ITS ADDRESS.

            CALL HASHC ( LENGTH, NCHAR, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               INFORM = 3
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2030 ) FIELD( 1: 10 )
               RETURN
            END IF
            IARRAY( 2 + INDCES ) = INLIST( IFIELD )
         END IF
         IF ( FIELDA( I: I ) .EQ. ',' ) GO TO 30

!  THE ARRAY DEFINITION IS COMPLETE.

      IARRAY( 2 ) = INDCES
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,  &
              ' not recognised ' )
 2370 FORMAT( ' ** Exit from GPSMPS - incorrect array name', A10,  &
              ' in do-loop ')
 2360 FORMAT( ' ** Exit from GPSMPS - > 3 array name indices ' )

!  END OF INTFIE.

      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      SUBROUTINE FREEFM( NULINE, LELINE, MENDAT, INDIC8, LENIND, NULINA,  &
                         MAXNUL, NLINES, ISSDIF, INFORM, IOUT )
      INTEGER          LELINE, MENDAT, MAXNUL, NLINES, INFORM, IOUT
      LOGICAL          ISSDIF
      INTEGER          LENIND( MENDAT )
      CHARACTER * 1    NULINE( LELINE )
      CHARACTER * 12   INDIC8( MENDAT )
      CHARACTER * 65   NULINA( MAXNUL )

!  CONSTRUCT A FIXED FORMAT LINE FROM A FREE FORMAT ONE.

!  NICK GOULD, 12/07/90
!  FOR CGT PRODUCTIONS.

      INTEGER I, J, K, NFIELD, NSTFIE, LFIELD, LEN, ICARD, LELIP1
      LOGICAL FIELD, NEXTL, WAITNL, SPSAL2
      INTEGER LENFIE( 6, 2 ), ISTFIE( 6, 2 ), NFIE( 2 )
      DATA    LENFIE / 2, 10, 10, 12, 10, 12,  &
                       2, 10, 10, 41, 0, 0 /
      DATA    ISTFIE / 2, 5, 15, 25, 40, 50,  &
                       2, 5, 15, 25, 65, 65 /
      DATA    NFIE / 6, 4  /

!  IF ISSDIF IS .TRUE., THE CALL IS MADE FROM SUBROUTINE GPS, WHERE THE
!  CARD LENGTH IS 61. OTHERWISE, THE CARD LENGTH IS 65.

      IF ( ISSDIF ) THEN
         ICARD = 1
      ELSE
         ICARD = 2
      END IF
      LELIP1 = LELINE + 1
      NFIELD = 0
      NLINES = 0
      FIELD = .FALSE.
      NEXTL = .FALSE.
      SPSAL2 = .FALSE.
      WAITNL = .TRUE.

!  PROCESS THE NEXT CHARACTER ON THE CARD.

      DO 500 I = 1, LELINE

!  COPY COMMENTS UNCHANGED.

         IF ( WAITNL .AND. NULINE( I ) .EQ. '$' ) THEN
            NLINES = NLINES + 1
            DO 10 J = 1, 65
               NULINA( NLINES )( J: J ) = ' '
   10       CONTINUE
            NULINA( NLINES )( 1: 1 ) = '*'
            DO 20 J = 2, LELIP1 - I
               NULINA( NLINES )( J: J ) = NULINE( I + J - 1 )
   20       CONTINUE
            GO TO 600
         END IF

!  IF WE ARE LOOKING FOR AN END OF LINE MARKER, CHECK WHETHER WE HAVE
!  FOUND IT.

         IF ( NEXTL ) THEN
            IF ( NULINE( I ) .EQ. ';' ) THEN
               NEXTL = .FALSE.
               WAITNL = .TRUE.

!  RESET THE CARD TYPE TO 2 WHEN A SPECIAL CARD HAS BEEN FINISHED.

               IF ( SPSAL2 ) THEN
                  SPSAL2 = .FALSE.
                  ICARD = 2
               END IF
            END IF
            GO TO 500
         END IF

!  NEXT CHECK WHETHER WE HAVE FOUND AN END OF LINE MARKER ANYWAY.

         IF ( NULINE( I ) .EQ. ';' ) THEN
            WAITNL = .TRUE.

!  FINISH OFF THE CURRENT LINE.

            J = ISTFIE( NFIELD, ICARD ) - 1
            DO 30 K = 1, LFIELD
               NULINA( NLINES )( J + K: J + K ) =&
                  NULINE( NSTFIE + K )
   30       CONTINUE
            FIELD = .FALSE.

!  RESET THE CARD TYPE TO 2 WHEN A SPECIAL CARD HAS BEEN FINISHED.

            IF ( SPSAL2 ) THEN
               SPSAL2 = .FALSE.
               ICARD = 2
            END IF
            GO TO 500
         END IF

!  A FIELD HAS BEEN STARTED.

         IF ( FIELD ) THEN

!  THE FIELD HAS NOW COME TO AN END.

            IF ( ( NULINE( I ) .EQ. ' ' .AND. .NOT.&
                    ( ICARD .EQ. 2 .AND. NFIELD .EQ. 4 ) ) .OR.&
                 NULINE( I ) .EQ. '_' ) THEN
               FIELD = .FALSE.

!  STORE THE FIELD IN ITS CORRECT POSITION IN NULINA.

               J = ISTFIE( NFIELD, ICARD ) - 1
               DO 40 K = 1, LFIELD
                  NULINA( NLINES )( J + K: J + K ) =&
                     NULINE( NSTFIE + K )
   40          CONTINUE

!  THE FIELD HAS NOW COME TO AN END AND A BLANK FIELD FOLLOWS.

               IF ( NULINE( I ) .EQ. '_' ) THEN
                  NFIELD = NFIELD + 1
                  LFIELD = 0
                  IF ( NFIELD .GT. NFIE( ICARD ) ) THEN
                     INFORM = 45
                     WRITE( IOUT, 2450 )
                     RETURN
                  END IF
               END IF
            ELSE

!  AN EXTRA CHARACTER HAS BEEN ADDED TO THE FIELD.

               LFIELD = LFIELD + 1

!  CHECK THAT THE FIELD HAS NOT EXCEEDED ITS ALLOWED SPACE.
!  THIS MAY HAPPEN IF A) THERE IS AN ERROR ON THE CARD, B) THE
!  CARD IS OF TYPE 2 AND ONLY BLANKS REMAIN OR C) THE FIELD IS
!  ACTUALLY AN INDICATOR CARD. CHECK WHICH.

               IF ( LENFIE( NFIELD, ICARD ) .LT. LFIELD ) THEN

!  IF WE ARE CONSIDERING FIELD 4 WHEN THE CARD IS OF TYPE 2 AND
!  ALL THE SPACE HAS BEEN EXHAUSTED, FINISH THE LINE.

                  IF ( ICARD .EQ. 2 .AND. NFIELD .EQ. 4 ) THEN
                     WAITNL = .TRUE.
                     J = ISTFIE( NFIELD, ICARD ) - 1
                     DO 50 K = 1, LFIELD
                        NULINA( NLINES )( J + K: J + K ) =&
                           NULINE( NSTFIE + K )
   50                CONTINUE
                     FIELD = .FALSE.
                     GO TO 500
                  END IF

!  THERE IS AN ERROR IN THE CURRENT FIELD.

                  IF ( NFIELD .GT. 1 ) THEN
                     INFORM = 44
                     WRITE( IOUT, 2440 ) NFIELD
                     RETURN
                  ELSE

!  THE FIRST FIELD MAY BE AN INDICATOR CARD. CHECK.

                     DO 70 J = 2, MENDAT
                        LEN = LENIND( J )
                        DO 60 K = 1, LEN
                           IF ( NULINE( NSTFIE + K ) .NE.&
                           INDIC8( J )( K: K ) ) GO TO 70
   60                   CONTINUE
                        GO TO 80
   70                CONTINUE

!  THE INDICATOR CARD IS UNKNOWN. EXIT WITH AN ERROR MESSAGE.

                     INFORM = 2
                     WRITE( IOUT, 2020 )
                     RETURN

!  THE INDICATOR CARD IS RECOGNISED. OUTPUT THIS CARD AS THE NEXT
!  LINE AND AWAIT A FURTHER LINE. (THE TITLE CARD IS AN EXCEPTION AS
!  THE TITLE HAS STILL TO ARRIVE)

   80                CONTINUE
                     IF ( J .NE. 4 ) THEN
                        FIELD = .FALSE.
                        NEXTL = .TRUE.
                        NULINA( NLINES )( 1: 12 ) =&
                             INDIC8( J )( 1: 12 )
                     END IF
                  END IF
               END IF
            END IF

!  WE ARE BETWEEN FIELDS.

         ELSE

!  A NEW FIELD HAS STARTED.

            IF ( NULINE( I ) .NE. ' ' ) THEN

!  IT IS THE FIRST FIELD.

               IF ( WAITNL ) THEN
                  WAITNL = .FALSE.
                  NLINES = NLINES + 1

!  INITIALIZE THE NEW LINE, NULINA( NLINES ), AS A BLANK LINE.

                  DO 90 J = 1, 65
                     NULINA( NLINES )( J: J ) = ' '
   90             CONTINUE
                  NFIELD = 1
                  IF ( NULINE( I ) .EQ. '_' ) LFIELD = 0

!  IF A SPECIAL CARD OCCURS (THAT IS, A CARD ON WHICH THE RANGE
!  TRANSFORMATION IS SPECIFIED WITHIN MAKEFN), MARK IT.

                  IF ( NULINE( I ) .EQ. 'R' .AND. ICARD .EQ. 2 ) THEN
                     SPSAL2 = .TRUE.
                     ICARD = 1
                  END IF
               ELSE

!  IT IS NOT THE FIRST FIELD.

                  NFIELD = NFIELD + 1
                  IF ( NFIELD .GT. NFIE( ICARD ) ) THEN
                     INFORM = 45
                     WRITE( IOUT, 2450 )
                     RETURN
                  END IF

!  IF THE STRING IS IN FIELDS 3 OR 5 AND STARTS WITH A '$', THE
!  REMAINDER OF THE CARD IS CONSIDERED TO BE A COMMENT.

                  IF ( ( NFIELD .EQ. 3 .OR. NFIELD .EQ. 5 ) .AND.&
                       NULINE( I ) .EQ. '$' ) THEN
                     J = ISTFIE( NFIELD, ICARD ) - 1
                     DO 100 K = 1, 66 - I
                        NULINA( NLINES )( J + K: J + K ) =&
                           NULINE( I + K - 1 )
  100                CONTINUE
                     GO TO 600
                  END IF
               END IF

!  SKIP A FIELD IF A '_' IS ENCOUNTERED.

               IF ( NULINE( I ) .EQ. '_' ) THEN
                  LFIELD = 0
               ELSE

!  SET THE CURRENT LENGTH OF THE FIELD, LFIELD, THE STARTING ADDRESS
!  IN NULINE - 1, NSTFIE, AND THE FIELD NUMBER, NFIELD.

                  FIELD = .TRUE.
                  LFIELD = 1
                  NSTFIE = I - 1
               END IF
            END IF
         END IF
  500 CONTINUE
  600 CONTINUE

!  FINISH OFF THE LAST LINE.

      IF ( FIELD ) THEN
         J = ISTFIE( NFIELD, ICARD ) - 1
         DO 610 K = 1, LFIELD
            NULINA( NLINES )( J + K: J + K ) =&
               NULINE( NSTFIE + K )
  610    CONTINUE
      END IF
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2020 FORMAT( ' ** Exit from GPSMPS - indicator card not recognised ' )
 2440 FORMAT( ' ** Exit from GPSMPS - field ', I1, ' on free-form',  &
              ' card too long' )
 2450 FORMAT( ' ** Exit from GPSMPS - too many fields on free-form',  &
              ' card' )

!  END OF FREEFM.

      END
!  THIS VERSION: 28/04/1992 AT 10:12:34 AM.
      INTEGER FUNCTION ONLY1( NUM )
      INTEGER                 NUM

!  RETURNS THE VALUE 1 IF NUM IS ONE AND 2 OTHERWISE

!  NICK GOULD, FOR CGT PRODUCTIONS.
!  DECEMBER 7TH, 1990.

      ONLY1 = 2
      IF ( NUM .EQ. 1 ) ONLY1 = 1
      RETURN

!  END OF ONLY1.

      END








!  THIS VERSION: 04/08/1995 AT 11:38:29 AM.
!  ** VERSION B **
! ** Correction report.
! ** Correction 1. 27/03/2002: allow for non-zero defaults for QPs
      SUBROUTINE INLANC( N     , NLVARS, NG    , NELNUM, NOBJ  , LENGTH, &
                         LSTADG, LELVAR, LSTAEV, LNTVAR, LICNA , LSTADA, &
                         LA, LB, LBNDS , LINTRE, LIWK  , LWK   , NMAX  , &
                         NGMAX , NBMAX , NSMAX , NLMAX , NELMAX, NEGMAX, &
                         NOBMAX, NGRMAX, NGPVMX, NEPVMX, NOMAX , NLISGP, &
                         NBND  , NNZA  , NCONST, NSTART, NRANGE, NOBJGR, &
                         NOBBND, NELTYP, NGRTYP, &
                         PNAME , ONEOBJ, NAMEOB, NAMERH, NAMERA,  &
                         NAMEBN, NAMEST, NAMEOF, ISTADG, IELVAR, ISTAEV,  &
                         INTVAR, ICNA  , ISTADA, ICOORD, INLIST, ITABLE,  &
                         ISTATE, IDROWS, IELV  , IINV  , IGPA  , IELING,  &
                         ISTEP , ISTGP , ITYPEE, ITYPEG, ITYPEV, IWK   ,  &
                         A, BND, VSTART, CSTART, RSCALE, CSCALE, &
                         RDROWS, DFAULT, WEIGHT, BNDFLT, WK, &
                         GPVALU, EPVALU, FBOUND, ABYROW, B , BL, BU, X , &
                         CLMULT, ESCALE, GSCALE, VSCALE, INTREP, GXEQX ,  &
                         KEY   , GNAMES, VNAMES, BNAMES, SNAMES, ONAMES,  &
                         ETYPES, GTYPES, OBNAME, IALGOR, IAUTO ,  &
                         IOUT  , IOUTDA, SINGLE, INFORM, DEBUG  )

!  CONVERT THE OUTPUT FROM GPSMPS INTO A FORM SUITABLE FOR ANY OF THE
!  ------------------------------------------------------------------
!  LANCELOT PROGRAMS.
!  ------------------
!  SBMIN ( IALGOR = 1 ), AUGLG ( IALGOR = 2 ) OR BARIA ( IALGOR = 3 ).
!  -------------------------------------------------------------------

!  NICK GOULD, 16/01/1990
!  FOR CGT PRODUCTIONS.

      INTEGER          N , NG, LENGTH, LSTADG, LELVAR, LSTAEV, LNTVAR
      INTEGER          LSTADA, LA, LB, LBNDS , LINTRE, LIWK  , LWK
      INTEGER          NMAX  , NGMAX , NBMAX , NSMAX , NLMAX , NLVARS
      INTEGER          NEGMAX, NGRMAX, NGPVMX, NEPVMX, NLISGP, NOMAX
      INTEGER          NBND  , NNZA  , NCONST, NSTART, NRANGE, NOBJGR
      INTEGER          NELMAX, IALGOR, IOUT  , IOUTDA, INFORM, NOBBND
      INTEGER          NELTYP, NGRTYP, LICNA , NOBMAX, NOBJ  , NELNUM
      INTEGER          IAUTO
      LOGICAL          ONEOBJ, SINGLE, DEBUG
      CHARACTER * 8    PNAME
      CHARACTER *10    NAMEOB, NAMERH, NAMERA, NAMEBN, NAMEST,NAMEOF
      INTEGER          ISTADG( LSTADG ), IELVAR( LELVAR ), ICNA( LICNA )
      INTEGER          ISTAEV( LSTAEV ), INTVAR( LNTVAR )
      INTEGER          ISTADA( LSTADA ), ITABLE( LENGTH )
      INTEGER          ICOORD( LA, 2  ), INLIST( LENGTH )
      INTEGER          ISTATE( NGMAX  ), IDROWS( 2, NGMAX )
      INTEGER          IELV  ( NLMAX  ), IINV  ( NLMAX  )
      INTEGER          ISTEP ( NELMAX ), ISTGP ( NGMAX  )
      INTEGER          IELING( NEGMAX ), IGPA  ( NGRMAX ) 
      INTEGER          ITYPEE( NELMAX ), ITYPEG( NGMAX ), IWK( LIWK )
      INTEGER          ITYPEV( NMAX )
      DOUBLE PRECISION A( LA ),  RDROWS( 2, NGMAX ), WEIGHT( NEGMAX )
      DOUBLE PRECISION BND( 2, NMAX, NBMAX ), VSTART( NMAX, NSMAX )
      DOUBLE PRECISION BNDFLT( 2, NBMAX ), CSTART( NGMAX, NSMAX )
      DOUBLE PRECISION RSCALE( NGMAX ), CSCALE( NMAX )
      DOUBLE PRECISION GPVALU( NGPVMX ), EPVALU( NEPVMX )
      DOUBLE PRECISION DFAULT( NMAX ), FBOUND( 2, NOBMAX ), WK( LWK )
      DOUBLE PRECISION ABYROW( LA ), B( LB ), BL( LBNDS ), BU( LBNDS )
      DOUBLE PRECISION X( NMAX ), GSCALE( NGMAX ), ESCALE( NEGMAX )
      DOUBLE PRECISION VSCALE( NMAX ), CLMULT( NGMAX )
      LOGICAL          INTREP( LINTRE ), GXEQX( NGMAX )
      CHARACTER * 12 KEY( LENGTH  )
      CHARACTER * 10 GNAMES( NGMAX ), BNAMES( NBMAX ), ONAMES( NOMAX )
      CHARACTER * 10 VNAMES( NMAX  ), SNAMES( NSMAX ), OBNAME( NOBMAX )
      CHARACTER * 10 ETYPES( NLMAX ), GTYPES( NGRMAX )
      EXTERNAL        HASHC

!  LOCAL VARIABLES.

      INTEGER          I, IC, IFIELD, IG, IROW, IS, ITYPE, J, JBND, K
      INTEGER          K1, K2, NELV, NINV, NG1, NEL, NGPV, NGR
      INTEGER          JSTART, NEL1, JCOL, JCONST, JRANGE
      INTEGER          NNZ, NSLACK, JOBBND
      DOUBLE PRECISION AVALUE, BIG, RZERO, RROW
      DOUBLE PRECISION ONE, ZERO, BIGINF, OBFBND( 2 )
! ** Correction 1a. 1 line added
      CHARACTER * 10   CQGROU
      CHARACTER * 12   FIELD
      INTRINSIC        ABS, MAX
      EXTERNAL         REORDA
      PARAMETER      ( ZERO = 0.0D+0,  ONE = 1.0D+0, RZERO = 0.0D+0 )
      PARAMETER      ( BIGINF = 1.0D+20, BIG = 1.0E+20 )
! ** Correction 1b. 1 line added
      PARAMETER      ( CQGROU = '123456789G' )
      IF ( LIWK .LT.  NG .OR. LWK .LT. MAX( N, NLISGP ) ) THEN
         INFORM = - 1
         IF ( IOUT  .GT. 0 ) WRITE( IOUT, 2000 )
         GO TO 800
      END IF

!  DECIDE WHICH OPTIMIZATION METHOD TO USE.
!  ---------------------------------------

      IF ( IALGOR .LE. 0 ) THEN
         IALGOR = 1
         DO 10 IG = 1, NG
            IF ( ABS( ISTATE( IG ) ) .GE. 2 ) IALGOR = 2
   10    CONTINUE   
      END IF
      DO 20 IG = 1, NG
         ISTATE( IG ) = ABS( ISTATE( IG ) )
   20 CONTINUE   

!  SELECT THE BOUNDS ON THE VARIABLES.
!  -----------------------------------

      IF ( NBND .EQ. 1 ) THEN
         JBND = 1
      ELSE

!  FIND THE KEY WORD IN THE LIST OF BOUNDS.

         DO 110 JBND = 1, NBND
            IF ( NAMEBN .EQ. BNAMES( JBND ) ) GO TO 120
  110    CONTINUE
         INFORM = 47
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2470 ) NAMEBN
         GO TO 800
  120    CONTINUE
      END IF

!  THE REQUIRED VECTOR OF BOUNDS IS COLUMN JBND OF BND. COPY THIS
!  VECTOR INTO BL( ) AND BU( ). RECORD THE SCALE FACTORS.

      DO 130 J = 1, NLVARS
         BL( J ) = BND( 1, J, JBND )
         BU( J ) = BND( 2, J, JBND )
         VSCALE( J ) = CSCALE( J )
  130 CONTINUE

!  THE BOUNDS ON THE NONLINEAR VARIABLES ARE SET TO DEFAULT VALUES.

      DO 140 J = NLVARS + 1, N
         BL( J ) = BNDFLT( 1, JBND )
         BU( J ) = BNDFLT( 2, JBND )
  140 CONTINUE

!  SELECT THE CONSTANT/RHS AND RANGES.
!  -----------------------------------

!  FIND THE NAMED CONSTANT (R.H.S.) VECTOR IN THE LIST.

      IF ( NCONST .EQ. 1 ) THEN
         JCONST = NLVARS + 1
      ELSE
         FIELD = NAMERH // 'CO'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .GT. 0 ) THEN
            JCONST = INLIST( IFIELD )
         ELSE
            INFORM = 46
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2460 ) NAMERH
            GO TO 800
         END IF
      END IF

!  FIND THE NAMED RANGE VECTOR IN THE LIST.

      IF ( NRANGE .GT. 0 ) THEN
         IF ( NRANGE .EQ. 1 ) THEN
            JRANGE = NLVARS + NCONST + 1
         ELSE
            FIELD = NAMERA // 'RA'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .GT. 0 ) THEN
               JRANGE = INLIST( IFIELD )
            ELSE
               INFORM = 48
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2480 ) NAMERA
               GO TO 800
            END IF
         END IF
      ELSE
         JRANGE = 0
      END IF

!  INITIALIZE THE VECTOR OF CONSTANTS, B, AS ITS DEFAULT VALUE.

      DO 210 I = 1, NG
! ** Correction 1c. 6 lines added
        IF ( GNAMES ( I ) .EQ. CQGROU ) THEN
         B( I ) = ZERO
         BL( N + I ) = ZERO
         BU( N + I ) = BIGINF
         GSCALE( I ) = ONE
        ELSE
         B( I ) = DFAULT( JCONST )

!  INITIALIZE LOWER AND UPPER BOUNDS ON SLACK VARIABLES AS ZERO
!  AND THE DEFAULT RESPECTIVELY.

         BL( N + I ) = ZERO
         IF ( NRANGE .EQ. 0 ) THEN
            BU( N + I ) = BIGINF
         ELSE
            BU( N + I ) = DFAULT( JRANGE )
         END IF

!  RECORD THE GROUP SCALE FACTORS.

         GSCALE( I ) = ONE / RSCALE( I )
! ** Correction 1d. 1 line added
        END IF
  210 CONTINUE

!  SWEEP THROUGH THE ENTRIES OF A. LOOK FOR ENTRIES IN COLUMNS
!  JCONST AND JRANGE. SUBSEQUENTLY REMOVE ALL CONSTANT/RHS AND
!  RANGE COLUMNS TO LEAVE ONLY ENTRIES CORRESPONDING TO LINEAR
!  ELEMENTS.

      NNZ = 0
      DO 220 K = 1, NNZA
         I = ICOORD( K, 1 )
         J = ICOORD( K, 2 )
         AVALUE = A( K )

!  SEE IF THE ENTRY BELONGS TO THE SELECTED CONSTANT/RHS VECTOR.

         IF ( J .EQ. JCONST ) B( I ) = AVALUE

!  SEE IF THE ENTRY BELONGS TO THE SELECTED RANGE VECTOR.

         IF ( J .EQ. JRANGE ) BU( N + I ) = AVALUE

!  CHECK IF THE ENTRY BELONGS TO A LINEAR ELEMENT.

         IF ( J .LE. NLVARS ) THEN
            NNZ = NNZ + 1

!  RECORD THE COORDINATES AND VALUE OF THE ENTRY FROM THE LINEAR
!  ELEMENT.

            ICOORD( NNZ, 1 ) = I
            ICOORD( NNZ, 2 ) = J
            A( NNZ ) = AVALUE
         END IF
  220 CONTINUE
      NNZA = NNZ

!  THE MATRIX IS STORED IN COORDINATE FORM. RESORT IT SO THAT
!  IT IS STORED BY ROWS.

      IF ( NNZA .GT. 0 ) CALL REORDA( NG, NNZA, ICOORD( 1, 2 ),  &
                                      ICOORD( 1, 1 ), A, ISTADA, IWK )

!  DECODE THE 'D'-GROUPS/ROWS. SET THE WORKSPACE ARRAY WK TO ZERO.

      NNZ = 0
      NSLACK = 0
      DO 310 I = 1, N
         WK( I ) = RZERO
  310 CONTINUE

!  GXEQX IS TRUE IF THE GROUP FUNCTION IS TRIVIAL.

      DO 315 IG = 1, NG
        GXEQX( IG ) = ITYPEG( IG ) .EQ. 0
  315 CONTINUE

!  SET THE COEFFICIENTS OF THE LINEAR ELEMENTS.
!  --------------------------------------------

!  CONSIDER THE GROUPS IN ORDER.

      DO 390 IG = 1, NG
         K1 = ISTADA( IG )
         ISTADA( IG ) = NNZ + 1
         IF ( ISTATE( IG ) .LE. 4 ) THEN

!  First pass: determine the nonzeros in the row

            DO 320 K = k1, ISTADA( IG + 1 ) - 1
               J = ICOORD( K, 2 )
               IF ( WK( J ) .NE. RZERO ) THEN
                  WK( J ) = WK( J ) + A( K )
               ELSE
                  WK( J ) = A( K )
               END IF
  320       CONTINUE

!  Second pass: only record nonzeros

            DO 325 K = K1, ISTADA( IG + 1 ) - 1
               J = ICOORD( K, 2 )
               IF ( WK( J ) .NE. RZERO ) THEN
                  NNZ = NNZ + 1
                  ICNA( NNZ ) = J
                  ABYROW( NNZ ) = WK( J )
                  WK( J ) = RZERO
!               ELSE
!                 write(6,*) ' duplicate or zero removed ', J, A( K )
               END IF
  325       CONTINUE
         ELSE

!  THE IG-TH GROUP IS A 'D'-GROUP. CONSTRUCT THE NEW GROUP FROM ITS
!  TWO DONORS. CONSIDER EACH DONOR ROW IN TURN. FORM THE NEW ROW IN WK.

            DO 340 I = 1, 2
               IROW = IDROWS( I, IG )
               RROW = RDROWS( I, IG )
               DO 330 K = ISTADA( IROW ), ISTADA( IROW + 1 ) - 1
                  IC = ICNA( K )
                  IF ( IC .LE. NLVARS ) WK( IC ) = WK( IC ) +&
                                   ABYROW( K ) * RROW
  330          CONTINUE
  340       CONTINUE

!  MOVE THE NEW ROW INTO ABYROW, RESETTING WK TO ZERO AS WE PROCEED.

            DO 360 I = 1, 2
               IROW = IDROWS( I, IG )
               DO 350 K = ISTADA( IROW ), ISTADA( IROW + 1 ) - 1
                  IC = ICNA( K )
                  IF ( IC .LE. NLVARS ) THEN
                     IF ( WK( IC ) .NE. RZERO ) THEN
                        NNZ = NNZ + 1
                        ICNA( NNZ ) = IC
                        ABYROW( NNZ ) = WK( IC )
                        WK( IC ) = RZERO
                     END IF
                  END IF
  350          CONTINUE
  360       CONTINUE
         END IF

!  IF THE GROUP IS OF TYPE 'L' OR 'G', INSERT A SLACK VARIABLE IN
!  THE LINEAR ELEMENT.

         IS = ISTATE( IG )
         IF ( IS .GT. 4 ) IS = IS - 4
         IF ( IALGOR .LE. 2 ) THEN
            IF ( IS .NE. 1 .AND. IS .NE. 2 ) THEN
               IF ( .NOT. GXEQX( IG ) ) THEN
                 WRITE( IOUT, 1990 )
                 INFORM = 51
                 GO TO 800
               END IF
               NNZ = NNZ + 1
               NSLACK = NSLACK  + 1
               JCOL = N + NSLACK
               ICNA( NNZ ) = JCOL
               IF ( IS .EQ. 3 ) THEN
                  ABYROW( NNZ ) =   ONE
               ELSE
                  ABYROW( NNZ ) = - ONE
               END IF

!  GIVE THE SLACK VARIABLE THE SAME NAME AS ITS CORRESPONDING GROUP.

               VNAMES( JCOL ) = GNAMES( IG )

!  ASSIGN THE CORRECT BOUNDS FOR THE SLACK VARIABLE.

               BL( JCOL ) = BL( N + IG )
               BU( JCOL ) = BU( N + IG )
            END IF
         ELSE
            IF ( IS .EQ. 3 ) BL( N + IG ) = - BU( N + IG )
            IF ( IS .EQ. 1 .OR. IS .EQ. 2 .OR. IS .EQ. 3 )  &
                             BU( N + IG ) = ZERO
         END IF
         ISTATE( IG ) = IS
  390 CONTINUE

!  RESET THE NUMBER OF VARIABLES TO INCLUDE THE SLACKS.

      N = N + NSLACK
      NNZA = NNZ + 1
      NG1 = NG + 1
      ISTADA( NG1 ) = NNZA
      IF ( DEBUG .AND. IOUT .GT. 0 ) WRITE( IOUT, 3020 )  &
          ( ( I, ICNA( K ), ABYROW( K ), K = ISTADA( I ),  &
              ISTADA( I + 1 ) - 1 ), I = 1, NG )

!  SELECT THE STARTING POINT FOR THE MINIMIZATION.
!  ----------------------------------------------

      IF ( NSTART .EQ. 1 ) THEN
         JSTART = 1
      ELSE

!  FIND THE KEY WORD IN THE LIST OF STARTING POINTS.

         DO 410 JSTART = 1, NSTART
            IF ( NAMEST .EQ. SNAMES( JSTART ) ) GO TO 420
  410    CONTINUE
         INFORM = 49
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2490 ) NAMEST
         GO TO 800
  420    CONTINUE
      END IF

!  RECORD THE STARTING POINT.

      DO 430 J = 1, NLVARS
         X( J ) = VSTART( J, JSTART )
  430 CONTINUE

!  INITIALIZE ALL SLACK AND NONLINEAR VARIABLES AS ZERO, WITH WEIGHT 1.

      DO 440 J = NLVARS + 1, N
         X( J ) = ZERO
         VSCALE( J ) = ONE
  440 CONTINUE

!  RECORD THE LAGRANGE MULTIPLIERS, IF ANY.

      IF ( IALGOR .GE. 2 ) THEN
         DO 450 IG = 1, NG
            CLMULT( IG ) = CSTART( IG, JSTART )
  450    CONTINUE
      END IF

!  NONLINEAR ELEMENT INFORMATION.
!  ------------------------------

!  THE PARAMETER VALUES FOR THE NONLINEAR ELEMENTS MAY BE UNORDERED.
!  ORDER THE PARAMETERS SO THAT THOSE FOR GROUP I PRECEDE THOSE
!  FROM GROUP I+1. PLACE THE REORDERED SET IN WK.

      J = 1
      DO 520 IG = 1, NG
         K = ITYPEG( IG )
         ISTGP( IG ) = J
         IF ( K .GT. 0 ) THEN
            K1 = IGPA( K + 1 ) - IGPA( K )
            K2 = ISTGP( IG ) - 1
            DO 510 I = 1, K1
               WK( J ) = GPVALU( K2 + I )
               J = J + 1
  510       CONTINUE
         END IF
  520 CONTINUE
      ISTGP( NG1 ) = J

!  OVERWRITE GPVALU WITH WK.

      DO 530 I = 1, J - 1
         GPVALU( I ) = WK( I )
  530 CONTINUE

!  RECORD THE SCALE FACTORS FOR THE NONLINEAR ELEMENTS.

      DO 540 I = 1, ISTADG( NG1 ) - 1
         ESCALE( I ) = WEIGHT( I )
  540 CONTINUE
      DO 550 I = 1, NELNUM

!  DETERMINE WHETHER THE NONLINEAR ELEMENTS HAVE INTERNAL
!  REPRESENTATIONS.

         ITYPE = ITYPEE( I )
         NELV = IELV( ITYPE + 1 ) - IELV( ITYPE )
         NINV = IINV( ITYPE + 1 ) - IINV( ITYPE )
         INTREP( I ) = NINV .LT. NELV

!  STORE THE NUMBER OF INTERNAL VARIABLES FOR EACH ELEMENT.

         INTVAR( I ) = NINV
  550 CONTINUE
      IF ( IALGOR .GE. 2 ) THEN

!  SELECT THE OBJECTIVE FUNCTION GROUP.
!  ------------------------------------

!  FIND THE NAMED OBJECTIVE FUNCTION GROUP IN THE LIST.
!  MARK THE REMAINING OBJECTIVE FUNCTION GROUPS FOR REMOVAL.

         IF ( ONEOBJ ) THEN
            NOBJGR = 0
            DO 610 I = 1, NOBJ
               FIELD = ONAMES( I ) // 'GR'
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               K = INLIST( IFIELD )
               IF ( NAMEOF .EQ. ONAMES( I ) ) THEN
                  NOBJGR = K
                  IF ( IOUT .GT. 0 .AND. DEBUG )  &
                       WRITE( IOUT, 3010 ) ONAMES( I ), K
               ELSE
                  ISTADG( K ) = - ISTADG( K )
                  IF ( IOUT .GT. 0 .AND. DEBUG )  &
                       WRITE( IOUT, 3000 ) ONAMES( I )
               END IF
  610       CONTINUE

!  REMOVE REDUNDANT GROUP INFORMATION.

             IF ( NOBJ .GT. 1 .OR.&
                ( NOBJ .EQ. 1 .AND. NOBJGR .EQ. 0 ) ) THEN
               NNZ = 1
               NEL = 1
               NGPV = 1
               NGR = 0
               DO 650 I = 1, NG
                  IF ( ISTADG( I ) .GT. 0 ) THEN
                     NGR = NGR + 1
                     IF ( I .EQ. NOBJGR ) NOBJGR = NGR

!  SHIFT THE GROUP STATUS, NAME, TYPE, CONSTANT,
!  TRIVIALITY INDICATOR, LAGRANGE MULTIPLIER AND WEIGHT.

                     IF ( IALGOR .GE. 2 ) ISTATE( NGR ) = ISTATE( I )
                     GNAMES( NGR ) = GNAMES( I )
                     ITYPEG( NGR ) = ITYPEG( I )
                     B( NGR ) = B( I )
                     GXEQX( NGR ) = GXEQX( I )
                     IF ( IALGOR .GE. 2 ) CLMULT( NGR ) = CLMULT( I )
                     GSCALE( NGR ) = GSCALE( I )

!  SHIFT THE LIST OF ELEMENTS AND WEIGHTS IN THE I-TH GROUP.

                     K1 = ISTADG( I )
                     ISTADG( NGR ) = NEL
                     DO 620 K = K1, ABS( ISTADG( I + 1 ) ) - 1
                        IELING( NEL ) = IELING( K )
                        ESCALE( NEL ) = ESCALE( K )
                        NEL = NEL + 1
  620                CONTINUE

!  SHIFT THE LIST OF PARAMETERS IN THE I-TH GROUP.

                     K1 = ISTGP( I )
                     ISTGP( NGR ) = NGPV
                     DO 630 K = K1, ISTGP( I + 1 ) - 1
                        GPVALU( NGPV ) = GPVALU( K )
                        NGPV = NGPV + 1
  630             CONTINUE

!  SHIFT THE LIST OF COEFFICIENTS AND POSITIONS OF THE NONZEROS
!  FOR THE LINEAR ELEMENT IN THE I-TH GROUP.

                     K1 = ISTADA( I )
                     ISTADA( NGR ) = NNZ
                     DO 640 K = K1, ISTADA( I + 1 ) - 1
                        A( NNZ ) = A( K )
                        ICNA( NNZ ) = ICNA( K )
                        NNZ = NNZ + 1
  640                CONTINUE
                  END IF
  650          CONTINUE
               NG = NGR
               ISTADG( NG + 1 ) = NEL
               ISTGP ( NG + 1 ) = NGPV
               ISTADA( NG + 1 ) = NNZ
            END IF
         END IF
      END IF

!  SET THE REQUIRED LOWER AND UPPER BOUNDS ON THE OBJECTIVE FUNCTION.

      IF ( NOBBND .EQ. 0 ) THEN
         OBFBND( 1 ) = - BIGINF
         OBFBND( 2 ) =   BIGINF
      ELSE

!  FIND THE KEY WORD IN THE LIST OF STARTING POINTS.

         DO 660 JOBBND = 1, NOBBND
            IF ( NAMEOB .EQ. OBNAME( JOBBND ) ) GO TO 670
  660    CONTINUE
         INFORM = 50
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2500 ) NAMEOB
         GO TO 800
  670    CONTINUE
         OBFBND( 1 ) = FBOUND( 1, JOBBND )
         OBFBND( 2 ) = FBOUND( 2, JOBBND )
      END IF

!  IF NO OUTPUT IS REQUIRED, EXIT.

      IF ( IOUTDA .LE. 0 ) GO TO 900
      NEL1 = NELNUM + 1
      WRITE( IOUTDA, 3180 ) N,  NG, NELNUM,     ISTADG( NG1  ) - 1,  &
                            ISTAEV( NEL1 ) - 1, ISTADA( NG1  ) - 1,  &
                            ISTGP ( NG1  ) - 1, ISTEP ( NEL1 ) - 1,  &
                            NELTYP, NGRTYP 

!  PRINT OUT PROBLEM DATA. OUTPUT THE NUMBER OF VARIABLES, GROUPS AND
!  ELEMENTS AND, PERHAPS, THE IDENTITY OF THE OBJECTIVE FUNCTION GROUP.

      WRITE( IOUTDA, 3100 ) IALGOR, PNAME, IAUTO
      IF ( IALGOR .EQ. 2 ) WRITE( IOUTDA, 3170 ) NSLACK, NOBJGR

!  OUTPUT THE STARTING ADDRESSES OF THE ELEMENTS IN EACH GROUP,
!  OF THE PARAMETERS USED FOR EACH GROUP AND
!  OF THE NONZEROS OF THE LINEAR ELEMENT IN EACH GROUP.

      WRITE( IOUTDA, 3110 ) ( ISTADG( I ), I = 1, NG1 )
      WRITE( IOUTDA, 3110 ) ( ISTGP ( I ), I = 1, NG1 )
      WRITE( IOUTDA, 3110 ) ( ISTADA( I ), I = 1, NG1 )

!  OUTPUT THE STARTING ADDRESSES OF THE VARIABLES AND PARAMETERS
!  IN EACH ELEMENT.

      WRITE( IOUTDA, 3110 ) ( ISTAEV( I ), I = 1, NEL1 )
      WRITE( IOUTDA, 3110 ) ( ISTEP( I ), I = 1, NEL1 )

!  OUTPUT THE GROUP TYPE OF EACH GROUP AND ITS STATUS.

      WRITE( IOUTDA, 3110 ) ( ITYPEG( I ), I = 1, NG )
      IF ( IALGOR .GE. 2 ) WRITE( IOUTDA, 3110 )( ISTATE( I ), I = 1,NG)

!  OUTPUT THE ELEMENT TYPE OF EACH ELEMENT.

      WRITE( IOUTDA, 3110 ) ( ITYPEE( I ), I = 1, NELNUM )

!  OUTPUT THE ELEMENT TYPE OF EACH ELEMENT
!  AND ITS NUMBER OF INTERNAL VARIABLES.

      WRITE( IOUTDA, 3110 ) ( INTVAR( I ), I = 1, NELNUM )

!  OUTPUT THE IDENTITY OF EACH INDIVIDUAL ELEMENT.

      WRITE( IOUTDA, 3110 ) ( IELING( I ), I = 1, ISTADG( NG1 ) - 1 )

!  OUTPUT THE VARIABLES IN EACH GROUP'S ELEMENTS.

      WRITE( IOUTDA, 3110 ) ( IELVAR( I ), I = 1, ISTAEV( NEL1 ) - 1 )

!  OUTPUT THE COLUMN ADDRESSES OF THE NONZEROS IN EACH LINEAR ELEMENT.

      NNZA = ISTADA( NG1 ) - 1
      WRITE( IOUTDA, 3110 ) ( ICNA( I ), I = 1, NNZA )

!  WRITE SINGLE PRECISION FORMAT

      IF ( SINGLE ) THEN

!  OUTPUT THE VALUES OF THE NONZEROS IN EACH LINEAR ELEMENT, THE
!  CONSTANT TERM IN EACH GROUP, THE LOWER AND UPPER BOUNDS ON
!  THE VARIABLES AND THE STARTING POINT FOR THE MINIMIZATION

         WRITE( IOUTDA, 3121 ) ( ABYROW( I ), I = 1, NNZA )
         WRITE( IOUTDA, 3121 ) ( B( I ), I = 1, NG )
         IF ( IALGOR .LE. 2 ) THEN
            WRITE( IOUTDA, 3121 ) ( BL( I ), I = 1, N )
            WRITE( IOUTDA, 3121 ) ( BU( I ), I = 1, N )
         ELSE
            WRITE( IOUTDA, 3121 ) ( BL( I ), I = 1, N + NG )
            WRITE( IOUTDA, 3121 ) ( BU( I ), I = 1, N + NG )
         END IF
         WRITE( IOUTDA, 3121 ) ( X( I ), I = 1, N )
         IF ( IALGOR .GE. 2 ) WRITE( IOUTDA, 3121 )( CLMULT( I ), &
                                                     I = 1, NG )

!  OUTPUT THE PARAMETERS IN EACH GROUP.

         WRITE( IOUTDA, 3121 ) ( GPVALU( I ), I = 1, ISTGP( NG1 ) - 1 )

!  OUTPUT THE PARAMETERS IN EACH INDIVIDUAL ELEMENT.

         WRITE( IOUTDA, 3121 ) ( EPVALU( I ), I = 1, ISTEP( NEL1 ) - 1 )

!  OUTPUT THE SCALE FACTORS FOR THE NONLINEAR ELEMENTS.

         WRITE( IOUTDA, 3121 ) ( ESCALE( I ), I = 1, ISTADG( NG1 ) - 1 )

!  OUTPUT THE SCALE FACTORS FOR THE GROUPS.

         WRITE( IOUTDA, 3121 ) ( GSCALE( I ), I = 1, NG )

!  OUTPUT THE SCALE FACTORS FOR THE VARIABLES.

         WRITE( IOUTDA, 3121 ) ( VSCALE( I ), I = 1, N )

!  OUTPUT THE LOWER AND UPPER BOUNDS ON THE OBJECTIVE FUNCTION.

         WRITE( IOUTDA, 3161 ) OBFBND( 1 ), OBFBND( 2 )

!  WRITE DOUBLE PRECISION FORMAT

      ELSE

!  OUTPUT THE VALUES OF THE NONZEROS IN EACH LINEAR ELEMENT, THE
!  CONSTANT TERM IN EACH GROUP, THE LOWER AND UPPER BOUNDS ON
!  THE VARIABLES AND THE STARTING POINT FOR THE MINIMIZATION

         WRITE( IOUTDA, 3120 ) ( ABYROW( I ), I = 1, NNZA )
         WRITE( IOUTDA, 3120 ) ( B( I ), I = 1, NG )
         IF ( IALGOR .LE. 2 ) THEN
            WRITE( IOUTDA, 3120 ) ( BL( I ), I = 1, N )
            WRITE( IOUTDA, 3120 ) ( BU( I ), I = 1, N )
         ELSE
            WRITE( IOUTDA, 3120 ) ( BL( I ), I = 1, N + NG )
            WRITE( IOUTDA, 3120 ) ( BU( I ), I = 1, N + NG )
         END IF
         WRITE( IOUTDA, 3120 ) ( X( I ), I = 1, N )
         IF ( IALGOR .GE. 2 ) WRITE( IOUTDA, 3120 )( CLMULT( I ), &
                                                     I = 1, NG )

!  OUTPUT THE PARAMETERS IN EACH GROUP.

         WRITE( IOUTDA, 3120 ) ( GPVALU( I ), I = 1, ISTGP( NG1 ) - 1 )

!  OUTPUT THE PARAMETERS IN EACH INDIVIDUAL ELEMENT.

         WRITE( IOUTDA, 3120 ) ( EPVALU( I ), I = 1, ISTEP( NEL1 ) - 1 )

!  OUTPUT THE SCALE FACTORS FOR THE NONLINEAR ELEMENTS.

         WRITE( IOUTDA, 3120 ) ( ESCALE( I ), I = 1, ISTADG( NG1 ) - 1 )

!  OUTPUT THE SCALE FACTORS FOR THE GROUPS.

         WRITE( IOUTDA, 3120 ) ( GSCALE( I ), I = 1, NG )

!  OUTPUT THE SCALE FACTORS FOR THE VARIABLES.

         WRITE( IOUTDA, 3120 ) ( VSCALE( I ), I = 1, N )

!  OUTPUT THE LOWER AND UPPER BOUNDS ON THE OBJECTIVE FUNCTION.

         WRITE( IOUTDA, 3160 ) OBFBND( 1 ), OBFBND( 2 )
      END IF

!  OUTPUT A LOGICAL ARRAY WHICH SAYS WHETHER AN ELEMENT HAS INTERNAL
!  VARIABLES.

      WRITE( IOUTDA, 3130 ) ( INTREP( I ), I = 1, NELNUM )

!  OUTPUT A LOGICAL ARRAY WHICH SAYS WHETHER A GROUP IS TRIVIAL.

      WRITE( IOUTDA, 3130 ) ( GXEQX( I ), I = 1, NG )

!  OUTPUT THE NAMES GIVEN TO THE GROUPS AND TO THE VARIABLES.

      WRITE( IOUTDA, 3140 ) ( GNAMES( I ), I = 1, NG )
      WRITE( IOUTDA, 3140 ) ( VNAMES( I ), I = 1, N )

!  OUTPUT THE NAMES GIVEN TO THE ELEMENT AND GROUP TYPES.

      WRITE( IOUTDA, 3140 ) ( ETYPES( I ), I = 1, NELTYP )
      WRITE( IOUTDA, 3140 ) ( GTYPES( I ), I = 1, NGRTYP )

!  OUTPUT THE TYPE OF EACH VARIABLE.

      WRITE( IOUTDA, 3110 ) ( ITYPEV( I ), I = 1, N )
      GO TO 900

!  INCORRECT DATA SPECIFIED.

  800 CONTINUE
      RETURN

!  SUCCESSFUL RETURN

  900 CONTINUE
      INFORM = 0
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 1990 FORMAT( ' ** Exit from INLANC - ', /, &
              ' Although the manual may suggest otherwise,',  &
              ' non-trivial',/,  &
              ' groups are not allowed for inequality constraints',/)
 2000 FORMAT( ' ** Exit from INLANC - increase one or more of NGPVMX,  &
                NELMAX and NGMAX ' )
 2460 FORMAT( ' ** Exit from INLANC - constant name ', A8,  &
              ' not recognised ' )
 2470 FORMAT( ' ** Exit from INLANC - bound name ', A8,  &
              ' not recognised ' )
 2480 FORMAT( ' ** Exit from INLANC - range name ', A8,  &
              ' not recognised ' )
 2490 FORMAT( ' ** Exit from INLANC - start point name ', A8,  &
              ' not recognised ' )
 2500 FORMAT( ' ** Exit from INLANC - obj. bound name ', A8,  &
              ' not recognised ' )
 3000 FORMAT( ' Group ', A10, ' removed as a redundant objective ' )
 3010 FORMAT( /, ' Objective function ', A10, ' is group number ', I8 )
 3020 FORMAT( /, 3('  Row   Col    Value  '),  &
              /, 3('  ---   ---    -----  '),  &
              /, ( 3( 2I5, 1P, D12.4 ) ) )
 3100 FORMAT( I2, A8, I2 )
 3110 FORMAT( ( 10I8 ) )
 3120 FORMAT( ( 1P, 4D16.8 ) )
 3121 FORMAT( ( 1P, 4E16.8 ) )
 3130 FORMAT( ( 72L1 ) )
 3140 FORMAT( ( 8A10 ) )
 3160 FORMAT( 1P, 2D16.8 )
 3161 FORMAT( 1P, 2E16.8 )
 3170 FORMAT( 2I8 )
 3180 FORMAT( 10I8 )

!  END OF INLANC.

      END

!  THIS VERSION: 26/02/2001 AT 09:30:00 AM.
!     ( Last modified on 15 Mar 2001 at 22:28:00 )
! ** Correction report.
! ** Correction -1. 03/03/00: Integer formats increased
! ** Correction 0. 20/12/99: Array holding variable types introduced.
! ** Correction 1. 13/01/94: 1 line corrected **
! ** Correction 2. 26/02/01: 3 dummy arguments removed **
! ** End of Correction report.
! ** Correction 2. 26/02/01: 5 dummy arguments removed **
      SUBROUTINE PRINTP( NMAX, NGMAX, NLMAX,  &
                         NELMAX, NETMAX,  &
                         NEVMAX, NEPMAX, NGRMAX, NEGMAX, NEPVMX,  &
                         NGPVMX, NGPMAX, LSTADA, LICNA, LIWK,  &
                         N, NG, NLVARS, NELNUM,  &
                         ISTATE, ISTADG, IELVAR, ITYPEG, ITYPEE,  &
                         IELV, IINV, IEPA, IGPA,  &
                         ISTADA, ICNA, ISTGP, ISTEP, ISTEV,
! ** Correction 0. 20/12/99: Array holding variable types introduced.&
                         IELING, ITYPEV, IWK,  &
                         ABYROW, B, BL, BU, X, EPVALU, GPVALU,  &
                         GSCALE, ESCALE, VSCALE,  &
                         PNAME, VNAMES, GNAMES,  &
                         LNAMES, ETYPES, ENAMES,  &
                         ANAMES, EPNAME, GPNAME, GTYPES,  &
                         IOUT, IPRINT )
      INTEGER       IOUT, IPRINT
      INTEGER       NMAX, NGMAX, NLMAX, NELMAX, NETMAX
      INTEGER       NEPMAX, NEVMAX, NGRMAX, LIWK
      INTEGER       N, NG
      INTEGER       NLVARS, NELNUM, LICNA, LSTADA
      INTEGER       NEGMAX, NEPVMX, NGPVMX, NGPMAX
      DOUBLE PRECISION EPVALU( NEPVMX ), GPVALU( NGPVMX )
      INTEGER       IELING( NEGMAX )
      INTEGER       ISTATE( NGMAX ), ISTADA( LSTADA ), ICNA( LICNA )
      INTEGER       IELV  ( NLMAX ), IINV  ( NLMAX )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
      INTEGER       IELVAR( NEVMAX ), ITYPEV( NMAX )
      INTEGER       ISTADG(  NGMAX ), ITYPEG( NGMAX ), ITYPEE( NELMAX )
      INTEGER       IEPA( NLMAX ), IGPA( NGRMAX ), IWK( LIWK )
! ** Correction 1. 13/01/94: 1 line corrected **
      INTEGER       ISTEP( NELMAX ), ISTEV( NELMAX ), ISTGP( NGMAX )
! ** Correction 1. 13/01/94:  end of correction **
      CHARACTER * 8  PNAME
      CHARACTER * 10 GNAMES( NGMAX ), VNAMES( NMAX )
      CHARACTER * 10 ETYPES( NLMAX ), LNAMES( NELMAX )
      CHARACTER * 10 ENAMES( NETMAX )
      CHARACTER * 10 ANAMES( NGRMAX ), GTYPES( NGRMAX )
      CHARACTER * 10 EPNAME( NEPMAX ), GPNAME( NGPMAX )
      DOUBLE PRECISION B( NGMAX ), BL( NMAX ), BU( NMAX ), X( NMAX )
      DOUBLE PRECISION GSCALE( NGMAX ), ESCALE( NEGMAX ), VSCALE( NMAX )
      DOUBLE PRECISION ABYROW( LICNA )

!  PRINT DETAILS OF THE PROBLEM PREVIOUSLY SPECIFIED IN AN SIF FILE.
!  ------------------------------------------------------------------

! THE LEVEL OF PRINTING PERFORMED IS DETERMINED BY THE VALUE OF IPRINT.
!  POSSIBLE VALUES ARE:

!  >= 1, A SIMPLE SUMMARY OF THE PROBLEM NAME, THE NUMBER OF VARIABLES,
!        GROUPS AND ELEMENTS.
!  >= 2, A LIST OF THE VARIABLES USED.
!  >= 3, A BREAKDOWN OF THE GROUPS. A LIST OF THE NONLINEAR ELEMENTS
!        USED, THE TYPES OF EACH GROUP, THE STATUS OF THE GROUP AND
!        A STATEMENT THAT THE GROUP USES OR DOES NOT USE A LINEAR
!        ELEMENT.
! = 4, FURTHER DETAILS OF EACH GROUP. THE NAME OF THE GROUP-TYPE
!        VARIABLE AND A LIST OF THE VALUES ASSOCIATED WITH EACH
!        PARAMETER.
!  >= 5, DETAILS OF EACH ELEMENT. THE NUMBERS OF ELEMENTAL AND
!        INTERNAL VARIABLES AND THE NUMBERS OF PARAMETERS.
!  >= 6, FURTHER DETAILS OF EACH ELEMENT. THE NAMES OF THE
!        ELEMENTAL VARIABLES TOGETHER WITH THEIR ASSOCIATED
!        PROBLEM VARIABLES, A LIST OF THE VALUES ASSOCIATED
!        WITH EACH PARAMETER AND THE VARIABLES INVOLVED IN
!        THE LINEAR ELEMENT.
!  >= 7, DETAILS OF THE COEFFICIENTS OF THE LINEAR ELEMENTS.
!  >= 8, FULL DETAILS OF THE VARIABLES USED INCLUDING THEIR LOWER
!        AND UPPER BOUNDS AND STARTING VALUES.
!  >= 9, ALL OF THE ABOVE.

!  NICK GOULD 28/07/1989
!  FOR CGT PRODUCTIONS.

      INTEGER I, IEL, IG, IS, J, K, K1, K2, K3, K4, K5, K6, L
      INTEGER IELTYP
      INTRINSIC ABS
      CHARACTER * 12 STATUS( 4 )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
      CHARACTER * 3 VARTYP( 3 )
      DATA VARTYP / '   ', '0-1', 'int' /
      DATA STATUS / 'an objective', 'an equality ',  &
                    'a negativity', 'a positivity' /&
                    
      IF ( IOUT .LE. 0 ) RETURN
      IF ( IPRINT .GE. 1 ) THEN
         DO 10 I = 1, NELNUM
            IWK( I ) = 0
   10    CONTINUE
         WRITE( IOUT, 2000 ) PNAME, N, N - NLVARS, NG, NELNUM

!  LIST OF VARIABLES.

         IF ( IPRINT .GE. 2 ) THEN
             WRITE( IOUT, 2010 ) ( VNAMES( J ), J = 1, NLVARS )
             IF ( NLVARS .LT. N )  &
                WRITE( IOUT, 2020 ) ( VNAMES( J ), J = NLVARS + 1, N )

!  GROUP DETAILS.

            IF ( IPRINT .GE. 3 ) THEN
               DO 500 IG = 1, NG
                  IS = ITYPEG( IG )
                  IF ( IS .EQ. 0 ) THEN
                     IF ( ISTATE( IG ) .EQ. 1 ) THEN
                       WRITE( IOUT, 2030 ) IG, GNAMES( IG ),  &
                       STATUS( 1 ), 'TRIVIAL   ', GSCALE( IG )
                     ELSE
                       WRITE( IOUT, 2030 ) IG, GNAMES( IG ),  &
                       STATUS( ISTATE( IG ) ),  &
                       'TRIVIAL   ', GSCALE( IG )
                     END IF
                  ELSE
                     IF ( ABS( ISTATE( IG ) ) .EQ. 1 ) THEN
                       WRITE( IOUT, 2030 ) IG, GNAMES( IG ),  &
                       STATUS( 1 ), GTYPES( IS ), GSCALE( IG )
                     ELSE
                       WRITE( IOUT, 2030 ) IG, GNAMES( IG ),  &
                       STATUS( ISTATE( IG ) ),  &
                       GTYPES( IS ), GSCALE( IG )
                     END IF
                  END IF
                  K1 = ISTADG( IG )
                  K2 = ISTADG( IG + 1 ) - 1
                  L = K2 - K1 + 1
                  IF ( K1 .LE. K2 ) THEN
                     IF ( K1 .EQ. K2 ) THEN
                        WRITE( IOUT, 2060 ) LNAMES( IELING( K1 ) )
                     ELSE
                        WRITE( IOUT, 2070 ) L,  &
                             ( LNAMES( IELING( K ) ), K = K1, K2 )
                     END IF
                  ELSE
                    WRITE( IOUT, 2080 )
                  END IF

!  FURTHER GROUP DETAILS.

                  IF ( IPRINT .EQ. 4 .OR. IPRINT .GE. 7 ) THEN
                     IF ( IS .GT. 0 ) THEN
                        K3 = IGPA( IS ) - 1
                        K4 = ISTGP( IG ) - 1
                        L = ISTGP( IG + 1 ) - K4 - 1
                        IF ( IS .GT. 0 ) THEN
                           IF ( L .EQ. 1 ) THEN
                              WRITE( IOUT, 2090 )  &
                                ANAMES( IS ), 'is ', L, '. '
                           ELSE
                              WRITE( IOUT, 2090 )  &
                                ANAMES( IS ), 'are', L, 's.'
                           END IF
                        END IF
                        IF ( L .GT. 0 )  &
                           WRITE( IOUT, 2100 ) ( GPNAME( K3 + I ),  &
                                  GPVALU( K4 + I ), I = 1, L )
                     END IF
                  END IF

!  ELEMENT DETAILS.

                  IF ( IPRINT .GE. 5 ) THEN
                     DO 400 K = K1, K2
                        IEL = IELING( K )
                        IELTYP = ITYPEE( IEL )
                        IF ( IWK( IEL ) .EQ. 0 ) THEN
                           WRITE( IOUT, 2110 ) LNAMES( IEL ),  &
                                  IEL, ETYPES( IELTYP ),  &
                                  IELV( IELTYP + 1 ) - IELV( IELTYP ),  &
                                  IINV( IELTYP + 1 ) - IINV( IELTYP ),  &
                                  IEPA( IELTYP + 1 ) - IEPA( IELTYP ),  &
                                  ESCALE( K )
                           IF ( IPRINT .LT. 6 ) IWK( IEL ) = 1
                        ELSE
                           WRITE( IOUT, 2120 ) LNAMES( IEL ), IEL,  &
                                  ESCALE( K )
                        END IF

!  FURTHER ELEMENT DETAILS.

                        IF ( IPRINT .GE. 6 ) THEN
                           IF ( IWK( IEL ) .EQ. 0 ) THEN
                              IWK( IEL ) = 1
                              K3 = IELV ( IELTYP ) - 1
                              K4 = ISTEV( IEL ) - 1
                              L = ISTEV( IEL + 1 ) - K4 - 1
                              WRITE( IOUT, 2130 ) ( ENAMES( K3 + I ),  &
                                     VNAMES( IELVAR( K4 + I ) ),  &
                                                          I = 1, L )
                              K3 = IEPA( IELTYP )  - 1
                              K4 = ISTEP( IEL ) - 1
                              L = ISTEP( IEL + 1 ) - K4 - 1
                              IF ( L .GT. 0 )  &
                                WRITE( IOUT, 2150 ) ( EPNAME( K3 + I ),  &
                                       EPVALU( K4 + I ), I = 1, L )
                           END IF
                        END IF
  400                CONTINUE
                  END IF

!  LINEAR ELEMENT DETAILS.

                  K5 = ISTADA( IG )
                  K6 = ISTADA( IG + 1 ) - 1
                  IF ( IPRINT .EQ. 6 ) THEN
                     IF ( K5 .LE. K6 ) THEN
                        IF ( K5 .EQ. K6 ) THEN
                           WRITE( IOUT, 2040 ) K6 - K5 + 1, '. '
                           IF ( IPRINT .GE. 6 ) WRITE( IOUT, 2140 ) ' ',  &
                             ( VNAMES( ICNA( K ) ), K = K5, K6 )
                        ELSE
                           WRITE( IOUT, 2040 ) K6 - K5 + 1, 's.'
                           IF ( IPRINT .GE. 6 ) WRITE( IOUT, 2140 ) 's',  &
                             ( VNAMES( ICNA( K ) ), K = K5, K6 )
                        END IF
                     ELSE
                        IF ( ABS( B( IG ) ) .LT. 1.0D-12 ) THEN
                           WRITE( IOUT, 2050 )
                        ELSE
                           WRITE( IOUT, 2160 )
                        END IF
                     END IF
                  ELSE

!  FURTHER LINEAR ELEMENT DETAILS.

                     IF ( K5 .LE. K6 ) THEN
                        IF ( K5 .EQ. K6 ) THEN
                           WRITE( IOUT, 2040 ) K6 - K5 + 1, '. '
                           IF ( IPRINT .GE. 6 ) WRITE( IOUT, 2200 ) ' ',  &
                             ') ', ( VNAMES( ICNA( K ) ),  &
                               ABYROW( K ), K = K5, K6 )
                        ELSE
                           WRITE( IOUT, 2040 ) K6 - K5 + 1, 's.'
                           IF ( IPRINT .GE. 6 ) WRITE( IOUT, 2200 ) 's',  &
                             's)', ( VNAMES( ICNA( K ) ),  &
                               ABYROW( K ), K = K5, K6 )
                        END IF
                        IF ( ABS( B( IG ) ) .LT. 1.0D-12 ) THEN
                           WRITE( IOUT, 2230 )
                        ELSE
                           WRITE( IOUT, 2220 ) B( IG )
                        END IF
                     ELSE
                        IF ( ABS( B( IG ) ) .LT. 1.0D-12 ) THEN
                           WRITE( IOUT, 2050 )
                        ELSE
                           WRITE( IOUT, 2210 ) B( IG )
                        END IF
                     END IF
                  END IF
  500          CONTINUE
            END IF
         END IF
      END IF
      IF ( IPRINT .GE. 8 ) THEN
         WRITE( IOUT, 2170 )
         DO 510 I = 1, N
! ** Correction 0. 20/12/99: Array holding variable types introduced.
            WRITE( IOUT, 2180 ) I, VNAMES( I ), BL( I ), X( I ),  &
                                BU( I ), VSCALE( I ), &
                                VARTYP( ITYPEV( I ) + 1 )
  510    CONTINUE
      END IF
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 2000 FORMAT( /, ' Problem name ', A8, /,  &
              /, ' There are ', I8, ' VARIABLES of which ', I8,  &
                 ' are artificials',  &
              /, ' There are ', I8, ' GROUPS',  &
              /, ' There are ', I8, ' NONLINEAR ELEMENTS ' )
 2010 FORMAT( /, ' Names of problem variables ',  &
              /, ' ----- -- ------- --------- ',  &
              /, 7( 1X, A10 ) )
 2020 FORMAT( /, ' Names of artificial variables ',  &
              /, ' ----- -- ---------- --------- ',  &
              /, 7( 1X, A10 ) )
! ** Correction -1a. 03/03/00: Integer formats increased
 2030 FORMAT( /, ' Group ', I8, ' is named ', A10, /, '  * It is ',  &
              A12, ' group of type ', A10,  &
              /, '  * The group is scaled by the factor ', 1P, D12.4 )
 2040 FORMAT( '  * The group has a LINEAR ELEMENT with ', I6,  &
              ' variable', A2 )
 2050 FORMAT( '  * The group has no LINEAR ELEMENT. ' )
 2060 FORMAT( '  * The group uses a single NONLINEAR',  &
                 ' ELEMENT.  This is element ', A10 )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
 2070 FORMAT( '  * The group uses ', I5, ' NONLINEAR',  &
              ' ELEMENTS. These are elements ', A10,  &
               /, ( 3X, 6( 1X, A10 ) ) )
 2080 FORMAT( '  * The group uses no NONLINEAR ELEMENTS. ' )
! ** Correction -1b. 03/03/00: Integer format increased
 2090 FORMAT( '  * The group-type argument is ', A10, ' and there ', A3,  &
                 I8, ' parameter', A2 )
 2100 FORMAT( ( '    * Group parameter ', A10, ' has the value ',  &
                1P, D12.4, '.' ) )
! ** Correction -1c. 03/03/00: Integer format increased
 2110 FORMAT(  '  * Group uses nonlinear element ', A10,  &
               ' number ', I5, ' of type ', A10, /,  &
               '    * No. elemental variables =', I8,  &
               '. No. internal variables =', I8, '.',  &
               /, '    * No. parameter values =', I8, '.', /,  &
               '    * The element is scaled by the factor ', 1P, D12.4 )
 2120 FORMAT( '  * Group uses nonlinear element ', A10,  &
              ' number ', I5, ' described above. ', /,  &
              '    * The element is scaled by the factor ', 1P, D12.4 )
 2130 FORMAT( ( '    * Elemental variable ', A10, '  is assigned',  &
                ' problem variable ' , A10 ) )
 2140 FORMAT( '    * The linear element uses variable', A1, 1X, 3A10,  &
              /, ( 6X, 6A10 ) )
 2150 FORMAT( ( '    * Elemental parameter ', A10, ' has the value ',  &
                1P, D12.4, '.' ) )
 2160 FORMAT( '  * The group has a constant LINEAR ELEMENT. ' )
! ** Correction 0. 20/12/99: Array holding variable types introduced.
 2170 FORMAT( /, '  #  variable name ',  &
                 'lower bound start value upper bound scale factor',  &
              ' type', /, '   -  ------------- ',  &
                 '----------- ----------- ----------- ------------',  &
              ' ----' )
 2180 FORMAT( I5, 2X, A10, 1X, 1P, 4D12.4, 3X, A3 )
 2200 FORMAT( '    * The linear element uses variable', A1,  &
              ' (with coefficient', A2, &
              /, ( 6X, 3( A10, 1X, 1P, D10.2, 1X ) ) )
 2210 FORMAT( '    * The group has a constant LINEAR ELEMENT with',  &
              ' coefficient ', 1P, D10.2 )
 2220 FORMAT( '    * The constant term for the LINEAR ELEMENT has',  &
              ' coefficient ', 1P, D10.2 )
 2230 FORMAT( '    * There is no constant term for the LINEAR ELEMENT' )

!  END OF PRINTP.

      END

!  THIS VERSION: 26/02/2001 AT 09:30:00 AM.
!     ( Last modified on 15 Mar 2001 at 22:28:00 )
! ** Correction report.
! ** Correction -4. 13/02/02: Combine arguments in RANGE
! ** Correction -3. 14/01/02: Add arguments to RANGE/ELFUN (and change names)
! ** Correction -2. 29/12/01: Remove SETTYP and add extra argument to RANGES
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
! ** Correction 1. 19/07/93: 12 lines added **
! ** Correction 2. 19/07/93: 2 lines added **
! ** Correction 3. 12/01/94: 1 line corrected **
! ** Correction 4. 12/01/94: 1 line corrected **
! ** Correction 5. 21/02/94: 1 line corrected **
! ** Correction 6. 21/02/94: 1 line corrected, 2 lines added **
! ** Correction 7. 15/08/95: 3 lines corrected **
! ** Correction 8. 15/08/95: 3 lines corrected **
! ** Correction 9. 26/02/01: 2 dummy arguments removed **
! ** End of Correction report.
      SUBROUTINE MAKEFN( INPUT , IOUT  , IOUTFN, IOUTRA, INFORM,
! ** Correction 9. 26/02/01: 2 dummy arguments removed **&
                         NLMAX , NIMAX , NETMAX, NINMAX, NUMAX ,  &
                         NEL   , NELTYP, PNAME , ENAMES, INAMES, RENAME,  &
                         INNAME, LONAME, MINAME, EXNAME, ETYPES, LDEFND,  &
                         LENGTH, ITABLE, KEY   , IELV  , IINV  , INLIST,  &
                         EPNAME, IEPA  , NEPMAX, DEBUG , IJUMP ,  &
                         U     , SETVEC, NSETVC, SINGLE, NULINE, GOTLIN,  &
                         IPRINT )
      INTEGER            INPUT , IOUT  , IOUTFN, IOUTRA, INFORM
      INTEGER            NLMAX , NIMAX , NETMAX, NELTYP, NINMAX
      INTEGER            NEPMAX, LENGTH, NSETVC, NEL   , NUMAX , IPRINT
      LOGICAL            DEBUG , SINGLE, GOTLIN
      INTEGER            ITABLE( LENGTH ), IJUMP ( NLMAX  )
      INTEGER            IELV  ( NLMAX  ), IINV  ( NLMAX  )
      INTEGER            INLIST( LENGTH )
      INTEGER            IEPA  ( NLMAX  )
      LOGICAL            LDEFND( NLMAX  ), SETVEC( NSETVC )
      DOUBLE PRECISION   U     ( NUMAX  )
      CHARACTER * 12     KEY   ( LENGTH )
      CHARACTER * 8      PNAME
      CHARACTER * 10     EPNAME( NEPMAX ), EXNAME( NINMAX )
      CHARACTER * 10     INAMES( NIMAX  ), RENAME( NINMAX )
      CHARACTER * 10     LONAME( NINMAX ), INNAME( NINMAX )
      CHARACTER * 10     MINAME( NINMAX ), ENAMES( NETMAX )
      CHARACTER * 10     ETYPES( NLMAX  )
      CHARACTER * 160    NULINE

!  MAKE A FUNCTION EVALUATION SUBROUTINE AND A RANGE TRANSFORMATION
!  ----------------------------------------------------------------
!  SUBROUTINE FROM A GPS FUNCTION DATA FILE.
!  -----------------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

!  -------------------------------------------------------------------

!  FUNCTION INDICATOR CARDS.
!  -------------------------

!  DEFINITION   PURPOSE.
!  ----------   --------
!  ELEMENTS     PROBLEM NAME.
!  TEMPORARIES  NAMES OF ADDITIONAL PARAMETERS USED IN FUNCTION DEFS.
!  GLOBALS      GENERAL PARAMETER ASSIGNMENTS.
!  INDIVIDUALS  DEFINE THE TRANSFORMATION FROM THE ELEMENTAL TO THE
!               INTERNAL VARIABLES FOR ALL ELEMENTS WITH INTERNAL VARS.
!               SET FUNCTION AND DERIVATIVE VALUES AND MAKE
!               ELEMENT SPECIFIC PARAMETER ASSIGNMENTS.
!  ENDATA       END OF INPUT DATA.

!  DATA CARD DESCRIPTION.
!  ----------------------

!  SEE 'A PROPOSAL FOR A STANDARD DATA INPUT FORMAT FOR LARGE-SCALE
!       NONLINEAR PROGRAMMING PROBLEMS', SECTION 3,
!       A. R. CONN, N. I. M. GOULD AND PH. L. TOINT, 
!       REPORT CS-89-61, DEPT OF COMPUTER SCIENCE, U. OF WATERLOO,
!       WATERLOO, ONTARIO, N2L3G1, CANADA.

!  -------------------------------------------------------------------
!  RETURNS WITH NEGATIVE VALUES OF INFORM INDICATE THAT INSUFFICIENT
!  ARRAY SPACE HAS BEEN ALLOWED, AS FOLLOWS:

!    INFORM = - 1  WHEN LENGTH NOT LARGE ENOUGH
!    INFORM = - 2  WHEN MAX( NINNAM, NRENAM, NLONAM, NENAM, NMINAM )
!                  .GT. NINMAX
!    INFORM = - 11 WHEN NUMAX NOT LARGE ENOUGH
!    INFORM = - 12 WHEN NSETVC NOT LARGE ENOUGH

      INTEGER          IFIELD, IFREE, IHVAR, IVAR, INTYPE, NMINAM
      INTEGER          JVAR, K, K1, K2, NH, NHESS, NVARS, ISETTY
      INTEGER          I, NINAME, NINNAM, NINVAR, NLOOP, NRENAM, NEXNAM
      INTEGER          ITYPE, J, IS, JS, NOVALS, NELV, NINV, NN, NENAME
      INTEGER          NPNAME, NINCRS, LINENO, IIRES, ILINES, NLINES
      INTEGER          MBLANK, MFIXED, MFREE, MNAME, MTEMP, MGLOB
      INTEGER          MINDIV, MENDAT, MAXNUL, MXRECL, NLONAM
      DOUBLE PRECISION ZERO, VALUES( 2 )
      LOGICAL          NOINTE, DEFNAM, ENDPAR, ENDGEN, FIRSTL, SETRAN
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      LOGICAL          STARTF, STARTG, STARTH, STARTP, QPROD
      LOGICAL          ENDOFF, ENDOFG, ENDOFH, FIXED, NOMORG
      PARAMETER        ( IIRES = 32 )
! ** Correction 5. 21/02/94: 1 line corrected **
      PARAMETER        ( NINCRS = 12 )
! ** Correction 5. 21/02/94: end of correction **
      CHARACTER * 2    FIELD1
      CHARACTER * 6    INCRSE( NINCRS )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      CHARACTER * 10   FIELD2
      CHARACTER * 8    FIELD3, FIELDS( 2 ), FIELDI( IIRES )
      CHARACTER * 12   FIELD
      CHARACTER * 41   FIELD7
      INTRINSIC        MIN
      EXTERNAL         HASHB , HASHC , GETVAL, OUTRAN

!  PARAMETER DEFINITIONS.

      PARAMETER        ( MXRECL = 160 )
      CHARACTER * 160    BLNKLN
      PARAMETER        ( MBLANK =  1, MFIXED =  2, MFREE = 3  )
      PARAMETER        ( MNAME =  4, MTEMP =  5 )
      PARAMETER        ( MGLOB =  6, MINDIV =  7, MENDAT =  8 )
      INTEGER          LENIND( MENDAT )
      CHARACTER * 12   INDIC8( MENDAT ), HEADER
      PARAMETER        ( MAXNUL = 20 )
      CHARACTER * 65   NULINA( MAXNUL )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      CHARACTER * 10   CQSQR, CQPROD
      PARAMETER      ( CQSQR = '123456789S' )
      PARAMETER      ( CQPROD = '123456789P' )

!  DATA DECLARATIONS.

! ** Correction 6. 21/02/94: 1 line corrected, 2 lines added **
      DATA INCRSE / 'LENGTH', 'NINMAX', '      ', '      ', '      ',  &
                    '      ', '      ', '      ', '      ', '      ',  &
                    'NUMAX ', 'NSETVC'                               /
! ** Correction 6. 21/02/94: end of correction **
      DATA INDIC8( MBLANK ) / '            ' /, LENIND( MBLANK ) / 0  / 
      DATA INDIC8( MFIXED ) / 'FIXED FORMAT' /, LENIND( MFIXED ) / 12 /
      DATA INDIC8( MFREE  ) / 'FREE FORMAT ' /, LENIND( MFREE  ) / 11 /
      DATA INDIC8( MNAME  ) / 'ELEMENTS    ' /, LENIND( MNAME  ) / 8  /
      DATA INDIC8( MTEMP  ) / 'TEMPORARIES ' /, LENIND( MTEMP  ) / 11 / 
      DATA INDIC8( MGLOB  ) / 'GLOBALS     ' /, LENIND( MGLOB  ) / 7  /
      DATA INDIC8( MINDIV ) / 'INDIVIDUALS ' /, LENIND( MINDIV ) / 11 / 
      DATA INDIC8( MENDAT ) / 'ENDATA      ' /, LENIND( MENDAT ) / 6  /
      DATA FIELDI(  1 ) / 'ELFUN   ' /,  FIELDI(  2 ) / 'LFUVAL  ' /
      DATA FIELDI(  3 ) / 'FUVALS  ' /,  FIELDI(  4 ) / 'XVALUE  ' /
      DATA FIELDI(  5 ) / 'NCALCF  ' /,  FIELDI(  6 ) / 'ITYPEE  ' /
      DATA FIELDI(  7 ) / 'ISTAEV  ' /,  FIELDI(  8 ) / 'IELVAR  ' /
      DATA FIELDI(  9 ) / 'INTVAR  ' /,  FIELDI( 10 ) / 'ISTADH  ' /
      DATA FIELDI( 11 ) / 'ICALCF  ' /,  FIELDI( 12 ) / 'IFFLAG  ' /
      DATA FIELDI( 13 ) / 'IELEMN  ' /,  FIELDI( 14 ) / 'IELTYP  ' /
      DATA FIELDI( 15 ) / 'IHSTRT  ' /,  FIELDI( 16 ) / 'ILSTRT  ' /
      DATA FIELDI( 17 ) / 'IGSTRT  ' /,  FIELDI( 18 ) / 'EPVALU  ' /
      DATA FIELDI( 19 ) / 'ISTEPA  ' /,  FIELDI( 20 ) / 'IPSTRT  ' /
      DATA FIELDI( 21 ) / 'JCALCF  ' /,  FIELDI( 22 ) / 'LTYPEE  ' /  
      DATA FIELDI( 23 ) / 'LSTAEV  ' /,  FIELDI( 24 ) / 'LELVAR  ' /
      DATA FIELDI( 25 ) / 'LNTVAR  ' /,  FIELDI( 26 ) / 'LSTADH  ' /
      DATA FIELDI( 27 ) / 'LSTEPA  ' /,  FIELDI( 28 ) / 'LCALCF  ' /    
      DATA FIELDI( 29 ) / 'LFVALU  ' /,  FIELDI( 30 ) / 'LXVALU  ' /
      DATA FIELDI( 31 ) / 'LEPVLU  ' /,  FIELDI( 32 ) / 'IFSTAT  ' /
      DATA ZERO         / 0.0D+0 /
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2900 )

!  SET INITIAL VALUES FOR INTEGER VARIABLES.

      NINNAM = 0
      NRENAM = 0
      NLONAM = 0
      NMINAM = 0
      NEXNAM = 0
      LINENO = 0
      NLOOP = NELTYP + 1
      INTYPE = 1
      ILINES = 0
      NLINES = 0

!  SET INITIAL VALUES FOR LOGICAL VARIABLES.

      DEFNAM = .FALSE.
      ENDPAR = .FALSE.
      STARTH = .FALSE.
      STARTP = .FALSE.
      ENDGEN = .FALSE.
      FIRSTL = .TRUE.
      NOINTE = .FALSE.
      FIXED = .TRUE.
      GOTLIN = .FALSE.

!  CREATE A DICTIONARY OF THE INTERNAL VARIABLE NAMES USED.

      NINAME = IINV( NELTYP + 1 ) - 1
      DO 20 I = 1, NINAME
         FIELD = INAMES( I ) // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = INAMES( I )
         END IF
   20 CONTINUE

!  INCLUDE THE NAMES OF THE ELEMENTAL VARIABLES USED IN THIS DICTIONARY.

      NENAME = IELV( NELTYP + 1 ) - 1
      DO 30 I = 1, NENAME
         FIELD = ENAMES( I ) // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = ENAMES( I )
         END IF
   30 CONTINUE

!  INCLUDE THE NAMES OF THE ELEMENTAL PARAMETERS USED
!  IN THIS DICTIONARY.

      NPNAME = IEPA( NELTYP + 1 ) - 1
      DO 40 I = 1, NPNAME
         FIELD = EPNAME( I ) // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = EPNAME( I )
         END IF
   40 CONTINUE

!  FIND WHICH ELEMENT TYPES HAVE AN INTERNAL REPRESENTATION.

      ISETTY = 0
      DO 50 ITYPE = 1, NELTYP
         LDEFND( ITYPE ) = .FALSE.
         IF ( IELV( ITYPE + 1 ) - IELV( ITYPE ) .EQ.&
              IINV( ITYPE + 1 ) - IINV( ITYPE ) ) THEN
            IJUMP( ITYPE ) = 99998
            NOINTE = .TRUE.
         ELSE
            IJUMP( ITYPE ) = ITYPE
         END IF
   50 CONTINUE

!  SET A BLANK LINE.

      DO 60 I = 1, MXRECL
         BLNKLN( I: I ) = ' '
   60 CONTINUE    

!  READ NEXT LINE.

  100 CONTINUE
      IF ( ILINES + 1 .GT. NLINES ) THEN

!  READ NEXT LINE FROM THE INPUT FILE.

         LINENO = LINENO + 1
         NULINE = BLNKLN
         IF ( FIXED ) THEN
            READ ( INPUT, 1000, END = 590, ERR = 590 ) NULINE
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2990 )  &
                 LINENO, NULINE
         ELSE
            READ ( INPUT, 1010, END = 590, ERR = 590 ) NULINE
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2970 )  &
                 LINENO, NULINE

!  IF THE CARD IS IN FREE FORMAT, TRANSLATE IT INTO FIXED FORMAT.

            CALL  FREEFM( NULINE, MXRECL, MENDAT, INDIC8, LENIND,  &
                          NULINA, MAXNUL, NLINES, .FALSE.,  &
                          INFORM, IOUT )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( NLINES .GT. 0 ) THEN

!  IF THERE ARE NON-BLANK LINES ON THE FREE FORMAT CARD, READ THE FIRST.

               ILINES = 1
               NULINE = BLNKLN
               NULINE = NULINA( ILINES )
               IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
                    LINENO, ILINES, NULINE
            ELSE

!  THERE ARE ONLY BLANK LINES ON THE FREE FORMAT CARD.

               GO TO 100
            END IF
         END IF
      ELSE

!  READ NEXT LINE FROM THE LAST ENCOUNTERED FREE FORMAT CARD.

         ILINES = ILINES + 1
         NULINE = BLNKLN
         NULINE = NULINA( ILINES )
         IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
              LINENO, ILINES, NULINE
      END IF

!  CONSIDER THE HEADER PART OF THE CARD.

      HEADER = NULINE( 1: 12 )

!  IGNORE BLANK LINES.

      IF ( HEADER .EQ. INDIC8( MBLANK ) ) GO TO 100
      IF ( NULINE( 1: 1 ) .NE. ' ' ) THEN

!  IGNORE COMMENT CARDS.

         IF ( NULINE( 1: 1 ) .EQ. '*' ) GO TO 100

!  CHECK IF WE HAVE ENTERED FIXED-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFIXED ) ) THEN
            FIXED = .TRUE.
            GO TO 100
         END IF

!  CHECK IF WE HAVE ENTERED FREE-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFREE ) ) THEN
            FIXED = .FALSE.
            GO TO 100
         END IF

!  CHECK THAT THE FIRST ENCOUNTERED INDICATOR CARD IS THE ELEMENTS CARD.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( HEADER .NE. INDIC8( MNAME ) ) THEN
               IF ( NELTYP .GT. 0 ) GO TO 930
               IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010)
               GOTLIN = .TRUE.
               GO TO 600
            ELSE

!  INDICATOR CARD IS ELEMENTS.
!  ---------------------------

               IF ( PNAME  .NE. NULINE( 15: 22 ) ) THEN
                  INFORM = 51
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2510 )
                  GO TO 800
               ELSE
                  DEFNAM = .TRUE.

!  -------- SET UP SUBROUTINE CALL FOR RANGE ROUTINE.

                  IF ( SINGLE ) THEN
                     WRITE( IOUTRA, 4001 ) PNAME
                  ELSE
                     WRITE( IOUTRA, 4000 ) PNAME
                  END IF
                  IF ( NELTYP .GT. 1 ) THEN
                     WRITE( IOUTRA, 4040 ) ( IJUMP( I ), I = 1, NELTYP )
                     WRITE( IOUTRA, 4050 )
                  END IF
                  GO TO 100
               END IF
            END IF
         END IF

!  AN INDICATOR CARD HAS BEEN FOUND.

         DO 110 I = INTYPE, MENDAT
            IF ( HEADER .EQ. INDIC8( I ) ) THEN
               INTYPE = I
               GO TO 120
            END IF
  110    CONTINUE

!  THE INDICATOR CARD IS NOT RECOGNISED.

         INFORM = 2
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2020 )
         GO TO 800
  120    CONTINUE

!  THE PARAMETER VALUES HAVE BEEN COMPLETED. WRITE OUT THE
!  FIRST PART OF THE GENERATED SUBROUTINE.

         IF ( INTYPE .GE. MGLOB .AND. .NOT. ENDPAR ) THEN
            ENDPAR = .TRUE.

!  INSERT THE LIST OF RESERVED INTEGER/REAL/LOGICAL VARIABLES INTO
!  THE DICTIONARY.

            DO 130 I = 1, IIRES
               FIELD = FIELDI( I ) // '  PF'
               CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
               IF ( IFREE .LE. 0 ) THEN
                  IF ( IFREE .EQ. 0 ) THEN
                     INFORM = - 1
                     GO TO 700
                  END IF
                  INFORM = 59
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2590 ) FIELDI( I )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- SET UP SUBROUTINE CALL AND RESERVED PARAMETER DECLARATIONS.

            IF ( SINGLE ) THEN
               WRITE( IOUTFN, 3001 ) FIELDI( 1 )(1:6),  &
         FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6), &
       ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
         FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
         FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
         FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 22, 32 ),  &
         FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
         FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
         FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
         FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
         FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
         FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
         FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
         FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
         FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
         FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
         PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
         FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
         FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            ELSE   
               WRITE( IOUTFN, 3000 ) FIELDI( 1 )(1:6),  &
         FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
         FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
         FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
         FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 22, 32 ),  &
         FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
         FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
         FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
         FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
         FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
         FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
         FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
         FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
         FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
         FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
         PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
         FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
         FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            END IF

! --------- INSERT INTEGER DECLARATIONS.

            IF ( NINNAM .GT. 0 )  &
               WRITE( IOUTFN, 3010 ) ( INNAME( I ), I = 1, NINNAM )

! --------- INSERT REAL DECLARATIONS.

            IF ( NRENAM .GT. 0 ) THEN
               IF ( SINGLE ) THEN
                  WRITE( IOUTFN, 3019 ) ( RENAME( I ), I = 1, NRENAM )
               ELSE
                  WRITE( IOUTFN, 3020 ) ( RENAME( I ), I = 1, NRENAM )
               END IF
            END IF

! --------- INSERT LOGICAL DECLARATIONS.

            IF ( NLONAM .GT. 0 )  &
               WRITE( IOUTFN, 3023 ) ( LONAME( I ), I = 1, NLONAM )

! --------- INSERT INTRINSIC DECLARATIONS.

            IF ( NMINAM .GT. 0 )  &
               WRITE( IOUTFN, 3021 ) ( MINAME( I ), I = 1, NMINAM )

! --------- INSERT EXTERNAL DECLARATIONS.

            IF ( NEXNAM .GT. 0 )  &
               WRITE( IOUTFN, 3022 ) ( EXNAME( I ), I = 1, NEXNAM )
            WRITE( IOUTFN, 3009 ) FIELDI( 32 )(1:6)
         END IF

!  THE GENERAL PARAMETER ASSIGNMENTS HAVE BEEN COMPLETED.
!  CONTINUE WITH THE CONSTRUCTION OF THE GENERATED SUBROUTINE.

         IF ( INTYPE .GE. MINDIV .AND. .NOT. ENDGEN ) THEN
            ENDGEN = .TRUE.

! --------- START LOOP OVER ELEMENTS.

            WRITE( IOUTFN, 3050 ) NLOOP,  &
                   FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),  &
                   FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),  &
                   FIELDI( 21 )(1:6),  &
                   FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),  &
                   FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),  &
                   FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),  &
                   FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),  &
                   FIELDI( 13 )(1:6),  &
                   FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),  &
                   FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
            IF ( NELTYP .GT. 1 ) THEN
               WRITE( IOUTFN, 3051 )  &
                  FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),  &
                  FIELDI( 13 )(1:6), ( I, I = 1, NELTYP )
               WRITE( IOUTFN, 3052 ) FIELDI( 14 )(1:6)
            END IF
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.

!  MAKE SURE THAT QUADRATIC HESSIAN TERMS ARE INCLUDED.

            DO 140 ITYPE = 1, MIN( 2, NELTYP )

!  DIAGONAL TERM.

               IF ( ETYPES( ITYPE ) .EQ. CQSQR ) THEN
                  WRITE( IOUTFN, 3060 ) ETYPES( ITYPE )
                  IF ( NELTYP .GT. 1 ) WRITE( IOUTFN, 3061 ) ITYPE
                  IF ( SINGLE ) THEN
                    WRITE( IOUTFN, 3053 ) 'E', 'E'
                  ELSE
                    WRITE( IOUTFN, 3053 ) 'D', 'D'
                  END IF
                  LDEFND( ITYPE ) = .TRUE.
                  ISETTY = ISETTY + 1
                  IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFN, 3191 ) NLOOP

!  OFF-DIAGONAL TERM.

               ELSE IF ( ETYPES( ITYPE ) .EQ. CQPROD ) THEN
                  WRITE( IOUTFN, 3060 ) ETYPES( ITYPE )
                  IF ( NELTYP .GT. 1 ) WRITE( IOUTFN, 3061 ) ITYPE
                  IF ( SINGLE ) THEN
                    WRITE( IOUTFN, 3054 ) 'E', 'E', 'E'
                  ELSE
                    WRITE( IOUTFN, 3054 ) 'D', 'D', 'D'
                  END IF
                  LDEFND( ITYPE ) = .TRUE.
                  ISETTY = ISETTY + 1
                  IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFN, 3191 ) NLOOP
               END IF
  140       CONTINUE   
         END IF

!  INDICATOR CARD IS ENDATA.
!  -------------------------

         IF ( INTYPE .EQ. MENDAT ) GO TO 900
         GO TO 100
      ELSE

!  CHECK THAT THE FIRST NON COMMMENT CARD IS THE ELEMENTS INDICATOR CARD.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( NELTYP .GT. 0 ) GO TO 930
            IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )
            GOTLIN = .TRUE.
            GO TO 600
         END IF

!  A DATA CARD HAS BEEN FOUND.
!  READ THE CHARACTER FIELDS 1 AND 2 FROM THE CARD.

         FIELD1 = NULINE(  2:  3 )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
         FIELD2 = NULINE(  5: 14 )
         IF ( INTYPE .EQ. MINDIV .AND. FIELD1 .EQ. 'R ' ) THEN

!  READ THE CHARACTER FIELDS 3 AND 5 FROM THE CARD.

            FIELDS( 1 ) = NULINE( 15: 22 )
            FIELDS( 2 ) = NULINE( 40: 47 )

!  CHECK TO SEE IF THERE IS ARE ANY NUMERICAL VALUES TO BE READ.

            NOVALS = 0
            IF ( FIELDS( 1 ) .NE. '        ' .AND.&
                 NULINE( 15: 15 ) .NE. '[' ) THEN
               NOVALS = 1
               CALL GETVAL( NULINE( 25: 36 ), VALUES( 1 ) )
               IF ( FIELDS( 2 ) .NE. '        ' .AND.&
                 NULINE( 40: 40 ) .NE. '[' ) THEN
                  NOVALS = 2
                  CALL GETVAL( NULINE( 50: 61 ), VALUES( 2 ) )

!  REMOVE FIELDS WITH NUMERICAL VALUES OF ZERO.

                  IF ( VALUES( 2 ) .EQ. ZERO ) THEN
                     NOVALS = 1
                  END IF
               END IF
               IF ( VALUES( 1 ) .EQ. ZERO ) THEN
                  IF ( NOVALS .EQ. 2 ) THEN
                     VALUES( 1 ) = VALUES( 2 )
                     FIELDS( 1 ) = FIELDS( 2 )
                  END IF
                  NOVALS = NOVALS - 1
               END IF
            END IF
         ELSE

!  READ THE CHARACTER FIELDS 3 AND 7 FROM THE CARD.

            FIELD3 = NULINE( 15: 22 )
            FIELD7 = NULINE( 25: 65 )
! ** Correction 1. 19/07/93: 12 lines added **

!  CHECK THAT FIELD3 IS BLANK ON 'A', 'F' AND 'G' CARDS.

            IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR.&
                 FIELD1( 1: 1 ) .EQ. 'F' .OR. &
                 FIELD1( 1: 1 ) .EQ. 'G' ) THEN
               IF ( FIELD3 .NE. '       ' ) THEN
                  INFORM = 72
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2720 )
                  GO TO 800
               END IF
            END IF
! ** Correction 1. 19/07/93: end of correction **
         END IF
      END IF

!  BRANCH ON THE VALUE OF INTYPE.

      GO TO ( 100, 100, 100, 100, 200, 300, 400, 900 ), INTYPE

!  INDICATOR CARD IS TEMPORARIES.
!  ------------------------------

  200 CONTINUE

!  CHECK TO SEE IF THE PARAMETER IS INTEGER, REAL, LOGICAL OR A FUNCTION.

      IF ( FIELD1 .NE. 'I ' .AND. FIELD1 .NE. 'R ' .AND.&
           FIELD1 .NE. 'M ' .AND. FIELD1 .NE. 'F ' .AND.&
           FIELD1 .NE. 'L ' ) THEN
         INFORM = 54
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2540 )
         GO TO 800
      END IF

!  IF THE PARAMETER IS A FUNCTION, CHECK TO SEE THAT THE NAME HAS
!  NOT ALREADY BEEN USED.

      IF ( FIELD1 .EQ. 'F ' ) THEN
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
         FIELD = FIELD2 // 'FU'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NEXNAM = NEXNAM + 1
            IF ( NEXNAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            EXNAME( NEXNAM ) = FIELD2
         END IF
      ELSE

!  CHECK TO SEE THAT THE PARAMETER NAME HAS NOT ALREADY BEEN USED.

! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
         FIELD = FIELD2 // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            IF ( FIELD1 .EQ. 'R ' ) THEN
               NRENAM = NRENAM + 1
               IF ( NRENAM .GT. NINMAX ) THEN
                  INFORM = - 2
                  GO TO 700
               END IF
               RENAME( NRENAM ) = FIELD2
            ELSE
               IF ( FIELD1 .EQ. 'M ' ) THEN
                  NMINAM = NMINAM + 1
                  IF ( NMINAM .GT. NINMAX ) THEN
                     INFORM = - 2
                     GO TO 700
                  END IF
                  MINAME( NMINAM ) = FIELD2
               ELSE
                  IF ( FIELD1 .EQ. 'L ' ) THEN
                     NLONAM = NLONAM + 1
                     IF ( NLONAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     LONAME( NLONAM ) = FIELD2
                  ELSE
                     NINNAM = NINNAM + 1
                     IF ( NINNAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     INNAME( NINNAM ) = FIELD2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS GLOBALS.
!  --------------------------

  300 CONTINUE
      IF ( FIELD1 .EQ. 'A ' .OR. FIELD1 .EQ. 'I ' .OR.&
           FIELD1 .EQ. 'E ' ) THEN
         STARTP = .TRUE.

!  START A PARAMETER ASSIGNMENT. CHECK TO SEE THAT THE PARAMETER HAS
!  BEEN DEFINED.

! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
         FIELD = FIELD2 // 'PF'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
            INFORM = 57
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
            GO TO 800
         END IF

! --------- MAKE GENERAL PARAMETER ASSIGNMENTS.

         IF ( FIELD1 .EQ. 'A ' ) THEN
            WRITE( IOUTFN, 3030 ) FIELD2(1:6), FIELD7

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

         ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

            FIELD = FIELD3 // '  PF'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  GO TO 700
               END IF
               INFORM = 57
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
               GO TO 800
            END IF
            IF ( FIELD1 .EQ. 'I ' ) THEN
               WRITE( IOUTFN, 3031 ) FIELD2(1:6),  &
                                     FIELD3(1:6), FIELD7
            ELSE
               WRITE( IOUTFN, 3032 ) FIELD2(1:6),  &
                                     FIELD3(1:6), FIELD7
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) .EQ. '+' .AND. STARTP ) THEN

! --------- CONTINUE A PARAMETER ASSIGNMENT.

            WRITE( IOUTFN, 3040 ) FIELD7
         ELSE
            INFORM = 55
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS INDIVIDUALS.
!  ------------------------------

  400 CONTINUE

!  CHECK IF A NEW ELEMENT HAS BEEN ENCOUNTERED.

      IF ( FIELD1 .EQ. 'T ' ) THEN

!  CHECK TO SEE IF THE RANGE OF A NEW ELEMENT IS TO BE DEFINED.

         IF ( FIRSTL ) THEN

!  CHECK IF THIS IS THE FIRST ELEMENT.

            FIRSTL = .FALSE.
         ELSE

!  FINISH OF THE PREVIOUS ELEMENT, IF ANY.

            IF ( STARTH ) THEN
               IF ( .NOT. ENDOFH ) THEN
                  DO 410 IHVAR = 1, NHESS

! --------- SET A COMPONENT OF H.

                     IF ( .NOT. SETVEC( IHVAR ) ) THEN
                        IF ( SINGLE ) THEN
                           WRITE( IOUTFN, 3162 ) FIELDI(  3 )(1:6),  &
                                  FIELDI( 15 )(1:6), IHVAR
                        ELSE
                           WRITE( IOUTFN, 3161 ) FIELDI(  3 )(1:6),  &
                                 FIELDI( 15 )(1:6), IHVAR
                        END IF   
                     END IF   
  410             CONTINUE
                  ENDOFH = .TRUE.
               END IF

! ---------- WIND UP H.

               WRITE( IOUTFN, 3180 )
            END IF
            IF ( STARTG ) THEN

!  SET THE REMAINING GRADIENT COMPONENTS TO ZERO.

               IF ( .NOT. ENDOFG ) THEN
                  DO 415 IVAR = 1, NINVAR

! --------- SET A COMPONENT OF G.

                     IF ( .NOT. SETVEC( IVAR ) ) THEN
                        IF ( SINGLE ) THEN
                           WRITE( IOUTFN, 3132 )  &
                              FIELDI(  3 )(1:6),  &
                              FIELDI( 17 )(1:6), IVAR
                        ELSE
                           WRITE( IOUTFN, 3131 )  &
                              FIELDI(  3 )(1:6),  &
                              FIELDI( 17 )(1:6), IVAR
                        END IF   
                     END IF   
  415             CONTINUE
                  ENDOFG = .TRUE.
               END IF   

! ---------- WIND UP F AND G

            END IF
            IF ( STARTF ) THEN
               WRITE( IOUTFN, 3190 )
            ELSE
               INFORM = 61
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
               GO TO 800
            END IF
            IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFN, 3191 ) NLOOP
         END IF

!  FIND ITYPE, THE ELEMENT TYPE.

! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
         FIELD = FIELD2 // 'ET'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )

!  THE ELEMENT TYPE IS UNKNOWN.

         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 9
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2090 )
            GO TO 800
         END IF

! --------- FIND TYPE OF CURRENT ELEMENT.

         ITYPE = INLIST( IFIELD )
         WRITE( IOUTFN, 3060 ) FIELD2
         IF ( NELTYP .GT. 1 ) WRITE( IOUTFN, 3061 ) ITYPE
         IF ( LDEFND( ITYPE ) ) THEN
            INFORM = 67
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2670 )
            GO TO 800
         ELSE
            LDEFND( ITYPE ) = .TRUE.
            ISETTY = ISETTY + 1
         END IF

!  FIND THE ROW AND COLUMN DIMENSIONS (NINV AND NELV, RESP.) OF THE
!  TRANSFORMATION MATRIX U. U IS STORED IN VECTOR FORM BY COLUMNS.

         IS = IINV( ITYPE ) - 1
         JS = IELV( ITYPE ) - 1
         NELV = IELV( ITYPE + 1 ) - IELV( ITYPE )
         NINV = IINV( ITYPE + 1 ) - IINV( ITYPE )
         NN = NINV * NELV
         IF ( NN .GT. NUMAX ) THEN
            INFORM = - 11
            GO TO 700
         END IF

! --------- FIND TYPE OF CURRENT ELEMENT.

         IF ( NELV .GT. NINV ) WRITE( IOUTRA, 4060 ) FIELD2, ITYPE

!  INITIALIZE U AS THE ZERO MATRIX.

         DO 420 I = 1, NN
            U( I ) = ZERO
  420    CONTINUE
         SETRAN = NELV .GT. NINV

! --------- SET ELEMENTAL VARIABLES.

         K1 = IELV( ITYPE )
         K2 = IELV( ITYPE + 1 ) - 1
         DO 430 K = K1, K2
            IVAR = K - K1 + 1
            WRITE( IOUTFN, 3070 ) ENAMES( K ), FIELDI(  4 )(1:6),  &
                FIELDI(  8 )(1:6), FIELDI( 16 )(1:6), IVAR
  430    CONTINUE

! --------- SET ELEMENTAL PARAMETERS.

         K1 = IEPA( ITYPE )
         K2 = IEPA( ITYPE + 1 ) - 1
         DO 435 K = K1, K2
            IVAR = K - K1 + 1
            WRITE( IOUTFN, 3071 ) EPNAME( K ), FIELDI( 18 )(1:6),  &
                FIELDI( 20 )(1:6), IVAR
  435    CONTINUE

!  FIND THE NUMBER OF INTERNAL VARIABLES AND THE REQUIRED SIZE OF
!  THE LOWER TRIANGULAR PORTION OF THE HESSIAN MATRIX.

         K1 = IINV( ITYPE )
         K2 = IINV( ITYPE + 1 ) - 1
         NINVAR = K2 - K1 + 1
         NHESS = NINVAR * ( NINVAR + 1 ) / 2
         NVARS = 0
         NH = 0
         STARTP = .FALSE.
         STARTF = .FALSE.
         STARTG = .FALSE.
         STARTH = .FALSE.
         ENDOFF = .TRUE.
         ENDOFG = .TRUE.
         ENDOFH = .TRUE.
         NOMORG = .FALSE.
      ELSE
         IF ( FIELD1 .EQ. 'R ' ) THEN

!  THE RANGE TRANSFORMATION MATRIX U IS NOW DEFINED, ENTRY BY ENTRY.
!  DETERMINE WHICH INTERNAL VARIABLE IS GIVEN IN FIELD2.

            DO 440 I = 1, NINV
               IF ( FIELD2 .EQ. INAMES( IS + I ) ) GO TO 450
  440       CONTINUE

!  THE INTERNAL VARIABLE NAME IS UNRECOGNISED.

            INFORM = 65
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2650 )
            GO TO 800

!  THE INTERNAL VARIABLE IS THE I-TH IN THE LIST.

  450       CONTINUE

!  DETERMINE WHICH ELEMENTAL VARIABLE(S) OCCUR IN FIELDS.

            IF ( NOVALS .GT. 0 ) THEN
               DO 480 K = 1, NOVALS
                  DO 460 J = 1, NELV
                     IF ( FIELDS( K ) .EQ. ENAMES( JS + J ) ) GO TO 470
  460             CONTINUE

!  THE ELEMENTAL VARIABLE NAME IS UNRECOGNISED.

                  INFORM = 66
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2660 )
                  GO TO 800

!  THE ELEMENTAL VARIABLE IS THE J-TH IN THE LIST.

  470             CONTINUE

!  INSERT THE VALUE OF THE NEW NONZERO INTO U.

                  U( NINV * ( J - 1 ) + I ) = VALUES( K )
  480          CONTINUE
            END IF
         ELSE
            IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR. FIELD1( 1: 1 ) &
                 .EQ. 'I' .OR. FIELD1( 1: 1 ) .EQ. 'E' ) THEN
               IF ( STARTF ) THEN
                  IF ( .NOT. ENDOFF ) THEN
                     WRITE( IOUTFN, 3120 )
                     ENDOFF = .TRUE.
                  END IF
               END IF
               IF ( STARTG ) THEN
                  IF ( ENDOFG .AND. .NOT. NOMORG ) THEN
                     WRITE( IOUTFN, 3150 ) FIELDI( 12 )(1:6)
                     NOMORG = .TRUE.
                  END IF
               END IF

!  START A PARAMETER ASSIGNMENT.

               IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                  STARTP = .TRUE.

!  SET UP THE TRANSFORMATIONS FOR THE ELEMENT.

                  IF ( SETRAN ) THEN
                      CALL OUTRAN( NELV, NINV, U, IOUTFN, IOUTRA,  &
                                   ENAMES( JS + 1 ), INAMES( IS + 1 ),  &
                                   SINGLE )
                      SETRAN = .FALSE.
                  END IF

!  CHECK TO SEE THAT THE PARAMETER HAS BEEN DEFINED.

! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
                  FIELD = FIELD2 // 'PF'
                  CALL HASHC (LENGTH, 12, FIELD, KEY, ITABLE, IFIELD)
                  IF ( IFIELD .LE. 0 ) THEN
                     INFORM = 58
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2580 )
                     GO TO 800
                  END IF

! --------- MAKE ELEMENT-SPECIFIC PARAMETER ASSIGNMENTS.

                  IF ( FIELD1( 1: 1 ) .EQ. 'A' ) THEN
                     IF ( .NOT. STARTF ) THEN
                        WRITE( IOUTFN, 3080 ) FIELD2(1:6), FIELD7
                     ELSE
                        WRITE( IOUTFN, 3083 ) FIELD2(1:6), FIELD7
                     END IF

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

                  ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

                     FIELD = FIELD3 // '  PF'
                     CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE,  &
                                  IFIELD )
                     IF ( IFIELD .LE. 0 ) THEN
                        IF ( IFREE .EQ. 0 ) THEN
                           INFORM = - 1
                           GO TO 700
                        END IF
                        INFORM = 58
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2580 )
                        GO TO 800
                     END IF
                     IF ( FIELD1( 1: 1 ) .EQ. 'I' ) THEN
                        IF ( .NOT. STARTF ) THEN
                           WRITE( IOUTFN, 3081 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                        ELSE
                           WRITE( IOUTFN, 3084 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                        END IF
                     ELSE
                     IF ( .NOT. STARTF ) THEN
                           WRITE( IOUTFN, 3082 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                        ELSE
                           WRITE( IOUTFN, 3085 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                        END IF
                     END IF   
                  END IF
               ELSE
                  IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                     IF ( STARTP ) THEN

! --------- CONTINUATION OF A PARAMETER ASSIGNMENT.

                     IF ( .NOT. STARTF ) THEN
                        WRITE( IOUTFN, 3090 ) FIELD7
                     ELSE
                        WRITE( IOUTFN, 3091 ) FIELD7
                     END IF
                     ELSE
                        INFORM = 56
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            ELSE
               STARTP = .FALSE.
               IF ( FIELD1( 1: 1 ) .EQ. 'F' ) THEN

!  SET THE FUNCTION VALUE.

                  IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                     STARTF = .TRUE.
                     ENDOFF = .FALSE.

!  SET UP THE TRANSFORMATIONS FOR THE ELEMENT.

                     IF ( SETRAN ) THEN
                         CALL OUTRAN( NELV, NINV, U, IOUTFN, IOUTRA,  &
                                   ENAMES( JS + 1 ), INAMES( IS + 1 ),  &
                                   SINGLE )
                         SETRAN = .FALSE.
                     END IF

! --------- START F.

                     WRITE( IOUTFN, 3100 ) FIELDI( 12 )(1:6),  &
                     FIELDI( 3 )(1:6), FIELDI( 13 )(1:6), FIELD7
                  ELSE
                     IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                        IF ( STARTF ) THEN

! --------- CONTINUATION OF F.

                           WRITE( IOUTFN, 3110 ) FIELD7
                        ELSE
                           INFORM = 56
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               ELSE
                  IF ( FIELD1( 1: 1 ) .EQ. 'G' ) THEN

!  NO FUNCTION VALUE HAS BEEN SPECIFIED.

                     IF ( .NOT. STARTF ) THEN
                        INFORM = 61
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
                        GO TO 800
                     END IF

!  SET THE GRADIENT VALUES.

                     IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                        IF ( .NOT. STARTG ) THEN
                           STARTG = .TRUE.
                           ENDOFG = .FALSE.

!  USE THE LOGICAL ARRAY SETVEC TO ENSURE THAT ALL GRADIENTS ARE SET.

                           IF ( NINVAR .GT. NSETVC ) THEN
                              INFORM = - 12
                              GO TO 700
                           END IF
                           DO 510 I = 1, NINVAR
                              SETVEC( I ) = .FALSE.
  510                      CONTINUE

! --------- START G.

                           IF ( .NOT. ENDOFF ) THEN
                              WRITE( IOUTFN, 3120 )
                              ENDOFF = .TRUE.
                           END IF
                        END IF

!  FIND WHICH COMPONENT IS TO BE SET.

                        DO 520 K = K1, K2
                           IVAR = K - K1 + 1
                           IF ( FIELD2 .EQ. INAMES( K ) ) GO TO 525
  520                   CONTINUE

!  THE COMPONENT NAME IS UNRECOGNISED.

                        INFORM = 60
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2600 )
                        GO TO 800
  525                   CONTINUE

! --------- SET A COMPONENT OF G.

                        IF ( SETVEC( IVAR ) ) THEN
                           INFORM = 69
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2690 )
                           GO TO 800
                        END IF
                        SETVEC( IVAR ) = .TRUE.
                        NVARS = NVARS + 1
                        ENDOFG = NVARS .EQ. NINVAR
                        WRITE( IOUTFN, 3130 ) FIELDI(  3 )(1:6),  &
                               FIELDI( 17 )(1:6), IVAR, FIELD7
                     ELSE
                        IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                           IF ( STARTG .AND. .NOT. NOMORG ) THEN

! --------- CONTINUATION OF G.

                              WRITE( IOUTFN, 3140 ) FIELD7
                           ELSE
                              INFORM = 56
                              IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                              GO TO 800
                           END IF
                        END IF
                     END IF
                  ELSE
                     IF ( FIELD1( 1: 1 ) .EQ. 'H' ) THEN

!  SET THE HESSIAN VALUES.

                        IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                           IF ( .NOT. STARTH ) THEN

!  SET THE REMAINING GRADIENT COMPONENTS TO ZERO.

                              IF ( .NOT. STARTG ) THEN
                                 DO 530 IVAR = 1, NINVAR

! --------- SET A COMPONENT OF G.

                                    IF ( SINGLE ) THEN
                                       WRITE( IOUTFN, 3132 )  &
                                          FIELDI(  3 )(1:6),  &
                                          FIELDI( 17 )(1:6), IVAR
                                    ELSE
                                       WRITE( IOUTFN, 3131 )  &
                                          FIELDI(  3 )(1:6),  &
                                          FIELDI( 17 )(1:6), IVAR
                                    END IF   
  530                            CONTINUE
                                 STARTG = .TRUE.
                                 ENDOFG = .TRUE.
                              END IF
                              IF ( .NOT. ENDOFG ) THEN
                                 DO 535 IVAR = 1, NINVAR

! --------- SET A COMPONENT OF G.

                                    IF ( .NOT. SETVEC( IVAR ) ) THEN
                                       IF ( SINGLE ) THEN
                                          WRITE( IOUTFN, 3132 )  &
                                             FIELDI(  3 )(1:6),  &
                                             FIELDI( 17 )(1:6), IVAR
                                       ELSE
                                          WRITE( IOUTFN, 3131 )  &
                                             FIELDI(  3 )(1:6),  &
                                             FIELDI( 17 )(1:6), IVAR
                                       END IF   
                                    END IF   
  535                            CONTINUE
                                 ENDOFG = .TRUE.
                              END IF
                              IF ( .NOT. NOMORG ) THEN
                                 WRITE( IOUTFN, 3150 ) &
                                        FIELDI( 12 )(1:6)
                                 NOMORG = .TRUE.
                              END IF
                              STARTH = .TRUE.
                              ENDOFH = .FALSE.

!  USE THE LOGICAL ARRAY SETVEC TO ENSURE THAT ALL HESSIANS ARE SET.

                              IF ( NHESS .GT. NSETVC ) THEN
! ** Correction 7. 21/02/94: 1 line corrected **
                                 INFORM = - 12
! ** Correction 7. 21/02/94: end of correction **
                                 GO TO 700
                              END IF
                              DO 540 I = 1, NHESS
                                 SETVEC( I ) = .FALSE.
  540                         CONTINUE
                           END IF

! ---------  START H.


!  FIND WHICH COMPONENT IS TO BE SET.

                           DO 550 K = K1, K2
                              IVAR = K - K1 + 1
                              IF ( FIELD2 .EQ. INAMES( K ) ) GO TO 560
  550                      CONTINUE

!  THE COMPONENT NAME FIELD2 IS UNRECOGNISED.

                           INFORM = 71
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2710 )
                           GO TO 800
  560                      CONTINUE
                           DO 570 K = K1, K2
                              JVAR = K - K1 + 1
                              IF ( FIELD3 .EQ. INAMES( K ) ) GO TO 580
  570                      CONTINUE

!  THE COMPONENT NAME FIELD3 IS UNRECOGNISED.

                           INFORM = 71
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2710 )
                           GO TO 800
  580                      CONTINUE

!  FIND THE ADDRESS OF THE COMPONENT OF THE HESSIAN. THE MATRIX IS
!  STORED AS AN UPPER TRIANGLE BY ROWS.

                           IF ( IVAR .GT. JVAR ) THEN
                              I = IVAR
                              IVAR = JVAR
                              JVAR = I
                           END IF
                           IHVAR = IVAR + JVAR * ( JVAR - 1 ) / 2

!  ENSURE THAT THE COMPONENT HAS NOT ALREADY BEEN SET.

                           IF ( SETVEC( IHVAR ) ) THEN
                              INFORM = 70
                              IF ( IOUT .GT. 0 ) WRITE( IOUT, 2700 )
                              GO TO 800
                           END IF
                           SETVEC( IHVAR ) = .TRUE.
                           NH = NH + 1
                           ENDOFH = NH .EQ. NHESS

! --------- SET A COMPONENT OF H.

                           WRITE( IOUTFN, 3160 ) FIELDI(  3 )(1:6),  &
                                  FIELDI( 15 )(1:6), IHVAR, FIELD7
                        ELSE
                           IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                              IF ( STARTH ) THEN

! --------- CONTINUATION OF H.

                                 WRITE( IOUTFN, 3170 ) FIELD7
                              ELSE
                                 INFORM = 56
                                 IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                                 GO TO 800
                              END IF
                           END IF
                        END IF
                     ELSE
                        INFORM = 56
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  THE END OF THE INPUT FILE HAS BEEN REACHED BEFORE THE ENDATA CARD.

  590 CONTINUE 

!  IF THE ELEMENTS CARD HAS NOT BEEN ENCOUNTERED, EXIT.

      IF ( DEFNAM ) THEN
         INFORM = 52
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2520 )
         RETURN
      END IF
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      QPROD = .FALSE.
      DO 591 ITYPE = 1, MIN( 2, NELTYP ) 
         IF ( ETYPES( ITYPE ) .NE. CQSQR .AND.&
              ETYPES( ITYPE ) .NE. CQPROD ) GO TO 930
         IF ( ETYPES( ITYPE ) .EQ. CQPROD ) QPROD = .TRUE.
  591 CONTINUE   
      IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )

!  A DUMMY ROUTINE WILL BE SUBSTITUTED.

  600 CONTINUE 

!  WRITE A DUMMY ELFUNS ROUTINE.

! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      IF ( NELTYP .EQ. 0 ) THEN
         IF ( SINGLE ) THEN
            WRITE( IOUTFN, 3003 ) FIELDI( 1 )(1:6),  &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
           FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 22, 32 ),  &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), PNAME
         ELSE   
            WRITE( IOUTFN, 3002 ) FIELDI( 1 )(1:6),  &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
           FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 22, 32 ),  &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), PNAME
         END IF
         WRITE( IOUTFN, 3009 ) FIELDI( 32 )(1:6)
         WRITE( IOUTFN, 3201 )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
      ELSE
         IF ( SINGLE ) THEN
            WRITE( IOUTFN, 3001 ) FIELDI( 1 )(1:6),  &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
           FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6), &
           FIELDI( 12 )(1:6), ( FIELDI(  I )(1:6), I = 22, 32 ),  &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
           PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
           FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
           FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( QPROD ) THEN
               WRITE( IOUTFN, 3019 ) 'X     ', 'Y     '
            ELSE
               WRITE( IOUTFN, 3019 ) 'X     '
            END IF
         ELSE   
            WRITE( IOUTFN, 3000 ) FIELDI( 1 )(1:6),  &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
           FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6), &
           FIELDI( 12 )(1:6), ( FIELDI(  I )(1:6), I = 22, 32 ),  &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
           PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
           FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
           FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( QPROD ) THEN
               WRITE( IOUTFN, 3020 ) 'X     ', 'Y     '
            ELSE
               WRITE( IOUTFN, 3020 ) 'X     '
            END IF
         END IF
         WRITE( IOUTFN, 3009 ) FIELDI( 32 )(1:6)
         WRITE( IOUTFN, 3050 ) NLOOP,  &
                FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),  &
                FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),  &
                FIELDI( 21 )(1:6),  &
                FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),  &
                FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),  &
                FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),  &
                FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),  &
                FIELDI( 13 )(1:6),  &
                FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),  &
                FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
         IF ( NELTYP .GT. 1 ) THEN
            WRITE( IOUTFN, 3051 )  &
               FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),  &
               FIELDI( 13 )(1:6), ( I, I = 1, NELTYP )
            WRITE( IOUTFN, 3052 ) FIELDI( 14 )(1:6)
         END IF

!  MAKE SURE THAT QUADRATIC HESSIAN TERMS ARE INCLUDED.

         DO 640 ITYPE = 1, MIN( 2, NELTYP )

!  DIAGONAL TERM.

            IF ( ETYPES( ITYPE ) .EQ. CQSQR ) THEN
               WRITE( IOUTFN, 3060 ) ETYPES( ITYPE )
               IF ( NELTYP .GT. 1 ) WRITE( IOUTFN, 3061 ) ITYPE
               IF ( SINGLE ) THEN
                 WRITE( IOUTFN, 3053 ) 'E', 'E'
               ELSE
                 WRITE( IOUTFN, 3053 ) 'D', 'D'
               END IF
               LDEFND( ITYPE ) = .TRUE.
               ISETTY = ISETTY + 1
               IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFN, 3191 ) NLOOP

!  OFF-DIAGONAL TERM.

            ELSE IF ( ETYPES( ITYPE ) .EQ. CQPROD ) THEN
               WRITE( IOUTFN, 3060 ) ETYPES( ITYPE )
               IF ( NELTYP .GT. 1 ) WRITE( IOUTFN, 3061 ) ITYPE
               IF ( SINGLE ) THEN
                 WRITE( IOUTFN, 3054 ) 'E', 'E', 'E'
               ELSE
                 WRITE( IOUTFN, 3054 ) 'D', 'D', 'D'
               END IF
               LDEFND( ITYPE ) = .TRUE.
               ISETTY = ISETTY + 1
               IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFN, 3191 ) NLOOP
            END IF
  640    CONTINUE   
         WRITE( IOUTFN, 3200 ) NLOOP
      END IF

!  WRITE A DUMMY RANGE ROUTINE.

      IF ( SINGLE ) THEN
         WRITE( IOUTRA, 4003 ) PNAME
      ELSE
         WRITE( IOUTRA, 4002 ) PNAME
      END IF
      WRITE( IOUTRA, 4080 )
      WRITE( IOUTRA, 4090 )
      INFORM = 0
      RETURN

!  INSUFFICIENT SPACE TO CONTINUE CONSTRUCTION.

  700 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2000 ) INCRSE( - INFORM )
      RETURN

!  SUBROUTINE INCOMPLETE.

  800 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2990 ) LINENO, NULINE
      RETURN

!  SUBROUTINE SUCCESSFULLY COMPLETED.

  900 CONTINUE
      IF ( .NOT. FIRSTL ) THEN

!  FINISH OF THE PREVIOUS ELEMENT, IF ANY.

         IF ( STARTH ) THEN
            IF ( .NOT. ENDOFH ) THEN
               DO 910 IHVAR = 1, NHESS

! --------- SET A COMPONENT OF H.

                  IF ( .NOT. SETVEC( IHVAR ) ) THEN
                     IF ( SINGLE ) THEN
                        WRITE( IOUTFN, 3162 ) FIELDI(  3 )(1:6),  &
                               FIELDI( 15 )(1:6), IHVAR
                     ELSE
                        WRITE( IOUTFN, 3161 ) FIELDI(  3 )(1:6),  &
                               FIELDI( 15 )(1:6), IHVAR
                     END IF   
                  END IF   
  910          CONTINUE
               ENDOFH = .TRUE.
            END IF

! ---------- WIND UP H.

            WRITE( IOUTFN, 3180 )
         END IF
         IF ( STARTG ) THEN

!  SET THE REMAINING GRADIENT COMPONENTS TO ZERO.

            IF ( .NOT. ENDOFG ) THEN
               DO 920 IVAR = 1, NINVAR

! --------- SET A COMPONENT OF G.

                  IF ( .NOT. SETVEC( IVAR ) ) THEN
                     IF ( SINGLE ) THEN
                        WRITE( IOUTFN, 3132 )  &
                           FIELDI(  3 )(1:6),  &
                           FIELDI( 17 )(1:6), IVAR
                     ELSE
                        WRITE( IOUTFN, 3131 )  &
                           FIELDI(  3 )(1:6),  &
                           FIELDI( 17 )(1:6), IVAR
                     END IF   
                  END IF   
  920          CONTINUE
               ENDOFG = .TRUE.
            END IF

! ---------- WIND UP F AND G.

         END IF
         IF ( STARTF ) THEN
            WRITE( IOUTFN, 3190 )
         ELSE
            INFORM = 61
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
            GO TO 800
         END IF
         IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFN, 3191 ) NLOOP
      END IF

! ---------- SUCCESSFUL RUN. WIND UP OUTPUT.

      INFORM = 0
      WRITE( IOUTFN, 3200 ) NLOOP
      IF ( NOINTE ) WRITE( IOUTRA, 4070 )
      IF ( NELTYP .EQ. 0 ) WRITE( IOUTRA, 4080 )
      WRITE( IOUTRA, 4090 )

!   CHECK THAT ALL ELEMENT TYPES HAVE BEEN DEFINED.

  930 CONTINUE
      DO 940 ITYPE = 1, NELTYP
         IF ( .NOT. LDEFND( ITYPE ) ) THEN
            INFORM = 68
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2680 ) ETYPES( ITYPE )
         END IF
  940 CONTINUE
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKEFN - insufficient space.',  &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAKEFN - warning.',  &
              ' First card not elements. ', /, '    A dummy',  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKEFN - indicator card not recognised ' )
 2090 FORMAT( ' ** Exit from MAKEFN - element type not recognised ' )
 2510 FORMAT( ' ** Exit from MAKEFN -',  &
              ' name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKEFN - data file incomplete.',  &
              ' No ENDATA card ' )
 2540 FORMAT( ' ** Exit from MAKEFN -',  &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKEFN -',  &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKEFN -',  &
              ' unrecognised field 1 in INDIVIDUALS section' )
 2570 FORMAT( ' ** Exit from MAKEFN -',  &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKEFN -',  &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKEFN - repeated parameter name ', A8 )
 2600 FORMAT( ' ** Exit from MAKEFN - unknown component of gradient ' )
 2610 FORMAT( ' ** Exit from MAKEFN - function not set '  )
 2650 FORMAT( ' ** Exit from MAKEFN -',  &
              ' internal variable not recognised ' )
 2660 FORMAT( ' ** Exit from MAKEFN -',  &
              ' elemental variable not recognised ' )
 2670 FORMAT( ' ** Exit from MAKEFN - element type already defined ' )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
 2680 FORMAT( ' ** Exit from MAKEFN - warning, element type ', A10,  &
              ' undefined ' )
 2690 FORMAT( ' ** Exit from MAKEFN -',  &
              ' gradient component already defined ' )
 2700 FORMAT( ' ** Exit from MAKEFN -',  &
              ' Hessian component already defined ' )
 2710 FORMAT( ' ** Exit from MAKEFN - unknown component of Hessian '  )
! ** Correction 2. 19/07/93: 2 lines added **
 2720 FORMAT( ' ** Exit from MAKEFN - field 3 not blank on',  &
              ' A, F or G card ' )
! ** Correction 2. 19/07/93: end of correction **
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', I5, 4X, A160 )
 2980 FORMAT( ' Line ', I5, '.', I2, 1X, A65 )
 2990 FORMAT( ' Line ', I5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              '      INTEGER ', 5( A6, ', ' ), A6, /,  &
              '      INTEGER ', A6 )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              '      INTEGER ', 5( A6, ', ' ), A6, /,  &
              '      INTEGER ', A6 )
 3002 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3003 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, :, 4( ', ', A6, : ) ) )
 3019 FORMAT( ( '      REAL             ', A6, :, 4( ', ', A6, : ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, :, 4( ', ', A6, : ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, :, 4( ', ', A6, : ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, :, 4( ', ', A6, : ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', I5, 1X, A6, ' = 1, ', A6, /,  &
              '       ', A6, ' = ', A6, '(', A6, ') ', /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,  &
              '       IF ( ', A6, ' .EQ. 3 ) ',  &
                      A6, ' = ', A6, '(', A6, ') - 1' )
! ** Correction 7. 15/08/95: 3 lines corrected **
 3051 FORMAT( '       ', A6, ' = ', A6, '(', A6, ')', /,  &
              '       GO TO (', 8( I5, :, ',' ), /,  &
            ( '     *        ', 8( I5, :, ',' ) ) )
! ** Correction 7. 15/08/95: end of correction **
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
 3053 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,  &
              '       ELSE', /,  &
              '        FUVALS(IGSTRT+     1)= X', /,  &
              '        IF ( IFFLAG .EQ. 3 ) THEN', /,  &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,  &
              '        END IF', /,  &
              '       END IF' )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
 3054 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= X * Y', /,  &
              '       ELSE', /,  &
              '        FUVALS(IGSTRT+     1)= Y', /,  &
              '        FUVALS(IGSTRT+     2)= X', /,  &
              '        IF ( IFFLAG .EQ. 3 ) THEN', /,  &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,  &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,  &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,  &
              '        END IF', /,  &
              '       END IF' )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
 3060 FORMAT( 'C', /, 'C  ELEMENT TYPE : ', A10, /, 'C' )
 3061 FORMAT( I5, '  CONTINUE' )
 3070 FORMAT( '       ', A6, ' = ', A6, '(', A6, '(', A6, '+', I6, '))')
 3071 FORMAT( '       ', A6, ' = ', A6, '(', A6, '+', I6, ')')
 3080 FORMAT( '       ', A6, ' = ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
! ** Correction 3. 12/01/94: 1 line corrected **
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
! ** Correction 3. 12/01/94: end of correction **
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
! ** Correction 4. 12/01/94: 1 line corrected **
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
! ** Correction 4. 12/01/94: end of correction **
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( ', A6, ' .EQ. 1 ) THEN', /,  &
              '        ', A6, '(', A6, ')= ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3130 FORMAT( '        ', A6, '(', A6, '+', I6, ')= ', A41 )
 3131 FORMAT( '        ', A6, '(', A6, '+', I6, ')= 0.0D+0' )
 3132 FORMAT( '        ', A6, '(', A6, '+', I6, ')= 0.0E+0' )
 3140 FORMAT( '     *                         ', A41 )
 3150 FORMAT( '        IF ( ', A6, ' .EQ. 3 ) THEN' )
 3160 FORMAT( '         ', A6, '(', A6, '+', I6, ')=', A41 )
 3161 FORMAT( '         ', A6, '(', A6, '+', I6, ')=0.0D+0' )
 3162 FORMAT( '         ', A6, '(', A6, '+', I6, ')=0.0E+0' )
 3170 FORMAT( '     *                         ', A41 )
 3180 FORMAT( '        END IF' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', I6 )
 3200 FORMAT( I5,  ' CONTINUE', /, '      RETURN', /,  &
              '      END' )
 3201 FORMAT( '      RETURN', /,  &
              '      END' )
 4000 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C', /,  &
              '      INTEGER I' )
 4001 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      REAL             W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C', /,  &
              '      INTEGER I' )
 4002 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C' )
 4003 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      REAL             W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C' )
! ** Correction 8. 15/08/95: 3 lines corrected **
 4040 FORMAT( '      GO TO (', 8( I5, :, ',' ), /,  &
            ( '     *       ', 8( I5, :, ',' ) ) )
! ** Correction 8. 15/08/95: end of correction **
 4050 FORMAT( '     *        ', 48X, '), ITYPE' )
! ** Correction -1. 20/12/99: Code to process QUADOBJ cards added.
 4060 FORMAT( 'C', /, 'C  ELEMENT TYPE : ', A10, /, 'C', /,  &
              I5, ' CONTINUE', /,  &
              '      IF ( TRANSP ) THEN' )
 4070 FORMAT( 'C', /,  &
              'C  ELEMENTS WITHOUT INTERNAL VARIABLES.', /,  &
              'C', /,  &
              '99998 CONTINUE', /,  &
              '      DO 99999 I = 1, NELVAR', /,  &
              '         W2( I ) = W1( I )', /,  &
              '99999 CONTINUE', /,  &
              '      RETURN' )
 4080 FORMAT( '      RETURN' )
 4090 FORMAT( '      END' )

!  END OF MAKEFN.

      END
!     THIS VERSION: 21ST JUNE 1990.
      SUBROUTINE OUTRAN( NELV, NINV, U, IOUTFN, IOUTRA, ENAMES, INAMES,  &
                         SINGLE )
      INTEGER          NELV, NINV, IOUTFN, IOUTRA
      LOGICAL          SINGLE
      DOUBLE PRECISION U( NINV, NELV )
      CHARACTER * 10   ENAMES( * ), INAMES( * )

!  PRINT OUT THE GATHER AND SCATTER PART OF THE GENERATED RANGE ROUTINE
!  AND THE GATHER PART OF THE GENERATED FUNCTION EVALUATION ROUTINE.

      INTEGER          I, J, K
      DOUBLE PRECISION UIJ, ONE, ZERO, EPSMCH, DMACHR
      LOGICAL          ANYNNZ
      CHARACTER * 6    EVNAME, IVNAME
      INTRINSIC        DABS, MOD
      EXTERNAL         DMACHR
      DATA ZERO, ONE / 0.0D+0, 1.0D+0 /
      EPSMCH = DMACHR( 1 )

!  PRINT OUT THE SCATTER PART.

      DO 20 J = 1, NELV
         K = 0         
         ANYNNZ = .FALSE.
         DO 10 I = 1, NINV
            UIJ = U( I, J )

!  IGNORE ZERO ENTRIES.

            IF ( DABS( UIJ ) .LE. EPSMCH ) GO TO 10
            K = K + 1
            IF ( UIJ .GT. ZERO ) THEN

!  THE NONZERO IS POSITIVE.

               IF ( DABS( UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4030 ) I
                  ELSE
                     WRITE( IOUTRA, 4040 ) J, I
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4050 ) I, UIJ
                  ELSE
                     WRITE( IOUTRA, 4060 ) J, I, UIJ
                  END IF
               END IF
            ELSE

!  THE NONZERO IS NEGATIVE.

               IF ( DABS( - UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE - 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4070 ) I
                  ELSE
                     WRITE( IOUTRA, 4080 ) J, I
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN - 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4090 ) I, - UIJ
                  ELSE
                     WRITE( IOUTRA, 4100 ) J, I, - UIJ
                  END IF
               END IF
            END IF
            ANYNNZ = .TRUE.
            IF ( MOD( K, 19 ) .EQ. 0 ) WRITE( IOUTRA, 4112 ) J, J
   10    CONTINUE
         IF ( .NOT. ANYNNZ ) THEN
            IF ( SINGLE ) THEN
               WRITE( IOUTRA, 4111 ) J
            ELSE
               WRITE( IOUTRA, 4110 ) J
            END IF
         END IF
   20 CONTINUE

!  ----- THE SCATTER HAS BEEN COMPLETED; START THE GATHER.

      WRITE( IOUTRA, 4010 )

!  PRINT OUT THE GATHER PART.

      DO 40 I = 1, NINV
         K = 0
         ANYNNZ = .FALSE.
         IVNAME = INAMES( I )(1:6)
         DO 30 J = 1, NELV
            EVNAME = ENAMES( J )(1:6)
            UIJ = U( I, J )

!  IGNORE ZERO ENTRIES.

            IF ( DABS( UIJ ) .LE. EPSMCH ) GO TO 30
            K = K + 1
            IF ( UIJ .GT. ZERO ) THEN

!  THE NONZERO IS POSITIVE.

               IF ( DABS( UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTFN, 3030 ) EVNAME
                     WRITE( IOUTRA, 4030 ) J
                  ELSE
                     WRITE( IOUTFN, 3040 ) IVNAME, EVNAME
                     WRITE( IOUTRA, 4040 ) I, J
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTFN, 3050 ) EVNAME, UIJ
                     WRITE( IOUTRA, 4050 ) J, UIJ
                  ELSE
                     WRITE( IOUTFN, 3060 ) IVNAME, EVNAME, UIJ
                     WRITE( IOUTRA, 4060 ) I, J, UIJ
                  END IF
               END IF
             ELSE

!  THE NONZERO IS NEGATIVE.

               IF ( DABS( - UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE - 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTFN, 3070 ) EVNAME
                     WRITE( IOUTRA, 4070 ) J
                  ELSE
                     WRITE( IOUTFN, 3080 ) IVNAME, EVNAME
                     WRITE( IOUTRA, 4080 ) I, J
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN - 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTFN, 3090 ) EVNAME, - UIJ
                     WRITE( IOUTRA, 4090 ) J, - UIJ
                  ELSE
                     WRITE( IOUTFN, 3100 ) IVNAME, EVNAME, - UIJ
                     WRITE( IOUTRA, 4100 ) I, J, - UIJ
                  END IF
               END IF
            END IF
            ANYNNZ = .TRUE.
            IF ( MOD( K, 19 ) .EQ. 0 ) THEN
               WRITE( IOUTFN, 3040 ) IVNAME, IVNAME
               WRITE( IOUTRA, 4112 ) I, I
            END IF
   30    CONTINUE
         IF ( .NOT. ANYNNZ ) THEN
            IF ( SINGLE ) THEN
               WRITE( IOUTFN, 3111 ) IVNAME
               WRITE( IOUTRA, 4111 ) I
            ELSE
               WRITE( IOUTFN, 3110 ) IVNAME
               WRITE( IOUTRA, 4110 ) I
            END IF
         END IF
   40 CONTINUE

!  ----- THE GATHER HAS BEEN COMPLETED; WIND UP THE ELEMENT.

      WRITE( IOUTRA, 4020 )
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 3030 FORMAT( '     *          + ', A6 )
 3040 FORMAT( '       ', A6, ' =   ', A6 )
 3050 FORMAT( '     *          + ', A6, ' * ', F12.5 )
 3060 FORMAT( '       ', A6, ' =   ', A6, ' * ', F12.5 )
 3070 FORMAT( '     *          - ', A6 )
 3080 FORMAT( '       ', A6, ' = - ', A6 )
 3090 FORMAT( '     *          - ', A6, ' * ', F12.5 )
 3100 FORMAT( '       ', A6, ' = - ', A6, ' * ', F12.5 )
 3110 FORMAT( '       ', A6, ' = 0.0D+0 ' )
 3111 FORMAT( '       ', A6, ' = 0.0E+0 ' )
 4010 FORMAT( '      ELSE' )
 4020 FORMAT( '      END IF', /, '      RETURN' )
 4030 FORMAT( '     *                 + W1(', I6, ' ) ' )
 4040 FORMAT( '         W2(', I6, ' ) =   W1(', I6, ' ) ' )
 4050 FORMAT( '     *                 + W1(', I6, ' ) * ', F12.5 )
 4060 FORMAT( '         W2(', I6, ' ) =   W1(', I6, ' ) * ', F12.5 )
 4070 FORMAT( '     *                 - W1(', I6, ' ) ' )
 4080 FORMAT( '         W2(', I6, ' ) = - W1(', I6, ' ) ' )
 4090 FORMAT( '     *                 - W1(', I6, ' ) * ', F12.5 )
 4100 FORMAT( '         W2(', I6, ' ) = - W1(', I6, ' ) * ', F12.5 )
 4110 FORMAT( '         W2(', I6, ' ) = 0.0D+0 ' )
 4111 FORMAT( '         W2(', I6, ' ) = 0.0E+0 ' )
 4112 FORMAT( '         W2(', I6, ' ) =   W2(', I6, ' ) ' )

!  END OF OUTRAN.

      END

!  THIS VERSION: 04/08/1995 AT 11:40:35 AM.
!  ** VERSION B **

      SUBROUTINE MAFNAD( INPUT , IOUT  , IOUTFF, IOUTFD, IOUTRA, &
                         IOUTEM, INFORM, NLMAX , NIMAX , NETMAX, &
                         NINMAX, NUMAX , NEL   , NELTYP, PNAME , ENAMES, &
                         INAMES, RENAME, INNAME, LONAME, MINAME, EXNAME, &
                         ETYPES, LDEFND, LENGTH, ITABLE, KEY   , IELV  , &
                         IINV  , INLIST, EPNAME, IEPA  , NEPMAX, DEBUG , &
                         IJUMP , U     , SETVEC, NSETVC, SINGLE, &
                         NULINE, GOTLIN, IAUTO , IAD0  , IPRINT )
      INTEGER            INPUT , IOUT  , IOUTFF, IOUTRA, INFORM
      INTEGER            NLMAX , NIMAX , NETMAX, NELTYP, NINMAX
      INTEGER            NEPMAX, LENGTH, NSETVC, NEL   , NUMAX , IPRINT
      INTEGER            IOUTFD, IOUTEM, IAUTO , IAD0
      LOGICAL            DEBUG , SINGLE, GOTLIN 
      INTEGER            ITABLE( LENGTH ), IJUMP ( NLMAX  )
      INTEGER            IELV  ( NLMAX  ), IINV  ( NLMAX  )
      INTEGER            INLIST( LENGTH )
      INTEGER            IEPA  ( NLMAX  )
      LOGICAL            LDEFND( NLMAX  ), SETVEC( NSETVC )
      DOUBLE PRECISION   U     ( NUMAX  )
      CHARACTER * 12     KEY   ( LENGTH )
      CHARACTER * 8      PNAME
      CHARACTER * 10     EPNAME( NEPMAX ), EXNAME( NINMAX )
      CHARACTER * 10     INAMES( NIMAX  ), RENAME( NINMAX )
      CHARACTER * 10     LONAME( NINMAX ), INNAME( NINMAX )
      CHARACTER * 10     MINAME( NINMAX ), ENAMES( NETMAX )
      CHARACTER * 10     ETYPES( NLMAX  )
      CHARACTER * 160    NULINE

!  Make a function evaluation subroutine, suitable for automatic
!  -------------------------------------------------------------
!  differentiation, and a range transformation subroutine from a 
!  -------------------------------------------------------------
!  GPS function data file.
!  -----------------------

!  Nick Gould 04/05/1995
!  For CGT Productions.

!  -------------------------------------------------------------------

!  Function indicator cards.
!  -------------------------

!  Definition   Purpose.
!  ----------   --------
!  ELEMENTS     Problem name.
!  TEMPORARIES  Names of additional parameters used in function defs.
!  GLOBALS      General parameter assignments.
!  INDIVIDUALS  Define the transformation from the elemental to the
!               internal variables for all elements with internal vars.
!               Set function and derivative values and make
!               element specific parameter assignments.
!  ENDATA       End of input data.

!  Data card description.
!  ----------------------

!  See 'A Proposal for a standard data input format for large-scale
!       nonlinear programming problems', Section 3,
!       A. R. Conn, N. I. M. Gould AND PH. L. Toint, 
!       Report CS-89-61, Dept of Computer Science, U. of Waterloo,
!       Waterloo, Ontario, N2L3G1, Canada.

!  -------------------------------------------------------------------
!  Returns with negative values of INFORM indicate that insufficient
!  array space has been allowed, as follows:

!    INFORM = - 1  when LENGTH not large enough
!    INFORM = - 2  when MAX( NINNAM, NRENAM, NLONAM, NENAM, NMINAM )
!                  .GT. NINMAX
!    INFORM = - 11 when NUMAX not large enough
!    INFORM = - 12 when nsetvc not large enough

!     INTEGER          JHVAR
      INTEGER          IFIELD, IFREE, IHVAR, IVAR, INTYPE, NMINAM
      INTEGER          K, K1, K2, NH, NHESS, NVARS, ISETTY
      INTEGER          I, NINAME, NINNAM, NINVAR, NLOOP, NRENAM, NEXNAM
      INTEGER          ITYPE, J, IS, JS, NOVALS, NELV, NINV, NN, NENAME
      INTEGER          NPNAME, NINCRS, LINENO, IIRES, ILINES, NLINES
      INTEGER          MBLANK, MFIXED, MFREE, MNAME, MTEMP, MGLOB
      INTEGER          MINDIV, MENDAT, MAXNUL, MXRECL, NLONAM
      INTEGER          MAXNEL, MAXNIN, NTEM  , NRENM1, NRENM2
!     INTEGER          NETNAM
      INTEGER          I1, I2, I3, I4, I5, I6
      DOUBLE PRECISION ZERO, VALUES( 2 )
      LOGICAL          NOINTE, DEFNAM, ENDPAR, ENDGEN, FIRSTL, SETRAN
      LOGICAL          STARTF, STARTP, STARTV, QPROD
      LOGICAL          ENDOFF, FIXED , CQSQRT, CQPRDT, OUTFF
      PARAMETER        ( IIRES = 33 )
      PARAMETER        ( NINCRS = 12 )
      CHARACTER * 4    AD0
      CHARACTER * 6    NUNAME
      CHARACTER * 2    FIELD1
      CHARACTER * 6    XVAR  , YVAR  , INCRSE( NINCRS )
      CHARACTER * 10   FIELD2
      CHARACTER * 8    FIELD3, FIELDS( 2 ), FIELDI( IIRES )
      CHARACTER * 10   CTEMP
      CHARACTER * 12   FIELD
      CHARACTER * 15   AORB
      CHARACTER * 41   FIELD7
      CHARACTER * 72   CTEM
      INTRINSIC        MIN
      EXTERNAL         HASHB , HASHC , GETVAL, OUTRN2, NUNAME

!  Parameter definitions.

      PARAMETER        ( MXRECL = 160 )
      CHARACTER * 160    BLNKLN
      PARAMETER        ( MBLANK =  1, MFIXED =  2, MFREE = 3  )
      PARAMETER        ( MNAME =  4, MTEMP =  5 )
      PARAMETER        ( MGLOB =  6, MINDIV =  7, MENDAT =  8 )
      INTEGER          LENIND( MENDAT )
      CHARACTER * 12   INDIC8( MENDAT ), HEADER
      PARAMETER        ( MAXNUL = 20 )
      CHARACTER * 65   NULINA( MAXNUL )
      CHARACTER * 10   CQSQR, CQPROD
      PARAMETER      ( CQSQR = '123456789S' )
      PARAMETER      ( CQPROD = '123456789P' )

!  Data declarations.

      DATA INCRSE / 'LENGTH', 'NINMAX', '      ', '      ', '      ',  &
                    '      ', '      ', '      ', '      ', '      ',  &
                    'NUMAX ', 'NSETVC'                               /
      DATA INDIC8( MBLANK ) / '            ' /, LENIND( MBLANK ) / 0  / 
      DATA INDIC8( MFIXED ) / 'FIXED FORMAT' /, LENIND( MFIXED ) / 12 /
      DATA INDIC8( MFREE  ) / 'FREE FORMAT ' /, LENIND( MFREE  ) / 11 /
      DATA INDIC8( MNAME  ) / 'ELEMENTS    ' /, LENIND( MNAME  ) / 8  /
      DATA INDIC8( MTEMP  ) / 'TEMPORARIES ' /, LENIND( MTEMP  ) / 11 / 
      DATA INDIC8( MGLOB  ) / 'GLOBALS     ' /, LENIND( MGLOB  ) / 7  /
      DATA INDIC8( MINDIV ) / 'INDIVIDUALS ' /, LENIND( MINDIV ) / 11 / 
      DATA INDIC8( MENDAT ) / 'ENDATA      ' /, LENIND( MENDAT ) / 6  /
      DATA FIELDI(  1 ) / 'ELFUNF  ' /,  FIELDI(  2 ) / 'LFUVAL  ' /
      DATA FIELDI(  3 ) / 'FUVALS  ' /,  FIELDI(  4 ) / 'XVALUE  ' /
      DATA FIELDI(  5 ) / 'NCALCF  ' /,  FIELDI(  6 ) / 'ITYPEE  ' /
      DATA FIELDI(  7 ) / 'ISTAEV  ' /,  FIELDI(  8 ) / 'IELVAR  ' /
      DATA FIELDI(  9 ) / 'INTVAR  ' /,  FIELDI( 10 ) / 'ISTADH  ' /
      DATA FIELDI( 11 ) / 'ICALCF  ' /,  FIELDI( 12 ) / 'IFFLAG  ' /
      DATA FIELDI( 13 ) / 'IELEMN  ' /,  FIELDI( 14 ) / 'IELTYP  ' /
      DATA FIELDI( 15 ) / 'IHSTRT  ' /,  FIELDI( 16 ) / 'ILSTRT  ' /
      DATA FIELDI( 17 ) / 'IGSTRT  ' /,  FIELDI( 18 ) / 'EPVALU  ' /
      DATA FIELDI( 19 ) / 'ISTEPA  ' /,  FIELDI( 20 ) / 'IPSTRT  ' /
      DATA FIELDI( 21 ) / 'JCALCF  ' /,  FIELDI( 22 ) / 'LTYPEE  ' /  
      DATA FIELDI( 23 ) / 'LSTAEV  ' /,  FIELDI( 24 ) / 'LELVAR  ' /
      DATA FIELDI( 25 ) / 'LNTVAR  ' /,  FIELDI( 26 ) / 'LSTADH  ' /
      DATA FIELDI( 27 ) / 'LSTEPA  ' /,  FIELDI( 28 ) / 'LCALCF  ' /    
      DATA FIELDI( 29 ) / 'LFVALU  ' /,  FIELDI( 30 ) / 'LXVALU  ' /
      DATA FIELDI( 31 ) / 'LEPVLU  ' /,  FIELDI( 32 ) / 'IFSTAT  ' /    
      DATA FIELDI( 33 ) / 'ELFUN   ' /    
!     DATA FIELDI( 33 ) / 'ELFUND  ' /    
      DATA ZERO         / 0.0D+0 /
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2900 )

!  Set initial values for integer variables.

      NINNAM = 0
      NRENAM = 0
      NLONAM = 0
      NMINAM = 0
      NEXNAM = 0
      LINENO = 0
      NLOOP = NELTYP + 1
      INTYPE = 1
      ILINES = 0
      NLINES = 0
      NTEM = 0

!  Set initial values for logical variables.

      DEFNAM = .FALSE.
      ENDPAR = .FALSE.
      STARTP = .FALSE.
      ENDGEN = .FALSE.
      FIRSTL = .TRUE.
      NOINTE = .FALSE.
      FIXED = .TRUE.
      GOTLIN = .FALSE.
      OUTFF = IOUTFF .GT. 0

!  Assign unique names to variables from quadratic terms

      I1 = 0
      I2 = 1
      I3 = 1
      I4 = 1
      I5 = 1
      I6 = 1
      CQSQRT = .FALSE.
      CQPRDT = .FALSE.
      DO 1 ITYPE = 1, MIN( 2, NELTYP )
         IF ( ETYPES( ITYPE ) .EQ. CQSQR ) CQSQRT = .TRUE.
         IF ( ETYPES( ITYPE ) .EQ. CQPROD ) CQPRDT = .TRUE.
    1 CONTINUE
      IF ( CQPRDT ) THEN
         YVAR = NUNAME( I1, I2, I3, I4, I5, I6, IIRES, &
                        NINMAX, NRENAM, NINNAM, NLONAM, &
                        NMINAM, NEXNAM, NLMAX , NELTYP, '      ',  &
                        FIELDI, RENAME, INNAME, LONAME, &
                        MINAME, EXNAME, ETYPES )
      ELSE
         YVAR = '      '
      END IF
      IF ( CQSQRT .OR.  CQPRDT ) THEN
         XVAR = NUNAME( I1, I2, I3, I4, I5, I6, IIRES, &
                        NINMAX, NRENAM, NINNAM, NLONAM, &
                        NMINAM, NEXNAM, NLMAX , NELTYP, YVAR  ,  &
                        FIELDI, RENAME, INNAME, LONAME, &
                        MINAME, EXNAME, ETYPES )
      ELSE
         XVAR = '      '
      END IF

!  Create a dictionary of the internal variable names used.

      NINAME = IINV( NELTYP + 1 ) - 1
      DO 20 I = 1, NINAME
         FIELD = INAMES( I ) // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = INAMES( I )
         END IF
   20 CONTINUE

!  Include the names of the elemental variables used in this dictionary.

      NENAME = IELV( NELTYP + 1 ) - 1
      DO 30 I = 1, NENAME
         FIELD = ENAMES( I ) // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = ENAMES( I )
         END IF
   30 CONTINUE
!     NETNAM = NRENAM

!  Include the names of the elemental parameters used
!  in this dictionary.

      NPNAME = IEPA( NELTYP + 1 ) - 1
      DO 40 I = 1, NPNAME
         FIELD = EPNAME( I ) // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = EPNAME( I )
         END IF
   40 CONTINUE

!  Find which element types have an internal representation.

      MAXNIN = 1
      MAXNEL = 1
      ISETTY = 0
      DO 50 ITYPE = 1, NELTYP
         LDEFND( ITYPE ) = .FALSE.
         IF ( IELV( ITYPE + 1 ) - IELV( ITYPE ) .EQ.&
              IINV( ITYPE + 1 ) - IINV( ITYPE ) ) THEN
            IJUMP( ITYPE ) = 99998
            NOINTE = .TRUE.
         ELSE
            IJUMP( ITYPE ) = ITYPE
         END IF
         MAXNIN = MAX( MAXNIN, IINV( ITYPE + 1 ) - IINV( ITYPE ) )
         MAXNEL = MAX( MAXNEL, IELV( ITYPE + 1 ) - IELV( ITYPE ) )
   50 CONTINUE

!  Set a blank line.

      DO 60 I = 1, MXRECL
         BLNKLN( I: I ) = ' '
   60 CONTINUE    

!  Read next line.

  100 CONTINUE
      IF ( ILINES + 1 .GT. NLINES ) THEN

!  Read next line from the input file.

         LINENO = LINENO + 1
         NULINE = BLNKLN
         IF ( FIXED ) THEN
            READ ( INPUT, 1000, END = 590, ERR = 590 ) NULINE
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2990 )  &
                 LINENO, NULINE
         ELSE
            READ ( INPUT, 1010, END = 590, ERR = 590 ) NULINE
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2970 )  &
                 LINENO, NULINE

!  If the card is in free format, translate it into fixed format.

            CALL  FREEFM( NULINE, MXRECL, MENDAT, INDIC8, LENIND,  &
                          NULINA, MAXNUL, NLINES, .FALSE.,  &
                          INFORM, IOUT )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( NLINES .GT. 0 ) THEN

!  If there are non-blank lines on the free format card, read the first.

               ILINES = 1
               NULINE = BLNKLN
               NULINE = NULINA( ILINES )
               IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
                    LINENO, ILINES, NULINE
            ELSE

!  There are only blank lines on the free format card.

               GO TO 100
            END IF
         END IF
      ELSE

!  Read next line from the last encountered free format card.

         ILINES = ILINES + 1
         NULINE = BLNKLN
         NULINE = NULINA( ILINES )
         IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
              LINENO, ILINES, NULINE
      END IF

!  Consider the header part of the card.

      HEADER = NULINE( 1: 12 )

!  Ignore blank lines.

      IF ( HEADER .EQ. INDIC8( MBLANK ) ) GO TO 100
      IF ( NULINE( 1: 1 ) .NE. ' ' ) THEN

!  Ignore comment cards.

         IF ( NULINE( 1: 1 ) .EQ. '*' ) GO TO 100

!  Check if we have entered fixed-format input.

         IF ( HEADER .EQ. INDIC8( MFIXED ) ) THEN
            FIXED = .TRUE.
            GO TO 100
         END IF

!  Check if we have entered free-format input.

         IF ( HEADER .EQ. INDIC8( MFREE ) ) THEN
            FIXED = .FALSE.
            GO TO 100
         END IF

!  Check that the first encountered indicator card is the elements card.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( HEADER .NE. INDIC8( MNAME ) ) THEN
               IF ( NELTYP .GT. 0 ) GO TO 930
               IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010)
               GOTLIN = .TRUE.
               GO TO 600
            ELSE

!  Indicator card is elements.
!  ---------------------------

               IF ( PNAME  .NE. NULINE( 15: 22 ) ) THEN
                  INFORM = 51
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2510 )
                  GO TO 800
               ELSE
                  DEFNAM = .TRUE.

!  -------- set up subroutine call for RANGE routine.

                  IF ( SINGLE ) THEN
                     WRITE( IOUTRA, 4001 ) PNAME
                  ELSE
                     WRITE( IOUTRA, 4000 ) PNAME
                  END IF
                  IF ( NELTYP .GT. 1 ) THEN
                     WRITE( IOUTRA, 4040 ) ( IJUMP( I ), I = 1, NELTYP )
                     WRITE( IOUTRA, 4050 )
                  END IF
                  GO TO 100
               END IF
            END IF
         END IF

!  An indicator card has been found.

         DO 110 I = INTYPE, MENDAT
            IF ( HEADER .EQ. INDIC8( I ) ) THEN
               INTYPE = I
               GO TO 120
            END IF
  110    CONTINUE

!  The indicator card is not recognised.

         INFORM = 2
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2020 )
         GO TO 800
  120    CONTINUE

!  The parameter values have been completed. Write out the
!  first part of the generated subroutine.

         IF ( INTYPE .GE. MGLOB .AND. .NOT. ENDPAR ) THEN
            ENDPAR = .TRUE.

!  Insert the list of reserved INTEGER/REAL/LOGICAL variables into
!  the dictionary.

            DO 130 I = 1, IIRES
               FIELD = FIELDI( I ) // '  PF'
               CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
               IF ( IFREE .LE. 0 ) THEN
                  IF ( IFREE .EQ. 0 ) THEN
                     INFORM = - 1
                     GO TO 700
                  END IF
                  INFORM = 59
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2590 ) FIELDI( I )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- Set up subroutine call and reserved parameter declarations.

            IF ( IAUTO .EQ. 1 ) THEN
               IF ( SINGLE ) THEN
                  AORB = 'FORWARD_SINGLE '
               ELSE
                  AORB = 'FORWARD_DOUBLE '
               END IF
            ELSE
               IF ( SINGLE ) THEN
                  AORB = 'BACKWARD_SINGLE'
               ELSE
                  AORB = 'BACKWARD_DOUBLE'
               END IF
            END IF
            IF ( IAD0 .EQ. 1 ) THEN
               AD0 = 'AD01'
            ELSE
               AD0 = 'AD02'
            END IF
            IF ( SINGLE ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3001 ) FIELDI( 1 )(1:6),  &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
           FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6), &
           FIELDI( 12 )(1:6), ( FIELDI(  I )(1:6), I = 22, 32 ),  &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
           PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
           FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
           FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
               WRITE( IOUTFD, 3005 ) FIELDI( 33 )(1:6),  &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
        ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
          FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), AD0, AORB,  &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
        ( FIELDI(  I )(1:6), I = 22, 32 ), FIELDI(  6 )(1:6), &
          FIELDI( 22 )(1:6), FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
          PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
          FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
          FIELDI( 17 )(1:6), FIELDI( 20 )(1:6),  &
          FIELDI( 21 )(1:6), MAXNIN
            ELSE   
               IF ( OUTFF ) WRITE( IOUTFF, 3000 ) FIELDI( 1 )(1:6),  &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
        ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
          FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6), &
          FIELDI( 12 )(1:6), ( FIELDI(  I )(1:6), I = 22, 32 ),  &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
          FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
          PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
          FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
          FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
               WRITE( IOUTFD, 3004 ) FIELDI( 33 )(1:6),  &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
        ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
          FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), AD0, AORB,  &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
        ( FIELDI(  I )(1:6), I = 22, 32 ),  &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), FIELDI(  7 )(1:6), &
          FIELDI( 23 )(1:6), FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
          PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
          FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
          FIELDI( 17 )(1:6), FIELDI( 20 )(1:6),  &
          FIELDI( 21 )(1:6), MAXNIN
            END IF
            IF ( IAD0 .EQ. 1 ) THEN
               WRITE( IOUTFD, 3024 ) MAXNEL, MAXNIN
            ELSE
               WRITE( IOUTFD, 3025 ) MAXNEL, MAXNIN
            END IF

! --------- Insert integer declarations.

            IF ( NINNAM .GT. 0 .AND. OUTFF )  &
               WRITE( IOUTFF, 3010 ) ( INNAME( I ), I = 1, NINNAM )
            IF ( NINNAM .GT. 0 )  &
               WRITE( IOUTFD, 3010 ) ( INNAME( I ), I = 1, NINNAM )

!  Order the real values so that the list of variables which belong
!  to intrinsic or external functions follow those which do not.

            IF ( NRENAM .GT. 0 ) THEN
               NRENM1 = 0
               NRENM2 = NRENAM + 1
  140          CONTINUE
               IF ( NRENM1 + 1 .EQ. NRENM2 ) GO TO 180
               DO 150 I = 1, NMINAM
                  IF ( RENAME( NRENM1 + 1 ) .EQ. MINAME( I ) ) GO TO 170
  150          CONTINUE
               DO 160 I = 1, NEXNAM
                  IF ( RENAME( NRENM1 + 1 ) .EQ. EXNAME( I ) ) GO TO 170
  160          CONTINUE
               NRENM1 = NRENM1 + 1
               GO TO 140
  170          CONTINUE
               NRENM2 = NRENM2 - 1
               CTEMP = RENAME( NRENM2 )
               RENAME( NRENM2 ) = RENAME( NRENM1 + 1 )
               RENAME( NRENM1 + 1 ) = CTEMP
               GO TO 140
  180          CONTINUE

! --------- Insert real declarations.

               IF ( OUTFF ) THEN
                  IF ( SINGLE ) THEN
                    WRITE( IOUTFF, 3019 ) ( RENAME( I ), I = 1, NRENAM )
                  ELSE
                    WRITE( IOUTFF, 3020 ) ( RENAME( I ), I = 1, NRENAM )
                  END IF
               END IF
               IF ( IAD0 .EQ. 1 ) THEN
                  IF ( NRENM1 .GT. 0 ) WRITE( IOUTFD, 3018 ) &
                       ( RENAME( I ), I = 1, NRENM1 )
               ELSE
                  IF ( NRENM1 .GT. 0 ) WRITE( IOUTFD, 3017 ) &
                       ( AD0, RENAME( I ), I = 1, NRENM1 )
               END IF
               IF ( NRENM2 .LE. NRENAM ) WRITE( IOUTFD, 3017 ) &
                    ( AD0, RENAME( I ), I = NRENM2, NRENAM )
            END IF

! --------- Insert logical declarations.

            IF ( NLONAM .GT. 0 .AND. OUTFF )  &
               WRITE( IOUTFF, 3023 ) ( LONAME( I ), I = 1, NLONAM )
            IF ( NLONAM .GT. 0 )  &
               WRITE( IOUTFD, 3023 ) ( LONAME( I ), I = 1, NLONAM )

! --------- Insert intrinsic declarations.

            IF ( NMINAM .GT. 0 .AND. OUTFF )  &
               WRITE( IOUTFF, 3021 ) ( MINAME( I ), I = 1, NMINAM )

! --------- Insert external declarations.

            IF ( NEXNAM .GT. 0 .AND. OUTFF )  &
               WRITE( IOUTFF, 3022 ) ( EXNAME( I ), I = 1, NEXNAM )
            IF ( NEXNAM .GT. 0 )  &
               WRITE( IOUTFD, 3022 ) ( EXNAME( I ), I = 1, NEXNAM )

! --------- Insert variables for quadratic terms (if any)

            IF ( XVAR .NE. '      ' ) THEN
               IF ( YVAR .NE. '      ' ) THEN
                  IF ( SINGLE ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3019 ) XVAR, YVAR
                     WRITE( IOUTFD, 3019 ) XVAR, YVAR
                  ELSE
                     IF ( OUTFF ) WRITE( IOUTFF, 3020 ) XVAR, YVAR
                     WRITE( IOUTFD, 3020 ) XVAR, YVAR
                  END IF
               ELSE
                  IF ( SINGLE ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3019 ) XVAR
                     WRITE( IOUTFD, 3019 ) XVAR
                  ELSE
                     IF ( OUTFF ) WRITE( IOUTFF, 3020 ) XVAR
                     WRITE( IOUTFD, 3020 ) XVAR
                  END IF
               END IF
            END IF
            IF ( OUTFF ) WRITE( IOUTFF, 3009 ) FIELDI( 32 )(1:6)
            WRITE( IOUTFD, 3009 ) FIELDI( 32 )(1:6)
         END IF

!  The general parameter assignments have been completed.
!  Continue with the construction of the generated subroutine.

         IF ( INTYPE .GE. MINDIV .AND. .NOT. ENDGEN ) THEN
            ENDGEN = .TRUE.

! --------- Start loop over elements.

            IF ( OUTFF ) WRITE( IOUTFF, 3050 ) NLOOP,  &
                   FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),  &
                   FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),  &
                   FIELDI( 21 )(1:6),  &
                   FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),  &
                   FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),  &
                   FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),  &
                   FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),  &
                   FIELDI( 13 )(1:6),  &
                   FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),  &
                   FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
            IF ( IAD0 .EQ. 1 ) THEN
              WRITE( IOUTFD, 3008 ) 
            ELSE
              WRITE( IOUTFD, 3011 )
!             DO I = 1, NETNAM
              DO I = 1, NRENM1
                 WRITE( IOUTFD, 3016 ) AD0, RENAME( I )
              END DO
            END IF  
            WRITE( IOUTFD, 3050 ) NLOOP,  &
                   FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),  &
                   FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),  &
                   FIELDI( 21 )(1:6),  &
                   FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),  &
                   FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),  &
                   FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),  &
                   FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),  &
                   FIELDI( 13 )(1:6),  &
                   FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),  &
                   FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
            IF ( NELTYP .GT. 1 ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3051 )  &
                  FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),  &
                  FIELDI( 13 )(1:6), ( I, I = 1, NELTYP )
               WRITE( IOUTFD, 3051 )  &
                  FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),  &
                  FIELDI( 13 )(1:6), ( I, I = 1, NELTYP )
               IF ( OUTFF ) WRITE( IOUTFF, 3052 ) FIELDI( 14 )(1:6)
               WRITE( IOUTFD, 3052 ) FIELDI( 14 )(1:6)
            END IF

!  Make sure that quadratic Hessian terms are included

            DO 190 ITYPE = 1, MIN( 2, NELTYP )

!  Diagonal term

               IF ( ETYPES( ITYPE ) .EQ. CQSQR ) THEN
                  IF ( OUTFF ) WRITE( IOUTFF, 3060 ) ETYPES( ITYPE )
                  WRITE( IOUTFD, 3060 ) ETYPES( ITYPE )
                  IF ( NELTYP .GT. 1 ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3061 ) ITYPE
                     WRITE( IOUTFD, 3061 ) ITYPE
                  END IF
                  IF ( SINGLE ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3055 ) 'E'
                     WRITE( IOUTFD, 3057 ) &
                        XVAR, 'E', XVAR, XVAR, XVAR, 'E'
                  ELSE
                     IF ( OUTFF ) WRITE( IOUTFF, 3055 ) 'D'
                     WRITE( IOUTFD, 3057 )  &
                        XVAR, 'D', XVAR, XVAR, XVAR, 'D'
                  END IF
                  LDEFND( ITYPE ) = .TRUE.
                  ISETTY = ISETTY + 1
                  IF ( ISETTY .LT. NELTYP ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3191 ) NLOOP
                     WRITE( IOUTFD, 3191 ) NLOOP
                  END IF

!  Off-diagonal term

               ELSE IF ( ETYPES( ITYPE ) .EQ. CQPROD ) THEN
                  IF ( OUTFF ) WRITE( IOUTFF, 3060 ) ETYPES( ITYPE )
                  WRITE( IOUTFD, 3060 ) ETYPES( ITYPE )
                  IF ( NELTYP .GT. 1 ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3061 ) ITYPE
                     WRITE( IOUTFD, 3061 ) ITYPE
                  END IF
                  IF ( SINGLE ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3056 )
                     WRITE( IOUTFD, 3058 ) &
                       XVAR, YVAR, XVAR, YVAR, YVAR, XVAR, 'E', 'E', 'E'
                  ELSE
                     IF ( OUTFF ) WRITE( IOUTFF, 3056 )
                     WRITE( IOUTFD, 3058 )  &
                       XVAR, YVAR, XVAR, YVAR, YVAR, XVAR, 'D', 'D', 'D'
                  END IF
                  LDEFND( ITYPE ) = .TRUE.
                  ISETTY = ISETTY + 1
                  IF ( ISETTY .LT. NELTYP ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3191 ) NLOOP
                     WRITE( IOUTFD, 3191 ) NLOOP
                  END IF
               END IF
  190       CONTINUE   
         END IF

!  Indicator card is ENDATA.
!  -------------------------

         IF ( INTYPE .EQ. MENDAT ) GO TO 900
         GO TO 100
      ELSE

!  Check that the first non commment card is the elements indicator card.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( NELTYP .GT. 0 ) GO TO 930
            IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )
            GOTLIN = .TRUE.
            GO TO 600
         END IF

!  A data card has been found.
!  Read the character fields 1 and 2 from the card.

         FIELD1 = NULINE(  2:  3 )
         FIELD2 = NULINE(  5: 14 )
         IF ( INTYPE .EQ. MINDIV .AND. FIELD1 .EQ. 'R ' ) THEN

!  Read the character fields 3 and 5 from the card.

            FIELDS( 1 ) = NULINE( 15: 22 )
            FIELDS( 2 ) = NULINE( 40: 47 )

!  CHECK TO SEE IF THERE IS ARE ANY NUMERICAL VALUES TO BE READ.

            NOVALS = 0
            IF ( FIELDS( 1 ) .NE. '        ' .AND.&
                 NULINE( 15: 15 ) .NE. '[' ) THEN
               NOVALS = 1
               CALL GETVAL( NULINE( 25: 36 ), VALUES( 1 ) )
               IF ( FIELDS( 2 ) .NE. '        ' .AND.&
                 NULINE( 40: 40 ) .NE. '[' ) THEN
                  NOVALS = 2
                  CALL GETVAL( NULINE( 50: 61 ), VALUES( 2 ) )

!  REMOVE FIELDS WITH NUMERICAL VALUES OF ZERO.

                  IF ( VALUES( 2 ) .EQ. ZERO ) THEN
                     NOVALS = 1
                  END IF
               END IF
               IF ( VALUES( 1 ) .EQ. ZERO ) THEN
                  IF ( NOVALS .EQ. 2 ) THEN
                     VALUES( 1 ) = VALUES( 2 )
                     FIELDS( 1 ) = FIELDS( 2 )
                  END IF
                  NOVALS = NOVALS - 1
               END IF
            END IF
         ELSE

!  READ THE CHARACTER FIELDS 3 AND 7 FROM THE CARD.

            FIELD3 = NULINE( 15: 22 )
            FIELD7 = NULINE( 25: 65 )

!  CHECK THAT FIELD3 IS BLANK ON 'A', 'F' AND 'G' CARDS.

            IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR.&
                 FIELD1( 1: 1 ) .EQ. 'F' .OR. &
                 FIELD1( 1: 1 ) .EQ. 'G' ) THEN
               IF ( FIELD3 .NE. '       ' ) THEN
                  INFORM = 72
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2720 )
                  GO TO 800
               END IF
            END IF
         END IF
      END IF

!  BRANCH ON THE VALUE OF INTYPE.

      GO TO ( 100, 100, 100, 100, 200, 300, 400, 900 ), INTYPE

!  INDICATOR CARD IS TEMPORARIES.
!  ------------------------------

  200 CONTINUE

!  CHECK TO SEE IF THE PARAMETER IS INTEGER, REAL, LOGICAL OR A FUNCTION.

      IF ( FIELD1 .NE. 'I ' .AND. FIELD1 .NE. 'R ' .AND.&
           FIELD1 .NE. 'M ' .AND. FIELD1 .NE. 'F ' .AND.&
           FIELD1 .NE. 'L ' ) THEN
         INFORM = 54
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2540 )
         GO TO 800
      END IF

!  IF THE PARAMETER IS A FUNCTION, CHECK TO SEE THAT THE NAME HAS
!  NOT ALREADY BEEN USED.

      IF ( FIELD1 .EQ. 'F ' ) THEN
         FIELD = FIELD2 // 'FU'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NEXNAM = NEXNAM + 1
            IF ( NEXNAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            EXNAME( NEXNAM ) = FIELD2
         END IF
      ELSE

!  CHECK TO SEE THAT THE PARAMETER NAME HAS NOT ALREADY BEEN USED.

         FIELD = FIELD2 // 'PF'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            IF ( FIELD1 .EQ. 'R ' ) THEN
               NRENAM = NRENAM + 1
               IF ( NRENAM .GT. NINMAX ) THEN
                  INFORM = - 2
                  GO TO 700
               END IF
               RENAME( NRENAM ) = FIELD2
            ELSE
               IF ( FIELD1 .EQ. 'M ' ) THEN
                  NMINAM = NMINAM + 1
                  IF ( NMINAM .GT. NINMAX ) THEN
                     INFORM = - 2
                     GO TO 700
                  END IF
                  MINAME( NMINAM ) = FIELD2
               ELSE
                  IF ( FIELD1 .EQ. 'L ' ) THEN
                     NLONAM = NLONAM + 1
                     IF ( NLONAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     LONAME( NLONAM ) = FIELD2
                  ELSE
                     NINNAM = NINNAM + 1
                     IF ( NINNAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     INNAME( NINNAM ) = FIELD2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS GLOBALS.
!  --------------------------

  300 CONTINUE
      IF ( FIELD1 .EQ. 'A ' .OR. FIELD1 .EQ. 'I ' .OR.&
           FIELD1 .EQ. 'E ' ) THEN
         STARTP = .TRUE.

!  START A PARAMETER ASSIGNMENT. CHECK TO SEE THAT THE PARAMETER HAS
!  BEEN DEFINED.

         FIELD = FIELD2 // 'PF'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
            INFORM = 57
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
            GO TO 800
         END IF

! --------- MAKE GENERAL PARAMETER ASSIGNMENTS.

         IF ( FIELD1 .EQ. 'A ' ) THEN
            IF ( OUTFF ) WRITE( IOUTFF, 3030 ) FIELD2(1:6), FIELD7
            NTEM = NTEM + 1
            WRITE( IOUTEM, 3080 ) FIELD2(1:6), FIELD7

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

         ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

            FIELD = FIELD3 // '  PF'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  GO TO 700
               END IF
               INFORM = 57
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
               GO TO 800
            END IF
            IF ( FIELD1 .EQ. 'I ' ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3031 ) FIELD2(1:6),  &
                                     FIELD3(1:6), FIELD7
               NTEM = NTEM + 1
               WRITE( IOUTEM, 3081 ) FIELD2(1:6),  &
                                     FIELD3(1:6), FIELD7
            ELSE
               IF ( OUTFF ) WRITE( IOUTFF, 3032 ) FIELD2(1:6),  &
                                     FIELD3(1:6), FIELD7
               NTEM = NTEM + 1
               WRITE( IOUTEM, 3082 ) FIELD2(1:6),  &
                                     FIELD3(1:6), FIELD7
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) .EQ. '+' .AND. STARTP ) THEN

! --------- CONTINUE A PARAMETER ASSIGNMENT.

           IF ( OUTFF ) WRITE( IOUTFF, 3040 ) FIELD7
            NTEM = NTEM + 1
            WRITE( IOUTEM, 3040 ) FIELD7
         ELSE
            INFORM = 55
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS INDIVIDUALS.
!  ------------------------------

  400 CONTINUE

!  CHECK IF A NEW ELEMENT HAS BEEN ENCOUNTERED.

      IF ( FIELD1 .EQ. 'T ' ) THEN

!  CHECK TO SEE IF THE RANGE OF A NEW ELEMENT IS TO BE DEFINED.

         IF ( FIRSTL ) THEN

!  CHECK IF THIS IS THE FIRST ELEMENT.

            FIRSTL = .FALSE.
         ELSE

!  FINISH OF THE PREVIOUS ELEMENT, IF ANY.

            IF ( STARTF ) THEN
               IF ( .NOT. ENDOFF ) THEN
                  IF ( OUTFF ) THEN
                     WRITE( IOUTFF, 3120 )
                     WRITE( IOUTFF, 3121 )
                  END IF
                  IF ( IAD0 .EQ. 1 ) THEN
                     WRITE( IOUTFD, 3122 ) &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                        NINVAR
                  ELSE
                     WRITE( IOUTFD, 3123 ) &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                        NINVAR
                  END IF
                  WRITE( IOUTFD, 3150 ) FIELDI( 12 )(1:6)
                  IF ( IAD0 .EQ. 1 ) THEN
                    WRITE( IOUTFD, 3151 ) 
                  ELSE
                    WRITE( IOUTFD, 3152 ) 
                  END IF
                  DO 402 JS = 1, NINVAR
                     DO 401 IS = 1, JS
                        IHVAR = ( JS * ( JS - 1 ) ) / 2 + IS
!                       JHVAR = NINVAR * ( IS - 1 ) + JS -
!    *                          ( IS * ( IS - 1 ) ) / 2
                        IF ( IS .EQ. JS ) THEN
                           WRITE( IOUTFD, 3163 ) &
                              FIELDI(  3 )(1:6),  &
                              FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                        FIELDI( 15 )(1:6), IHVAR, JHVAR
                        ELSE
                           WRITE( IOUTFD, 3164 ) &
                              FIELDI(  3 )(1:6),  &
                              FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                        FIELDI( 15 )(1:6), IHVAR, JHVAR
                        END IF
  401                CONTINUE
  402             CONTINUE   
                  ENDOFF = .TRUE.
               END IF
               IF ( OUTFF ) WRITE( IOUTFF, 3190 )
               WRITE( IOUTFD, 3180 )
               WRITE( IOUTFD, 3190 )
            ELSE
               INFORM = 61
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
               GO TO 800
            END IF
            IF ( ISETTY .LT. NELTYP .AND. OUTFF ) &
               WRITE( IOUTFF, 3191 ) NLOOP
            IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFD, 3191 ) NLOOP
         END IF

!  FIND ITYPE, THE ELEMENT TYPE.

         FIELD = FIELD2 // 'ET'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )

!  THE ELEMENT TYPE IS UNKNOWN.

         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 9
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2090 )
            GO TO 800
         END IF

! --------- FIND TYPE OF CURRENT ELEMENT.

         ITYPE = INLIST( IFIELD )
         IF ( OUTFF ) WRITE( IOUTFF, 3060 ) FIELD2
         WRITE( IOUTFD, 3060 ) FIELD2
         IF ( NELTYP .GT. 1 .AND. OUTFF ) WRITE( IOUTFF, 3061 ) ITYPE
         IF ( NELTYP .GT. 1 ) WRITE( IOUTFD, 3061 ) ITYPE
         IF ( LDEFND( ITYPE ) ) THEN
            INFORM = 67
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2670 )
            GO TO 800
         ELSE
            LDEFND( ITYPE ) = .TRUE.
            ISETTY = ISETTY + 1
         END IF

!  FIND THE ROW AND COLUMN DIMENSIONS (NINV AND NELV, RESP.) OF THE
!  TRANSFORMATION MATRIX U. U IS STORED IN VECTOR FORM BY COLUMNS.

         IS = IINV( ITYPE ) - 1
         JS = IELV( ITYPE ) - 1
         NELV = IELV( ITYPE + 1 ) - IELV( ITYPE )
         NINV = IINV( ITYPE + 1 ) - IINV( ITYPE )
         NN = NINV * NELV
         IF ( NN .GT. NUMAX ) THEN
            INFORM = - 11
            GO TO 700
         END IF

! --------- FIND TYPE OF CURRENT ELEMENT.

         IF ( NELV .GT. NINV ) WRITE( IOUTRA, 4060 ) FIELD2, ITYPE

!  INITIALIZE U AS THE ZERO MATRIX.

         DO 420 I = 1, NN
            U( I ) = ZERO
  420    CONTINUE
         SETRAN = NELV .GT. NINV

! --------- SET ELEMENTAL VARIABLES.

         K1 = IELV( ITYPE )
         K2 = IELV( ITYPE + 1 ) - 1
         IF ( SETRAN ) THEN
            IF ( IAD0 .EQ. 1 ) THEN
               WRITE( IOUTFD, 3230 ) NELV, &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6), &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), NELV
            ELSE
               WRITE( IOUTFD, 3231 ) NELV, &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6), &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), NELV
            END IF
            DO 430 K = K1, K2
               IVAR = K - K1 + 1
               IF ( OUTFF ) &
               WRITE( IOUTFF, 3070 ) ENAMES( K ), FIELDI(  4 )(1:6),  &
                   FIELDI(  8 )(1:6), FIELDI( 16 )(1:6), IVAR
               WRITE( IOUTFD, 3220 ) ENAMES( K ), IVAR
  430       CONTINUE
         ELSE
            IF ( IAD0 .EQ. 1 ) THEN
               WRITE( IOUTFD, 3210 ) FIELDI( 12 )(1:6), NINV, &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6), &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), NINV
            ELSE
               WRITE( IOUTFD, 3211 ) FIELDI( 12 )(1:6), NINV, &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6), &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), NINV
            END IF
            DO 431 K = K1, K2
               IVAR = K - K1 + 1
               IF ( OUTFF ) &
               WRITE( IOUTFF, 3070 ) ENAMES( K ), FIELDI(  4 )(1:6),  &
                   FIELDI(  8 )(1:6), FIELDI( 16 )(1:6), IVAR
               WRITE( IOUTFD, 3220 ) ENAMES( K ), IVAR
  431       CONTINUE
         END IF

!  FIND THE NUMBER OF INTERNAL VARIABLES AND THE REQUIRED SIZE OF
!  THE LOWER TRIANGULAR PORTION OF THE HESSIAN MATRIX.

         K1 = IINV( ITYPE )
         K2 = IINV( ITYPE + 1 ) - 1
         NINVAR = K2 - K1 + 1
         NHESS = NINVAR * ( NINVAR + 1 ) / 2
         NVARS = 0
         NH = 0
         STARTP = .FALSE.
         STARTF = .FALSE.
         ENDOFF = .TRUE.
         STARTV = .FALSE.
      ELSE
         IF ( FIELD1 .EQ. 'R ' ) THEN

!  THE RANGE TRANSFORMATION MATRIX U IS NOW DEFINED, ENTRY BY ENTRY.
!  DETERMINE WHICH INTERNAL VARIABLE IS GIVEN IN FIELD2.

            DO 440 I = 1, NINV
               IF ( FIELD2 .EQ. INAMES( IS + I ) ) GO TO 450
  440       CONTINUE

!  THE INTERNAL VARIABLE NAME IS UNRECOGNISED.

            INFORM = 65
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2650 )
            GO TO 800

!  THE INTERNAL VARIABLE IS THE I-TH IN THE LIST.

  450       CONTINUE

!  DETERMINE WHICH ELEMENTAL VARIABLE(S) OCCUR IN FIELDS.

            IF ( NOVALS .GT. 0 ) THEN
               DO 480 K = 1, NOVALS
                  DO 460 J = 1, NELV
                     IF ( FIELDS( K ) .EQ. ENAMES( JS + J ) ) GO TO 470
  460             CONTINUE

!  THE ELEMENTAL VARIABLE NAME IS UNRECOGNISED.

                  INFORM = 66
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2660 )
                  GO TO 800

!  THE ELEMENTAL VARIABLE IS THE J-TH IN THE LIST.

  470             CONTINUE

!  INSERT THE VALUE OF THE NEW NONZERO INTO U.

                  U( NINV * ( J - 1 ) + I ) = VALUES( K )
  480          CONTINUE
            END IF
         ELSE
            IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR. FIELD1( 1: 1 ) &
                 .EQ. 'I' .OR. FIELD1( 1: 1 ) .EQ. 'E' ) THEN

!  Finish the function assignment

               IF ( STARTF .AND. .NOT. ENDOFF ) THEN
                  IF ( OUTFF ) THEN
                     WRITE( IOUTFF, 3120 )
                     WRITE( IOUTFF, 3121 )
                  END IF
                  IF ( IAD0 .EQ. 1 ) THEN
                     WRITE( IOUTFD, 3122 ) &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                        NINVAR
                  ELSE
                     WRITE( IOUTFD, 3123 ) &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                        NINVAR
                  END IF
                  WRITE( IOUTFD, 3150 ) FIELDI( 12 )(1:6)
                  IF ( IAD0 .EQ. 1 ) THEN
                    WRITE( IOUTFD, 3151 ) 
                  ELSE
                    WRITE( IOUTFD, 3152 ) 
                  END IF
                  DO 482 JS = 1, NINVAR
                     DO 481 IS = 1, JS
                        IHVAR = ( JS * ( JS - 1 ) ) / 2 + IS
!                       JHVAR = NINVAR * ( IS - 1 ) + JS -
!    *                          ( IS * ( IS - 1 ) ) / 2
                        IF ( IS .EQ. JS ) THEN
                           WRITE( IOUTFD, 3163 ) &
                              FIELDI(  3 )(1:6),  &
                              FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                        FIELDI( 15 )(1:6), IHVAR, JHVAR
                        ELSE
                           WRITE( IOUTFD, 3164 ) &
                              FIELDI(  3 )(1:6),  &
                              FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                        FIELDI( 15 )(1:6), IHVAR, JHVAR
                        END IF
  481                CONTINUE
  482             CONTINUE   
                  ENDOFF = .TRUE.
               END IF
               IF ( .NOT. STARTF ) THEN

!  START A PARAMETER ASSIGNMENT.

                  IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                     STARTP = .TRUE.

!  SET UP THE TRANSFORMATIONS FOR THE ELEMENT.

                     IF ( SETRAN ) THEN
                         CALL OUTRN2( NELV, NINV, U, IOUTFF, IOUTFD,  &
                                      IOUTRA, ENAMES( JS + 1 ), &
                                      INAMES( IS + 1 ), SINGLE, AD0 )
                         SETRAN = .FALSE.
                     END IF

! --------- SET ELEMENTAL PARAMETERS.

                     K1 = IEPA( ITYPE )
                     DO 483 K = K1, IEPA( ITYPE + 1 ) - 1
                        IVAR = K - K1 + 1
                        IF ( OUTFF ) &
                        WRITE( IOUTFF, 3071 ) EPNAME( K ), &
                        FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), IVAR
!                       IF ( IAD0 .EQ. 2 ) 
!    *                     WRITE( IOUTFD, 3015 ) AD0, EPNAME( K )
                        WRITE( IOUTFD, 3071 ) EPNAME( K ), &
                            FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), IVAR
  483                CONTINUE

!  Include the global parameters

                     IF ( .NOT. STARTV ) THEN
                        REWIND( IOUTEM )
                        DO 484 I = 1, NTEM
                           READ( IOUTEM, 1000 ) CTEM
                           WRITE( IOUTFD, 1000 ) CTEM
  484                   CONTINUE   
                        STARTV = .TRUE.
                     END IF

!  CHECK TO SEE THAT THE PARAMETER HAS BEEN DEFINED.

                     FIELD = FIELD2 // 'PF'
                     CALL HASHC (LENGTH, 12, FIELD, KEY, ITABLE, IFIELD)
                     IF ( IFIELD .LE. 0 ) THEN
                        INFORM = 58
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2580 )
                        GO TO 800
                     END IF

! --------- MAKE ELEMENT-SPECIFIC PARAMETER ASSIGNMENTS.

                     IF ( FIELD1( 1: 1 ) .EQ. 'A' ) THEN
                        IF ( .NOT. STARTF ) THEN
                           IF ( OUTFF ) &
                           WRITE( IOUTFF, 3080 ) FIELD2(1:6), FIELD7
                           WRITE( IOUTFD, 3080 ) FIELD2(1:6), FIELD7
                        ELSE
                           IF ( OUTFF ) &
                           WRITE( IOUTFF, 3083 ) FIELD2(1:6), FIELD7
                           WRITE( IOUTFD, 3083 ) FIELD2(1:6), FIELD7
                        END IF

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

                     ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

                        FIELD = FIELD3 // '  PF'
                        CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE,  &
                                     IFIELD )
                        IF ( IFIELD .LE. 0 ) THEN
                           IF ( IFREE .EQ. 0 ) THEN
                              INFORM = - 1
                              GO TO 700
                           END IF
                           INFORM = 58
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2580 )
                           GO TO 800
                        END IF
                        IF ( FIELD1( 1: 1 ) .EQ. 'I' ) THEN
                           IF ( .NOT. STARTF ) THEN
                              IF ( OUTFF ) &
                              WRITE( IOUTFF, 3081 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                              WRITE( IOUTFD, 3081 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                           ELSE
                              IF ( OUTFF ) &
                              WRITE( IOUTFF, 3084 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                              WRITE( IOUTFD, 3084 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                           END IF
                        ELSE
                        IF ( .NOT. STARTF ) THEN
                              IF ( OUTFF ) &
                              WRITE( IOUTFF, 3082 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                              WRITE( IOUTFD, 3082 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                           ELSE
                              IF ( OUTFF ) &
                              WRITE( IOUTFF, 3085 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                              WRITE( IOUTFD, 3085 ) FIELD2(1:6),  &
                                                 FIELD3(1:6), FIELD7
                           END IF
                        END IF   
                     END IF
                  ELSE
                     IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                        IF ( STARTP ) THEN

! --------- CONTINUATION OF A PARAMETER ASSIGNMENT.

                           IF ( .NOT. STARTF ) THEN
                              IF ( OUTFF ) WRITE( IOUTFF, 3090 ) FIELD7
                              WRITE( IOUTFD, 3090 ) FIELD7
                           ELSE
                              IF ( OUTFF ) WRITE( IOUTFF, 3091 ) FIELD7
                              WRITE( IOUTFD, 3091 ) FIELD7
                           END IF
                        ELSE
                           INFORM = 56
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               END IF
            ELSE
               STARTP = .FALSE.
               IF ( FIELD1( 1: 1 ) .EQ. 'F' ) THEN

!  SET THE FUNCTION VALUE.

                  IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                     STARTF = .TRUE.
                     ENDOFF = .FALSE.

!  SET UP THE TRANSFORMATIONS FOR THE ELEMENT.

                     IF ( SETRAN ) THEN
                         CALL OUTRN2( NELV, NINV, U, IOUTFF, IOUTFD, &
                                      IOUTRA, ENAMES( JS + 1 ), &
                                      INAMES( IS + 1 ), SINGLE, AD0 )
                         SETRAN = .FALSE.
                     END IF

! --------- SET ELEMENTAL PARAMETERS.

                     K1 = IEPA( ITYPE )
                     DO 485 K = K1, IEPA( ITYPE + 1 ) - 1
                        IVAR = K - K1 + 1
                        IF ( OUTFF ) &
                        WRITE( IOUTFF, 3071 ) EPNAME( K ), &
                        FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), IVAR
!                       IF ( IAD0 .EQ. 2 ) 
!    *                     WRITE( IOUTFD, 3015 ) AD0, EPNAME( K )
                        WRITE( IOUTFD, 3071 ) EPNAME( K ), &
                            FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), IVAR
  485                CONTINUE

!  Include the global parameters

                     IF ( .NOT. STARTV ) THEN
                        REWIND( IOUTEM )
                        DO 486 I = 1, NTEM
                           READ( IOUTEM, 1000 ) CTEM
                           WRITE( IOUTFD, 1000 ) CTEM
  486                      CONTINUE   
                        STARTV = .TRUE.
                     END IF

! --------- START F.

                     IF ( OUTFF ) &
                     WRITE( IOUTFF, 3100 ) FIELDI( 12 )(1:6),  &
                     FIELDI( 3 )(1:6), FIELDI( 13 )(1:6), FIELD7
                     WRITE( IOUTFD, 3101 ) FIELD7
                  ELSE
                     IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                        IF ( STARTF ) THEN

! --------- CONTINUATION OF F.

                           IF ( OUTFF ) WRITE( IOUTFF, 3110 ) FIELD7
                           WRITE( IOUTFD, 3110 ) FIELD7
                        ELSE
                           INFORM = 56
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               ELSE IF ( FIELD1( 1: 1 ) .EQ. 'G' .OR.&
                         FIELD1( 1: 1 ) .EQ. 'H' ) THEN
                  IF ( STARTF .AND. .NOT. ENDOFF ) THEN
                     IF ( OUTFF ) THEN
                        WRITE( IOUTFF, 3120 )
                        WRITE( IOUTFF, 3121 )
                     END IF
                     IF ( IAD0 .EQ. 1 ) THEN
                        WRITE( IOUTFD, 3122 ) &
                           FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                           FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                           FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                           NINVAR
                     ELSE
                        WRITE( IOUTFD, 3123 ) &
                           FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                           FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                           FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                           NINVAR
                     END IF
                     WRITE( IOUTFD, 3150 ) FIELDI( 12 )(1:6)
                     IF ( IAD0 .EQ. 1 ) THEN
                       WRITE( IOUTFD, 3151 ) 
                     ELSE
                       WRITE( IOUTFD, 3152 ) 
                     END IF
                     DO 512 JS = 1, NINVAR
                        DO 511 IS = 1, JS
                           IHVAR = ( JS * ( JS - 1 ) ) / 2 + IS
!                          JHVAR = NINVAR * ( IS - 1 ) + JS -
!    *                             ( IS * ( IS - 1 ) ) / 2
                           IF ( IS .EQ. JS ) THEN
                              WRITE( IOUTFD, 3163 ) &
                                 FIELDI(  3 )(1:6),  &
                                 FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                           FIELDI( 15 )(1:6), IHVAR, JHVAR
                           ELSE
                              WRITE( IOUTFD, 3164 ) &
                                 FIELDI(  3 )(1:6),  &
                                 FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                           FIELDI( 15 )(1:6), IHVAR, JHVAR
                           END IF
  511                   CONTINUE
  512                CONTINUE   
                     ENDOFF = .TRUE.
                  END IF
               ELSE
                  INFORM = 56
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                  GO TO 800
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  THE END OF THE INPUT FILE HAS BEEN REACHED BEFORE THE ENDATA CARD.

  590 CONTINUE 

!  IF THE ELEMENTS CARD HAS NOT BEEN ENCOUNTERED, EXIT.

      IF ( DEFNAM ) THEN
         INFORM = 52
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2520 )
         RETURN
      END IF
!     IF ( NELTYP .GT. 0 ) GO TO 930
      QPROD = .FALSE.
      DO 591 ITYPE = 1, MIN( 2, NELTYP ) 
         IF ( ETYPES( ITYPE ) .NE. CQSQR .AND.&
              ETYPES( ITYPE ) .NE. CQPROD ) GO TO 930
         IF ( ETYPES( ITYPE ) .EQ. CQPROD ) QPROD = .TRUE.
  591 CONTINUE   
      IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )

!  A DUMMY ROUTINE WILL BE SUBSTITUTED.

  600 CONTINUE 

!  WRITE A DUMMY ELFUNS ROUTINE.

      IF ( IAUTO .EQ. 1 ) THEN
         IF ( SINGLE ) THEN
            AORB = 'FORWARD_SINGLE '
         ELSE
            AORB = 'FORWARD_DOUBLE '
         END IF
      ELSE
         IF ( SINGLE ) THEN
            AORB = 'BACKWARD_SINGLE'
         ELSE
            AORB = 'BACKWARD_DOUBLE'
         END IF
      END IF
      IF ( IAD0 .EQ. 1 ) THEN
         AD0 = 'AD01'
      ELSE
         AD0 = 'AD02'
      END IF
      IF ( NELTYP .EQ. 0 ) THEN
         IF ( SINGLE ) THEN
            IF ( OUTFF ) WRITE( IOUTFF, 3003 ) FIELDI( 1 )(1:6),  &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),  &
           FIELDI( 18 )(1:6), ( FIELDI(  I )(1:6), I = 5, 10 ),  &
           FIELDI( 19 )(1:6), FIELDI( 11 )(1:6), &
         ( FIELDI(  I )(1:6), I = 22, 31 ),  &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 22, 32 ),  &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), PNAME
            WRITE( IOUTFD, 3007 ) FIELDI( 33 )(1:6),  &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),  &
           FIELDI( 18 )(1:6), ( FIELDI(  I )(1:6), I = 5, 10 ),  &
           FIELDI( 19 )(1:6), FIELDI( 11 )(1:6), &
         ( FIELDI(  I )(1:6), I = 22, 31 ), &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), AD0, AORB, &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
         ( FIELDI(  I )(1:6), I = 22, 32 ),  &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), PNAME
         ELSE   
            IF ( OUTFF ) WRITE( IOUTFF, 3002 ) FIELDI( 1 )(1:6),  &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),  &
          FIELDI( 18 )(1:6), ( FIELDI(  I )(1:6), I = 5, 10 ),  &
          FIELDI( 19 )(1:6), FIELDI( 11 )(1:6), &
        ( FIELDI(  I )(1:6), I = 22, 31 ),  &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
        ( FIELDI(  I )(1:6), I = 22, 32 ),  &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
          FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), PNAME
            WRITE( IOUTFD, 3006 ) FIELDI( 33 )(1:6),  &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),  &
          FIELDI( 18 )(1:6), ( FIELDI(  I )(1:6), I = 5, 10 ),  &
          FIELDI( 19 )(1:6), FIELDI( 11 )(1:6), &
        ( FIELDI(  I )(1:6), I = 22, 31 ), &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), AD0, AORB, &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
        ( FIELDI(  I )(1:6), I = 22, 32 ),  &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
          FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), PNAME
         END IF
         IF ( OUTFF ) WRITE( IOUTFF, 3009 ) FIELDI( 32 )(1:6)
         WRITE( IOUTFD, 3009 ) FIELDI( 32 )(1:6)
         IF ( OUTFF ) WRITE( IOUTFF, 3201 )
!        IF ( IAD0 .EQ. 2 ) THEN
!          WRITE( IOUTFD, 3203 )
!        ELSE
            WRITE( IOUTFD, 3201 )
!        END IF
      ELSE
         IF ( SINGLE ) THEN
           IF ( OUTFF ) WRITE( IOUTFF, 3001 ) FIELDI( 1 )(1:6),  &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
        FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6), &
       FIELDI( 12 )(1:6), ( FIELDI(  I )(1:6), I = 22, 32 ),  &
       FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
       FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
       PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            WRITE( IOUTFD, 3001 ) FIELDI( 33 )(1:6),  &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
       FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
       FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 22, 32 ),  &
       FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
       FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
       PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( QPROD ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3019 ) 'X     ', 'Y     '
               WRITE( IOUTFD, 3019 ) 'X     ', 'Y     '
            ELSE
               IF ( OUTFF ) WRITE( IOUTFF, 3019 ) 'X     '
               WRITE( IOUTFD, 3019 ) 'X     '
            END IF
         ELSE   
            IF ( OUTFF ) WRITE( IOUTFF, 3000 ) FIELDI( 1 )(1:6),  &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
       FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),  &
       FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 22, 32 ), FIELDI(  6 )(1:6), &
       FIELDI( 22 )(1:6), FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
       PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6),  &
       FIELDI( 21 )(1:6)
            WRITE( IOUTFD, 3000 ) FIELDI( 33 )(1:6),  &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),  &
       ( FIELDI(  I )(1:6), I = 5, 10 ), FIELDI( 19 )(1:6), &
       FIELDI( 11 )(1:6), ( FIELDI(  I )(1:6), I = 22, 31 ),  &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6), &
       FIELDI( 12 )(1:6), ( FIELDI(  I )(1:6), I = 22, 32 ),  &
       FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), &
       FIELDI(  7 )(1:6), FIELDI( 23 )(1:6), &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6), &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6), &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6), &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6), &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6), &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6), &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),  &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),  &
       PNAME, FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),  &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),  &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( QPROD ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3020 ) 'X     ', 'Y     '
               WRITE( IOUTFD, 3020 ) 'X     ', 'Y     '
            ELSE
               IF ( OUTFF ) WRITE( IOUTFF, 3020 ) 'X     '
               WRITE( IOUTFD, 3020 ) 'X     '
            END IF
         END IF
         IF ( OUTFF ) WRITE( IOUTFF, 3009 ) FIELDI( 32 )(1:6)
         WRITE( IOUTFD, 3009 ) FIELDI( 32 )(1:6)
         IF ( OUTFF ) WRITE( IOUTFF, 3050 ) NLOOP,  &
                FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),  &
                FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),  &
                FIELDI( 21 )(1:6),  &
                FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),  &
                FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),  &
                FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),  &
                FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),  &
                FIELDI( 13 )(1:6),  &
                FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),  &
                FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
         WRITE( IOUTFD, 3050 ) NLOOP,  &
                FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),  &
                FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),  &
                FIELDI( 21 )(1:6),  &
                FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),  &
                FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),  &
                FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),  &
                FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),  &
                FIELDI( 13 )(1:6),  &
                FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),  &
                FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
         IF ( NELTYP .GT. 1 ) THEN
            IF ( OUTFF ) WRITE( IOUTFF, 3051 )  &
               FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),  &
               FIELDI( 13 )(1:6), ( I, I = 1, NELTYP )
            WRITE( IOUTFD, 3051 )  &
               FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),  &
               FIELDI( 13 )(1:6), ( I, I = 1, NELTYP )
           IF ( OUTFF ) WRITE( IOUTFF, 3052 ) FIELDI( 14 )(1:6)
            WRITE( IOUTFD, 3052 ) FIELDI( 14 )(1:6)
         END IF

!  Make sure that quadratic Hessian terms are included

         DO 640 ITYPE = 1, MIN( 2, NELTYP )

!  Diagonal term

            IF ( ETYPES( ITYPE ) .EQ. CQSQR ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3060 ) ETYPES( ITYPE )
               WRITE( IOUTFD, 3060 ) ETYPES( ITYPE )
               IF ( NELTYP .GT. 1 ) WRITE( IOUTFF, 3061 ) ITYPE
               IF ( NELTYP .GT. 1 ) WRITE( IOUTFD, 3061 ) ITYPE
               IF ( SINGLE ) THEN
                 IF ( OUTFF ) WRITE( IOUTFF, 3055 ) 'E'
                 WRITE( IOUTFD, 3053 ) 'E', 'E'
               ELSE
                 IF ( OUTFF ) WRITE( IOUTFF, 3055 ) 'D'
                 WRITE( IOUTFD, 3053 ) 'D', 'D'
               END IF
               LDEFND( ITYPE ) = .TRUE.
               ISETTY = ISETTY + 1
               IF ( ISETTY .LT. NELTYP ) THEN
                  IF ( OUTFF ) WRITE( IOUTFF, 3191 ) NLOOP
                  WRITE( IOUTFD, 3191 ) NLOOP
               END IF

!  Off-diagonal term

            ELSE IF ( ETYPES( ITYPE ) .EQ. CQPROD ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3060 ) ETYPES( ITYPE )
               WRITE( IOUTFD, 3060 ) ETYPES( ITYPE )
               IF ( NELTYP .GT. 1 ) THEN
                  IF ( OUTFF ) WRITE( IOUTFF, 3061 ) ITYPE
                  WRITE( IOUTFD, 3061 ) ITYPE
               END IF
               IF ( SINGLE ) THEN
                 IF ( OUTFF ) WRITE( IOUTFF, 3056 )
                 WRITE( IOUTFD, 3054 ) 'E', 'E', 'E'
               ELSE
                 IF ( OUTFF ) WRITE( IOUTFF, 3056 )
                 WRITE( IOUTFD, 3054 ) 'D', 'D', 'D'
               END IF
               LDEFND( ITYPE ) = .TRUE.
               ISETTY = ISETTY + 1
               IF ( ISETTY .LT. NELTYP ) THEN
                  IF ( OUTFF ) WRITE( IOUTFF, 3191 ) NLOOP
                  WRITE( IOUTFD, 3191 ) NLOOP
               END IF
            END IF
  640    CONTINUE   
         IF ( OUTFF ) WRITE( IOUTFF, 3200 ) NLOOP
         IF ( IAD0 .EQ. 2 ) THEN
           WRITE( IOUTFD, 3202 ) NLOOP
         ELSE
           WRITE( IOUTFD, 3200 ) NLOOP
         END IF
      END IF

!  WRITE A DUMMY RANGE ROUTINE.

      IF ( SINGLE ) THEN
         WRITE( IOUTRA, 4003 ) PNAME
      ELSE
         WRITE( IOUTRA, 4002 ) PNAME
      END IF
      WRITE( IOUTRA, 4080 )
      WRITE( IOUTRA, 4090 )
      INFORM = 0
      RETURN

!  INSUFFICIENT SPACE TO CONTINUE CONSTRUCTION.

  700 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2000 ) INCRSE( - INFORM )
      RETURN

!  SUBROUTINE INCOMPLETE.

  800 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2990 ) LINENO, NULINE
      RETURN

!  SUBROUTINE SUCCESSFULLY COMPLETED.

  900 CONTINUE
      IF ( .NOT. FIRSTL ) THEN

!  FINISH OF THE PREVIOUS ELEMENT, IF ANY.

         IF ( STARTF ) THEN
            IF ( .NOT. ENDOFF ) THEN
               IF ( OUTFF ) THEN
                  WRITE( IOUTFF, 3120 )
                  WRITE( IOUTFF, 3121 )
               END IF
               IF ( IAD0 .EQ. 1 ) THEN
                  WRITE( IOUTFD, 3122 ) &
                     FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                     FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                     FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                     NINVAR
               ELSE
                  WRITE( IOUTFD, 3123 ) &
                     FIELDI( 12 )(1:6), FIELDI( 3  )(1:6), &
                     FIELDI( 13 )(1:6), FIELDI( 3  )(1:6), &
                     FIELDI( 17 )(1:6), FIELDI( 17 )(1:6), &
                     NINVAR
               END IF
               WRITE( IOUTFD, 3150 ) FIELDI( 12 )(1:6)
               IF ( IAD0 .EQ. 1 ) THEN
                 WRITE( IOUTFD, 3151 ) 
               ELSE
                 WRITE( IOUTFD, 3152 ) 
               END IF
               DO 902 JS = 1, NINVAR
                  DO 901 IS = 1, JS
                     IHVAR = ( JS * ( JS - 1 ) ) / 2 + IS
!                    JHVAR = NINVAR * ( IS - 1 ) + JS -
!    *                       ( IS * ( IS - 1 ) ) / 2
                     IF ( IS .EQ. JS ) THEN
                        WRITE( IOUTFD, 3163 ) &
                           FIELDI(  3 )(1:6),  &
                           FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                     FIELDI( 15 )(1:6), IHVAR, JHVAR
                     ELSE
                        WRITE( IOUTFD, 3164 ) &
                           FIELDI(  3 )(1:6),  &
                           FIELDI( 15 )(1:6), IHVAR, IHVAR
!    *                     FIELDI( 15 )(1:6), IHVAR, JHVAR
                     END IF
  901             CONTINUE
  902          CONTINUE   
               ENDOFF = .TRUE.
            END IF
           IF ( OUTFF ) WRITE( IOUTFF, 3190 )
            WRITE( IOUTFD, 3180 )
            WRITE( IOUTFD, 3190 )
         ELSE
            INFORM = 61
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
            GO TO 800
         END IF
         IF ( ISETTY .LT. NELTYP .AND. OUTFF ) &
            WRITE( IOUTFF, 3191 ) NLOOP
         IF ( ISETTY .LT. NELTYP ) WRITE( IOUTFD, 3191 ) NLOOP
      END IF

! ---------- SUCCESSFUL RUN. WIND UP OUTPUT.

      INFORM = 0
      IF ( OUTFF ) WRITE( IOUTFF, 3200 ) NLOOP
      IF ( IAD0 .EQ. 2 ) THEN
        WRITE( IOUTFD, 3202 ) NLOOP
      ELSE
        WRITE( IOUTFD, 3200 ) NLOOP
      END IF
      IF ( NOINTE ) WRITE( IOUTRA, 4070 )
      IF ( NELTYP .EQ. 0 ) WRITE( IOUTRA, 4080 )
      WRITE( IOUTRA, 4090 )

!   CHECK THAT ALL ELEMENT TYPES HAVE BEEN DEFINED.

  930 CONTINUE
      DO 940 ITYPE = 1, NELTYP
         IF ( .NOT. LDEFND( ITYPE ) ) THEN
            INFORM = 68
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2680 ) ETYPES( ITYPE )
         END IF
  940 CONTINUE
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAFNAD - insufficient space.',  &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAFNAD - warning.',  &
              ' First card not elements. ', /, '    A dummy',  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAFNAD - indicator card not recognised ' )
 2090 FORMAT( ' ** Exit from MAFNAD - element type not recognised ' )
 2510 FORMAT( ' ** Exit from MAFNAD -',  &
              ' name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAFNAD - data file incomplete.',  &
              ' No ENDATA card ' )
 2540 FORMAT( ' ** Exit from MAFNAD -',  &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAFNAD -',  &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAFNAD -',  &
              ' unrecognised field 1 in INDIVIDUALS section' )
 2570 FORMAT( ' ** Exit from MAFNAD -',  &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAFNAD -',  &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAFNAD - repeated parameter name ', A8 )
 2600 FORMAT( ' ** Exit from MAFNAD - unknown component of gradient ' )
 2610 FORMAT( ' ** Exit from MAFNAD - function not set '  )
 2650 FORMAT( ' ** Exit from MAFNAD -',  &
              ' internal variable not recognised ' )
 2660 FORMAT( ' ** Exit from MAFNAD -',  &
              ' elemental variable not recognised ' )
 2670 FORMAT( ' ** Exit from MAFNAD - element type already defined ' )
 2680 FORMAT( ' ** Exit from MAFNAD - warning, element type ', A10,  &
              ' undefined ' )
 2690 FORMAT( ' ** Exit from MAFNAD -',  &
              ' gradient component already defined ' )
 2700 FORMAT( ' ** Exit from MAFNAD -',  &
              ' Hessian component already defined ' )
 2710 FORMAT( ' ** Exit from MAFNAD - unknown component of Hessian '  )
 2720 FORMAT( ' ** Exit from MAFNAD - field 3 not blank on',  &
              ' A, F or G card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', I5, 4X, A160 )
 2980 FORMAT( ' Line ', I5, '.', I2, 1X, A65 )
 2990 FORMAT( ' Line ', I5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
        '     *                   ', 5( A6, ', ' ), /,  &
        '     *                   ', 5( A6, ', ' ), /,  &
        '     *                   ', 5( A6, ', ' ), /,  &
        '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
        '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                             A6, '(', A6, ')', /,  &
        '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                             A6, '(', A6, ')', /,  &
        '      INTEGER ', A6, '(', A6, ')', /,  &
        '      DOUBLE PRECISION ', A6, '(', A6, '), ',  &
                                   A6, '(', A6, '), ', &
                                   A6, '(', A6, ')', /,  &
        'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
        '      INTEGER ', 5( A6, ', ' ), A6, /,  &
        '      INTEGER ', A6 )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
        '     *                   ', 5( A6, ', ' ), /,  &
        '     *                   ', 5( A6, ', ' ), /,  &
        '     *                   ', 5( A6, ', ' ), /,  &
        '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
        '      INTEGER ', A6, /,  &
        '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                             A6, '(', A6, ')', /,  &
        '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                             A6, '(', A6, ')', /,  &
        '      INTEGER ', A6, '(', A6, ')', /,  &
        '      REAL             ', A6, '(', A6, '), ',  &
                                   A6, '(', A6, '), ', &
                                   A6, '(', A6, ')', /,  &
        'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
        '      INTEGER ', 5( A6, ', ' ), A6, /,  &
        '      INTEGER ', A6 )
 3002 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3003 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3004 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
       '      USE HSL_', A4, '_', A15, /, '      INTEGER ', &
        5( A6, ', ' ),  A6, /, '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
       '      INTEGER ', A6, /, '      INTEGER ', 2( A6, '(', &
       A6, '), ' ), A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
       '      INTEGER ', 5( A6, ', ' ), A6, /, '      INTEGER ', A6, /, &
              '      INTEGER, POINTER :: H_index( : ) ', /, &
              '      DOUBLE PRECISION, POINTER :: H_result( : ) ', /, &
              '      DOUBLE PRECISION X_int( ', I6, ') ' )
 3005 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
       '      USE HSL_', A4, '_', A15, /, '      INTEGER ', 5( A6, &
       ', ' ),  A6, /, '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
       '      INTEGER ', A6, /, '      INTEGER ', 2( A6, '(', A6, &
       '), ' ), A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
       '      INTEGER ', 5( A6, ', ' ), A6, /, '      INTEGER ', A6, /,  &
              '      INTEGER, POINTER :: H_index( : ) ', /, &
              '      REAL, POINTER :: H_result( : ) ', /, &
              '      REAL X_int( ', I6, ') ' )
 3006 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      USE HSL_', A4, '_', A15, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3007 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,  &
              '      USE HSL_', A4, '_', A15, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,  &
              '      INTEGER ', A6, /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', 2( A6, '(', A6, '), ' ), &
                                   A6, '(', A6, ')', /,  &
              '      INTEGER ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, '), ',  &
                                         A6, '(', A6, '), ', &
                                         A6, '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3008 FORMAT( '      X_AD01_int = AD01_UNDEFINED' )
!3008 FORMAT( '      CALL AD01_UNDEFINE( X_AD01_int ) ' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, :, 4( ', ', A6, : ) ) )
!3011 FORMAT( '      NULLIFY( DATA_AD02 )', /,
!    *        '      CALL AD02_INITIALIZE(IFFLAG-1, X_value(1),', /,
!    *        '     *          XVALUE(IELVAR(ISTAEV(1)+1)),', /,
!    *        '     *                      DATA_AD02, 0)' )
 3011 FORMAT( '      CALL AD02_INITIALIZE_DATA(DATA_AD02, ERROR_AD02)' )
!3015 FORMAT( '       CALL ', A4, '_UNDEFINE( ', A6,
!    *        ', DATA_AD02 )' )
 3016 FORMAT( '      CALL ', A4, '_UNDEFINE( ', A6,  &
              ', DATA_AD02 )' )
 3017 FORMAT( ( '      TYPE (', A4, '_REAL) :: ', A6 ) )
 3018 FORMAT( ( '      TYPE (AD01_REAL) :: ', A6, &
                ' = AD01_UNDEFINED' ) )
 3019 FORMAT( ( '      REAL             ', A6, :, 4( ', ', A6, : ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, :, 4( ', ', A6, : ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, :, 4( ', ', A6, : ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, :, 4( ', ', A6, : ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3024 FORMAT( '      TYPE (AD01_REAL) :: F_value = AD01_UNDEFINED', /,  &
              '      TYPE (AD01_REAL) :: X_value(', I6, '),', &
                   ' X_AD01_int(',I6, ')' )
 3025 FORMAT( '      INTEGER :: ERROR_AD02', /, &
              '      TYPE (AD02_REAL) :: F_value', /,  &
              '      TYPE (AD02_REAL) :: X_value(', I6, '),', &
                   ' X_AD02_int(',I6, ')', /, &
              '      TYPE (AD02_DATA), POINTER :: DATA_AD02' )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', I5, 1X, A6, ' = 1, ', A6, /,  &
              '       ', A6, ' = ', A6, '(', A6, ') ', /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,  &
              '       IF ( ', A6, ' .EQ. 3 ) ',  &
                      A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       ', A6, ' = ', A6, '(', A6, ')', /,  &
              '       GO TO (', 8( I5, :, ',' ), /,  &
            ( '     *        ', 8( I5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3053 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,  &
              '       ELSE', /,  &
              '        FUVALS(IGSTRT+     1)= X', /,  &
              '        IF ( IFFLAG .EQ. 3 ) THEN', /,  &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,  &
              '        END IF', /,  &
              '       END IF' )
 3054 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= X * Y', /,  &
              '       ELSE', /,  &
              '        FUVALS(IGSTRT+     1)= Y', /,  &
              '        FUVALS(IGSTRT+     2)= X', /,  &
              '        IF ( IFFLAG .EQ. 3 ) THEN', /,  &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,  &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,  &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,  &
              '        END IF', /,  &
              '       END IF' )
 3055 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,  &
              '       ELSE', /,  &
              '        WRITE(6,*) '' impossible value IFFLAG = '', ', &
              'IFFLAG, '' in ELFUNF '' ', /,  &
              '       END IF' )
 3056 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= X * Y', /,  &
              '       ELSE', /,  &
              '        WRITE(6,*) '' impossible value IFFLAG = '', ', &
              'IFFLAG, '' in ELFUNF '' ', /,  &
              '       END IF' )
 3057 FORMAT( '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * ', A6, &
              ' * ', A6, /,  &
              '       ELSE', /,  &
              '        FUVALS(IGSTRT+     1)= ', A6, /,  &
              '        IF ( IFFLAG .EQ. 3 ) THEN', /,  &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,  &
              '        END IF', /,  &
              '       END IF' )
 3058 FORMAT( '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     1))', /,  &
              '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     2))', /,  &
              '       IF ( IFFLAG .EQ. 1 ) THEN', /,  &
              '        FUVALS(IELEMN)= ', A6, ' * ', A6, /,  &
              '       ELSE', /,  &
              '        FUVALS(IGSTRT+     1)= ', A6, /,  &
              '        FUVALS(IGSTRT+     2)= ', A6, /,  &
              '        IF ( IFFLAG .EQ. 3 ) THEN', /,  &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,  &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,  &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,  &
              '        END IF', /,  &
              '       END IF' )
 3060 FORMAT( 'C', /, 'C  ELEMENT TYPE : ', A10, /, 'C' )
 3061 FORMAT( I5, '  CONTINUE' )
 3070 FORMAT( '       ', A6, ' = ', A6, '(', A6, '(', A6, '+', I6, '))')
 3071 FORMAT( '       ', A6, ' = ', A6, '(', A6, '+', I6, ')')
 3080 FORMAT( '       ', A6, ' = ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( ', A6, ' .EQ. 1 ) THEN', /,  &
              '        ', A6, '(', A6, ')= ', A41 )
 3101 FORMAT( '       F_value = ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3121 FORMAT( '        WRITE(6,*) '' impossible value IFFLAG = '', ', &
              'IFFLAG, '' in ELFUNF '' ' )
 3122 FORMAT( '       IF ( ', A6, ' .EQ. 1 ) THEN', /,  &
              '        CALL AD01_VALUE(F_value, ', A6, '(', A6, '))', /,  &
              '       ELSE',/,  &
              '        CALL AD01_GRAD(F_value, ', A6, '(', A6, '+1:',  &
              A6, '+', I6, '))' )
 3123 FORMAT( '       IF ( ', A6, ' .EQ. 1 ) THEN', /,  &
              '        CALL AD02_VALUE(F_value, ', A6, '(', A6, '),', &
              ' ERROR_AD02)', /,  &
              '       ELSE',/,  &
              '        CALL AD02_GRAD(F_value, ', A6, '(', A6, '+1:',  &
              A6, '+', I6, '),', /, &
              '     *                 ERROR_AD02)' )
 3130 FORMAT( '        ', A6, '(', A6, '+', I6, ')= ', A41 )
 3131 FORMAT( '        ', A6, '(', A6, '+', I6, ')= 0.0D+0' )
 3132 FORMAT( '        ', A6, '(', A6, '+', I6, ')= 0.0E+0' )
 3140 FORMAT( '     *                         ', A41 )
 3150 FORMAT( '        IF ( ', A6, ' .EQ. 3 ) THEN' )
 3151 FORMAT( '         CALL AD01_DERIVS(F_value, 2,',  &
              ' H_index, H_result)' )
 3152 FORMAT( '         CALL AD02_DERIVS(F_value, 2,',  &
              ' H_index, H_result, ERROR_AD02)' )
 3160 FORMAT( '         ', A6, '(', A6, '+', I6, ')=', A41 )
 3161 FORMAT( '         ', A6, '(', A6, '+', I6, ')=0.0D+0' )
 3162 FORMAT( '         ', A6, '(', A6, '+', I6, ')=0.0E+0' )
 3163 FORMAT( '         ', A6, '(', A6, '+', I6, ')=2.0*H_result(', &
              I6, ')')
 3164 FORMAT( '         ', A6, '(', A6, '+', I6, ')=H_result(', I6, ')')
 3170 FORMAT( '     *                         ', A41 )
 3180 FORMAT( '        END IF' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', I6 )
 3200 FORMAT( I5,  ' CONTINUE', /, '      RETURN', /,  &
              '      END' )
 3201 FORMAT( '      RETURN', /,  &
              '      END' )
 3202 FORMAT( I5,  ' CONTINUE', /, &
              '      CALL AD02_FINALIZE_DATA(DATA_AD02, ERROR_AD02)', /,  &
              '      RETURN', /,   '      END' )
 3203 FORMAT( '      CALL AD02_FINALIZE_DATA(DATA_AD02, ERROR_AD02)', /, &
              '      RETURN', /, '      END' )
 3210 FORMAT( '       CALL AD01_INITIALIZE(',A6,' - 1, X_value(:', I6, &
                     '),', /, '     *', 22X, &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', I6, ')), 0) ' )
!3211 FORMAT( '       CALL AD02_INITIALIZE(',A6,' - 1, X_value(:', I6, 
!    *               '),', /, '     *', 22X, 
!    *               A6,'(',A6,'(',A6,'+1:',A6,'+', I6, ')),', /, 
!    *        '     *                      DATA_AD02, 0)' )
 3211 FORMAT( '       CALL AD02_INITIALIZE_COMP(',A6,' - 1, X_value(:', &
                     I6, '),', /, '     *', 22X, &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', I6, ')),', /, &
              '     *                      DATA_AD02, ERROR_AD02, 0)' )
 3220 FORMAT( '       ', A6, ' = X_value(', I6, ')' )
 3230 FORMAT( '       CALL AD01_INITIALIZE(0, X_value(:', I6, &
                     '),', /, '     *', 22X, &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', I6, ')), 0) ' )
 3231 FORMAT( '       CALL AD02_INITIALIZE_COMP(0, X_value(:', I6, &
                     '),', /, '     *', 22X, &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', I6, ')),', /, &
              '     *                      DATA_AD02, ERROR_AD02, 0)' )
 4000 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C', /,  &
              '      INTEGER I' )
 4001 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      REAL             W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C', /,  &
              '      INTEGER I' )
 4002 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C' )
 4003 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',  &
              ' W2, NELVAR, NINVAR,', /,  &
              '     *                  ITYPE, LW1, LW2 )', /,  &
              '      INTEGER IELEMN, NELVAR, NINVAR, ITYPE,',  &
              ' LW1, LW2', /,  &
              '      LOGICAL TRANSP', /,  &
              '      REAL             W1( LW1 ), W2( LW2 )', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,  &
              'C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1', /,  &
              'C' )
 4040 FORMAT( '      GO TO (', 8( I5, :, ',' ), /,  &
            ( '     *       ', 8( I5, :, ',' ) ) )
 4050 FORMAT( '     *        ', 48X, '), ITYPE' )
 4060 FORMAT( 'C', /, 'C  ELEMENT TYPE : ', A10, /, 'C', /,  &
              I5, ' CONTINUE', /,  &
              '      IF ( TRANSP ) THEN' )
 4070 FORMAT( 'C', /,  &
              'C  ELEMENTS WITHOUT INTERNAL VARIABLES.', /,  &
              'C', /,  &
              '99998 CONTINUE', /,  &
              '      DO 99999 I = 1, NELVAR', /,  &
              '         W2( I ) = W1( I )', /,  &
              '99999 CONTINUE', /,  &
              '      RETURN' )
 4080 FORMAT( '      RETURN' )
 4090 FORMAT( '      END' )

!  END OF MAFNAD.

      END
!     THIS VERSION: 21ST JUNE 1990.

      SUBROUTINE OUTRN2( NELV, NINV, U, IOUTFF, IOUTFD, IOUTRA, &
                         ENAMES, INAMES, SINGLE, AD0 )
      INTEGER          NELV, NINV, IOUTFF, IOUTFD, IOUTRA
      LOGICAL          SINGLE
      CHARACTER * 4    AD0
      DOUBLE PRECISION U( NINV, NELV )
      CHARACTER * 10   ENAMES( * ), INAMES( * )

!  PRINT OUT THE GATHER AND SCATTER PART OF THE GENERATED RANGE ROUTINE
!  AND THE GATHER PART OF THE GENERATED FUNCTION EVALUATION ROUTINE.

      INTEGER          I, J, K
      DOUBLE PRECISION UIJ, ONE, ZERO, EPSMCH, DMACHR
      LOGICAL          ANYNNZ, OUTFF
      CHARACTER * 6    EVNAME, IVNAME
      INTRINSIC        DABS, MOD
      EXTERNAL         DMACHR
      DATA ZERO, ONE / 0.0D+0, 1.0D+0 /
      OUTFF = IOUTFF .GT. 0
      EPSMCH = DMACHR( 1 )

!  PRINT OUT THE SCATTER PART.

      DO 20 J = 1, NELV
         K = 0         
         ANYNNZ = .FALSE.
         DO 10 I = 1, NINV
            UIJ = U( I, J )

!  IGNORE ZERO ENTRIES.

            IF ( DABS( UIJ ) .LE. EPSMCH ) GO TO 10
            K = K + 1
            IF ( UIJ .GT. ZERO ) THEN

!  THE NONZERO IS POSITIVE.

               IF ( DABS( UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4030 ) I
                  ELSE
                     WRITE( IOUTRA, 4040 ) J, I
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4050 ) I, UIJ
                  ELSE
                     WRITE( IOUTRA, 4060 ) J, I, UIJ
                  END IF
               END IF
            ELSE

!  THE NONZERO IS NEGATIVE.

               IF ( DABS( - UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE - 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4070 ) I
                  ELSE
                     WRITE( IOUTRA, 4080 ) J, I
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN - 1.

                  IF ( ANYNNZ ) THEN
                     WRITE( IOUTRA, 4090 ) I, - UIJ
                  ELSE
                     WRITE( IOUTRA, 4100 ) J, I, - UIJ
                  END IF
               END IF
            END IF
            ANYNNZ = .TRUE.
            IF ( MOD( K, 19 ) .EQ. 0 ) WRITE( IOUTRA, 4112 ) J, J
   10    CONTINUE
         IF ( .NOT. ANYNNZ ) THEN
            IF ( SINGLE ) THEN
               WRITE( IOUTRA, 4111 ) J
            ELSE
               WRITE( IOUTRA, 4110 ) J
            END IF
         END IF
   20 CONTINUE

!  ----- THE SCATTER HAS BEEN COMPLETED; START THE GATHER.

      WRITE( IOUTRA, 4010 )

!  PRINT OUT THE GATHER PART.

      DO 40 I = 1, NINV
         K = 0
         ANYNNZ = .FALSE.
         IVNAME = INAMES( I )(1:6)
         DO 30 J = 1, NELV
            EVNAME = ENAMES( J )(1:6)
            UIJ = U( I, J )

!  IGNORE ZERO ENTRIES.

            IF ( DABS( UIJ ) .LE. EPSMCH ) GO TO 30
            K = K + 1
            IF ( UIJ .GT. ZERO ) THEN

!  THE NONZERO IS POSITIVE.

               IF ( DABS( UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE 1.

                  IF ( ANYNNZ ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3030 ) EVNAME
                     WRITE( IOUTFD, 3030 ) EVNAME
                     WRITE( IOUTRA, 4030 ) J
                  ELSE
                     IF ( OUTFF ) WRITE( IOUTFF, 3040 ) IVNAME, EVNAME
                     WRITE( IOUTFD, 3041 ) AD0, I, EVNAME
                     WRITE( IOUTRA, 4040 ) I, J
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN 1.

                  IF ( ANYNNZ ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3050 ) EVNAME, UIJ
                     WRITE( IOUTFD, 3050 ) EVNAME, UIJ
                     WRITE( IOUTRA, 4050 ) J, UIJ
                  ELSE
                     IF ( OUTFF ) &
                        WRITE( IOUTFF, 3060 ) IVNAME, EVNAME, UIJ
                     WRITE( IOUTFD, 3061 ) AD0, I, EVNAME, UIJ
                     WRITE( IOUTRA, 4060 ) I, J, UIJ
                  END IF
               END IF
             ELSE

!  THE NONZERO IS NEGATIVE.

               IF ( DABS( - UIJ - ONE ) .LE. EPSMCH ) THEN

!  SPECIAL CASE IF NONZERO HAS THE VALUE - 1.

                  IF ( ANYNNZ ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3070 ) EVNAME
                     WRITE( IOUTFD, 3070 ) EVNAME
                     WRITE( IOUTRA, 4070 ) J
                  ELSE
                     IF ( OUTFF ) WRITE( IOUTFF, 3080 ) IVNAME, EVNAME
                     WRITE( IOUTFD, 3081 ) AD0, I, EVNAME
                     WRITE( IOUTRA, 4080 ) I, J
                  END IF
               ELSE

!  NONZERO HAS A VALUE OTHER THAN - 1.

                  IF ( ANYNNZ ) THEN
                     IF ( OUTFF ) WRITE( IOUTFF, 3090 ) EVNAME, - UIJ
                     WRITE( IOUTFD, 3090 ) EVNAME, - UIJ
                     WRITE( IOUTRA, 4090 ) J, - UIJ
                  ELSE
                     IF ( OUTFF ) &
                        WRITE( IOUTFF, 3100 ) IVNAME, EVNAME, - UIJ
                     WRITE( IOUTFD, 3101 ) AD0, I, EVNAME, - UIJ
                     WRITE( IOUTRA, 4100 ) I, J, - UIJ
                  END IF
               END IF
            END IF
            ANYNNZ = .TRUE.
            IF ( MOD( K, 19 ) .EQ. 0 ) THEN
               IF ( OUTFF ) WRITE( IOUTFF, 3040 ) IVNAME, IVNAME
               WRITE( IOUTFD, 3041 ) AD0, I, IVNAME
               WRITE( IOUTRA, 4112 ) I, I
            END IF
   30    CONTINUE
         IF ( .NOT. ANYNNZ ) THEN
            IF ( SINGLE ) THEN
              IF ( OUTFF ) WRITE( IOUTFF, 3120 ) IVNAME
               WRITE( IOUTFD, 3121 ) AD0, I
               WRITE( IOUTRA, 4111 ) I
            ELSE
              IF ( OUTFF ) WRITE( IOUTFF, 3110 ) IVNAME
               WRITE( IOUTFD, 3111 ) AD0, I
               WRITE( IOUTRA, 4110 ) I
            END IF
         END IF
         IF ( AD0 .EQ. 'AD01' ) THEN
            WRITE( IOUTFD, 3150 ) I, I
         ELSE
            WRITE( IOUTFD, 3151 ) I, I
         END IF
   40 CONTINUE

!  ----- THE GATHER HAS BEEN COMPLETED; WIND UP THE ELEMENT.

      WRITE( IOUTRA, 4020 )
      IF ( AD0 .EQ. 'AD01' ) THEN
         WRITE( IOUTFD, 3130 ) NINV, NINV
      ELSE
         WRITE( IOUTFD, 3131 ) NINV, NINV
      END IF
      DO 50 I = 1, NINV
         IVNAME = INAMES( I )(1:6)
         WRITE( IOUTFD, 3140 ) IVNAME, I
   50 CONTINUE   
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 3030 FORMAT( '     *          + ', A6 )
 3040 FORMAT( '       ', A6, ' =   ', A6 )
 3041 FORMAT( '       X_', A4, '_int(', I6, ') =   ', A6 )
 3050 FORMAT( '     *          + ', A6, ' * ', F12.5 )
 3060 FORMAT( '       ', A6, ' =   ', A6, ' * ', F12.5 )
 3061 FORMAT( '       X_', A4, '_int(', I6, ') =   ', A6, ' * ', F12.5 )
 3070 FORMAT( '     *          - ', A6 )
 3080 FORMAT( '       ', A6, ' = - ', A6 )
 3081 FORMAT( '       X_', A4, '_int(', I6, ') = - ', A6 )
 3090 FORMAT( '     *          - ', A6, ' * ', F12.5 )
 3100 FORMAT( '       ', A6, ' = - ', A6, ' * ', F12.5 )
 3101 FORMAT( '       X_', A4, '_int(', I6, ') = - ', A6, ' * ', F12.5 )
 3110 FORMAT( '       ', A6, ' = 0.0D+0 ' )
 3111 FORMAT( '       X_', A4, '_int(', I6, ') = 0.0D+0 ' )
 3120 FORMAT( '       ', A6, ' = 0.0E+0 ' )
 3121 FORMAT( '       X_', A4, '_int(', I6, ') = 0.0E+0 ' )
 3130 FORMAT( '       CALL AD01_INITIALIZE(IFFLAG - 1, X_value(:', I6, &
                     '),', /, '     *', 22X, 'X_int(:', I6, '), 0) ')
 3131 FORMAT( '       CALL AD02_INITIALIZE_COMP(IFFLAG - 1, X_value(:', &
                      I6, '),', /, '     *', 22X, 'X_int(:', I6, '),', &
              ' DATA_AD02, ERROR_AD02, 0)' )
 3140 FORMAT( '       ', A6, ' = X_value(', I6, ')' )
 3150 FORMAT( '       CALL AD01_VALUE(X_AD01_int(', I6, &
                      '), X_int(', I6, '))' )
 3151 FORMAT( '       CALL AD02_VALUE(X_AD02_int(', I6, &
                      '), X_int(', I6, '), ERROR_AD02)' )
 4010 FORMAT( '      ELSE' )
 4020 FORMAT( '      END IF', /, '      RETURN' )
 4030 FORMAT( '     *                 + W1(', I6, ' ) ' )
 4040 FORMAT( '         W2(', I6, ' ) =   W1(', I6, ' ) ' )
 4050 FORMAT( '     *                 + W1(', I6, ' ) * ', F12.5 )
 4060 FORMAT( '         W2(', I6, ' ) =   W1(', I6, ' ) * ', F12.5 )
 4070 FORMAT( '     *                 - W1(', I6, ' ) ' )
 4080 FORMAT( '         W2(', I6, ' ) = - W1(', I6, ' ) ' )
 4090 FORMAT( '     *                 - W1(', I6, ' ) * ', F12.5 )
 4100 FORMAT( '         W2(', I6, ' ) = - W1(', I6, ' ) * ', F12.5 )
 4110 FORMAT( '         W2(', I6, ' ) = 0.0D+0 ' )
 4111 FORMAT( '         W2(', I6, ' ) = 0.0E+0 ' )
 4112 FORMAT( '         W2(', I6, ' ) =   W2(', I6, ' ) ' )

!  END OF OUTRN2.

      END
      CHARACTER * 6 FUNCTION NUNAME( I1, I2, I3, I4, I5, I6, IIRES, &
                                     NINMAX, NRENAM, NINNAM, NLONAM, &
                                     NMINAM, NEXNAM, NLMAX , NELTYP, &
                                     YNAME , FIELDI, RENAME, INNAME, &
                                     LONAME, MINAME, EXNAME, ETYPES )
! 
!  Find a name that does not occur in any other list

      INTEGER        I1, I2, I3, I4, I5, I6, IIRES, NLMAX, NINMAX
      INTEGER        NRENAM, NINNAM, NLONAM, NMINAM, NEXNAM, NELTYP
      CHARACTER * 6  YNAME
      CHARACTER * 8  FIELDI( IIRES )
      CHARACTER * 10 RENAME( NINMAX ), INNAME( NINMAX )
      CHARACTER * 10 LONAME( NINMAX )
      CHARACTER * 10 MINAME( NINMAX ), EXNAME( NINMAX )
      CHARACTER * 10 ETYPES( NLMAX  )
      CHARACTER * 1  CHARAC( 36 )
      DATA CHARAC / 'Z', 'Y', 'X', 'W', 'V', 'U', 'T', 'S', 'R', &
                    'Q', 'P', 'O', 'N', 'M', 'L', 'K', 'J', 'I',  &
                    'H', 'G', 'F', 'E', 'D', 'C', 'B', 'A', '0',  &
                    '9', '8', '7', '6', '5', '4', '3', '2', '1' / 
   10 CONTINUE

!  Find the next name in the list

      I1 = I1 + 1
      IF ( I1 .EQ. 27 ) THEN
         I1 = 1
         I2 = I2 + 1
         IF ( I2 .EQ. 37 ) THEN
            I2 = 1
            I3 = I3 + 1
            IF ( I3 .EQ. 37 ) THEN
               I3 = 1
               I4 = I4 + 1
               IF ( I4 .EQ. 37 ) THEN
                  I4 = 1
                  I5 = I5 + 1
                  IF ( I5 .EQ. 37 ) THEN
                     I5 = 1
                     I6 = I6 + 1
                     IF ( I6 .EQ. 37 ) THEN
                        write( 6, * ) ' no characters left '
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      NUNAME = CHARAC( I1 ) // CHARAC( I2 ) // CHARAC( I3 ) // &
               CHARAC( I4 ) // CHARAC( I5 ) // CHARAC( I6 )

!  See if the name has already been used

      DO 110 I = 1, NRENAM
         IF ( RENAME( I )(1:6) .EQ. NUNAME ) GO TO 10
  110 CONTINUE
      DO 120 I = 1, NINNAM
         IF ( INNAME( I )(1:6) .EQ. NUNAME ) GO TO 10
  120 CONTINUE
      DO 130 I = 1, NLONAM
         IF ( LONAME( I )(1:6) .EQ. NUNAME ) GO TO 10
  130 CONTINUE
      DO 140 I = 1, NMINAM
         IF ( MINAME( I )(1:6) .EQ. NUNAME ) GO TO 10
  140 CONTINUE
      DO 150 I = 1, NEXNAM
         IF ( EXNAME( I )(1:6) .EQ. NUNAME ) GO TO 10
  150 CONTINUE
      DO 160 I = 1, NELTYP
         IF ( ETYPES( I )(1:6) .EQ. NUNAME ) GO TO 10
  160 CONTINUE
      DO 170 I = 1, IIRES
         IF ( FIELDI( I )(1:6) .EQ. NUNAME ) GO TO 10
  170 CONTINUE
      IF ( NUNAME .EQ. YNAME ) GO TO 10
      RETURN

!  End of NUNAME

      END

! ** Correction report.
!  THIS VERSION: 04/04/2002 AT 09:30:00 AM.
!     ( Last modified on 15 Mar 2001 at 22:28:00 )
! ** Correction report.
! ** Correction 1. 19/07/93: 13 lines added **
! ** Correction 2. 19/07/93: 2 lines added **
! ** Correction 3. 12/01/94: 1 line corrected **
! ** Correction 4. 12/01/94: 1 line corrected **
! ** Correction 5. 11/08/95: 1 line corrected **
! ** Correction 6. 15/08/95: 2 lines corrected **
! ** Correction 7. 26/02/01: 2 dummy arguments removed **
! ** Correction 8. 04/04/02: 1 line corrected
! ** End of Correction report.
      SUBROUTINE MAKEGR( INPUT , IOUT  , IOUTGR, INFORM, NGRTYP,  &
                         NGRMAX, NLMAX , NINMAX, PNAME , ANAMES,  &
                         RENAME, INNAME, LONAME, MINAME, EXNAME, GTYPES,  &
                         LDEFND, GPNAME, IGPA  , NGPMAX, DEBUG , LENGTH,  &
                         ITABLE, KEY   , INLIST, SINGLE, NULINE, GOTLIN,  &
                         IPRINT )
      INTEGER            INPUT , IOUT  , IOUTGR, INFORM, LENGTH
      INTEGER            NLMAX , NGRTYP, NINMAX, NGPMAX, NGRMAX
      INTEGER            IPRINT
      LOGICAL            GOTLIN, DEBUG , SINGLE
      CHARACTER * 8      PNAME
      CHARACTER * 160    NULINE
      INTEGER            ITABLE( LENGTH ), IGPA( NGRMAX )
      INTEGER            INLIST( LENGTH )
      LOGICAL            LDEFND( NLMAX  )
      CHARACTER * 12     KEY   ( LENGTH )
      CHARACTER * 10     RENAME( NINMAX ), INNAME( NINMAX )
      CHARACTER * 10     GPNAME( NGPMAX ), EXNAME( NINMAX )
      CHARACTER * 10     LONAME( NINMAX ), GTYPES( NGRMAX )
      CHARACTER * 10     ANAMES( NGRMAX ), MINAME( NINMAX )

!  MAKE A GROUP FUNCTION EVALUATION SUBROUTINE
!  -------------------------------------------
!  FROM A GPS GROUP FUNCTION DATA FILE.
!  ------------------------------------

!  NICK GOULD 01/08/1989
!  FOR CGT PRODUCTIONS.

!  -------------------------------------------------------------------

!  FUNCTION INDICATOR CARDS.
!  -------------------------

!  DEFINITION   PURPOSE.
!  ----------   --------
!  GROUPS       PROBLEM NAME.
!  TEMPORARIES  NAMES OF ADDITIONAL PARAMETERS USED IN FUNCTION DEFS.
!  GLOBALS      GENERAL PARAMETER ASSIGNMENTS.
!  INDIVIDUALS  SET FUNCTION AND DERIVATIVE VALUES FOR EACH GROUP-TYPE.
!  ENDATA       END OF INPUT DATA.

!  DATA CARD DESCRIPTION.
!  ----------------------

!  SEE 'A PROPOSAL FOR A STANDARD DATA INPUT FORMAT FOR LARGE-SCALE
!       NONLINEAR PROGRAMMING PROBLEMS', SECTION 4,
!       A. R. CONN, N. I. M. GOULD AND PH. L. TOINT, 
!       REPORT CS-89-61, DEPT OF COMPUTER SCIENCE, U. OF WATERLOO,
!       WATERLOO, ONTARIO, N2L3G1, CANADA.

!  -------------------------------------------------------------------
!  RETURNS WITH NEGATIVE VALUES OF INFORM INDICATE THAT INSUFFICIENT
!  ARRAY SPACE HAS BEEN ALLOWED, AS FOLLOWS:

!    INFORM = - 1  WHEN LENGTH NOT LARGE ENOUGH
!    INFORM = - 2  WHEN MAX( NINNAM, NRENAM, NLONAM, NENAM, NMINAM )
!                  .GT. NINMAX

      INTEGER          I, IFIELD, IFREE, ITYPE, INTYPE, IVAR, K1, K2
      INTEGER          NINNAM, NLOOP, NRENAM, NMINAM, NPNAME
      INTEGER          NEXNAM, NINCRS, LINENO, IIRES, K, ILINES, NLINES
      INTEGER          MBLANK, MFIXED, MFREE, MNAME, MTEMP, MGLOB
      INTEGER          MINDIV, MENDAT, MAXNUL, MXRECL, NLONAM
      LOGICAL          DEFNAM, ENDPAR, ENDGEN, FIRSTG
      LOGICAL          SETF, SETG, SETH, STARTP, FIXED
      LOGICAL          ENDF
      PARAMETER        ( IIRES = 20 )
      PARAMETER        ( NINCRS = 2 )
      CHARACTER * 2    FIELD1
      CHARACTER * 6    INCRSE( NINCRS )
      CHARACTER * 8    FIELD2, FIELD3, FIELDI( IIRES )
      CHARACTER * 12   FIELD
      CHARACTER * 41   FIELD7
      EXTERNAL         HASHB , HASHC

!  PARAMETER DEFINITIONS.

      PARAMETER        ( MXRECL = 160 )
      CHARACTER * 160  BLNKLN
      PARAMETER        ( MBLANK =  1, MFIXED =  2, MFREE = 3  )
      PARAMETER        ( MNAME =  4, MTEMP =  5 )
      PARAMETER        ( MGLOB =  6, MINDIV =  7, MENDAT =  8 )
      INTEGER          LENIND( MENDAT )
      CHARACTER * 12   INDIC8( MENDAT ), HEADER
      PARAMETER        ( MAXNUL = 20 )
      CHARACTER * 65   NULINA( MAXNUL )

!  DATA DECLARATIONS.

      DATA INCRSE / 'LENGTH', 'NINMAX' /
      DATA INDIC8( MBLANK ) / '            ' /, LENIND( MBLANK ) / 0  / 
      DATA INDIC8( MFIXED ) / 'FIXED FORMAT' /, LENIND( MFIXED ) / 12 /
      DATA INDIC8( MFREE  ) / 'FREE FORMAT ' /, LENIND( MFREE  ) / 11 /
      DATA INDIC8( MNAME  ) / 'GROUPS      ' /, LENIND( MNAME  ) / 6  /
      DATA INDIC8( MTEMP  ) / 'TEMPORARIES ' /, LENIND( MTEMP  ) / 11 / 
      DATA INDIC8( MGLOB  ) / 'GLOBALS     ' /, LENIND( MGLOB  ) / 7  /
      DATA INDIC8( MINDIV ) / 'INDIVIDUALS ' /, LENIND( MINDIV ) / 11 / 
      DATA INDIC8( MENDAT ) / 'ENDATA      ' /, LENIND( MENDAT ) / 6  /
      DATA FIELDI(  1 ) / 'GROUP   ' /,  FIELDI(  2 ) / 'GVALUE  ' /
      DATA FIELDI(  3 ) / 'LGVALU  ' /,  FIELDI(  4 ) / 'FVALUE  ' /
      DATA FIELDI(  5 ) / 'NCALCG  ' /,  FIELDI(  6 ) / 'ITYPEG  ' /
      DATA FIELDI(  7 ) / 'ICALCG  ' /,  FIELDI(  8 ) / 'DERIVS  ' /
      DATA FIELDI(  9 ) / 'IGRTYP  ' /,  FIELDI( 10 ) / 'IGROUP  ' /
      DATA FIELDI( 11 ) / 'GPVALU  ' /,  FIELDI( 12 ) / 'ISTGPA  ' /
      DATA FIELDI( 13 ) / 'IPSTRT  ' /,  FIELDI( 14 ) / 'JCALCG  ' /
      DATA FIELDI( 15 ) / 'LTYPEG  ' /,  FIELDI( 16 ) / 'LSTGPA  ' /
      DATA FIELDI( 17 ) / 'LCALCG  ' /,  FIELDI( 18 ) / 'LFVALU  ' /
      DATA FIELDI( 19 ) / 'LGPVLU  ' /,  FIELDI( 20 ) / 'IGSTAT  ' /

      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2900 )

!  SET INITIAL VALUES FOR INTEGER VARIABLES.

      NINNAM = 0
      NRENAM = 0
      NLONAM = 0
      NEXNAM = 0
      NMINAM = 0
      LINENO = 0
      INTYPE = 1
      ILINES = 0
      NLINES = 0

!  SET INITIAL VALUES FOR LOGICAL VARIABLES.

      DEFNAM = .FALSE.
      ENDPAR = .FALSE.
      SETH = .FALSE.
      STARTP = .FALSE.
      ENDGEN = .FALSE.
      FIRSTG = .TRUE.
      FIXED = .TRUE.

!  FIND WHICH GROUP-TYPES ARE NONTRIVIAL.

      DO 20 ITYPE = 1, NGRTYP
         LDEFND( ITYPE ) = .FALSE.
   20 CONTINUE

!  INSERT THE LIST OF GROUP-TYPE ARGUMENTS INTO THE DICTIONARY.

      DO 30 ITYPE = 1, NGRTYP
         FIELD = ANAMES( ITYPE ) // 'PG'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = ANAMES( ITYPE )
         END IF
   30 CONTINUE

!  INCLUDE THE NAMES OF THE GROUP PARAMETERS USED
!  IN THIS DICTIONARY.

      IF ( NGRTYP .GT. 0 ) THEN
         NPNAME = IGPA( NGRTYP + 1 ) - 1
         DO 40 I = 1, NPNAME
            FIELD = GPNAME( I ) // 'PG'
            CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
            IF ( IFREE .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  GO TO 700
               END IF
            ELSE
               NRENAM = NRENAM + 1
               IF ( NRENAM .GT. NINMAX ) THEN
                  INFORM = - 2
                  GO TO 700
               END IF
               RENAME( NRENAM ) = GPNAME( I )
            END IF
   40    CONTINUE
      END IF

!  SET A BLANK LINE.

      DO 50 I = 1, MXRECL
         BLNKLN( I: I ) = ' '
   50 CONTINUE    

!  READ NEXT LINE.

  100 CONTINUE
      IF ( ILINES + 1 .GT. NLINES ) THEN

!  READ NEXT LINE FROM THE INPUT FILE.

         LINENO = LINENO + 1
         IF ( FIXED ) THEN
            IF ( GOTLIN ) THEN
               GOTLIN = .FALSE.
            ELSE
               NULINE = BLNKLN
               READ ( INPUT, 1000, END = 590, ERR = 590 ) NULINE
            END IF
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2990 )  &
                 LINENO, NULINE
         ELSE
            IF ( GOTLIN ) THEN
               GOTLIN = .FALSE.
            ELSE
               NULINE = BLNKLN
               READ ( INPUT, 1010, END = 590, ERR = 590 ) NULINE
            END IF
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2970 )  &
                 LINENO, NULINE

!  IF THE CARD IS IN FREE FORMAT, TRANSLATE IT INTO FIXED FORMAT.

            CALL  FREEFM( NULINE, MXRECL, MENDAT, INDIC8, LENIND,  &
                          NULINA, MAXNUL, NLINES, .FALSE.,  &
                          INFORM, IOUT )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( NLINES .GT. 0 ) THEN

!  IF THERE ARE NON-BLANK LINES ON THE FREE FORMAT CARD, READ THE FIRST.

               ILINES = 1
               NULINE = BLNKLN
               NULINE = NULINA( ILINES )
               IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
                    LINENO, ILINES, NULINE
            ELSE

!  THERE ARE ONLY BLANK LINES ON THE FREE FORMAT CARD.

               GO TO 100
            END IF
         END IF
      ELSE

!  READ NEXT LINE FROM THE LAST ENCOUNTERED FREE FORMAT CARD.

         ILINES = ILINES + 1
         NULINE = BLNKLN
         NULINE = NULINA( ILINES )
         IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
              LINENO, ILINES, NULINE
      END IF

!  CONSIDER THE HEADER PART OF THE CARD.

      HEADER = NULINE( 1: 12 )

!  IGNORE BLANK LINES.

      IF ( HEADER .EQ. INDIC8( MBLANK ) ) GO TO 100
      IF ( NULINE( 1: 1 ) .NE. ' ' ) THEN

!  IGNORE COMMENT CARDS.

         IF ( NULINE( 1: 1 ) .EQ. '*' ) GO TO 100

!  CHECK IF WE HAVE ENTERED FIXED-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFIXED ) ) THEN
            FIXED = .TRUE.
            GO TO 100
         END IF

!  CHECK IF WE HAVE ENTERED FREE-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFREE ) ) THEN
            FIXED = .FALSE.
            GO TO 100
         END IF

!  CHECK THAT THE FIRST ENCOUNTERED INDICATOR CARD IS THE GROUPS CARD.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( HEADER .NE. INDIC8( MNAME ) ) THEN
               IF ( NGRTYP .GT. 0 ) GO TO 930
               IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010)
               GOTLIN = .TRUE.
               GO TO 600
            ELSE

!  INDICATOR CARD IS GROUPS.
!  -------------------------

               IF ( PNAME  .NE. NULINE( 15: 22 ) ) THEN
                  INFORM = 51
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2510 )
                  GO TO 800
               ELSE
                  DEFNAM = .TRUE.
                  GO TO 100
               END IF
            END IF
         END IF

!  AN INDICATOR CARD HAS BEEN FOUND.

         DO 110 I = INTYPE, MENDAT
            IF ( HEADER .EQ. INDIC8( I ) ) THEN
               INTYPE = I
               GO TO 120
            END IF
  110    CONTINUE

!  THE INDICATOR CARD IS NOT RECOGNISED.

         INFORM = 2
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2020 )
         GO TO 800
  120    CONTINUE

!  THE PARAMETER VALUES HAVE BEEN COMPLETED. WRITE OUT THE
!  FIRST PART OF THE GENERATED SUBROUTINE.

         IF ( INTYPE .GE. MGLOB .AND. .NOT. ENDPAR ) THEN
            ENDPAR = .TRUE.
            NLOOP = NGRTYP + 1

!  INSERT THE LIST OF RESERVED INTEGER/REAL/LOGICAL VARIABLES INTO
!  THE DICTIONARY.

            DO 130 I = 1, IIRES
               FIELD = FIELDI( I ) // '  PG'
               CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
               IF ( IFREE .LE. 0 ) THEN
                  IF ( IFREE .EQ. 0 ) THEN
                     INFORM = - 1
                     GO TO 700
                  END IF
                  INFORM = 59
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2590 ) FIELDI( I )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- SET UP SUBROUTINE CALL AND RESERVED PARAMETER DECLARATIONS.

            IF ( SINGLE ) THEN
               WRITE( IOUTGR, 3001 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                          FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                          FIELDI(  8 )( 1 : 6 ), &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                          PNAME
            ELSE
               WRITE( IOUTGR, 3000 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                          FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                          FIELDI(  8 )( 1 : 6 ), &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                          PNAME
            END IF
! ** Correction 8. 04/04/02: 1 line replaced by 4
            IF ( NGRTYP .EQ. 0 ) THEN
              WRITE( IOUTGR, 3009 ) FIELDI( 20 )( 1 : 6 )
              GO TO 910
            END IF
            WRITE( IOUTGR, 3002 ) FIELDI(  9 )( 1 : 6 ),  &
                          FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),  &
                          FIELDI( 14 )( 1 : 6 )

! --------- INSERT INTEGER DECLARATIONS.

            IF ( NINNAM .GT. 0 )  &
               WRITE( IOUTGR, 3010 ) ( INNAME( I ), I = 1, NINNAM )

! --------- INSERT REAL DECLARATIONS.

            IF ( NRENAM .GT. 0 ) THEN
               IF ( SINGLE ) THEN
                  WRITE( IOUTGR, 3019 ) ( RENAME( I ), I = 1, NRENAM )
               ELSE
                  WRITE( IOUTGR, 3020 ) ( RENAME( I ), I = 1, NRENAM )
               END IF
            END IF

! --------- INSERT LOGICAL DECLARATIONS.

            IF ( NLONAM .GT. 0 )  &
               WRITE( IOUTGR, 3023 ) ( LONAME( I ), I = 1, NLONAM )

! --------- INSERT INTRINSIC DECLARATIONS.

            IF ( NMINAM .GT. 0 )  &
               WRITE( IOUTGR, 3021 ) ( MINAME( I ), I = 1, NMINAM )

! --------- INSERT EXTERNAL DECLARATIONS.

            IF ( NEXNAM .GT. 0 )  &
               WRITE( IOUTGR, 3022 ) ( EXNAME( I ), I = 1, NEXNAM )
            WRITE( IOUTGR, 3009 ) FIELDI( 20 )( 1 : 6 )
         END IF

!  THE GENERAL PARAMETER ASSIGNMENTS HAVE BEEN COMPLETED.
!  CONTINUE WITH THE CONSTRUCTION OF THE GENERATED SUBROUTINE.

         IF ( INTYPE .GE. MINDIV .AND. .NOT. ENDGEN ) THEN
            ENDGEN = .TRUE.

! --------- START LOOP OVER GROUPS.

            WRITE( IOUTGR, 3050 ) NLOOP,  FIELDI( 14 )( 1 : 6 ),  &
                   FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                   FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),  &
                   FIELDI(  9 )( 1 : 6 ),  &
                   FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                   FIELDI(  9 )( 1 : 6 ), NLOOP,  &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                   FIELDI( 10 )( 1 : 6 )
            IF ( NGRTYP .GT. 1 ) THEN
               WRITE( IOUTGR, 3051 ) ( I, I = 1, NGRTYP )
               WRITE( IOUTGR, 3052 ) FIELDI(  9 )( 1 : 6 )
            END IF
         END IF

!  INDICATOR CARD IS ENDATA.
!  -------------------------

         IF ( INTYPE .EQ. MENDAT ) GO TO 900
         GO TO 100
      ELSE

!  CHECK THAT THE FIRST NON COMMENT CARD IS THE GROUPS INDICATOR CARD.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( NGRTYP .GT. 0 ) GO TO 930
            IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )
            GOTLIN = .TRUE.
            GO TO 600
         END IF

!  A DATA CARD HAS BEEN FOUND.
!  READ THE CHARACTER FIELDS 1, 2, 3 AND 7 FROM THE CARD.

         FIELD1 = NULINE(  2:  3 )
         FIELD2 = NULINE(  5: 12 )
         FIELD3 = NULINE( 15: 22 )
         FIELD7 = NULINE( 25: 65 )
! ** Correction 1. 19/07/93: 13 lines added **

!  CHECK THAT FIELD3 IS BLANK ON 'A', 'F' AND 'G' CARDS.

            IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR.&
                 FIELD1( 1: 1 ) .EQ. 'F' .OR. &
                 FIELD1( 1: 1 ) .EQ. 'G' .OR. &
                 FIELD1( 1: 1 ) .EQ. 'H' ) THEN
               IF ( ( FIELD1( 1: 1 ) .NE. 'A' .AND. FIELD2 .NE. &
                    '       ' ) .OR.FIELD3 .NE. '       ' ) THEN
                  INFORM = 73
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2730 )
                  GO TO 800
               END IF
            END IF
! ** Correction 1. 19/07/93: end of correction **
      END IF

!  BRANCH ON THE VALUE OF INTYPE.

      GO TO ( 100, 100, 100, 100, 290, 300, 400, 900 ), INTYPE

!  INDICATOR CARD IS TEMPORARIES.
!  ------------------------------

  290 CONTINUE

!  CHECK TO SEE IF THE PARAMETER IS INTEGER, REAL, LOGICAL OR A FUNCTION.

      IF ( FIELD1 .NE. 'I ' .AND. FIELD1 .NE. 'R ' .AND.&
           FIELD1 .NE. 'M ' .AND. FIELD1 .NE. 'F ' .AND.&
           FIELD1 .NE. 'L' ) THEN
         INFORM = 54
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2540 )
         GO TO 800
      END IF

!  IF THE PARAMETER IS A FUNCTION, CHECK TO SEE THAT THE NAME HAS
!  NOT ALREADY BEEN USED.

      IF ( FIELD1 .EQ. 'F ' ) THEN
         FIELD = FIELD2 // '  GU'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NEXNAM = NEXNAM + 1
            IF ( NEXNAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            EXNAME( NEXNAM ) = FIELD2
         END IF
      ELSE

!  CHECK TO SEE THAT THE PARAMETER NAME HAS NOT ALREADY BEEN USED.

         FIELD = FIELD2 // '  PG'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            IF ( FIELD1 .EQ. 'R ' ) THEN
               NRENAM = NRENAM + 1
               IF ( NRENAM .GT. NINMAX ) THEN
                  INFORM = - 2
                  GO TO 700
               END IF
               RENAME( NRENAM ) = FIELD2
            ELSE
               IF ( FIELD1 .EQ. 'M ' ) THEN
                  NMINAM = NMINAM + 1
                  IF ( NMINAM .GT. NINMAX ) THEN
                     INFORM = - 2
                     GO TO 700
                  END IF
                  MINAME( NMINAM ) = FIELD2
               ELSE
                  IF ( FIELD1 .EQ. 'L ' ) THEN
                     NLONAM = NLONAM + 1
                     IF ( NLONAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     LONAME( NLONAM ) = FIELD2
                  ELSE
                     NINNAM = NINNAM + 1
                     IF ( NINNAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     INNAME( NINNAM ) = FIELD2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS GLOBAL.
!  -------------------------

  300 CONTINUE
      IF ( FIELD1 .EQ. 'A ' .OR. FIELD1 .EQ. 'I ' .OR.&
           FIELD1 .EQ. 'E ' ) THEN
         STARTP = .TRUE.

!  START A PARAMETER ASSIGNMENT. CHECK TO SEE THAT THE PARAMETER HAS
!  BEEN DEFINED.

         FIELD = FIELD2 // '  PG'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
            INFORM = 57
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
            GO TO 800
         END IF

! --------- MAKE GENERAL PARAMETER ASSIGNMENTS.

         IF ( FIELD1 .EQ. 'A ' ) THEN
            WRITE( IOUTGR, 3030 ) FIELD2( 1 : 6 ), FIELD7

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

         ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

            FIELD = FIELD3 // '  PG'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  GO TO 700
               END IF
               INFORM = 57
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
               GO TO 800
            END IF
            IF ( FIELD1 .EQ. 'I ' ) THEN
               WRITE( IOUTGR, 3031 ) FIELD2( 1 : 6 ),  &
                                     FIELD3( 1 : 6 ), FIELD7
            ELSE
               WRITE( IOUTGR, 3032 ) FIELD2( 1 : 6 ),  &
                                     FIELD3( 1 : 6 ), FIELD7
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) .EQ. '+' .AND. STARTP ) THEN

! --------- CONTINUE A PARAMETER ASSIGNMENT.

            WRITE( IOUTGR, 3040 ) FIELD7
         ELSE
            INFORM = 55
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS INDIVIDUALS.
!  ------------------------------

  400 CONTINUE

!  CHECK IF A NEW GROUP HAS BEEN ENCOUNTERED.

      IF ( FIELD1 .EQ. 'T ' ) THEN
         IF ( FIRSTG ) THEN

!  CHECK IF THIS IS THE FIRST GROUP-TYPE.

            FIRSTG = .FALSE.
         ELSE

!  FINISH OF THE PREVIOUS GROUP, IF ANY.

            IF ( .NOT. SETH ) THEN
               INFORM = 63
! ** Correction 5. 11/08/95: 1 line corrected **
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2630 )
! ** Correction 5. 11/08/95: end of correction **
               GO TO 800
            END IF
            IF ( .NOT. SETG ) THEN
               INFORM = 62
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2620 )
               GO TO 800
            END IF

! ---------- WIND UP F AND G

            IF ( SETF ) THEN
               WRITE( IOUTGR, 3190 )
            ELSE
               INFORM = 61
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
               GO TO 800
            END IF
            IF ( ITYPE .LT. NGRTYP ) WRITE( IOUTGR, 3191 ) NLOOP
         END IF

!  FIND ITYPE, THE GROUP-TYPE.

         FIELD = FIELD2 // '  GT'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )

!  THE GROUP-TYPE IS UNKNOWN.

         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 19
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2190 )
            GO TO 800
         END IF

! --------- FIND TYPE OF CURRENT GROUP.

         ITYPE = INLIST( IFIELD )
         WRITE( IOUTGR, 3060 ) FIELD2
         IF ( NGRTYP .GT. 1 ) WRITE( IOUTGR, 3061 ) ITYPE
         WRITE( IOUTGR, 3062 ) ANAMES( ITYPE )( 1 : 6 ),  &
                   FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )

! --------- SET GROUP PARAMETERS.

         K1 = IGPA( ITYPE )
         K2 = IGPA( ITYPE + 1 ) - 1
         DO 435 K = K1, K2
            IVAR = K - K1 + 1
            WRITE( IOUTGR, 3063 ) GPNAME( K ), FIELDI( 11 )( 1 : 6 ),  &
                FIELDI( 13 )( 1 : 6 ), IVAR
  435    CONTINUE
         IF ( LDEFND( ITYPE ) ) THEN
            INFORM = 64
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2640 )
            GO TO 800
         ELSE
            LDEFND( ITYPE ) = .TRUE.
         END IF

!  INITIALIZE LOGICALS WHICH DETERMINE WHETHER THE DATA HAS BEEN
!  INPUT IN THE CORRECT ORDER.

         STARTP = .FALSE.
         SETF = .FALSE.
         SETG = .FALSE.
         SETH = .FALSE.
         ENDF = .TRUE.
      ELSE
         IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR. FIELD1( 1: 1 ) &
              .EQ. 'I' .OR. FIELD1( 1: 1 ) .EQ. 'E' ) THEN
            IF ( SETF ) THEN
               IF ( .NOT. ENDF ) THEN
                  WRITE( IOUTGR, 3120 )
                  ENDF = .TRUE.
               END IF
            END IF

!  START A PARAMETER ASSIGNMENT. CHECK TO SEE THAT THE PARAMETER HAS
!  BEEN DEFINED.

            IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
               STARTP = .TRUE.
               FIELD = FIELD2 // '  PG'
               CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
               IF ( IFIELD .LE. 0 ) THEN
                  INFORM = 57
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
                  GO TO 800
               END IF

! --------- MAKE GROUP-SPECIFIC PARAMETER ASSIGNMENTS.

               IF ( FIELD1( 1: 1 ) .EQ. 'A' ) THEN
                  IF ( .NOT. SETF ) THEN
                     WRITE( IOUTGR, 3080 ) FIELD2( 1 : 6 ), FIELD7
                  ELSE
                     WRITE( IOUTGR, 3083 ) FIELD2( 1 : 6 ), FIELD7
                  END IF

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

               ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

                  FIELD = FIELD3 // '  PG'
                  CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
                  IF ( IFIELD .LE. 0 ) THEN
                     IF ( IFREE .EQ. 0 ) THEN
                        INFORM = - 1
                        GO TO 700
                     END IF
                     INFORM = 58
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2580 )
                     GO TO 800
                  END IF
                  IF ( FIELD1( 1: 1 ) .EQ. 'I' ) THEN
                     IF ( .NOT. SETF ) THEN
                        WRITE( IOUTGR, 3081 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                     ELSE
                        WRITE( IOUTGR, 3084 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                     END IF
                  ELSE
                     IF ( .NOT. SETF ) THEN
                        WRITE( IOUTGR, 3082 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                     ELSE
                        WRITE( IOUTGR, 3085 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                     END IF
                  END IF   
               END IF
            ELSE
               IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                  IF ( STARTP ) THEN

! --------- CONTINUATION OF A PARAMETER ASSIGNMENT.

                     IF ( .NOT. SETF ) THEN
                        WRITE( IOUTGR, 3090 ) FIELD7
                     ELSE
                        WRITE( IOUTGR, 3091 ) FIELD7
                     END IF
                  ELSE
                     INFORM = 56
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                     GO TO 800
                  END IF
               END IF
            END IF
         ELSE
            STARTP = .FALSE.
            IF ( FIELD1( 1: 1 ) .EQ. 'F' ) THEN

!  SET THE FUNCTION VALUE.

               IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                  SETF = .TRUE.
                  ENDF = .FALSE.

! --------- START G.

                  WRITE( IOUTGR, 3100 ) FIELDI(  8 )( 1 : 6 ),  &
                  FIELDI( 2 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ), FIELD7
               ELSE
                  IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                     IF ( SETF ) THEN

! --------- CONTINUATION OF G.

                        WRITE( IOUTGR, 3110 ) FIELD7
                     ELSE
                        INFORM = 56
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            ELSE
               IF ( FIELD1( 1: 1 ) .EQ. 'G' ) THEN

!  NO FUNCTION VALUE HAS BEEN SPECIFIED.

                  IF ( .NOT. SETF ) THEN
                     INFORM = 61
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
                     GO TO 800
                  END IF

!  SET THE FIRST DERIVATIVE VALUE.

                  IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                     IF ( .NOT. SETG ) THEN
                        SETG = .TRUE.

! --------- START GDASH.

                        IF ( .NOT. ENDF ) THEN
                           WRITE( IOUTGR, 3120 )
                           ENDF = .TRUE.
                        END IF
                     END IF
                     WRITE( IOUTGR, 3130 ) FIELDI( 2 )( 1 : 6 ),  &
                            FIELDI( 10 )( 1 : 6 ), 2, FIELD7
                  ELSE
                     IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                        IF ( SETG ) THEN

! --------- CONTINUATION OF GDASH.

                           WRITE( IOUTGR, 3140 ) FIELD7
                        ELSE
                           INFORM = 56
                           IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               ELSE
                  IF ( FIELD1( 1: 1 ) .EQ. 'H' ) THEN

!  SET THE SECOND DERIVATIVE VALUE.

                     IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                        IF ( .NOT. SETH ) THEN

!  THE FIRST DERIVATIVE HAS NOT BEEN SET.

                           IF ( .NOT. SETG ) THEN
                              INFORM = 62
                              IF ( IOUT .GT. 0 ) WRITE( IOUT, 2620 )
                              GO TO 800
                           END IF
                           SETH = .TRUE.
                        END IF

! --------- SET G2DASH.

                        WRITE( IOUTGR, 3130 ) FIELDI( 2 )( 1 : 6 ),  &
                               FIELDI( 10 )( 1 : 6 ), 3, FIELD7
                     ELSE
                        IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                           IF ( SETH ) THEN

! --------- CONTINUATION OF G2DASH.

                              WRITE( IOUTGR, 3140 ) FIELD7
                           ELSE
                              INFORM = 56
                              IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                              GO TO 800
                           END IF
                        END IF
                     END IF
                  ELSE
                     INFORM = 56
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                     GO TO 800
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  THE END OF THE INPUT FILE HAS BEEN REACHED BEFORE THE ENDATA CARD.

  590 CONTINUE 

!  IF THE ELEMENTS CARD HAS NOT BEEN ENCOUNTERED, EXIT.

      IF ( DEFNAM ) THEN
         INFORM = 52
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2520 )
         RETURN
      END IF
      IF ( NGRTYP .GT. 0 ) GO TO 930
      IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )

!  A DUMMY ROUTINE WILL BE SUBSTITUTED.

  600 CONTINUE 

!  WRITE A DUMMY GROUPS ROUTINE.

      IF ( SINGLE ) THEN
         WRITE( IOUTGR, 3001 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                    FIELDI(  8 )( 1 : 6 ), &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                    PNAME
      ELSE
         WRITE( IOUTGR, 3000 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                    FIELDI(  8 )( 1 : 6 ), &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                    PNAME
      END IF
      WRITE( IOUTGR, 3009 ) FIELDI( 20 )( 1 : 6 )
      WRITE( IOUTGR, 3210 )
      INFORM = 0
      RETURN

!  INSUFFICIENT SPACE TO CONTINUE CONSTRUCTION.

  700 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2000 ) INCRSE( - INFORM )
      RETURN

!  SUBROUTINE INCOMPLETE.

  800 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2990 ) LINENO, NULINE
      RETURN

!  SUBROUTINE SUCCESSFULLY COMPLETED.

  900 CONTINUE
      IF ( .NOT. FIRSTG ) THEN

!  FINISH OF THE PREVIOUS GROUP, IF ANY.

         IF ( .NOT. SETH ) THEN
            INFORM = 63
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2630 )
            GO TO 800
         END IF
         IF ( .NOT. SETG ) THEN
            INFORM = 62
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2620 )
            GO TO 800
         END IF

! ---------- WIND UP F AND G.

         IF ( SETF ) THEN
            WRITE( IOUTGR, 3190 )
         ELSE
            INFORM = 61
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
            GO TO 800
         END IF
         IF ( ITYPE .LT. NGRTYP ) WRITE( IOUTGR, 3191 ) NLOOP
      END IF

! ---------- END DO LOOP.

      WRITE( IOUTGR, 3200 ) NLOOP
  910 CONTINUE

! ---------- SUCCESSFUL RUN. WIND UP OUTPUT.

      WRITE( IOUTGR, 3210 )
      INFORM = 0

!   CHECK THAT ALL ELEMENT TYPES HAVE BEEN DEFINED.

  930 CONTINUE
      DO 940 ITYPE = 1, NGRTYP
         IF ( .NOT. LDEFND( ITYPE ) ) THEN
            INFORM = 53
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2530 ) GTYPES( ITYPE )
         END IF
  940 CONTINUE
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKEGR - insufficient space.',  &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAKEGR - warning.',  &
              ' First card not groups. ', /, '    A dummy',  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKEGR - indicator card not recognised ' )
 2190 FORMAT( ' ** Exit from MAKEGR - group type not recognised:',  &
              ' name is ', A10 )
 2510 FORMAT( ' ** Exit from MAKEGR -',  &
              ' Name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKEGR -',  &
              ' data file incomplete. No ENDATA card ' )
 2530 FORMAT( ' ** Exit from MAKEGR - warning, group type ', A8,  &
              ' undefined ' )
 2540 FORMAT( ' ** Exit from MAKEGR -',  &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKEGR -',  &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKEGR -',  &
              ' unrecognised field 1 in INDIVIDUAL section' )
 2570 FORMAT( ' ** Exit from MAKEGR -',  &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKEGR -',  &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKEGR - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKEGR - function not set '  )
 2620 FORMAT( ' ** Exit from MAKEGR -',  &
              ' one or more first derivatives not set ' )
 2630 FORMAT( ' ** Exit from MAKEGR -',  &
              ' one or more second derivatives not set ' )
 2640 FORMAT( ' ** Exit from MAKEGR - group type already defined ' )
! ** Correction 2. 19/07/93: 2 lines added **
 2730 FORMAT( ' ** Exit from MAKEGR - field 2 or 3 not blank on',  &
              ' A, F, G or H card ' )
! ** Correction 2. 19/07/93: end of correction **
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', I5, 4X, A160 )
 2980 FORMAT( ' Line ', I5, '.', I2, 1X, A65 )
 2990 FORMAT( ' Line ', I5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      LOGICAL ', A6, /,  &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6, &
                             '), ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',  &
                                         A6, '(', A6, '), ', A6, &
                                      '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      LOGICAL ', A6, /,  &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6, &
                             '), ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, ',3), ',  &
                                         A6, '(', A6, '), ', A6, &
                                      '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3002 FORMAT( '      INTEGER ', A6, ', ', A6, ', ', A6, ', ', A6 )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, 4( :, ', ', A6 ) ) )
 3019 FORMAT( ( '      REAL             ', A6, 4( :, ', ', A6 ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, 4( :, ', ', A6 ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, 4( :, ', ', A6 ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, 4( :, ', ', A6 ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', I5, 1X, A6, ' = 1, ', A6, /,  &
              '       ', A6, ' = ', A6, '(', A6, ')', /,  &
              '       ', A6, ' = ', A6, '(', A6, ')', /,  &
              '       IF ( ', A6, ' .EQ. 0 ) GO TO ', I5, /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1' )
! ** Correction 6. 15/08/95: 2 lines corrected **
 3051 FORMAT( '       GO TO (', 8( I5, :, ',' ), /,  &
            ( '     *        ', 8( I5, :, ',' ) ) )
! ** Correction 6. 15/08/95: end of correction **
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3060 FORMAT( 'C', /, 'C  GROUP TYPE : ', A8, /, 'C' )
 3061 FORMAT( I5, '  CONTINUE ' )
 3062 FORMAT( '       ', A6, '= ', A6, '(', A6, ')' )
 3063 FORMAT( '       ', A6, '= ', A6, '(', A6, '+', I6, ')' )
 3080 FORMAT( '       ', A6, '= ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
! ** Correction 3. 12/01/94: 1 line corrected **
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
! ** Correction 3. 12/01/94: end of correction **
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
! ** Correction 4. 12/01/94: 1 line corrected **
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
! ** Correction 4. 12/01/94: end of correction **
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( .NOT. ', A6, ' ) THEN', /,  &
              '        ', A6, '(', A6, ',1)= ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3130 FORMAT( '        ', A6, '(', A6, ',', I1, ')= ', A41 )
 3140 FORMAT( '     *                         ', A41 )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', I6 )
 3200 FORMAT( I5,  ' CONTINUE' )
 3210 FORMAT( '      RETURN', /,  '      END' )

!  END OF MAKEGR.

      END

! ** Correction report
! ** Correction 1. 04/04/02: 1 line corrected
! ** End of Correction report.
!  THIS VERSION: 04/04/2002 AT 09:30:00 AM.
!  ** VERSION B **

      SUBROUTINE MAGRAD( INPUT , IOUT  , IOUTGF, IOUTGD, IOUTEM, INFORM, &
                         NGRTYP, NGRMAX, NLMAX , NINMAX, &
                         PNAME , ANAMES, RENAME, INNAME, LONAME, MINAME, &
                         EXNAME, GTYPES, LDEFND, GPNAME, IGPA  , NGPMAX, &
                         DEBUG , LENGTH, ITABLE, KEY   , INLIST, SINGLE, &
                         NULINE, GOTLIN, IAUTO , IAD0  , IPRINT )
      INTEGER            INPUT , IOUT  , IOUTGF, INFORM, LENGTH
      INTEGER            NLMAX , NGRTYP, NINMAX, NGPMAX, NGRMAX
      INTEGER            IPRINT, IOUTGD, IOUTEM, IAUTO , IAD0
      LOGICAL            GOTLIN, DEBUG , SINGLE
      CHARACTER * 8      PNAME
      CHARACTER * 160    NULINE
      INTEGER            ITABLE( LENGTH ), IGPA( NGRMAX )
      INTEGER            INLIST( LENGTH )
      LOGICAL            LDEFND( NLMAX  )
      CHARACTER * 12     KEY   ( LENGTH )
      CHARACTER * 10     RENAME( NINMAX ), INNAME( NINMAX )
      CHARACTER * 10     GPNAME( NGPMAX ), EXNAME( NINMAX )
      CHARACTER * 10     LONAME( NINMAX ), GTYPES( NGRMAX )
      CHARACTER * 10     ANAMES( NGRMAX ), MINAME( NINMAX )

!  Make a group function evaluation subroutine, suitable for 
!  ---------------------------------------------------------
!  automatic differentiation from a GPS function data file
!  -------------------------------------------------------

!  Nick Gould 04/05/1995
!  For CGT Productions.

!  -------------------------------------------------------------------

!  Function indicator cards.
!  -------------------------

!  Definition   Purpose.
!  ----------   --------
!  GROUPS       Problem name.
!  TEMPORARIES  Names of additional parameters used in function defs.
!  GLOBALS      General parameter assignments.
!  INDIVIDUALS  Set function and derivative values for each group-type.
!  ENDATA       End of input data.

!  Data card description.
!  ----------------------

!  See 'A Proposal for a standard data input format for large-scale
!       nonlinear programming problems', Section 4,
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint, 
!       Report CS-89-61, Dept of Computer Science, U. of Waterloo,
!       Waterloo, Ontario, N2L3G1, Canada.

!  -------------------------------------------------------------------
!  Returns with negative values of INFORM indicate that insufficient
!  array space has been allowed, as follows:

!    INFORM = - 1  when LENGTH not large enough
!    INFORM = - 2  when MAX( NINNAM, NRENAM, NLONAM, NENAM, NMINAM )
!                  .GT. NINMAX

      INTEGER          I, IFIELD, IFREE, ITYPE, INTYPE, IVAR, K1, K2
      INTEGER          NINNAM, NLOOP, NRENAM, NMINAM, NPNAME, NRENM1
      INTEGER          NEXNAM, NINCRS, LINENO, IIRES, K, ILINES, NLINES
      INTEGER          MBLANK, MFIXED, MFREE, MNAME, MTEMP, MGLOB, NTEM
      INTEGER          MINDIV, MENDAT, MAXNUL, MXRECL, NLONAM, NRENM2
!     INTEGER          NGTNAM
      LOGICAL          DEFNAM, ENDPAR, ENDGEN, FIRSTG
      LOGICAL          SETF, STARTP, FIXED, STARTV
      LOGICAL          ENDF, OUTGF
      PARAMETER        ( IIRES = 21 )
      PARAMETER        ( NINCRS = 2 )
      CHARACTER * 2    FIELD1
      CHARACTER * 4    AD0
      CHARACTER * 6    INCRSE( NINCRS )
      CHARACTER * 8    FIELD2, FIELD3, FIELDI( IIRES )
      CHARACTER * 10   CTEMP
      CHARACTER * 12   FIELD
      CHARACTER * 15   AORB
      CHARACTER * 41   FIELD7
      CHARACTER * 72   CTEM
      EXTERNAL         HASHB , HASHC

!  Parameter definitions.

      PARAMETER        ( MXRECL = 160 )
      CHARACTER * 160  BLNKLN
      PARAMETER        ( MBLANK =  1, MFIXED =  2, MFREE = 3  )
      PARAMETER        ( MNAME =  4, MTEMP =  5 )
      PARAMETER        ( MGLOB =  6, MINDIV =  7, MENDAT =  8 )
      INTEGER          LENIND( MENDAT )
      CHARACTER * 12   INDIC8( MENDAT ), HEADER
      PARAMETER        ( MAXNUL = 20 )
      CHARACTER * 65   NULINA( MAXNUL )

!  Data declarations.

      DATA INCRSE / 'LENGTH', 'NINMAX' /
      DATA INDIC8( MBLANK ) / '            ' /, LENIND( MBLANK ) / 0  / 
      DATA INDIC8( MFIXED ) / 'FIXED FORMAT' /, LENIND( MFIXED ) / 12 /
      DATA INDIC8( MFREE  ) / 'FREE FORMAT ' /, LENIND( MFREE  ) / 11 /
      DATA INDIC8( MNAME  ) / 'GROUPS      ' /, LENIND( MNAME  ) / 6  /
      DATA INDIC8( MTEMP  ) / 'TEMPORARIES ' /, LENIND( MTEMP  ) / 11 / 
      DATA INDIC8( MGLOB  ) / 'GLOBALS     ' /, LENIND( MGLOB  ) / 7  /
      DATA INDIC8( MINDIV ) / 'INDIVIDUALS ' /, LENIND( MINDIV ) / 11 / 
      DATA INDIC8( MENDAT ) / 'ENDATA      ' /, LENIND( MENDAT ) / 6  /
      DATA FIELDI(  1 ) / 'GROUPF  ' /,  FIELDI(  2 ) / 'GVALUE  ' /
      DATA FIELDI(  3 ) / 'LGVALU  ' /,  FIELDI(  4 ) / 'FVALUE  ' /
      DATA FIELDI(  5 ) / 'NCALCG  ' /,  FIELDI(  6 ) / 'ITYPEG  ' /
      DATA FIELDI(  7 ) / 'ICALCG  ' /,  FIELDI(  8 ) / 'DERIVS  ' /
      DATA FIELDI(  9 ) / 'IGRTYP  ' /,  FIELDI( 10 ) / 'IGROUP  ' /
      DATA FIELDI( 11 ) / 'GPVALU  ' /,  FIELDI( 12 ) / 'ISTGPA  ' /
      DATA FIELDI( 13 ) / 'IPSTRT  ' /,  FIELDI( 14 ) / 'JCALCG  ' /
      DATA FIELDI( 15 ) / 'LTYPEG  ' /,  FIELDI( 16 ) / 'LSTGPA  ' /
      DATA FIELDI( 17 ) / 'LCALCG  ' /,  FIELDI( 18 ) / 'LFVALU  ' /
      DATA FIELDI( 19 ) / 'LGPVLU  ' /,  FIELDI( 20 ) / 'IGSTAT  ' /
      DATA FIELDI( 21 ) / 'GROUP   ' /
!     DATA FIELDI( 21 ) / 'GROUPD  ' /
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2900 )

!  Set initial values for integer variables.

      NINNAM = 0
      NRENAM = 0
      NLONAM = 0
      NEXNAM = 0
      NMINAM = 0
      LINENO = 0
      INTYPE = 1
      ILINES = 0
      NLINES = 0
      NTEM = 0
      OUTGF = IOUTGF .GT. 0

!  SET INITIAL VALUES FOR LOGICAL VARIABLES.

      DEFNAM = .FALSE.
      ENDPAR = .FALSE.
      STARTP = .FALSE.
      ENDGEN = .FALSE.
      FIRSTG = .TRUE.
      FIXED = .TRUE.

!  FIND WHICH GROUP-TYPES ARE NONTRIVIAL.

      DO 20 ITYPE = 1, NGRTYP
         LDEFND( ITYPE ) = .FALSE.
   20 CONTINUE

!  INSERT THE LIST OF GROUP-TYPE ARGUMENTS INTO THE DICTIONARY.

      DO 30 ITYPE = 1, NGRTYP
         FIELD = ANAMES( ITYPE ) // 'PG'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NRENAM = NRENAM + 1
            IF ( NRENAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            RENAME( NRENAM ) = ANAMES( ITYPE )
         END IF
   30 CONTINUE
!     NGTNAM = NRENAM

!  INCLUDE THE NAMES OF THE GROUP PARAMETERS USED
!  IN THIS DICTIONARY.

      IF ( NGRTYP .GT. 0 ) THEN
         NPNAME = IGPA( NGRTYP + 1 ) - 1
         DO 40 I = 1, NPNAME
            FIELD = GPNAME( I ) // 'PG'
            CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
            IF ( IFREE .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  GO TO 700
               END IF
            ELSE
               NRENAM = NRENAM + 1
               IF ( NRENAM .GT. NINMAX ) THEN
                  INFORM = - 2
                  GO TO 700
               END IF
               RENAME( NRENAM ) = GPNAME( I )
            END IF
   40    CONTINUE
      END IF

!  SET A BLANK LINE.

      DO 50 I = 1, MXRECL
         BLNKLN( I: I ) = ' '
   50 CONTINUE    

!  READ NEXT LINE.

  100 CONTINUE
      IF ( ILINES + 1 .GT. NLINES ) THEN

!  READ NEXT LINE FROM THE INPUT FILE.

         LINENO = LINENO + 1
         IF ( FIXED ) THEN
            IF ( GOTLIN ) THEN
               GOTLIN = .FALSE.
            ELSE
               NULINE = BLNKLN
               READ ( INPUT, 1000, END = 590, ERR = 590 ) NULINE
            END IF
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2990 )  &
                 LINENO, NULINE
         ELSE
            IF ( GOTLIN ) THEN
               GOTLIN = .FALSE.
            ELSE
               NULINE = BLNKLN
               READ ( INPUT, 1010, END = 590, ERR = 590 ) NULINE
            END IF
            IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2970 )  &
                 LINENO, NULINE

!  IF THE CARD IS IN FREE FORMAT, TRANSLATE IT INTO FIXED FORMAT.

            CALL  FREEFM( NULINE, MXRECL, MENDAT, INDIC8, LENIND,  &
                          NULINA, MAXNUL, NLINES, .FALSE.,  &
                          INFORM, IOUT )
            IF ( INFORM .GT. 0 ) GO TO 800
            IF ( NLINES .GT. 0 ) THEN

!  IF THERE ARE NON-BLANK LINES ON THE FREE FORMAT CARD, READ THE FIRST.

               ILINES = 1
               NULINE = BLNKLN
               NULINE = NULINA( ILINES )
               IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
                    LINENO, ILINES, NULINE
            ELSE

!  THERE ARE ONLY BLANK LINES ON THE FREE FORMAT CARD.

               GO TO 100
            END IF
         END IF
      ELSE

!  READ NEXT LINE FROM THE LAST ENCOUNTERED FREE FORMAT CARD.

         ILINES = ILINES + 1
         NULINE = BLNKLN
         NULINE = NULINA( ILINES )
         IF ( IOUT .GT. 0 .AND. DEBUG ) WRITE( IOUT, 2980 )  &
              LINENO, ILINES, NULINE
      END IF

!  CONSIDER THE HEADER PART OF THE CARD.

      HEADER = NULINE( 1: 12 )

!  IGNORE BLANK LINES.

      IF ( HEADER .EQ. INDIC8( MBLANK ) ) GO TO 100
      IF ( NULINE( 1: 1 ) .NE. ' ' ) THEN

!  IGNORE COMMENT CARDS.

         IF ( NULINE( 1: 1 ) .EQ. '*' ) GO TO 100

!  CHECK IF WE HAVE ENTERED FIXED-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFIXED ) ) THEN
            FIXED = .TRUE.
            GO TO 100
         END IF

!  CHECK IF WE HAVE ENTERED FREE-FORMAT INPUT.

         IF ( HEADER .EQ. INDIC8( MFREE ) ) THEN
            FIXED = .FALSE.
            GO TO 100
         END IF

!  CHECK THAT THE FIRST ENCOUNTERED INDICATOR CARD IS THE GROUPS CARD.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( HEADER .NE. INDIC8( MNAME ) ) THEN
               IF ( NGRTYP .GT. 0 ) GO TO 930
               IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010)
               GOTLIN = .TRUE.
               GO TO 600
            ELSE

!  INDICATOR CARD IS GROUPS.
!  -------------------------

               IF ( PNAME  .NE. NULINE( 15: 22 ) ) THEN
                  INFORM = 51
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2510 )
                  GO TO 800
               ELSE
                  DEFNAM = .TRUE.
                  GO TO 100
               END IF
            END IF
         END IF

!  AN INDICATOR CARD HAS BEEN FOUND.

         DO 110 I = INTYPE, MENDAT
            IF ( HEADER .EQ. INDIC8( I ) ) THEN
               INTYPE = I
               GO TO 120
            END IF
  110    CONTINUE

!  THE INDICATOR CARD IS NOT RECOGNISED.

         INFORM = 2
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2020 )
         GO TO 800
  120    CONTINUE

!  THE PARAMETER VALUES HAVE BEEN COMPLETED. WRITE OUT THE
!  FIRST PART OF THE GENERATED SUBROUTINE.

         IF ( INTYPE .GE. MGLOB .AND. .NOT. ENDPAR ) THEN
            ENDPAR = .TRUE.
            NLOOP = NGRTYP + 1

!  Insert the list of reserved integer/real/logical variables into
!  the dictionary.

            DO 130 I = 1, IIRES
               FIELD = FIELDI( I ) // '  PG'
               CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
               IF ( IFREE .LE. 0 ) THEN
                  IF ( IFREE .EQ. 0 ) THEN
                     INFORM = - 1
                     GO TO 700
                  END IF
                  INFORM = 59
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2590 ) FIELDI( I )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- Set up subroutine call and reserved parameter declarations.

            IF ( IAUTO .EQ. 1 ) THEN
               IF ( SINGLE ) THEN
                  AORB = 'FORWARD_SINGLE '
               ELSE
                  AORB = 'FORWARD_DOUBLE '
               END IF
            ELSE
               IF ( SINGLE ) THEN
                  AORB = 'BACKWARD_SINGLE'
               ELSE
                  AORB = 'BACKWARD_DOUBLE'
               END IF
            END IF
            IF ( IAD0 .EQ. 1 ) THEN
               AD0 = 'AD01'
            ELSE
               AD0 = 'AD02'
            END IF
            IF ( SINGLE ) THEN
               IF ( OUTGF ) &
               WRITE( IOUTGF, 3001 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                          FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                          FIELDI(  8 )( 1 : 6 ), &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                          PNAME
               WRITE( IOUTGD, 3005 ) FIELDI( 21 )( 1 : 6 ), &
                        ( FIELDI( I )( 1 : 6 ), I = 2, 4 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
               AD0, AORB, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                          FIELDI(  8 )( 1 : 6 ), &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                          PNAME
            ELSE
               IF ( OUTGF ) &
               WRITE( IOUTGF, 3000 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                          FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                          FIELDI(  8 )( 1 : 6 ), &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                          PNAME
               WRITE( IOUTGD, 3004 ) FIELDI( 21 )( 1 : 6 ), &
                        ( FIELDI( I )( 1 : 6 ), I = 2, 4 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
               AD0, AORB, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                        ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                          FIELDI(  8 )( 1 : 6 ), &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                          PNAME
            END IF
            IF ( IAD0 .EQ. 1 ) THEN
               WRITE( IOUTGD, 3006 )
            ELSE
               WRITE( IOUTGD, 3007 )
            END IF
! ** Correction 1. 04/04/02: 1 line replaced by 5
            IF ( NGRTYP .EQ. 0 ) THEN
               IF ( OUTGF ) WRITE( IOUTGF, 3009 ) FIELDI( 20 )( 1 : 6 )
               WRITE( IOUTGD, 3009 ) FIELDI( 20 )( 1 : 6 )
               GO TO 910
            END IF
            IF ( OUTGF ) WRITE( IOUTGF, 3002 ) FIELDI(  9 )( 1 : 6 ),  &
                         FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),  &
                         FIELDI( 14 )( 1 : 6 )
            WRITE( IOUTGD, 3002 ) FIELDI(  9 )( 1 : 6 ),  &
                          FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),  &
                          FIELDI( 14 )( 1 : 6 )

! --------- Insert integer declarations.

            IF ( NINNAM .GT. 0 .AND. OUTGF )  &
               WRITE( IOUTGF, 3010 ) ( INNAME( I ), I = 1, NINNAM )
            IF ( NINNAM .GT. 0 )  &
               WRITE( IOUTGD, 3010 ) ( INNAME( I ), I = 1, NINNAM )

!  Order the real values so that the list of variables which belong
!  to intrinsic or external functions follow those which do not.

            IF ( NRENAM .GT. 0 ) THEN
               NRENM1 = 0
               NRENM2 = NRENAM + 1
  140          CONTINUE
               IF ( NRENM1 + 1 .EQ. NRENM2 ) GO TO 180
               DO 150 I = 1, NMINAM
                  IF ( RENAME( NRENM1 + 1 ) .EQ. MINAME( I ) ) GO TO 170
  150          CONTINUE
               DO 160 I = 1, NEXNAM
                  IF ( RENAME( NRENM1 + 1 ) .EQ. EXNAME( I ) ) GO TO 170
  160          CONTINUE
               NRENM1 = NRENM1 + 1
               GO TO 140
  170          CONTINUE
               NRENM2 = NRENM2 - 1
               CTEMP = RENAME( NRENM2 )
               RENAME( NRENM2 ) = RENAME( NRENM1 + 1 )
               RENAME( NRENM1 + 1 ) = CTEMP
               GO TO 140
  180          CONTINUE

! --------- Insert real declarations.

               IF ( SINGLE ) THEN
                  IF ( OUTGF )                                                 &
                  WRITE( IOUTGF, 3019 ) ( RENAME( I ), I = 1, NRENAM )
               ELSE
                  IF ( OUTGF )                                                 &
                  WRITE( IOUTGF, 3020 ) ( RENAME( I ), I = 1, NRENAM )
               END IF
               IF ( IAD0 .EQ. 1 ) THEN
                  IF ( NRENM1 .GT. 0 ) WRITE( IOUTGD, 3018 )                   &
                       ( RENAME( I ), I = 1, NRENM1 )
               ELSE
                  IF ( NRENM1 .GT. 0 ) WRITE( IOUTGD, 3017 )                   &
                       ( AD0, RENAME( I ), I = 1, NRENM1 )
               END IF
               IF ( NRENM2 .LE. NRENAM ) WRITE( IOUTGD, 3017 )                 &
                    ( AD0, RENAME( I ), I = NRENM2, NRENAM )
            END IF

! --------- Insert logical declarations.

            IF ( NLONAM .GT. 0 .AND. OUTGF )                                   &
               WRITE( IOUTGF, 3023 ) ( LONAME( I ), I = 1, NLONAM )
            IF ( NLONAM .GT. 0 )                                               &
               WRITE( IOUTGD, 3023 ) ( LONAME( I ), I = 1, NLONAM )

! --------- Insert intrinsic declarations.

            IF ( NMINAM .GT. 0 .AND. OUTGF )                                   &
               WRITE( IOUTGF, 3021 ) ( MINAME( I ), I = 1, NMINAM )

! --------- Insert external declarations.

            IF ( NEXNAM .GT. 0 .AND. OUTGF )                                   &
               WRITE( IOUTGF, 3022 ) ( EXNAME( I ), I = 1, NEXNAM )
            IF ( NEXNAM .GT. 0 )                                               &
               WRITE( IOUTGD, 3022 ) ( EXNAME( I ), I = 1, NEXNAM )
            IF ( OUTGF ) WRITE( IOUTGF, 3009 ) FIELDI( 20 )( 1 : 6 )
            WRITE( IOUTGD, 3009 ) FIELDI( 20 )( 1 : 6 )
         END IF

!  THE GENERAL PARAMETER ASSIGNMENTS HAVE BEEN COMPLETED.
!  CONTINUE WITH THE CONSTRUCTION OF THE GENERATED SUBROUTINE.

         IF ( INTYPE .GE. MINDIV .AND. .NOT. ENDGEN ) THEN
            ENDGEN = .TRUE.

! --------- START LOOP OVER GROUPS.

            IF ( OUTGF ) &
            WRITE( IOUTGF, 3050 ) NLOOP,  FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ),                                      &
                   FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), NLOOP,                               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 )
            IF ( IAD0 .EQ. 2 ) THEN
              WRITE( IOUTGD, 3011 )
!             DO I = 1, NGTNAM
              DO I = 1, NRENM1
                 WRITE( IOUTGD, 3016 ) AD0, RENAME( I )
              END DO
            END IF
            WRITE( IOUTGD, 3050 ) NLOOP,  FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ),                                      &
                   FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), NLOOP,                               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 )
            IF ( NGRTYP .GT. 1 ) THEN
               IF ( OUTGF ) WRITE( IOUTGF, 3051 ) ( I, I = 1, NGRTYP )
               WRITE( IOUTGD, 3051 ) ( I, I = 1, NGRTYP )
               IF ( OUTGF ) WRITE( IOUTGF, 3052 ) FIELDI(  9 )( 1 : 6 )
               WRITE( IOUTGD, 3052 ) FIELDI(  9 )( 1 : 6 )
            END IF
         END IF

!  INDICATOR CARD IS ENDATA.
!  -------------------------

         IF ( INTYPE .EQ. MENDAT ) GO TO 900
         GO TO 100
      ELSE

!  CHECK THAT THE FIRST NON COMMENT CARD IS THE GROUPS INDICATOR CARD.

         IF ( .NOT. DEFNAM  ) THEN
            IF ( NGRTYP .GT. 0 ) GO TO 930
            IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )
            GOTLIN = .TRUE.
            GO TO 600
         END IF

!  A DATA CARD HAS BEEN FOUND.
!  READ THE CHARACTER FIELDS 1, 2, 3 AND 7 FROM THE CARD.

         FIELD1 = NULINE(  2:  3 )
         FIELD2 = NULINE(  5: 12 )
         FIELD3 = NULINE( 15: 22 )
         FIELD7 = NULINE( 25: 65 )

!  CHECK THAT FIELD3 IS BLANK ON 'A', 'F' AND 'G' CARDS.

            IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR.&
                 FIELD1( 1: 1 ) .EQ. 'F' .OR. &
                 FIELD1( 1: 1 ) .EQ. 'G' .OR. &
                 FIELD1( 1: 1 ) .EQ. 'H' ) THEN
               IF ( ( FIELD1( 1: 1 ) .NE. 'A' .AND. FIELD2 .NE. &
                    '       ' ) .OR.FIELD3 .NE. '       ' ) THEN
                  INFORM = 73
                  IF ( IOUT .GT. 0 ) WRITE( IOUT, 2730 )
                  GO TO 800
               END IF
            END IF
      END IF

!  BRANCH ON THE VALUE OF INTYPE.

      GO TO ( 100, 100, 100, 100, 290, 300, 400, 900 ), INTYPE

!  INDICATOR CARD IS TEMPORARIES.
!  ------------------------------

  290 CONTINUE

!  CHECK TO SEE IF THE PARAMETER IS INTEGER, REAL, LOGICAL OR A FUNCTION.

      IF ( FIELD1 .NE. 'I ' .AND. FIELD1 .NE. 'R ' .AND.&
           FIELD1 .NE. 'M ' .AND. FIELD1 .NE. 'F ' .AND.&
           FIELD1 .NE. 'L' ) THEN
         INFORM = 54
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2540 )
         GO TO 800
      END IF

!  IF THE PARAMETER IS A FUNCTION, CHECK TO SEE THAT THE NAME HAS
!  NOT ALREADY BEEN USED.

      IF ( FIELD1 .EQ. 'F ' ) THEN
         FIELD = FIELD2 // '  GU'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            NEXNAM = NEXNAM + 1
            IF ( NEXNAM .GT. NINMAX ) THEN
               INFORM = - 2
               GO TO 700
            END IF
            EXNAME( NEXNAM ) = FIELD2
         END IF
      ELSE

!  CHECK TO SEE THAT THE PARAMETER NAME HAS NOT ALREADY BEEN USED.

         FIELD = FIELD2 // '  PG'
         CALL HASHB ( LENGTH, 12, FIELD, KEY, ITABLE, IFREE )
         IF ( IFREE .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
         ELSE
            IF ( FIELD1 .EQ. 'R ' ) THEN
               NRENAM = NRENAM + 1
               IF ( NRENAM .GT. NINMAX ) THEN
                  INFORM = - 2
                  GO TO 700
               END IF
               RENAME( NRENAM ) = FIELD2
            ELSE
               IF ( FIELD1 .EQ. 'M ' ) THEN
                  NMINAM = NMINAM + 1
                  IF ( NMINAM .GT. NINMAX ) THEN
                     INFORM = - 2
                     GO TO 700
                  END IF
                  MINAME( NMINAM ) = FIELD2
               ELSE
                  IF ( FIELD1 .EQ. 'L ' ) THEN
                     NLONAM = NLONAM + 1
                     IF ( NLONAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     LONAME( NLONAM ) = FIELD2
                  ELSE
                     NINNAM = NINNAM + 1
                     IF ( NINNAM .GT. NINMAX ) THEN
                        INFORM = - 2
                        GO TO 700
                     END IF
                     INNAME( NINNAM ) = FIELD2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS GLOBAL.
!  -------------------------

  300 CONTINUE
      IF ( FIELD1 .EQ. 'A ' .OR. FIELD1 .EQ. 'I ' .OR.&
           FIELD1 .EQ. 'E ' ) THEN
         STARTP = .TRUE.

!  START A PARAMETER ASSIGNMENT. CHECK TO SEE THAT THE PARAMETER HAS
!  BEEN DEFINED.

         FIELD = FIELD2 // '  PG'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
         IF ( IFIELD .LE. 0 ) THEN
            IF ( IFREE .EQ. 0 ) THEN
               INFORM = - 1
               GO TO 700
            END IF
            INFORM = 57
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
            GO TO 800
         END IF

! --------- MAKE GENERAL PARAMETER ASSIGNMENTS.

         IF ( FIELD1 .EQ. 'A ' ) THEN
            IF ( OUTGF ) WRITE( IOUTGF, 3030 ) FIELD2( 1 : 6 ), FIELD7
            NTEM = NTEM + 1
            WRITE( IOUTEM, 3080 ) FIELD2( 1 : 6 ), FIELD7

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

         ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

            FIELD = FIELD3 // '  PG'
            CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
            IF ( IFIELD .LE. 0 ) THEN
               IF ( IFREE .EQ. 0 ) THEN
                  INFORM = - 1
                  GO TO 700
               END IF
               INFORM = 57
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
               GO TO 800
            END IF
            IF ( FIELD1 .EQ. 'I ' ) THEN
               IF ( OUTGF ) WRITE( IOUTGF, 3031 ) FIELD2( 1 : 6 ),  &
                                     FIELD3( 1 : 6 ), FIELD7
               NTEM = NTEM + 1
               WRITE( IOUTEM, 3081 ) FIELD2( 1 : 6 ),  &
                                     FIELD3( 1 : 6 ), FIELD7
            ELSE
               IF ( OUTGF ) WRITE( IOUTGF, 3032 ) FIELD2( 1 : 6 ),  &
                                     FIELD3( 1 : 6 ), FIELD7
               NTEM = NTEM + 1
               WRITE( IOUTEM, 3082 ) FIELD2( 1 : 6 ),  &
                                     FIELD3( 1 : 6 ), FIELD7
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) .EQ. '+' .AND. STARTP ) THEN

! --------- CONTINUE A PARAMETER ASSIGNMENT.

            IF ( OUTGF ) WRITE( IOUTGF, 3040 ) FIELD7
            NTEM = NTEM + 1
            WRITE( IOUTEM, 3040 ) FIELD7
         ELSE
            INFORM = 55
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  INDICATOR CARD IS INDIVIDUALS.
!  ------------------------------

  400 CONTINUE

!  CHECK IF A NEW GROUP HAS BEEN ENCOUNTERED.

      IF ( FIELD1 .EQ. 'T ' ) THEN
         IF ( FIRSTG ) THEN

!  CHECK IF THIS IS THE FIRST GROUP-TYPE.

            FIRSTG = .FALSE.
         ELSE

! ---------- WIND UP F AND G

            IF ( SETF ) THEN
               IF ( .NOT. ENDF ) THEN
                  IF ( IAD0 .EQ. 1 ) THEN
                     WRITE( IOUTGD, 3122 ) FIELDI( 8 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  ELSE
                     WRITE( IOUTGD, 3123 ) FIELDI( 8 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  END IF
                  ENDF = .TRUE.
               END IF
               WRITE( IOUTGD, 3190 )
            ELSE
               INFORM = 61
               IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
               GO TO 800
            END IF
            IF ( ITYPE .LT. NGRTYP .AND. OUTGF ) &
               WRITE( IOUTGF, 3191 ) NLOOP
            IF ( ITYPE .LT. NGRTYP ) WRITE( IOUTGD, 3191 ) NLOOP
         END IF

!  FIND ITYPE, THE GROUP-TYPE.

         FIELD = FIELD2 // '  GT'
         CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )

!  THE GROUP-TYPE IS UNKNOWN.

         IF ( IFIELD .LE. 0 ) THEN
            INFORM = 19
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2190 )
            GO TO 800
         END IF

! --------- FIND TYPE OF CURRENT GROUP.

         ITYPE = INLIST( IFIELD )
         IF ( OUTGF ) WRITE( IOUTGF, 3060 ) FIELD2
         WRITE( IOUTGD, 3060 ) FIELD2
         IF ( NGRTYP .GT. 1 .AND. OUTGF ) WRITE( IOUTGF, 3061 ) ITYPE
         IF ( NGRTYP .GT. 1 ) WRITE( IOUTGD, 3061 ) ITYPE
         IF ( OUTGF ) WRITE( IOUTGF, 3062 ) ANAMES( ITYPE )( 1 : 6 ),  &
                   FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
         IF ( IAD0 .EQ. 1 ) THEN
            WRITE( IOUTGD, 3064 ) &
                      FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                      ANAMES( ITYPE )( 1 : 6 )
         ELSE
            WRITE( IOUTGD, 3065 ) &
                      FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                      ANAMES( ITYPE )( 1 : 6 )
         END IF

! --------- SET GROUP PARAMETERS.

         K1 = IGPA( ITYPE )
         K2 = IGPA( ITYPE + 1 ) - 1
         DO 435 K = K1, K2
            IVAR = K - K1 + 1
            IF ( OUTGF ) &
            WRITE( IOUTGF, 3063 ) GPNAME( K ), FIELDI( 11 )( 1 : 6 ),  &
                FIELDI( 13 )( 1 : 6 ), IVAR
!           IF ( IAD0 .EQ. 2 ) WRITE( IOUTGD, 3015 ) AD0, GPNAME( K )
            WRITE( IOUTGD, 3063 ) GPNAME( K ), FIELDI( 11 )( 1 : 6 ),  &
                FIELDI( 13 )( 1 : 6 ), IVAR
  435    CONTINUE
         IF ( LDEFND( ITYPE ) ) THEN
            INFORM = 64
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2640 )
            GO TO 800
         ELSE
            LDEFND( ITYPE ) = .TRUE.
         END IF

!  INITIALIZE LOGICALS WHICH DETERMINE WHETHER THE DATA HAS BEEN
!  INPUT IN THE CORRECT ORDER.

         STARTP = .FALSE.
         SETF = .FALSE.
         ENDF = .TRUE.
         STARTV = .FALSE.
      ELSE
         IF ( FIELD1( 1: 1 ) .EQ. 'A' .OR. FIELD1( 1: 1 ) &
              .EQ. 'I' .OR. FIELD1( 1: 1 ) .EQ. 'E' ) THEN

!  Finish off the function assignment

            IF ( SETF ) THEN
               IF ( .NOT. ENDF ) THEN
                  IF ( IAD0 .EQ. 1 ) THEN
                     WRITE( IOUTGD, 3122 ) FIELDI( 8 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  ELSE
                     WRITE( IOUTGD, 3123 ) FIELDI( 8 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  END IF
                  ENDF = .TRUE.
               END IF
            END IF

!  START A PARAMETER ASSIGNMENT. CHECK TO SEE THAT THE PARAMETER HAS
!  BEEN DEFINED.

            IF ( .NOT. SETF ) THEN
               IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                  STARTP = .TRUE.

!  Include the global parameters

                  IF ( .NOT. STARTV ) THEN
                     REWIND( IOUTEM )
                     DO 483 I = 1, NTEM
                        READ( IOUTEM, 1000 ) CTEM
                        WRITE( IOUTGD, 1000 ) CTEM
  483                CONTINUE   
                     STARTV = .TRUE.
                  END IF
                  FIELD = FIELD2 // '  PG'
                  CALL HASHC ( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD )
                  IF ( IFIELD .LE. 0 ) THEN
                     INFORM = 57
                     IF ( IOUT .GT. 0 ) WRITE( IOUT, 2570 )
                     GO TO 800
                  END IF

! --------- MAKE GROUP-SPECIFIC PARAMETER ASSIGNMENTS.

                  IF ( FIELD1( 1: 1 ) .EQ. 'A' ) THEN
                     IF ( .NOT. SETF ) THEN
                        IF ( OUTGF ) &
                        WRITE( IOUTGF, 3080 ) FIELD2( 1 : 6 ), FIELD7
                        WRITE( IOUTGD, 3080 ) FIELD2( 1 : 6 ), FIELD7
                     ELSE
                        IF ( OUTGF ) &
                        WRITE( IOUTGF, 3083 ) FIELD2( 1 : 6 ), FIELD7
                        WRITE( IOUTGD, 3083 ) FIELD2( 1 : 6 ), FIELD7
                     END IF

! --------- MAKE CONDITIONAL PARAMETER ASSIGNMENTS.

                  ELSE   

!  CHECK THAT THE LOGICAL VARIABLE HAS BEEN DEFINED.

                     FIELD = FIELD3 // '  PG'
                     CALL HASHC( LENGTH, 12, FIELD, KEY, ITABLE, IFIELD)
                     IF ( IFIELD .LE. 0 ) THEN
                        IF ( IFREE .EQ. 0 ) THEN
                           INFORM = - 1
                           GO TO 700
                        END IF
                        INFORM = 58
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2580 )
                        GO TO 800
                     END IF
                     IF ( FIELD1( 1: 1 ) .EQ. 'I' ) THEN
                        IF ( .NOT. SETF ) THEN
                           IF ( OUTGF ) &
                           WRITE( IOUTGF, 3081 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                           WRITE( IOUTGD, 3081 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                        ELSE
                           IF ( OUTGF ) &
                           WRITE( IOUTGF, 3084 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                           WRITE( IOUTGD, 3084 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                        END IF
                     ELSE
                        IF ( .NOT. SETF ) THEN
                           IF ( OUTGF ) &
                           WRITE( IOUTGF, 3082 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                           WRITE( IOUTGD, 3082 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                        ELSE
                           IF ( OUTGF ) &
                           WRITE( IOUTGF, 3085 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                           WRITE( IOUTGD, 3085 ) FIELD2( 1 : 6 ),  &
                                              FIELD3( 1 : 6 ), FIELD7
                        END IF
                     END IF   
                  END IF
               ELSE
                  IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                     IF ( STARTP ) THEN

! --------- CONTINUATION OF A PARAMETER ASSIGNMENT.

                        IF ( .NOT. SETF ) THEN
                           IF ( OUTGF ) &
                           WRITE( IOUTGF, 3090 ) FIELD7
                           WRITE( IOUTGD, 3090 ) FIELD7
                        ELSE
                           IF ( OUTGF ) &
                           WRITE( IOUTGF, 3091 ) FIELD7
                           WRITE( IOUTGD, 3091 ) FIELD7
                        END IF
                     ELSE
                        INFORM = 56
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            END IF
         ELSE
            STARTP = .FALSE.
            IF ( FIELD1( 1: 1 ) .EQ. 'F' ) THEN

!  SET THE FUNCTION VALUE.

               IF ( FIELD1( 2: 2 ) .EQ. ' ' ) THEN
                  SETF = .TRUE.
                  ENDF = .FALSE.

!  Include the global parameters

                  IF ( .NOT. STARTV ) THEN
                     REWIND( IOUTEM )
                     DO 484 I = 1, NTEM
                        READ( IOUTEM, 1000 ) CTEM
                        WRITE( IOUTGD, 1000 ) CTEM
  484                CONTINUE   
                     STARTV = .TRUE.
                  END IF

! --------- START G.

                  IF ( OUTGF ) &
                  WRITE( IOUTGF, 3100 ) FIELDI(  8 )( 1 : 6 ),  &
                  FIELDI( 2 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ), FIELD7
                  WRITE( IOUTGD, 3101 ) FIELD7
               ELSE
                  IF ( FIELD1( 2: 2 ) .EQ. '+' ) THEN
                     IF ( SETF ) THEN

! --------- CONTINUATION OF G.

                        IF ( OUTGF ) WRITE( IOUTGF, 3110 ) FIELD7
                        WRITE( IOUTGD, 3110 ) FIELD7
                     ELSE
                        INFORM = 56
                        IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            ELSE IF ( FIELD1( 1: 1 ) .EQ. 'G' .OR.&
                      FIELD1( 1: 1 ) .EQ. 'H' ) THEN
               IF ( SETF ) THEN
                  IF ( .NOT. ENDF ) THEN
                     IF ( IAD0 .EQ. 1 ) THEN
                        WRITE( IOUTGD, 3122 ) FIELDI( 8 )( 1 : 6 ),  &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                     ELSE
                        WRITE( IOUTGD, 3123 ) FIELDI( 8 )( 1 : 6 ),  &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                     END IF
                     ENDF = .TRUE.
                  END IF
               END IF
            ELSE
              INFORM = 56
              IF ( IOUT .GT. 0 ) WRITE( IOUT, 2560 )
              GO TO 800
            END IF
         END IF
      END IF
      GO TO 100

!  THE END OF THE INPUT FILE HAS BEEN REACHED BEFORE THE ENDATA CARD.

  590 CONTINUE 

!  IF THE ELEMENTS CARD HAS NOT BEEN ENCOUNTERED, EXIT.

      IF ( DEFNAM ) THEN
         INFORM = 52
         IF ( IOUT .GT. 0 ) WRITE( IOUT, 2520 )
         RETURN
      END IF
      IF ( NGRTYP .GT. 0 ) GO TO 930
      IF ( IOUT .GT. 0 .AND. IPRINT .NE. 0 ) WRITE( IOUT, 2010 )

!  A DUMMY ROUTINE WILL BE SUBSTITUTED.

  600 CONTINUE 

!  WRITE A DUMMY GROUPS ROUTINE.

      IF ( IAUTO .EQ. 1 ) THEN
         IF ( SINGLE ) THEN
            AORB = 'FORWARD_SINGLE '
         ELSE
            AORB = 'FORWARD_DOUBLE '
         END IF
      ELSE
         IF ( SINGLE ) THEN
            AORB = 'BACKWARD_SINGLE'
         ELSE
            AORB = 'BACKWARD_DOUBLE'
         END IF
      END IF
      IF ( IAD0 .EQ. 1 ) THEN
         AD0 = 'AD01'
      ELSE
         AD0 = 'AD02'
      END IF
      IF ( SINGLE ) THEN
         IF ( OUTGF ) &
         WRITE( IOUTGF, 3001 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                    FIELDI(  8 )( 1 : 6 ), &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                    PNAME
         WRITE( IOUTGD, 3005 ) FIELDI( 21 )( 1 : 6 ), &
                  ( FIELDI( I )( 1 : 6 ), I = 2, 4 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
         AD0, AORB, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                    FIELDI(  8 )( 1 : 6 ), &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                    PNAME
      ELSE
         IF ( OUTGF ) &
         WRITE( IOUTGF, 3000 ) ( FIELDI( I )( 1 : 6 ), I = 1, 4 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                    FIELDI(  8 )( 1 : 6 ), &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                    PNAME
         WRITE( IOUTGD, 3004 ) FIELDI( 21 )( 1 : 6 ), &
                  ( FIELDI( I )( 1 : 6 ), I = 2, 4 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 19 ),  &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),  &
         AD0, AORB, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),  &
                  ( FIELDI(  I )( 1 : 6 ), I = 15, 20 ),  &
                    FIELDI(  8 )( 1 : 6 ), &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),  &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),  &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),  &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),  &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),  &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),  &
                    PNAME
      END IF
      IF ( OUTGF ) WRITE( IOUTGF, 3009 ) FIELDI( 20 )( 1 : 6 )
      WRITE( IOUTGD, 3009 ) FIELDI( 20 )( 1 : 6 )
      IF ( OUTGF ) WRITE( IOUTGF, 3210 )
      WRITE( IOUTGD, 3210 )
      INFORM = 0
      RETURN

!  INSUFFICIENT SPACE TO CONTINUE CONSTRUCTION.

  700 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2000 ) INCRSE( - INFORM )
      RETURN

!  SUBROUTINE INCOMPLETE.

  800 CONTINUE
      IF ( IOUT .GT. 0 ) WRITE( IOUT, 2990 ) LINENO, NULINE
      RETURN

!  SUBROUTINE SUCCESSFULLY COMPLETED.

  900 CONTINUE
      IF ( .NOT. FIRSTG ) THEN

! ---------- WIND UP F AND G.

         IF ( SETF ) THEN
            IF ( .NOT. ENDF ) THEN
               IF ( IAD0 .EQ. 1 ) THEN
                  WRITE( IOUTGD, 3122 ) FIELDI( 8 )( 1 : 6 ),  &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
               ELSE
                  WRITE( IOUTGD, 3123 ) FIELDI( 8 )( 1 : 6 ),  &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),  &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
               END IF
               ENDF = .TRUE.
            END IF
            WRITE( IOUTGD, 3190 )
         ELSE
            INFORM = 61
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2610 )
            GO TO 800
         END IF
         IF ( ITYPE .LT. NGRTYP .AND. OUTGF ) &
            WRITE( IOUTGF, 3191 ) NLOOP
         IF ( ITYPE .LT. NGRTYP ) WRITE( IOUTGD, 3191 ) NLOOP
      END IF

! ---------- END DO LOOP.

      IF ( OUTGF ) WRITE( IOUTGF, 3200 ) NLOOP
      WRITE( IOUTGD, 3200 ) NLOOP
      IF ( IAD0 .EQ. 2 ) WRITE( IOUTGD, 3192 )
  910 CONTINUE

! ---------- SUCCESSFUL RUN. WIND UP OUTPUT.

      IF ( OUTGF ) WRITE( IOUTGF, 3210 )
      WRITE( IOUTGD, 3210 )
      INFORM = 0

!   CHECK THAT ALL ELEMENT TYPES HAVE BEEN DEFINED.

  930 CONTINUE
      DO 940 ITYPE = 1, NGRTYP
         IF ( .NOT. LDEFND( ITYPE ) ) THEN
            INFORM = 53
            IF ( IOUT .GT. 0 ) WRITE( IOUT, 2530 ) GTYPES( ITYPE )
         END IF
  940 CONTINUE
      RETURN

!  NON-EXECUTABLE STATEMENTS.

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKEGR - insufficient space.',  &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAKEGR - warning.',  &
              ' First card not groups. ', /, '    A dummy',  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKEGR - indicator card not recognised ' )
 2190 FORMAT( ' ** Exit from MAKEGR - group type not recognised:',  &
              ' name is ', A10 )
 2510 FORMAT( ' ** Exit from MAKEGR -',  &
              ' Name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKEGR -',  &
              ' data file incomplete. No ENDATA card ' )
 2530 FORMAT( ' ** Exit from MAKEGR - warning, group type ', A8,  &
              ' undefined ' )
 2540 FORMAT( ' ** Exit from MAKEGR -',  &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKEGR -',  &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKEGR -',  &
              ' unrecognised field 1 in INDIVIDUAL section' )
 2570 FORMAT( ' ** Exit from MAKEGR -',  &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKEGR -',  &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKEGR - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKEGR - function not set '  )
 2620 FORMAT( ' ** Exit from MAKEGR -',  &
              ' one or more first derivatives not set ' )
 2630 FORMAT( ' ** Exit from MAKEGR -',  &
              ' one or more second derivatives not set ' )
 2640 FORMAT( ' ** Exit from MAKEGR - group type already defined ' )
 2730 FORMAT( ' ** Exit from MAKEGR - field 2 or 3 not blank on',  &
              ' A, F, G or H card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', I5, 4X, A160 )
 2980 FORMAT( ' Line ', I5, '.', I2, 1X, A65 )
 2990 FORMAT( ' Line ', I5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      LOGICAL ', A6, /,  &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6, &
                             '), ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',  &
                                         A6, '(', A6, '), ', A6, &
                                      '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      LOGICAL ', A6, /,  &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6, &
                             '), ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, ',3), ',  &
                                         A6, '(', A6, '), ', A6, &
                                      '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C' )
 3002 FORMAT( '      INTEGER ', A6, ', ', A6, ', ', A6, ', ', A6 )
 3004 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,  &
              '      USE HSL_', A4, '_', A15, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      LOGICAL ', A6, /,  &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6, &
                             '), ', A6, '(', A6, ')', /,  &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',  &
                                         A6, '(', A6, '), ', A6, &
                                      '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              '      INTEGER, POINTER :: H_index( : ) ', /, &
              '      DOUBLE PRECISION, POINTER :: H_result( : ) ', /, &
              '      DOUBLE PRECISION :: A_int( 1 ) ' )
 3005 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,  &
              '     *                   ', 5( A6, ', ' ), /,  &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,  &
              '      USE HSL_', A4, '_', A15, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      INTEGER ', 3( A6, ', ' ), A6, /,  &
              '      LOGICAL ', A6, /,  &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6, &
                             '), ', A6, '(', A6, ')', /,  &
              '      REAL             ', A6, '(', A6, ',3), ',  &
                                         A6, '(', A6, '), ', A6, &
                                      '(', A6, ')', /,  &
              'C', /, 'C  PROBLEM NAME : ', A8, /, 'C', /,  &
              '      INTEGER, POINTER :: H_index( : ) ', /, &
              '      REAL, POINTER :: H_result( : ) ', /, &
              '      REAL :: A_int( 1 ) ' )
 3006 FORMAT( '      TYPE (AD01_REAL) :: G_value = AD01_UNDEFINED', /,  &
              '      TYPE (AD01_REAL) :: A_value( 1 )' )
 3007 FORMAT( '      INTEGER :: ERROR_AD02', /, &
              '      TYPE (AD02_REAL) :: G_value', /,  &
              '      TYPE (AD02_REAL) :: A_value( 1 )', /,  &
              '      TYPE (AD02_DATA), POINTER :: DATA_AD02' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, 4( :, ', ', A6 ) ) )
!3011 FORMAT( '      NULLIFY( DATA_AD02 )', /,
!    *        '      CALL AD02_INITIALIZE(2, G_value,', /,
!    *        '     *                     GVALUE(1,1),', /,
!    *        '     *                     DATA_AD02, 0)' )
 3011 FORMAT( '      CALL AD02_INITIALIZE_DATA(DATA_AD02, ERROR_AD02)' )
!3015 FORMAT( '       CALL ', A4, '_UNDEFINE( ', A6,
!    *        ', DATA_AD02 )' )
 3016 FORMAT( '      CALL ', A4, '_UNDEFINE( ', A6,  &
              ', DATA_AD02 )' )
 3017 FORMAT( ( '      TYPE (', A4, '_REAL) :: ', A6 ) )
 3018 FORMAT( ( '      TYPE (AD01_REAL) :: ', A6, &
                ' = AD01_UNDEFINED' ) )
 3019 FORMAT( ( '      REAL             ', A6, 4( :, ', ', A6 ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, 4( :, ', ', A6 ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, 4( :, ', ', A6 ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, 4( :, ', ', A6 ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', /, &
              '     *   ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', I5, 1X, A6, ' = 1, ', A6, /,  &
              '       ', A6, ' = ', A6, '(', A6, ')', /,  &
              '       ', A6, ' = ', A6, '(', A6, ')', /,  &
              '       IF ( ', A6, ' .EQ. 0 ) GO TO ', I5, /,  &
              '       ', A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       GO TO (', 8( I5, :, ',' ), /,  &
            ( '     *        ', 8( I5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3060 FORMAT( 'C', /, 'C  GROUP TYPE : ', A8, /, 'C' )
 3061 FORMAT( I5, '  CONTINUE ' )
 3062 FORMAT( '       ', A6, '= ', A6, '(', A6, ')' )
 3063 FORMAT( '       ', A6, '= ', A6, '(', A6, '+', I6, ')' )
 3064 FORMAT( '       A_int( 1 ) = ', A6, '(', A6, ')', /,  &
              '       CALL AD01_INITIALIZE(2, A_value( : 1 ),', &
                      ' A_int( : 1 ), 0) ', /, &
              '       ', A6, ' = A_value( 1 ) ' ) 
 3065 FORMAT( '       A_int( 1 ) = ', A6, '(', A6, ')', /,  &
              '       CALL AD02_INITIALIZE_COMP(2, A_value( : 1 ),', &
                      ' A_int( : 1 ),', /,  &
              '     *                      DATA_AD02, ERROR_AD02, 0)', &
           /, '       ', A6, ' = A_value( 1 ) ' ) 
 3080 FORMAT( '       ', A6, '= ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( .NOT. ', A6, ' ) ',A6,'(', A6, ',1)= ', A41 )
 3101 FORMAT( '       G_value = ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3121 FORMAT( '        WRITE(6,*) '' impossible value IFLAG = '', ', &
              'IFFLAG, '' in GROUPF '' ' )
 3122 FORMAT( '       IF ( ', A6, ' ) THEN', /,  &
              '        CALL AD01_HESSIAN(G_value, ', &
              A6, '(', A6, ',3), ', A6, '(', A6, ',2))', /,  &
              '       ELSE',/,  &
              '        CALL AD01_VALUE(G_value, ',A6,'(', A6, ',1))' )
 3123 FORMAT( '       IF ( ', A6, ' ) THEN', /,  &
              '        CALL AD02_HESSIAN(G_value, ', &
              A6, '(', A6, ',3), ERROR_AD02,', /, &
              '     *                    ', A6, '(', A6, ',2))', /,  &
              '       ELSE',/,  &
              '        CALL AD02_VALUE(G_value, ',A6,'(', A6, ',1),',  &
              ' ERROR_AD02)' )
 3130 FORMAT( '        ', A6, '(', A6, ',', I1, ')= ', A41 )
 3140 FORMAT( '     *                         ', A41 )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', I6 )
 3192 FORMAT( '      CALL AD02_FINALIZE_DATA(DATA_AD02, ERROR_AD02)' )
 3200 FORMAT( I5,  ' CONTINUE' )
 3210 FORMAT( '      RETURN', /,  '      END' )

!  END OF MAGRAD.

      END

!  THIS VERSION: 11/08/1995 AT 14:44:21 AM.
!  ** VERSION B **

!     INTEGER IOUT, INPUT, OUTPUT, TEMPRY, IAUTO, LNAMES, LDUMMY
!     PARAMETER ( LNAMES = 10000, LDUMMY = LNAMES )
!     CHARACTER * 10 RNAMES( LNAMES ), ADUMMY( LDUMMY )
!     LOGICAL SINGLE
!     IOUT = 21
!     INPUT = 5
!     OUTPUT = 6
!     TEMPRY = 20
!     SINGLE = .FALSE.
!     IAUTO = 1
!     IAD0 = 2
!     CALL TRANS( IOUT, INPUT, OUTPUT, TEMPRY, SINGLE, IAUTO,
!    *            IAD0, RNAMES, LNAMES, ADUMMY, LDUMMY )
!     STOP
!     END

      SUBROUTINE TRANS( IOUT, INPUT, OUTPUT, TEMPRY, SINGLE, IAUTO,  &
                        IAD0, RNAMES, LNAMES, ADUMMY, LDUMMY )
      INTEGER IOUT, INPUT, OUTPUT, TEMPRY, IAUTO, IAD0, LNAMES, LDUMMY
      CHARACTER * 10 RNAMES( LNAMES ), ADUMMY( LDUMMY )
      LOGICAL SINGLE

!  Translate a fortran 77 program so that it is capable of
!  accepting AD01 or AD02 reals instead of ordinary reals

!  Nick Gould
!  August 11th, 1995

      INTEGER I, I1, I2, II, J, JJ, NREAL, NRE, LRE, MRE, NDUMMY
      LOGICAL INTRI, SUB, FUN, NOFIEL, EBRACK
      LOGICAL FREAL, ENDLIN, STARTR
      LOGICAL NUMBA, CHARA
      CHARACTER * 1 CARD
      CHARACTER * 4 AD0
      CHARACTER * 6 LSP, USP
      CHARACTER * 10 LF, UF, BLANK, FIELD
      CHARACTER * 11 LI, UI
      CHARACTER * 13 LS, US
      CHARACTER * 15 UN, ADTYPE
      CHARACTER * 72 NULINE, OLDLIN, BL72
      CHARACTER * 72 RELINE( 20 )
      CHARACTER * 18 LDP, UDP
      PARAMETER ( USP = ' REAL ', UDP = ' DOUBLE PRECISION ' )
      PARAMETER ( UF = ' FUNCTION ', US = ' SUBROUTINE ' )
      PARAMETER ( UI = ' INTRINSIC ' )
      PARAMETER ( LSP = ' real ', LDP = ' double precision ' ) 
      PARAMETER ( LF = ' function ', LS = ' subroutine ' )
      PARAMETER ( LI = ' intrinsic ' )
      PARAMETER ( UN = '=AD01_UNDEFINED' )

!  Create a blank name

      DO 5 I = 1, 10
         BLANK( I : I ) = ' '
    5 CONTINUE
      DO 6 I = 1, 72
         BL72( I : I ) = ' '
    6 CONTINUE

!  Determine the type of automatic derivative to be used

      IF ( IAUTO .EQ. 1 ) THEN
         IF ( SINGLE ) THEN
            ADTYPE = 'FORWARD_SINGLE '
         ELSE
            ADTYPE = 'FORWARD_DOUBLE '
         END IF
      ELSE
         IF ( SINGLE ) THEN
            ADTYPE = 'BACKWARD_SINGLE'
         ELSE
            ADTYPE = 'BACKWARD_DOUBLE'
         END IF
      END IF

!  Determine the AD routine to be used

      IF ( IAD0 .EQ. 1 ) THEN
         AD0 = 'AD01'
      ELSE
         AD0 = 'AD02'
      END IF

!  Initialize logicals

      INTRI = .FALSE.
      SUB = .FALSE.
      FUN = .FALSE.
      STARTR = .FALSE.
      REWIND( INPUT )
      REWIND( TEMPRY )
   10 CONTINUE

!  Read a new line

      READ ( INPUT, 1000, END = 600, ERR = 600 ) NULINE
      IF ( SUB .OR. FUN ) THEN

!  Ignore comments

         IF ( NULINE( 1: 1 ) .EQ. 'C' .OR. &
              NULINE( 1: 1 ) .EQ. 'c' ) GO TO 400

!  Is the line a continuation?

         IF ( NULINE( 6: 6 ) .NE. ' ' ) THEN
            IF ( CARD .EQ. 'I' ) GO TO 10
            II = 7
            IF ( CARD .EQ. 'B' .OR. CARD .EQ. 'E' ) &
               CALL GETDUM( NULINE, II, ADUMMY, LDUMMY, NDUMMY, BLANK )

!  Find what kind of line this is. Find its first nonzero character
!  Find the start of the name

         ELSE
            DO 20 I = 7, 72
               IF ( NULINE( I : I ) .NE. ' ' ) GO TO 30
   20       CONTINUE
            GO TO 10
   30       CONTINUE
            CARD = ' '
            IF ( NULINE( I : I + 6 ) .EQ. 'INTEGER' ) GO TO 400
            IF ( NULINE( I : I + 6 ) .EQ. 'COMPLEX' ) GO TO 400
            IF ( NULINE( I : I + 6 ) .EQ. 'LOGICAL' ) GO TO 400
            IF ( NULINE( I : I + 8 ) .EQ. 'CHARACTER' ) GO TO 400
            IF ( NULINE( I : I + 8 ) .EQ. 'DIMENSION' ) GO TO 400
            IF ( NULINE( I : I + 7 ) .EQ. 'IMPLICIT' ) GO TO 400
            IF ( NULINE( I : I + 10 ) .EQ. 'EQUIVALENCE' ) GO TO 400
            IF ( NULINE( I : I + 7 ) .EQ. 'EXTERNAL' ) THEN
                 CARD = 'E'
                 CALL GETDUM( NULINE, I + 8, ADUMMY, LDUMMY, &
                              NDUMMY, BLANK )
                 GO TO 400
            END IF
            IF ( SINGLE ) THEN
               IF ( NULINE( I : I + 15 ) .EQ. &
                       'DOUBLE PRECISION' ) GO TO 400
               IF ( NULINE( I : I + 3 ) .EQ. 'REAL' ) THEN
                  CARD = 'R'
                  II = I + 4
                  GO TO 200
               END IF
            ELSE
               IF ( NULINE( I : I + 3 ) .EQ. 'REAL' ) GO TO 400
               IF ( NULINE( I : I + 15 ) .EQ. &
                       'DOUBLE PRECISION' ) THEN
                  CARD = 'R'
                  II = I + 16
                  GO TO 200
               END IF
            END IF
            IF ( NULINE( I : I + 3 ) .EQ. 'SAVE' ) THEN
               WRITE( IOUT, 2100 ) AD0
               CARD = 'S'
               II = I + 4
               GO TO 200
            END IF
            IF ( NULINE( I : I + 8 ) .EQ. 'INTRINSIC' ) THEN
               CARD = 'I'
               GO TO 10
            END IF
            IF ( NULINE( I : I + 5 ) .EQ. 'COMMON' ) THEN
               WRITE( IOUT, 2110 ) AD0
               CARD = 'C'
               II = I + 6
               GO TO 200
            END IF
            IF ( NULINE( I : I + 8 ) .EQ. 'PARAMETER' ) THEN
               CARD = 'P'
               II = I + 9
               GO TO 200
            END IF
            IF ( NULINE( I : I + 3 ) .EQ. 'DATA' ) THEN
               CARD = 'D'
               II = I + 4
               GO TO 200
            END IF

!  The body of the procedure has been found. Complete the
!  introduction
! 
            REWIND( TEMPRY )
            CARD = 'B'
   60       CONTINUE
            READ ( TEMPRY, 1000, END = 190, ERR = 190 ) OLDLIN

!  Write out comments

            IF ( OLDLIN( 1: 1 ) .EQ. 'C' .OR. &
                 OLDLIN( 1: 1 ) .EQ. 'c' ) GO TO 180

!  Search for cards defining appropriate real values

            IF ( OLDLIN( 6: 6 ) .NE. ' ' ) THEN
               IF ( CARD .NE. 'R' ) GO TO 180
               II = 7
            ELSE
            IF ( CARD .EQ. 'B' ) THEN
               CARD = 'F'
               GO TO 180
            END IF
            IF ( CARD .EQ. 'F' ) WRITE( OUTPUT, 2020 ) AD0, ADTYPE

!  Write out the previous set of real values
!       
               IF ( STARTR ) THEN
                  IF ( NRE .GT. 0 ) THEN
                     DO 62 I = 1, MRE - 1 
                        CALL OUTLIN( RELINE( I ), 72, OUTPUT )
   62                CONTINUE   
                     CALL OUTLIN( RELINE( MRE ), LRE, OUTPUT )
                  END IF
                  STARTR = .FALSE.
               END IF
               DO 70 I = 7, 72
                  IF ( OLDLIN( I : I ) .NE. ' ' ) GO TO 80
   70          CONTINUE
               GO TO 60
   80          CONTINUE
               CARD = ' '
               IF ( OLDLIN( I : I + 3 ) .EQ. 'SAVE' ) GO TO 180
               IF ( OLDLIN( I : I + 3 ) .EQ. 'DATA' ) GO TO 180
               IF ( OLDLIN( I : I + 5 ) .EQ. 'COMMON' ) GO TO 180
               IF ( OLDLIN( I : I + 6 ) .EQ. 'INTEGER' ) GO TO 180
               IF ( OLDLIN( I : I + 6 ) .EQ. 'COMPLEX' ) GO TO 180
               IF ( OLDLIN( I : I + 6 ) .EQ. 'LOGICAL' ) GO TO 180
               IF ( OLDLIN( I : I + 7 ) .EQ. 'EXTERNAL' ) GO TO 180
               IF ( OLDLIN( I : I + 7 ) .EQ. 'IMPLICIT' ) GO TO 180
               IF ( OLDLIN( I : I + 8 ) .EQ. 'CHARACTER' ) GO TO 180
               IF ( OLDLIN( I : I + 8 ) .EQ. 'DIMENSION' ) GO TO 180
               IF ( OLDLIN( I : I + 8 ) .EQ. 'PARAMETER' ) GO TO 180
               IF ( OLDLIN( I : I + 10 ) .EQ. 'EQUIVALENCE' ) GO TO 180
               IF ( SINGLE ) THEN
                  IF ( OLDLIN( I : I + 15 ) .EQ. &
                       'DOUBLE PRECISION' ) GO TO 180
                  II = I + 4
                  RELINE( 1 ) = BL72
                  RELINE( 1 )( 1 : 11 ) = '      REAL '
                  LRE = 11
                  MRE = 1
               ELSE
                  IF ( OLDLIN( I : I + 3 ) .EQ. 'REAL' ) GO TO 180
                  II = I + 16
                  RELINE( 1 ) = BL72
                  RELINE( 1 )( 1 : 23 ) = '      DOUBLE PRECISION '
                  MRE = 1
                  LRE = 23
               END IF
               NRE = 0
               CARD = 'R'
               STARTR = .TRUE.
            END IF
  110       CONTINUE
            CALL GETFLD( II, I1, I2, ENDLIN, OLDLIN, BLANK, &
                         FIELD, NOFIEL, EBRACK, .FALSE. )
            IF ( NOFIEL ) GO TO 60
            J = II - I1
            DO 120 I = 1, NREAL
               IF ( FIELD .EQ. RNAMES( I ) ) THEN

!  The parameter will be of type AD01_REAL or AD02_REAL

                  DO 115 J = 1, NDUMMY
                     IF ( FIELD .EQ. ADUMMY( J ) ) THEN
                        WRITE( OUTPUT, 2060 ) AD0,  &
                           ( OLDLIN( JJ : JJ ), JJ = I1, II - 1 )
                        GO TO 130
                     END IF
  115             CONTINUE   
                  IF ( IAD0 .EQ. 1 ) THEN
                     WRITE( OUTPUT, 2060 ) AD0,  &
                        ( OLDLIN( JJ : JJ ), JJ = I1, II - 1 ),  &
                        ( UN( JJ : JJ ), JJ = 1, 15 )
                  ELSE
                     WRITE( OUTPUT, 2060 ) AD0,  &
                        ( OLDLIN( JJ : JJ ), JJ = I1, II - 1 )
                  END IF
                  GO TO 130
               END IF
  120       CONTINUE   

!  The parameter will be of type REAL

            IF ( NRE .GT. 0 ) THEN
               IF ( LRE + 1 .GT. 72 ) THEN
                  MRE = MRE + 1
                  RELINE( MRE ) = BL72
                  RELINE( MRE )( 1 : 8 ) = '     * ,'
                  LRE = 8
               ELSE
                  RELINE( MRE )( LRE + 1 : LRE + 1 ) = ','
                  LRE = LRE + 1
               END IF
            END IF                   
            IF ( LRE + J + 1 .GT. 72 ) THEN
               MRE = MRE + 1
               RELINE( MRE ) = BL72
               RELINE( MRE )( 1 : 7 ) = '     * '
               LRE = 7
            END IF
            RELINE( MRE )( LRE + 1 : LRE + J ) = OLDLIN( I1 : II - 1 )
            LRE = LRE + J
            NRE = NRE + 1
  130       CONTINUE   
            IF ( ENDLIN ) GO TO 60

!  Find the next parameter

            GO TO 110
  180       CONTINUE

!  Output the current line

            CALL OUTLIN( OLDLIN, 72, OUTPUT )
            GO TO 60
  190       CONTINUE

!  Write out any unfinished set of real values
!       
            IF ( STARTR ) THEN
               IF ( NRE .GT. 0 ) THEN
                  DO 192 I = 1, MRE - 1
                     CALL OUTLIN( RELINE( I ), 72, OUTPUT )
  192             CONTINUE   
                  CALL OUTLIN( RELINE( MRE ), LRE, OUTPUT )
               END IF
               STARTR = .FALSE.
            END IF

!  The introduction is complete

            IF ( SUB ) THEN
               SUB = .FALSE.
            ELSE
               FUN = .FALSE.
               IF ( FREAL .AND. IAD0 .EQ. 1 ) &
                  WRITE( OUTPUT, 2030 ) RNAMES( 1 )( 1 : 6 )
               IF ( FREAL .AND. IAD0 .EQ. 2 ) &
                  WRITE( OUTPUT, 2040 ) AD0, RNAMES( 1 )( 1 : 6 )
            END IF
            REWIND( TEMPRY )
            GO TO 410
         END IF

!  Find all variables mentioned on the current string

  200    CONTINUE

!  Add the variable to the list

         IF ( CARD .EQ. 'R' ) THEN
  210       CONTINUE
            CALL GETFLD( II, I1, I2, ENDLIN, NULINE, BLANK, &
                         FIELD, NOFIEL, EBRACK, .FALSE. )
            IF ( NOFIEL ) GO TO 400
            DO 220 I = 1, NREAL
               IF ( FIELD .EQ. RNAMES( I ) ) GO TO 230
  220       CONTINUE   
            NREAL = NREAL + 1
            RNAMES( NREAL ) = FIELD
  230       CONTINUE   
            IF ( ENDLIN ) GO TO 400
            GO TO 210
         END IF

!  Remove the variable from the list

         IF ( CARD .EQ. 'C' .OR. CARD .EQ. 'D' .OR. &
              CARD .EQ. 'P' .OR. CARD .EQ. 'S' ) THEN
  310       CONTINUE
            CALL GETFLD( II, I1, I2, ENDLIN, NULINE, BLANK, &
                         FIELD, NOFIEL, EBRACK, .FALSE. )
            IF ( NOFIEL ) GO TO 400
            DO 320 I = 1, NREAL
               IF ( FIELD .EQ. RNAMES( I ) ) THEN
                  RNAMES( I ) = RNAMES( NREAL )
                  NREAL = NREAL - 1
                  GO TO 330
               END IF
  320       CONTINUE   
  330       CONTINUE   
            IF ( ENDLIN ) GO TO 400

!  For parameter statements, skip the segments after the "="

            IF ( CARD .EQ. 'P' ) THEN
               DO 340 I = II, 72
                  IF ( NULINE( I : I ) .EQ. ',' .OR.&
                       NULINE( I : I ) .EQ. ')' ) THEN
                     II = I + 1
                     GO TO 310
                  END IF
  340          CONTINUE
            END IF
            GO TO 310
         END IF
  400    CONTINUE
         WRITE( TEMPRY, 2000 ) NULINE
         GO TO 10
      END IF
  410 CONTINUE
      IF ( .NOT. ( SUB .OR. FUN ) ) THEN

!  Ignore comments

         IF ( NULINE( 1: 1 ) .EQ. 'C' .OR. &
              NULINE( 1: 1 ) .EQ. 'c' ) GO TO 500

!  Remove lines mentioning intrinsic functions

         IF ( INTRI ) THEN
            IF ( NULINE( 6: 6 ) .NE. ' ' ) GO TO 10
            INTRI = .FALSE.
         END IF
         DO 420 I = 1, 62
            IF ( NULINE( I : I + 10 ) .EQ. UI .OR.&
                 NULINE( I : I + 10 ) .EQ. LI ) THEN
              INTRI = .TRUE.
              GO TO 10
            END IF         
  420    CONTINUE

!  Hunt for the start of a SUBROUTINE

         CARD = ' '
         DO 430 I = 1, 60
            IF ( NULINE( I : I + 11 ) .EQ. US .OR.&
                 NULINE( I : I + 11 ) .EQ. LS ) THEN
               II = I + 12
               SUB = .TRUE.
               CARD = 'B'
               NREAL = 0
               NDUMMY = 0
               CALL GETDUM( NULINE, II, ADUMMY, LDUMMY, NDUMMY, BLANK )
               WRITE( TEMPRY, 2000 ) NULINE
               GO TO 10
            END IF         
  430    CONTINUE   

!  Hunt for the start of a FUNCTION

         DO 440 I = 1, 63
            IF ( NULINE( I : I + 9 ) .EQ. UF .OR.&
                 NULINE( I : I + 9 ) .EQ. LF ) THEN
               II = I + 10
               FUN = .TRUE.
               CARD = 'B'

!  Find what kind of function this is

               FREAL = .FALSE.
               IF ( SINGLE ) THEN

!  Hunt for the string ' REAL '

                  DO 431 J = 1, I
                     IF ( NULINE( J : J + 5 ) .EQ. USP .OR.&
                          NULINE( J : J + 5 ) .EQ. LSP ) THEN
                        FREAL = .TRUE.
                        NULINE( J : J + 5 ) = '      '
                        NULINE( 6 : 6 ) = '*'
                        WRITE( TEMPRY, 2010 ) AD0
                        GO TO 433
                     END IF
  431             CONTINUE   

!  Hunt for the string ' DOUBLE PRECISION '

               ELSE
                  DO 432 J = 1, I
                     IF ( NULINE( J : J + 17 ) .EQ. UDP .OR.&
                          NULINE( J : J + 17 ) .EQ. LDP ) THEN
                        IF ( IAD0 .EQ. 1 ) THEN
                           NULINE( J : J + 17 ) = &
                              ' TYPE (' // AD0 // '_REAL) '
                        ELSE
                           NULINE( J : J + 17 ) = &
                              '                           '
                          AD0 = 'AD02'
                        END IF
                        FREAL = .TRUE.
                        GO TO 433
                     END IF
  432             CONTINUE
               END IF
               WRITE( TEMPRY, 2000 ) NULINE
               NDUMMY = 0
               CALL GETDUM( NULINE, II, ADUMMY, LDUMMY, NDUMMY, BLANK )
               GO TO 10

!  The function will be of type AD01_REAL or AD02_REAL. Find its name

  433          CONTINUE
               WRITE( TEMPRY, 2000 ) NULINE

!  Find the start of the name

               DO 434 J = II, 72
                  IF ( NULINE( J : J ) .NE. ' ' ) GO TO 435
  434          CONTINUE

!  No name has been found so far. Read the next card
!   
               READ ( INPUT, 1000, END = 600, ERR = 600 ) NULINE
               II = 7
               GO TO 433

!  Find the end of the name

  435          CONTINUE   
               DO 436 JJ = J + 1, MIN( 72, J + 5 ) 
                  IF ( .NOT. ( CHARA( NULINE( JJ : JJ ) ) .OR.&
                               NUMBA( NULINE( JJ : JJ ) ) ) ) GO TO 437
  436          CONTINUE
               JJ = MIN( 72, J + 6 )
  437          CONTINUE
               NREAL = 1
               RNAMES( NREAL ) = BLANK
               RNAMES( NREAL )( 1 : JJ - J ) = NULINE( J : JJ - 1 )
               NDUMMY = 0
               CALL GETDUM( NULINE, JJ, ADUMMY, LDUMMY, NDUMMY, BLANK )
               GO TO 10
            END IF         
  440    CONTINUE

!  Hunt for the start of a subroutine

  500    CONTINUE
         CALL OUTLIN( NULINE, 72, OUTPUT )
         GO TO 10
      END IF
  600 CONTINUE
      RETURN

!  Non-executable statements

 1000 FORMAT( A72 )
 2000 FORMAT( A72 )
 2010 FORMAT( '      TYPE ( ', A4, '_REAL )' )
 2020 FORMAT( '      USE HSL_', A4, '_', A15 )
!2030 FORMAT( '      CALL AD01_UNDEFINE(', A6, ')' )
 2030 FORMAT( '      ', A6, ' = AD01_UNDEFINED' )
 2040 FORMAT( '      TYPE (', A4, '_REAL) :: ', A6 )
 2060 FORMAT( '      TYPE (', A4, '_REAL) :: ', 46( A1, : ), /,  &
              ( '     *', 66( A1, : ) ) )
 2100 FORMAT( ' ** Warning: a user-supplied external procedure', &
              ' SAVEs parameters.', /,  &
              '    This is not allowed by the automatic',  &
              ' differentiation package ', A4, '.' )
 2110 FORMAT( ' ** Warning: a user-supplied external procedure', &
              ' saves parameters via common.', /,  &
              '    This is not allowed by the automatic',  &
              ' differentiation package ', A4, '.' )

!  End of TRANS

      END 

      LOGICAL FUNCTION CHARA( C )
      CHARACTER * 1 C
      INTEGER I
      CHARACTER * 1 LCHARS( 26 )
      CHARACTER * 1 UCHARS( 26 )
      DATA LCHARS / 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', &
                    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', &
                    'u', 'v', 'w', 'x', 'y', 'z' /
      DATA UCHARS / 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', &
                    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', &
                    'U', 'V', 'W', 'X', 'Y', 'Z' /
      DO 10 I = 1, 26
         IF ( C .EQ. LCHARS( I ) .OR. C .EQ. UCHARS( I ) ) THEN
            CHARA = .TRUE.
            RETURN
         END IF
   10 CONTINUE
      CHARA = .FALSE.
      RETURN
      END

      LOGICAL FUNCTION NUMBA( C )
      CHARACTER * 1 C
      INTEGER I
      CHARACTER * 1 CHARS( 10 )
      DATA CHARS / '0', '1', '2', '3', '4', '5', &
                   '6', '7', '8', '9'/
      DO 10 I = 1, 10
         IF ( C .EQ. CHARS( I ) ) THEN
            NUMBA = .TRUE.
            RETURN
         END IF
   10 CONTINUE
      NUMBA = .FALSE.
      RETURN
      END

      SUBROUTINE UPPER( C, N )
      INTEGER N
      CHARACTER * ( * ) C
      INTEGER I, J
      CHARACTER * 1 LCHARS( 26 )
      CHARACTER * 1 UCHARS( 26 )
      DATA LCHARS / 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', &
                    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', &
                    'u', 'v', 'w', 'x', 'y', 'z' /
      DATA UCHARS / 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', &
                    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', &
                    'U', 'V', 'W', 'X', 'Y', 'Z' /
      DO 20 J = 1, N
         DO 10 I = 1, 26
            IF ( C( J : J ) .EQ. LCHARS( I ) ) THEN
               C( J : J ) = UCHARS( I )
               GO TO 20
            END IF
   10    CONTINUE
   20 CONTINUE   
      RETURN
      END

      SUBROUTINE LOWER( C, N )
      INTEGER N
      CHARACTER * ( * ) C
      INTEGER I, J
      CHARACTER * 1 LCHARS( 26 )
      CHARACTER * 1 UCHARS( 26 )
      DATA LCHARS / 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', &
                    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', &
                    'u', 'v', 'w', 'x', 'y', 'z' /
      DATA UCHARS / 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', &
                    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', &
                    'U', 'V', 'W', 'X', 'Y', 'Z' /
      DO 20 J = 1, N
         DO 10 I = 1, 26
            IF ( C( J : J ) .EQ. UCHARS( I ) ) THEN
               C( J : J ) = LCHARS( I )
               GO TO 20
            END IF
   10    CONTINUE
   20 CONTINUE   
      RETURN
      END

      SUBROUTINE GETFLD( II, I1, I2, ENDLIN, NULINE, BLANK, &
                         FIELD, NOFIEL, EBRACK, IGNORB )
      INTEGER II, I1, I2, J
      LOGICAL ENDLIN, NOFIEL, EBRACK, IGNORB
      CHARACTER * 72 NULINE
      CHARACTER * 10 BLANK, FIELD

!  Find the first character string in NULINE beyond position II

      INTEGER I
      LOGICAL ARRAYS
      LOGICAL NUMBA, CHARA
      EBRACK = .TRUE.
      ENDLIN = .FALSE.
      DO 20 I = II, 72
         IF ( CHARA( NULINE( I : I ) ) ) THEN
            NOFIEL = .FALSE.
            I1 = I
            GO TO 30
         END IF
   20 CONTINUE   
      NOFIEL = .TRUE.
      RETURN

!  Next, find its last character

   30 CONTINUE   
      DO 40 I = I1 + 1, MIN( 72, I1 + 6 )
         IF ( .NOT. CHARA( NULINE( I : I ) ) .AND.&
              .NOT. NUMBA( NULINE( I : I ) ) ) THEN
            I2 = I - 1
            II = I
            IF ( IGNORB ) GO TO 70
            GO TO 50
         END IF
   40 CONTINUE   
      I2 = MIN( 72, I1 + 6 )
      ENDLIN = .TRUE.

!  Last, check to see if it is an array 

   50 CONTINUE
      ARRAYS = .FALSE.
      DO 60 I = I2 + 1, 72
         IF ( .NOT. ARRAYS ) THEN
            IF ( NULINE( I : I ) .NE. ' ' ) THEN
               IF ( NULINE( I : I ) .NE. '(' ) THEN
                  GO TO 70
               ELSE
                  ARRAYS = .TRUE.
               EBRACK = .FALSE.
               END IF
            END IF
         ELSE
            IF ( NULINE( I : I ) .EQ. ')' ) THEN
               EBRACK = .TRUE.
               II = I + 1
               GO TO 70
            END IF
         END IF
   60 CONTINUE   
   70 CONTINUE
!     WRITE( 6, * ) ' LINE, I1, I2, II ', LINE, I1, I2, II
      J = I2 - I1 + 1
      FIELD = BLANK
      FIELD( 1 : J ) = NULINE( I1 : I2 )
      CALL UPPER( FIELD, 10 )
      RETURN
      END


      SUBROUTINE GETDUM( NULINE, II, ADUMMY, LDUMMY, NDUMMY, BLANK )
      INTEGER II, NDUMMY, LDUMMY
      CHARACTER * 10 BLANK
      CHARACTER * 72 NULINE
      CHARACTER * 10 ADUMMY( LDUMMY )

!  Determine the list of variables beyond column II on the line NULINE

      INTEGER I, JJ, J1, J2
      CHARACTER * 10 FIELD
      LOGICAL NOFIEL, EBRACK, ENDLIN
      JJ = II
   10 CONTINUE
      CALL GETFLD( JJ, J1, J2, ENDLIN, NULINE, BLANK, &
                   FIELD, NOFIEL, EBRACK, .TRUE. )
      IF ( NOFIEL ) RETURN
      DO 20 I = 1, NDUMMY
         IF ( FIELD .EQ. ADUMMY( I ) ) GO TO 30
   20 CONTINUE   
      NDUMMY = NDUMMY + 1
      ADUMMY( NDUMMY ) = FIELD
   30 CONTINUE   
      IF ( ENDLIN ) RETURN
      GO TO 10
      END

      SUBROUTINE OUTLIN( NULINE, IEND, OUTPUT )
      INTEGER IEND, OUTPUT
      CHARACTER * 72 NULINE

!  Write out the current line, cutting off trailing blanks

      INTEGER I, I2
      DO 10 I2 = MIN( IEND, 72 ), 1, - 1
         IF ( NULINE( I2 : I2 ) .NE. ' ' ) GO TO 20
   10 CONTINUE
      I2 = 1
   20 CONTINUE
      WRITE( OUTPUT, 2000 ) ( NULINE( I : I ), I = 1, I2 )
      RETURN
 2000 FORMAT( 72( A1 : ) )
      END

!     ( Last modified on 15 Mar 2001 at 22:28:00 )
      SUBROUTINE REORDA( NC    , NNZ   , IRN   , JCN   , A , IP, IW )
      INTEGER            NC    , NNZ
      INTEGER            IRN   ( NNZ  ), JCN   ( NNZ )
      INTEGER            IW    ( *    ), IP    ( *   )
      DOUBLE PRECISION   A     ( NNZ  )

!  SORT A SPARSE MATRIX FROM ARBITRARY ORDER TO COLUMN ORDER.
!  NICK GOULD. 7TH NOVEMBER, 1990.
!  FOR CGT PRODUCTIONS.

      INTEGER          I , J , K , L , IC
      INTEGER          NCP1  , ITEMP , JTEMP,  LOCAT
      DOUBLE PRECISION ANEXT , ATEMP

!  INITIALIZE THE WORKSPACE AS ZERO.

      NCP1 = NC + 1
      DO 10 J = 1, NCP1
         IW( J ) = 0
   10 CONTINUE

!  PASS 1. COUNT THE NUMBER OF ELEMENTS IN EACH COLUMN.

      DO 20 K = 1, NNZ
        J = JCN( K )
        IW( J ) = IW( J ) + 1
!        IP( J ) = IP( J ) + 1
   20 CONTINUE

!  PUT THE POSITIONS WHERE EACH COLUMN BEGINS IN
!  A COMPRESSED COLLECTION INTO IP AND IW.

      IP( 1 ) = 1
      DO 30 J = 2, NCP1
        IP( J ) = IW( J - 1 ) + IP( J - 1 )
        IW( J - 1 ) = IP( J - 1 )
   30 CONTINUE

!  PASS 2. REORDER THE ELEMENTS INTO COLUMN ORDER. 
!          FILL IN EACH COLUMN IN TURN.

      DO 70 IC = 1, NC

!  CONSIDER THE NEXT UNFILLED POSITION IN COLUMN IC.

        DO 60 K = IW( IC ), IP( IC + 1 ) - 1

!  THE ENTRY SHOULD BE PLACED IN COLUMN J.

          I = IRN( K )
          J = JCN( K )
          ANEXT = A( K )
          DO 40 L = 1, NNZ

!  SEE IF THE ENTRY IS ALREADY IN PLACE.

             IF ( J .EQ. IC ) GO TO 50
             LOCAT = IW( J )
!          
!  AS A NEW ENTRY IS PLACED IN COLUMN J, INCREASE THE POINTER 
!  IW( J ) BY ONE.
!          
             IW( J  ) = LOCAT + 1

!  RECORD DETAILS OF THE ENTRY WHICH CURRENTLY OCCUPIES LOCATION LOCAT.

             ITEMP = IRN( LOCAT )
             JTEMP = JCN( LOCAT )
             ATEMP = A( LOCAT )

!  MOVE THE NEW ENTRY TO IT CORRECT PLACE. 

             IRN( LOCAT ) = I 
             JCN( LOCAT ) = J  
             A( LOCAT ) = ANEXT

!  MAKE THE DISPLACED ENTRY THE NEW ENTRY.

             I = ITEMP
             J = JTEMP
             ANEXT = ATEMP
   40     CONTINUE

!  MOVE THE NEW ENTRY TO IT CORRECT PLACE. 

   50     CONTINUE
          JCN( K ) = J
          IRN( K ) = I
          A( K ) = ANEXT
   60   CONTINUE
   70 CONTINUE
      RETURN

!  END OF REORDA.

      END

    END MODULE SIFDECODE
