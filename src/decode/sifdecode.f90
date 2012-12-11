! THIS VERSION: SIFDECODE 1.0 - 20/10/2012 AT 13:00 GMT.

!-*-*-*-*-*-*-*-*-*-*-*- S I F D E C O D E   M O D U l E -*-*-*-*-*-*-*-*-*-*-

!  Copyright reserved, Bongartz/Conn/Gould/Orban/Toint, for GALAHAD productions
!  Principal author: Nick Gould

!  History -
!   fortran 77 version originally released as part of CUTE, December 1990
!   Became separate subroutines in SifDec, April 2004
!   Updated fortran 2003 version packaged and released October 2012

!  For full documentation, see 
!   http://galahad.rl.ac.uk/galahad-www/specs.html

    MODULE SIFDECODE

      IMPLICIT NONE

      PRIVATE
      PUBLIC :: SIFDECODE_sdlanc

!---------------
!  V e r s i o n
!---------------

      CHARACTER ( LEN = 6 ) :: version = '1.0   '

!--------------------
!   P r e c i s i o n
!--------------------

      INTEGER, PARAMETER :: sp = KIND( 1.0E+0 )
      INTEGER, PARAMETER :: wp = KIND( 1.0D+0 )

      INTEGER, PARAMETER :: nbytes = 8

!----------------------
!   P a r a m e t e r s
!----------------------

      INTEGER, PARAMETER :: max_record_length = 160
      INTEGER, PARAMETER :: nfunct = 14
      INTEGER, PARAMETER :: nbytes_by_2 = nbytes / 2
      REAL ( KIND = wp ), PARAMETER :: zero = 0.0_wp
      REAL ( KIND = wp ), PARAMETER :: one = 1.0_wp
      REAL ( KIND = wp ), PARAMETER :: ten = 10.0_wp
      REAL ( KIND = wp ), PARAMETER :: biginf = ten ** 20
      REAL ( KIND = wp ), PARAMETER :: epsmch = EPSILON( one )
      LOGICAL, PARAMETER :: oneobj = .FALSE.

      CHARACTER ( len = 10 ), PARAMETER :: cqsqr = '123456789S'
      CHARACTER ( len = 10 ), PARAMETER :: cqprod = '123456789P'
      CHARACTER ( len = 10 ), PARAMETER :: cqgrou = '123456789G'
      CHARACTER ( len = 1 ), DIMENSION( 10 ), PARAMETER :: CHARS =             &
                 (/ '0', '1', '2', '3', '4', '5','6', '7', '8', '9'/)
      CHARACTER ( len = 1 ), DIMENSION( 26 ), PARAMETER :: LCHARS =            &
                 (/ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',          &
                    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',          &
                    'u', 'v', 'w', 'x', 'y', 'z' /)
      CHARACTER ( len = 1 ), DIMENSION( 26 ), PARAMETER :: UCHARS =            &
                 (/ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',          &
                    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',          &
                    'U', 'V', 'W', 'X', 'Y', 'Z' /)
      CHARACTER ( len = 6 ), DIMENSION( nfunct ), PARAMETER :: FUNCTN =        &
         (/ 'ABS   ', 'SQRT  ', 'EXP   ', 'LOG   ', 'LOG10 ',                  &
            'SIN   ', 'COS   ', 'TAN   ', 'ARCSIN', 'ARCCOS',                  &
            'ARCTAN', 'HYPSIN', 'HYPCOS', 'HYPTAN' /)

!--------------------------------
!  G l o b a l  v a r i a b l e s
!--------------------------------

      INTEGER :: hash_empty
      REAL ( KIND = wp ) :: hash_prime

    CONTAINS

!-*-*-*-*-*- S I F D E C O D E   S D L A N C   S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SIFDECODE_sdlanc( iingps, outda, iinfn, outfn, outff, outfd,  &
                                   outra, iingr, outgr, outgf, outgd, iinex,   &
                                   outex, outem, outea, print_level, out,      &
                                   noname, ialgor, iauto, iad0, single, status )
      INTEGER :: iingps, iinfn, iingr
      INTEGER :: outda, outra, outfn, outgr, outff, outfd, outgf, outgd, outem
      INTEGER :: outea, outex, print_level, out, ialgor, iad0, iauto, status
      LOGICAL :: noname, single

!  ------------------------------------------------------------------------
!  decode a SIF file and convert the data into a form suitable for input to 
!  SBMIN, AUGLG or BARIA
!  ------------------------------------------------------------------------

!  local variables

      INTEGER :: i, ig, isg, iinex, la, lb, length, nmax, ngmax
      INTEGER :: nobmax, nimax, liwk, lwk, nconst
      INTEGER :: nsmax, nbmax, netmax, nomax, nlmax, nelmax
      INTEGER :: nelnum, neling, negmax, nepvmx, ngpvmx
      INTEGER :: nindex, maxins, maxlev, maxara, narray, nobjgr
      INTEGER :: ngrmax, nrlndx, nepmax, ngpmax, nevmax
      INTEGER :: ninmax, numax, nsetvc, lstadg, lstada, lelvar
      INTEGER :: lstaev, lntvar, lbnds, lintre, licna, nreal
      INTEGER :: nlinob, nnlnob, nlineq, nnlneq, nlinin, nnlnin
      INTEGER :: nfree, nfixed, nlower, nupper, nboth, nslack
      INTEGER :: n, ng, nbnd, neltyp, nlvars, nobj, nrange
      INTEGER :: nnza, ngrtyp, nstart, nlisgp, nnlvrs, nobbnd
      REAL ( KIND = wp ) :: blo, bup
      LOGICAL :: debug, gotlin
      CHARACTER ( len = 10 ) :: nameof, namerh, namera, namebn, namest, nameob
      CHARACTER ( len = 72 ) :: lineex

!  ------------------------------------------------------

!  parameters whose value might be changed by the user:

!  the following parameters define the sizes of problem
!  dependent arrays. these may be changed by the user
!  to suit a particular problem or system configuration

!  an error messages will be issued if any of these sizes
!  is too small, telling which parameter to increase

!  ------------------------------------------------------

!  maximum number of variables

      PARAMETER      ( nmax   = 1000000 )

!  maximum number of groups

      PARAMETER      ( ngmax  = 600000 )

!  maximum number of different group types

      PARAMETER      ( ngrmax = 10    )

!  maximum total number of real parameters associated with groups

      PARAMETER      ( ngpvmx = 1200000 )

!  maximum number of nonlinear elements

      PARAMETER      ( nelmax = 1000000 )

!  maximum number of different nonlinear element types

      PARAMETER      ( nlmax  = 20     )

!  maximum total number of elemental variables

      PARAMETER      ( nevmax = 1000000 )

!  maximum total number of internal variables

      PARAMETER      ( ninmax = 150000  )

!  maximum number of entries in an element Hessian

      PARAMETER      ( nsetvc = 100    )

!  maximum number of real parameters associated with nonlinear elements

      PARAMETER      ( nepvmx = 1000000 )

!  maximum number of nonzeros in linear elements

      PARAMETER      ( la     = 8000000 )

!  maximum number of integer parameters

      PARAMETER      ( nindex = 100    )

!  maximum number of real parameters

      PARAMETER      ( nrlndx = 20000  )

!  maximum number of vectors of bounds

      PARAMETER      ( nbmax  = 2      )

!  maximum number of vectors of solutions

      PARAMETER      ( nsmax  = 3      )

!  maximum number of vectors of bounds on the objective function

      PARAMETER      ( nobmax = 2      )

!  ---------------------------------------------------------------------

!  end of parameters that might be changed by the user

!  ---------------------------------------------------------------------

!  dependencies on the maximum number of nontrivial group types
!  ngpmax is the total number of group parameters

      PARAMETER      ( ngpmax = ngrmax )

!  dependencies on the maximum number of groups

      PARAMETER      ( nomax = ngmax )
      PARAMETER      ( numax = ngmax )
      PARAMETER      ( lstadg = ngmax )
      PARAMETER      ( lstada = ngmax )
      PARAMETER      ( lb = ngmax )
      PARAMETER      ( lbnds = nmax + ngmax )

!  dependencies on the maximum total number of real
!  parameters associated with groups

      PARAMETER      ( lwk = ngpvmx )

!  dependencies on the maximum number of nonlinear element types
!  netmax, nimax and nepmax are the total number of elemental and
!  internal variables and parameters respectively

      PARAMETER      ( netmax = 5 * nlmax )
      PARAMETER      ( nimax = 5 * nlmax )
      PARAMETER      ( nepmax = 3 * nlmax )

!  dependencies on the maximum number of nonlinear elements

      PARAMETER      ( negmax = nelmax )
      PARAMETER      ( lstaev = nelmax )
      PARAMETER      ( lntvar = nelmax + 1 )
      PARAMETER      ( lintre = nelmax )
      PARAMETER      ( liwk = nelmax + ngmax )

!  dependencies on the maximum total number of elemental variables

      PARAMETER      ( lelvar = nevmax )

!  dependencies on the maximum number of nonzeros in linear elements

      PARAMETER      ( licna = la     )

!  maximum number of statements in a do-loop

      PARAMETER      ( maxins = 200    )

!  maximum nesting of do-loops

      PARAMETER      ( maxlev = 3      )

!  maximum number of array instructions

      PARAMETER      ( maxara = 150    )

!  maximum size of dictionary

      PARAMETER      ( length = nmax + ngmax + nelmax + ninmax + 1000 )

!  array definitions

      INTEGER :: INLIST( length ), ISTAEV( nelmax )
      INTEGER :: ISTATE( ngmax ), ITABLE ( length )
      INTEGER :: IELV  ( nlmax  ), IINV  ( nlmax )
      INTEGER :: ITYPEE( nelmax ), IELING( negmax, 2 )
      INTEGER :: IEPA  ( nlmax  ), IGPA  ( ngrmax )
      INTEGER :: IDROWS( 2, ngmax ), ITYPEG( ngmax )
      INTEGER :: IELVAR( lelvar ), INTVAR( lntvar )
      INTEGER :: ISTADA( lstada ), ICNA  ( licna  )
      INTEGER :: ISTEP ( nelmax ), ISTGP( ngmax ), IWK( liwk )
      INTEGER :: INDVAL( nindex ), INSTR( 5, maxins, maxlev )
      INTEGER :: NINSTR( maxlev ), IARRAY( 5, 3, maxara )
      INTEGER :: ICOORD( la, 2 ), ISTADG( ngmax ), IJUMP( nlmax )
      INTEGER :: ITYPEV( nmax )
      REAL ( KIND = wp ) :: GPTEMP( ngpvmx )
      REAL ( KIND = wp ) :: EPVALU( nepvmx ), GPVALU( ngpvmx ), DFAULT( NMAX)
      REAL ( KIND = wp ) :: A( la ), BND( 2, nmax, nbmax ), REALVL( nrlndx )
      REAL ( KIND = wp ) :: BNDFLT( 2, nbmax ), CSTART( ngmax, nsmax )
      REAL ( KIND = wp ) :: RSCALE( ngmax ), CSCALE( nmax ), WK( lwk )
      REAL ( KIND = wp ) :: RDROWS( 2, ngmax ), VSTART( nmax, nsmax )
      REAL ( KIND = wp ) :: RVALUE( maxara, 3 ), VARRAY( 2, maxara )
      REAL ( KIND = wp ) :: FBOUND( 2, nobmax ), WEIGHT( negmax )
      REAL ( KIND = wp ) :: ABYROW( la ), B( lb ), BL( lbnds ), BU( lbnds )
      REAL ( KIND = wp ) :: X( nmax ), U( numax ), ESCALE( negmax )
      REAL ( KIND = wp ) :: VSCALE( nmax ), GSCALE( ngmax ), CLMULT( ngmax )
      LOGICAL :: INTREP( lintre ), LDEFND( nlmax )
      LOGICAL :: SETVEC( nsetvc ), GXEQX( ngmax )
      CHARACTER ( len = 2 ) :: FARRAY( maxara )
      CHARACTER ( len = 8 ) :: pname
      CHARACTER ( len = 10 ) :: NAMIIN( nindex ), NAMRIN( nrlndx )
      CHARACTER ( len = 10 ) :: LONAME( ninmax ), BNAMES( nbmax  )
      CHARACTER ( len = 10 ) :: GNAMES( ngmax  ), VNAMES( nmax   )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax  ), GTYPES( ngrmax )
      CHARACTER ( len = 10 ) :: LNAMES( nelmax ), OBNAME( nobmax )
      CHARACTER ( len = 10 ) :: EPNAME( nepmax ), GPNAME( ngpmax )
      CHARACTER ( len = 10 ) :: ONAMES( nomax  ), ENAMES( netmax )
      CHARACTER ( len = 10 ) :: EXNAME( ninmax ), SNAMES( nsmax )
      CHARACTER ( len = 10 ) :: ANAMES( ngrmax ), INAMES( nimax  )
      CHARACTER ( len = 10 ) :: MINAME( ninmax ), RENAME( ninmax )
      CHARACTER ( len = 10 ) :: INNAME( ninmax )
      CHARACTER ( len = 10 ) :: ARRAY( 3, maxara ), CARRAY( 2, maxara )
      CHARACTER ( len = 12  ) :: KEY( length )
      CHARACTER ( len = max_record_length ) :: nuline

      CHARACTER ( len = 1 ), DIMENSION( 2 ), PARAMETER :: S = (/ ' ', 's' /)
      CHARACTER ( len = 3 ), DIMENSION( 2 ), PARAMETER :: ARE                  &
        = (/ 'is ', 'are' /)

      debug = print_level < 0
      IF ( single ) THEN
        WRITE( out, 2050 )
      ELSE
        WRITE( out, 2060 )
      END IF
      IF ( print_level /= 0 ) print_level = 9

!  read the gps mps data

      CALL GPSMPS( la, nmax, ngmax, nomax, nlmax, nelmax, nimax, netmax,       &
                   nevmax, ngrmax, nsmax, nepmax, ngpmax, nbmax, nobmax, nnza, &
                   length, n, ng, nobj, nconst, nrange, nbnd, nstart, neltyp,  &
                   ngrtyp, nlvars, nnlvrs, nlisgp, liwk, nelnum, neling,       &
                   narray, nindex, negmax, nepvmx, ngpvmx, maxins, maxlev,     &
                   maxara, nrlndx, nobbnd, pname, ICOORD, IELING, INLIST,      &
                   ITABLE, ISTATE, IELV, IINV, ITYPEE, IDROWS, IELVAR, ISTADG, &
                   ITYPEG, IEPA, IGPA, IWK, ISTEP, ISTAEV, ISTGP, INDVAL,      &
                   INSTR, ninstr, IARRAY, ITYPEV, A, BND, VSTART, CSTART,      &
                   RSCALE, CSCALE, RDROWS, REALVL, DFAULT, rvalue, VARRAY,     &
                   EPVALU, BNDFLT, GPVALU, GPTEMP, farray, FBOUND, WEIGHT,     &
                   NAMIIN, NAMRIN, GNAMES, VNAMES, BNAMES, ETYPES, INAMES,     &
                   lnames, ONAMES, ENAMES, SNAMES, ANAMES, GTYPES, EPNAME,     &
                   GPNAME, OBNAME, array, CARRAY, KEY, single, iingps, out,    &
                   status, debug )
      IF ( status /= 0 ) THEN
        WRITE( out, 2010 ) status
        RETURN
      END IF

!  assign the groups to constraint types and objectives

      nlinob = 0
      nnlnob = 0
      nlineq = 0
      nnlneq = 0
      nlinin = 0
      nnlnin = 0
      DO ig = 1, ng
        isg = ISTATE( ig )
        IF ( isg > 0 ) THEN
          isg = MOD( isg - 1, 4 )
          IF ( isg == 0 ) nlinob = nlinob + 1
          IF ( isg == 1 ) nlineq = nlineq + 1
          IF ( isg >= 2 ) nlinin = nlinin + 1
        ELSE
          isg = MOD( isg + 1, 4 )
          IF ( isg == 0 ) nnlnob = nnlnob + 1
          IF ( isg == - 1 ) nnlneq = nnlneq + 1
          IF ( isg <= - 2 ) nnlnin = nnlnin + 1
        END IF
      END DO

!  select rhs, ranges and bounds

      IF ( nconst > 0 ) namerh = VNAMES( nlvars + 1 )
      IF ( nrange > 0 ) namera = VNAMES( nlvars + nconst + 1 )
      IF ( nbnd > 0 ) namebn = BNAMES( 1 )
      IF ( nstart > 0 ) namest = SNAMES( 1 )
      IF ( nobj > 0 .AND. oneobj ) nameof = ONAMES( 1 )
      IF ( nobbnd > 0 ) nameob = OBNAME( 1 )
      IF ( print_level /= 0 ) WRITE( out, 2070 ) nconst, nrange, nbnd,         &
           nstart, nobj, nobbnd

!  convert to input for one of the lancelot programs

      CALL INLANC( n, nlvars, ng, nelnum, nobj, length, lstadg, lelvar,        &
                   lstaev, lntvar, licna, lstada, la, lb, lbnds, lintre, liwk, &
                   lwk, nmax, ngmax, nbmax, nsmax, nlmax, nelmax, negmax,      &
                   nobmax, ngrmax, ngpvmx, nepvmx, nomax, nlisgp, nbnd, nnza,  &
                   nconst, nstart, nrange, nobjgr, nobbnd, neltyp, ngrtyp,     &
                   pname, nameob, namerh, namera, namebn, namest,              &
                   nameof, ISTADG, IELVAR, ISTAEV, INTVAR, ICNA, ISTADA,       &
                   ICOORD, INLIST, ITABLE, ISTATE, IDROWS, IELV, IINV, IGPA,   &
                   IELING( 1, 1 ), ISTEP, ISTGP, ITYPEE, ITYPEG, ITYPEV, IWK,  &
                   A, BND, VSTART, CSTART, RSCALE, CSCALE, RDROWS, DFAULT,     &
                   WEIGHT, BNDFLT, WK, GPVALU, EPVALU, FBOUND, ABYROW, B, BL,  &
                   BU, X, CLMULT, ESCALE, GSCALE, VSCALE, INTREP, GXEQX, KEY,  &
                   GNAMES, VNAMES, BNAMES, SNAMES, ONAMES, ETYPES, GTYPES,     &
                   OBNAME, ialgor, iauto, out, outda, single, status, debug )
      IF ( status /= 0 ) THEN
         WRITE( out, 2020 ) status
         RETURN
      END IF


!  assign the variables to bound types

      nfree = 0
      nfixed = 0
      nlower = 0
      nupper = 0
      nboth = 0
      IF ( ialgor <= 2 ) THEN
         nslack = nlinin + nnlnin
      ELSE
         nslack = 0
      END IF
      nreal = n - nslack
      DO 110 i = 1, nreal
         blo = BL( i )
         bup = BU( i )
         IF ( blo <= - biginf .AND. bup >= biginf ) nfree = nfree  + 1
         IF ( blo <= - biginf .AND. bup < biginf ) nupper = nupper + 1
         IF ( blo > - biginf .AND. bup >= biginf ) nlower = nlower + 1
         IF ( blo > - biginf .AND. bup < biginf ) THEN
            IF ( blo == bup ) THEN
                nfixed = nfixed + 1
            ELSE
                nboth = nboth  + 1
            END IF
         END IF
  110 CONTINUE

!  print problem summary

      IF ( nlinob > 0 ) WRITE( out, 2100 ) nlinob, TRIM( S( ONLY1( nlinob ) ) )
      IF ( nnlnob > 0 ) WRITE( out, 2110 ) nnlnob, TRIM( S( ONLY1( nnlnob ) ) )
      IF ( nlineq + nlinin + nnlneq + nnlnin > 0 ) WRITE( out, 2000)
      IF ( nlineq > 0 ) WRITE( out, 2120 ) TRIM( ARE( ONLY1( nlineq ) )) ,     &
                nlineq, TRIM( S( ONLY1( nlineq ) ) )
      IF ( nlinin > 0 ) WRITE( out, 2130 ) TRIM( ARE( ONLY1( nlinin ) ) ),     &
                nlinin, TRIM( S( ONLY1( nlinin ) ) )
      IF ( nnlneq > 0 ) WRITE( out, 2140 ) TRIM( ARE( ONLY1( nnlneq ) ) ),     &
                nnlneq, TRIM( S( ONLY1( nnlneq ) ) )
      IF ( nnlnin > 0 ) WRITE( out, 2150 ) TRIM( ARE( ONLY1( nnlnin ) ) ),     &
                nnlnin, TRIM( S( ONLY1( nnlnin ) ) )
      WRITE( out, 2000 )
      IF ( nfree  > 0 ) WRITE( out, 2200 ) TRIM( ARE( ONLY1( nfree  ) ) ),     &
                nfree, TRIM( S( ONLY1( nfree  ) ) )
      IF ( nupper > 0 ) WRITE( out, 2210 ) TRIM( ARE( ONLY1( nupper ) ) ),     &
                nupper, TRIM( S( ONLY1( nupper ) ) )
      IF ( nlower > 0 ) WRITE( out, 2220 ) TRIM( ARE( ONLY1( nlower ) ) ),     &
                nlower, TRIM( S( ONLY1( nlower ) ) )
      IF ( nboth  > 0 ) WRITE( out, 2230 ) TRIM( ARE( ONLY1( nboth  ) ) ),     &
                nboth,  TRIM( S( ONLY1( nboth  ) ) )
      IF ( nfixed > 0 ) WRITE( out, 2240 ) TRIM( ARE( ONLY1( nfixed ) ) ),     &
                nfixed, TRIM( S( ONLY1( nfixed ) ) )
      IF ( nslack > 0 ) WRITE( out, 2250 ) TRIM( ARE( ONLY1( nslack ) ) ),     &
                nslack, TRIM( S( ONLY1( nslack ) ) )
      WRITE( outda, 2080 ) pname, nfree, nfixed, nlower, nupper, nboth,        &
                nslack, nlinob, nnlnob, nlineq, nnlneq, nlinin, nnlnin

!  print details of the problem

      CALL PRINTP( nmax, ngmax, nlmax, nelmax, netmax, nevmax, nepmax, ngrmax, &
                   negmax, nepvmx, ngpvmx, ngpmax, lstada, licna, liwk, n, ng, &
                   nlvars, nelnum, ISTATE, ISTADG, IELVAR, ITYPEG, ITYPEE,     &
                   IELV, IINV, IEPA, IGPA, ISTADA, ICNA, ISTGP, ISTEP, ISTAEV, &
                   IELING, ITYPEV, IWK, ABYROW, B, BL, BU, X, EPVALU, GPVALU,  &
                   GSCALE, ESCALE, VSCALE, pname, VNAMES, GNAMES, lnames,      &
                   ETYPES, ENAMES, ANAMES, EPNAME, GPNAME, GTYPES, out,        &
                   print_level )
      IF ( noname ) pname = '        '

!  make subroutines elfun and range

      IF ( iauto == 0 ) THEN
        CALL MAKE_elfun( iinfn, out, outfn, outra, status, nlmax, nimax,       &
                         netmax, ninmax, numax, neltyp, pname, ENAMES, INAMES, &
                         RENAME, INNAME, LONAME, MINAME, EXNAME, ETYPES,       &
                         LDEFND, length, ITABLE, KEY, IELV, IINV, INLIST,      &
                         EPNAME, IEPA, nepmax, debug, ijump, U, SETVEC,        &
                         nsetvc, single, nuline, gotlin, print_level )
        IF ( status /= 0 ) THEN
          WRITE( out, 2030 ) status
          RETURN
        END IF

!  make subroutines elfunf, elfund and range

      ELSE
        CALL MAKE_elfun_ad( iinfn, out, outff, outfd, outra, outem, status,    &
                            nlmax, nimax, netmax, ninmax, numax, neltyp,       &
                            pname, ENAMES, INAMES, RENAME, INNAME, LONAME,     &
                            MINAME, EXNAME, ETYPES, LDEFND, length, ITABLE,    &
                            KEY, IELV, IINV, INLIST, EPNAME, IEPA, nepmax,     &
                            debug, ijump, U, single, nuline, gotlin, iauto,    &
                            iad0, print_level )
        IF ( status /= 0 ) THEN
          WRITE( out, 2090 ) status
          RETURN
        END IF
      END IF

!  make subroutine group and obtain group information

      IF ( iauto == 0 ) THEN
        CALL MAKE_group( iingr, out, outgr, status, ngrtyp, ngrmax, nlmax,     &
                         ninmax, pname, ANAMES, RENAME, INNAME, LONAME,        &
                         MINAME, EXNAME, GTYPES, LDEFND, GPNAME, IGPA, ngpmax, &
                         debug, length, ITABLE, KEY, INLIST, single, nuline,   &
                         gotlin, print_level )
        IF ( status /= 0 ) THEN
          WRITE( out, 2040 ) status
          RETURN
        END IF

!  make subroutines groupf and groupd

      ELSE
        CALL MAKE_group_ad( iingr, out, outgf, outgd, outem, status, ngrtyp,   &
                            ngrmax, nlmax, ninmax, pname, ANAMES, RENAME,      &
                            INNAME, LONAME, MINAME, EXNAME, GTYPES, LDEFND,    &
                            GPNAME, IGPA, ngpmax, debug, length, ITABLE, KEY,  &
                            INLIST, single, nuline, gotlin, iauto, iad0,       &
                            print_level )
        IF ( status /= 0 ) THEN
           WRITE( out, 2160 ) status
           RETURN
        END IF
      END IF

!  finally, read any additional programs

  500 CONTINUE
      IF ( gotlin ) THEN
        LINEEX( 1 : 72 ) = NULINE( 1 : 72 )
        gotlin = .FALSE.
      ELSE
        READ( UNIT = iinex, FMT = 1000, END = 600, ERR = 600 ) lineex
      END IF

!  skip blank lines

      DO i = 1, 72
        IF ( LINEEX( I: i ) /= ' ' ) THEN
          WRITE( outex, 1000 ) lineex
          GO TO 500
        END IF
      END DO
      GO TO 500
  600 CONTINUE

!  if required, translate any external file to accept automatic 
!  differentiation constructs

      IF ( iauto == 1 .OR. iauto == 2 )                                        &
        CALL TRANS( out, outex, outea, outem, single, iauto,                   &
                    iad0, NAMRIN, nrlndx, NAMIIN, nindex )
      status = 0
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 2000 FORMAT( ' ' )
 2010 FORMAT( /, ' Return from GPSMPS, status = ', I0 )
 2020 FORMAT( /, ' Return from INLANC, status = ', I0 )
 2030 FORMAT( /, ' Return from MAKE_elfun, status = ', I0 )
 2040 FORMAT( /, ' Return from MAKE_group, status = ', I0 )
 2050 FORMAT( /, ' Single precision version will be formed', / )
 2060 FORMAT( /, ' Double precision version will be formed', / )
 2070 FORMAT( /, '  nconst  nrange    nbnd  nstart    nobj  nobbnd ', /, 6I8, /)
 2080 FORMAT( A8, 12I8 )
 2090 FORMAT( /, ' Return from MAKE_elfun_ad, status = ', I0 )
 2100 FORMAT( ' The objective function uses ', I0, ' linear group', A )
 2110 FORMAT( ' The objective function uses ', I0, ' nonlinear group', A )
 2120 FORMAT( ' There ', A, 1X, I0, ' linear equality constraint', A )
 2130 FORMAT( ' There ', A, 1X, I0, ' linear inequality constraint', A )
 2140 FORMAT( ' There ', A, 1X, I0, ' nonlinear equality constraint', A )
 2150 FORMAT( ' There ', A, 1X, I0, ' nonlinear inequality constraint', A )
 2160 FORMAT( /, ' Return from MAKE_group_ad, status = ', I0 )
 2200 FORMAT( ' There ', A, 1X, I0, ' free variable', A1 )
 2210 FORMAT( ' There ', A, 1X, I0, ' variable', A,                            &
                ' bounded only from above ' )
 2220 FORMAT( ' There ', A, 1X, I0, ' variable', A,                            &
                ' bounded only from below ' )
 2230 FORMAT( ' There ', A, 1X, I0,                                            &
              ' variable', A, ' bounded from below and above ' )
 2240 FORMAT( ' There ', A, 1X, I0, ' fixed variable', A )
 2250 FORMAT( ' There ', A, 1X, I0, ' slack variable', A )

!  end of subroutine SIFDECODE_sdlanc

      END SUBROUTINE SIFDECODE_sdlanc

!-*-*-*-*-*- S I F D E C O D E   G P S M P S   S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GPSMPS( la, nmax, ngmax, nomax, nlmax, nelmax,                &
                         nimax, netmax, nevmax, ngrmax, nsmax, nepmax,         &
                         ngpmax, nbmax, nobmax, nnza, length, n, ng,           &
                         nobj, nconst, nrange, nbnd, nstart, neltyp,           &
                         ngrtyp, nlvars, nnlvrs, nlisgp, liwk,                 &
                         nelnum, neling, narray, nindex, negmax, nepvmx,       &
                         ngpvmx, maxins, maxlev, maxara, nrlndx, nobbnd,       &
                         pname, ICOORD, IELING, INLIST, ITABLE, ISTATE,        &
                         IELV, IINV, ITYPEE, IDROWS, IELVAR, ISTADG,           &
                         ITYPEG, IEPA, IGPA, IWK, ISTEP, ISTAEV,               &
                         ISTGP, INDVAL, INSTR, ninstr, IARRAY, ITYPEV,         &
                         A, BND, VSTART, CSTART, RSCALE, CSCALE, RDROWS,       &
                         REALVL, DFAULT, rvalue, VARRAY, EPVALU, BNDFLT,       &
                         GPVALU, GPTEMP, farray, FBOUND, WEIGHT, NAMIIN,       &
                         NAMRIN, GNAMES, VNAMES, BNAMES, ETYPES, INAMES,       &
                         lnames, ONAMES, ENAMES, SNAMES, ANAMES, GTYPES,       &
                         EPNAME, GPNAME, OBNAME, array, CARRAY, KEY,           &
                         single, input, out, status, debug )
      INTEGER :: la, nmax, ngmax, nomax, nlmax, nelmax
      INTEGER :: nevmax, ngrmax, nsmax, nobmax, nobbnd, nrlndx
      INTEGER :: nepmax, ngpmax, nbmax, nnza, length, n, ng
      INTEGER :: nconst, nrange, nbnd, nstart, neltyp, ngrtyp
      INTEGER :: nlvars, nnlvrs, nelnum, neling, narray, nindex
      INTEGER :: negmax, nepvmx, ngpvmx, maxins, maxlev, maxara
      INTEGER :: nlisgp, input, netmax, out, status, nimax
      INTEGER :: nobj, liwk
      LOGICAL :: single, debug 
      CHARACTER ( len = 8 ) :: pname
      INTEGER :: INLIST( length ), ISTAEV( nelmax )
      INTEGER :: ISTATE( ngmax ), ITABLE ( length )
      INTEGER :: IELV  ( nlmax  ), IINV  ( nlmax )
      INTEGER :: ITYPEE( nelmax ), IELING( negmax, 2 )
      INTEGER :: IEPA  ( nlmax  ), IGPA  ( ngrmax )
      INTEGER :: IDROWS( 2, ngmax ), ITYPEG( ngmax )
      INTEGER :: IELVAR( nevmax ), ITYPEV( nmax )
      INTEGER :: ISTEP ( nelmax ), ISTGP( ngmax ), IWK( liwk )
      INTEGER :: INDVAL( nindex ), INSTR( 5, maxins, maxlev )
      INTEGER :: NINSTR( maxlev ), IARRAY( 5, 3, maxara )
      INTEGER :: ICOORD( la, 2 ), ISTADG( ngmax )
      REAL ( KIND = wp ) :: GPTEMP( ngpvmx )
      REAL ( KIND = wp ) :: EPVALU( nepvmx ), GPVALU( ngpvmx ), DFAULT( NMAX)
      REAL ( KIND = wp ) :: A( la ), BND( 2, nmax, nbmax ), REALVL( nrlndx )
      REAL ( KIND = wp ) :: BNDFLT( 2, nbmax ), CSTART( ngmax, nsmax )
      REAL ( KIND = wp ) :: RSCALE( ngmax ), CSCALE( nmax )
      REAL ( KIND = wp ) :: RDROWS( 2, ngmax ), VSTART( nmax, nsmax )
      REAL ( KIND = wp ) :: RVALUE( maxara, 3 ), VARRAY( 2, maxara )
      REAL ( KIND = wp ) :: FBOUND( 2, nobmax ), WEIGHT( negmax )
      CHARACTER ( len = 2 ) :: FARRAY( maxara )
      CHARACTER ( len = 10 ) :: NAMIIN( nindex ), NAMRIN( nrlndx )
      CHARACTER ( len = 10 ) :: BNAMES( nbmax  )
      CHARACTER ( len = 10 ) :: GNAMES( ngmax  ), VNAMES( nmax   )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax  ), INAMES( nimax  )
      CHARACTER ( len = 10 ) :: LNAMES( nelmax ), OBNAME( nobmax )
      CHARACTER ( len = 10 ) :: EPNAME( nepmax ), GPNAME( ngpmax )
      CHARACTER ( len = 10 ) :: ONAMES( nomax  ), ENAMES( netmax )
      CHARACTER ( len = 10 ) :: SNAMES( nsmax )
      CHARACTER ( len = 10 ) :: ANAMES( ngrmax ), GTYPES( ngrmax )
      CHARACTER ( len = 10 ) :: ARRAY( 3, maxara ), CARRAY( 2, maxara )
      CHARACTER ( len = 12 ) :: KEY( length )

!  -------------------------------------------------------------------
!  read a GPS MPS data file

!  MPS indicator cards
!  --------------------

!  definition   purpose
!  ----------   --------
!  NAME         problem name
!  ROWS         names of rows (alias group names)
!  COLUMNS      names of columns (alias variable names)
!  RHS          right-hand-sides (alias constant terms in groups)
!  RHS'         alias for rhs
!  RANGES       additional bounds on rows
!  BOUNDS       bounds on columns
!  ENDATA       end of input data

!  additional indicator cards
!  ---------------------------

!  definition   purpose
!  ----------   --------
!  GROUPS       alias for rows
!  CONSTRAINTS  alias for rows
!  VARIABLES    alias for columns
!  CONSTANTS    alias for rhs
!  START_POINT  estimate of minimizer
!  HESSIAN      quadratic terms
!  QUADRATIC    alias for Hessian
!  QUADS        alias for Hessian
!  QUADOBJ      alias for Hessian
!  QSECTION     alias for Hessian
!  QMATRIX      alias for Hessian
!  ELEMENT_TYPE types of nonlinear elements
!  ELEMENT_USES definitions of nonlinear elements
!  GROUP_TYPE   types of nontrivial groups
!  GROUP_USES   definitions of groups

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in 
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  -------------------------------------------------------------------
!  returns with negative values of inform indicate that insufficient
!  array space has been allowed, as follows:

!    inform = - 1  when length not large enough
!    inform = - 2  when nnza > la
!    inform = - 3  when neltyp >= nlmax
!    inform = - 4  when ngrtyp >= ngrmax
!    inform = - 5  when nobj > nomax
!    inform = - 6  when ng > ngmax
!    inform = - 7  when n > nmax
!    inform = - 8  when nstart > nsmax
!    inform = - 9  when nelnum > nelmax
!    inform = - 10 when neling > negmax
!    inform = - 11 when ninstr( 1 or 2 or 3 ) > maxins
!    inform = - 12 when narray > maxara
!    inform = - 13 when nbnd > nbmax
!    inform = - 14 when neln > netmax
!    inform = - 15 when nlisev > nevmax
!    inform = - 16 when ninn > nimax
!    inform = - 17 when nlisep > nepvmx
!    inform = - 18 when nlisgp > ngpvmx
!    inform = - 19 when nepn > nepmax
!    inform = - 20 when ngpn > ngpmax
!    inform = - 21 when nusein > nindex
!    inform = - 22 when nusere > nrlndx
!    inform = - 23 when nobbnd > nobmax
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, ip, is, intype, intypo, j, k, k1, k2, l, l1, l2, l3, level2
      INTEGER :: ifree, ifield, novals, nvar, ncol, neln, ninn, nlisep, nlisev
      INTEGER :: mblank, mname, mrows, mgroup, mcnstr, mobbnd
      INTEGER :: mvars, mconst, mrhs, mrhsp, mrange, mbound, mcols
      INTEGER :: mstart, metype, mgtype, meuses, mguses, mendat
      INTEGER :: mfree, mfixed, maxnul, nlines, ilines
      INTEGER :: nepn, ngpn, ngrupe, nelmnt, nusein, nusere, mqsect, mqmatr
      INTEGER :: mqhess, mquado, mquadr, mquads, iptype, istype, ndtype
      INTEGER :: level3, lev1, lev2, lev3, lev1s, lev2s, lev3s
      INTEGER :: lev1e, lev2e, lev3e, lev1i, lev2i, lev3i, levl3a
      INTEGER :: level, ijump, nincrs, lineno
      INTEGER, DIMENSION( 4 ) :: LOOP
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: defnam, inrep, defaut, doloop, strtgu
      LOGICAL :: endbnd, endst, setana, endelt, endgrt, delset
      LOGICAL :: endelu, grp1st, grpyet, varyet, fixed, dgrset
      LOGICAL :: qgroup, qsqr, qprod
      CHARACTER ( len = 2 ) :: field1, colfie
      CHARACTER ( len = 10 ) :: field2, field3, field5, grupe, elmnt
      CHARACTER ( len = 10 ) :: detype, dgtype
      CHARACTER ( len = 12 ) :: field
      CHARACTER ( len = max_record_length ) :: nuline, blnkln

!  parameter definitions

      PARAMETER        ( nincrs = 23 )
      CHARACTER ( len = 6 ) :: INCRSE( nincrs )
      PARAMETER        ( mblank =  1, mfixed =  2, mfree = 3  )
      PARAMETER        ( mname =  4, mrows =  5 )
      PARAMETER        ( mgroup =  6, mcnstr =  7, mcols =  8 )
      PARAMETER        ( mvars =  9, mconst = 10, mrhs = 11 )
      PARAMETER        ( mrhsp = 12, mrange = 13, mbound = 14 )
      PARAMETER        ( mstart = 15, mqhess = 16, mquadr = 17 )
      PARAMETER        ( mquads = 18, mquado = 19, mqsect = 20 )
      PARAMETER        ( mqmatr = 21 )
      PARAMETER        ( metype = 22, meuses = 23, mgtype = 24 )
      PARAMETER        ( mguses = 25, mobbnd = 26, mendat = 27 )
      INTEGER :: LENIND( mendat )
      CHARACTER ( len = 12 ) :: INDIC8( mendat ), header
      PARAMETER        ( maxnul = 20 )
      CHARACTER ( len = 65 ) :: NULINA( maxnul )
      LOGICAL :: adddoloop

!  data declarations

      DATA INCRSE / 'LENGTH', 'LA    ', 'NLMAX ', 'NGRMAX', 'NOMAX ',          &
                    'NGMAX ', 'NMAX  ', 'NSMAX ', 'NELMAX', 'NEGMAX',          &
                    'MAXINS', 'MAXARA', 'NBMAX ', 'NETMAX', 'NEVMAX',          &
                    'NIMAX ', 'NEPVMX', 'NGPVMX', 'NEPMAX', 'NGPMAX',          &
                    'NINDEX', 'NRLNDX', 'NOBMAX' /
      DATA INDIC8( mblank ) / '            ' /, LENIND( mblank ) / 0  /
      DATA INDIC8( mfixed ) / 'FIXED FORMAT' /, LENIND( mfixed ) / 12 /
      DATA INDIC8( mfree  ) / 'FREE FORMAT ' /, LENIND( mfree  ) / 11 /
      DATA INDIC8( mname  ) / 'NAME        ' /, LENIND( mname  ) / 4  /
      DATA INDIC8( mrows  ) / 'ROWS        ' /, LENIND( mrows  ) / 4  /
      DATA INDIC8( mgroup ) / 'GROUPS      ' /, LENIND( mgroup ) / 6  /
      DATA INDIC8( mcnstr ) / 'CONSTRAINTS ' /, LENIND( mcnstr ) / 11 /
      DATA INDIC8( mcols  ) / 'COLUMNS     ' /, LENIND( mcols  ) / 7  /
      DATA INDIC8( mvars  ) / 'VARIABLES   ' /, LENIND( mvars  ) / 9  /
      DATA INDIC8( mconst ) / 'CONSTANTS   ' /, LENIND( mconst ) / 9  /
      DATA INDIC8( mrhs   ) / 'RHS         ' /, LENIND( mrhs   ) / 3  /
      DATA INDIC8( mrhsp  ) / 'RHS''       ' /, LENIND( mrhsp  ) / 4  /
      DATA INDIC8( mrange ) / 'RANGES      ' /, LENIND( mrange ) / 6  /
      DATA INDIC8( mbound ) / 'BOUNDS      ' /, LENIND( mbound ) / 6  /
      DATA INDIC8( mstart ) / 'START POINT ' /, LENIND( mstart ) / 11 /
      DATA INDIC8( mqhess ) / 'HESSIAN     ' /, LENIND( mqhess ) / 7  /
      DATA INDIC8( mquadr ) / 'QUADRATIC   ' /, LENIND( mquadr ) / 9  /
      DATA INDIC8( mquads ) / 'QUADS       ' /, LENIND( mquads ) / 5  /
      DATA INDIC8( mquado ) / 'QUADOBJ     ' /, LENIND( mquado ) / 7  /
      DATA INDIC8( mqsect ) / 'QSECTION    ' /, LENIND( mqsect ) / 8  /
      DATA INDIC8( mqmatr ) / 'QMATRIX    '  /, LENIND( mqmatr ) / 7  /
      DATA INDIC8( metype ) / 'ELEMENT TYPE' /, LENIND( metype ) / 12 /
      DATA INDIC8( meuses ) / 'ELEMENT USES' /, LENIND( meuses ) / 12 /
      DATA INDIC8( mgtype ) / 'GROUP TYPE  ' /, LENIND( mgtype ) / 10 /
      DATA INDIC8( mguses ) / 'GROUP USES  ' /, LENIND( mguses ) / 10 /
      DATA INDIC8( mobbnd ) / 'OBJECT BOUND' /, LENIND( mobbnd ) / 12 /
      DATA INDIC8( mendat ) / 'ENDATA      ' /, LENIND( mendat ) / 6  /

!  set initial values for integer variables

      intype = 1
      intypo = 1
      status = 0
      lineno = 0
      nvar = 0
      nnza = 0
      ng = 0
      nbnd = 0
      nstart = 0
      nobj = 0
      neltyp = 0
      ngrtyp = 0
      nelnum = 0
      ngrupe = 0
      nlisev = 0
      nlisep = 0
      nlisgp = 0
      neling = 0
      nusein = 0
      nusere = 0
      nobbnd = 0
      ndtype = 0
      neln = 0
      ninn = 0
      nepn = 0
      nlvars = - 1
      nnlvrs = - 1
      nconst = - 1
      nrange = - 1
      level = 0
      ilines = 0
      nlines = 0
      iptype = 0
      istype = 0

!  set initial values for logical variables

      defnam = .FALSE.
      doloop = .FALSE.
      endbnd = .FALSE.
      endst = .FALSE.
      endelt = .FALSE.
      endelu = .FALSE.
      endgrt = .FALSE.
      strtgu = .FALSE.
      grpyet = .FALSE.
      varyet = .FALSE.
      delset = .FALSE.
      dgrset = .FALSE.
      grp1st = .TRUE.
      fixed = .TRUE.
      qgroup = .FALSE.
      qsqr = .FALSE.
      qprod = .FALSE.

!  set initial values for real variables

      value4 = 0.0D+0
      value6 = 0.0D+0

!  set up itable data

      CALL HASH_initialize( length, ITABLE )

!  initialize row data

      RSCALE( : ngmax ) = one

!  initialize column data

      ITYPEV( : nmax ) = 0
      CSCALE( : nmax ) = one
      BND( 1, : nmax, 1 ) = zero
      BND( 2, : nmax, 1 ) = biginf

!  initialize dictionary data

      INLIST( : length ) = 0

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( i : i ) = ' '
      END DO

!  start of main loop

  100 CONTINUE

!  read next line from the input file

      IF ( ilines + 1 > nlines ) THEN
        lineno = lineno + 1
        nuline = blnkln
        IF ( fixed ) THEN
          READ ( input, 1000, END = 810, ERR = 810 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
        ELSE
          READ ( input, 1010, END = 810, ERR = 810 ) nuline
          IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

          CALL FREE_format( nuline, max_record_length, mendat, INDIC8,         &
                            LENIND, NULINA, maxnul, nlines, .TRUE.,            &
                            status, out )
          IF ( status > 0 ) GO TO 800

!  if there are non-blank lines on the free format card, read the first

          IF ( nlines > 0 ) THEN
            ilines = 1
            nuline = blnkln
            nuline = NULINA( ilines )
            IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline

!  there are only blank lines on the free format card

          ELSE
            GO TO 100
          END IF
        END IF

!  read next line from the last encountered free format card

      ELSE
        ilines = ilines + 1
        nuline = blnkln
        nuline = NULINA( ilines )
        IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1: 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) THEN
        IF (  NULINE( 13: 14 ) == '  ' .AND.                                   &
              NULINE( 15: 24 ) == '          ' .AND.                           &
              NULINE( 40: 49 ) == '          ' ) GO TO 100
      END IF
      IF ( NULINE( 1: 1 ) /= ' ' ) THEN

!  ignore comment cards

        IF ( NULINE( 1: 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

        IF ( header == INDIC8( mfixed ) ) THEN
          fixed = .TRUE.
          GO TO 100
        END IF

!  check if we have entered free-format input

        IF ( header == INDIC8( mfree ) ) THEN
          fixed = .FALSE.
          GO TO 100
        END IF

!  check that the first encountered indicator card is the name card

        IF ( .NOT. defnam  ) THEN
          IF ( header /= INDIC8( mname ) ) THEN
            status = 1
            IF ( out > 0 ) WRITE( out, 2010 )
            GO TO 800

!  indicator card is name
!  -----------------------

          ELSE
            defnam = .TRUE.
            pname = NULINE( 15: 22 )
            GO TO 100
          END IF
        END IF

!  an indicator card has been found

        IF ( .NOT. grp1st ) intype = mrows
        DO i = intype, mendat
          IF ( header == INDIC8( i ) ) THEN
            intype = i
            GO TO 120
          END IF
        END DO

!  the indicator card is not recognised

        status = 2
        IF ( out > 0 ) WRITE( out, 2020 )
        GO TO 800
  120   CONTINUE
        IF ( intype == mgroup .OR. intype == MCNSTR) intype = mrows
        IF ( intype == mrhs .OR. intype == mrhsp ) intype = mconst
        IF ( intype == mvars ) intype = mcols
        IF ( intype == mquadr .OR. intype == mquads .OR.                       &
             intype == mquado .OR. intype == mqsect .OR.                       &
             intype == mqmatr ) intype = mqhess

!  ensure that the groups and variables sections do not get mixed up

        IF ( .NOT. grp1st .AND. varyet .AND. intype == mcols ) THEN
          status = 21
          IF ( out > 0 ) WRITE( out, 2210 )
          GO TO 800
        END IF
        IF ( intype == mrows ) grpyet = .TRUE.
        IF ( intype == mcols ) varyet = .TRUE.
        IF ( varyet .AND. .NOT. grpyet ) grp1st = .FALSE.

!  ensure that previously started do-loops have been finished

        IF ( intype /= intypo .AND. doloop ) THEN
           status = 38
           IF ( out > 0 ) WRITE( out, 2380 )
           GO TO 800
        END IF
        intypo = intype

!  all of the linear variables have been specified

        IF ( intype >= mconst ) THEN
          IF ( nlvars < 0 ) THEN
            nlvars = nvar
            n = nlvars
          END IF
        END IF

!  the right-hand-side vectors have been completed

        IF ( intype >= mrange ) THEN
          IF ( nconst < 0 ) nconst = nvar - nlvars
          IF ( nconst == 0 ) THEN
            nconst = 1
            nvar = nvar + 1
          END IF
        END IF

!  the range vectors have been completed

        IF ( intype >= mbound ) THEN
          IF ( nrange < 0 ) nrange = nvar - nconst - nlvars
        END IF

!  the bound vectors have been completed

        IF ( intype >= mstart ) THEN
          IF ( .NOT. endbnd ) THEN
            endbnd = .TRUE.
            IF ( nbnd == 0 ) nbnd = 1
          END IF
        END IF

!  the starting vectors have been completed

        IF ( intype >= mqhess ) THEN
          IF ( .NOT. endst ) THEN
            endst = .TRUE.
            IF ( nstart == 0 ) nstart = 1
          END IF
        END IF

!  the quadratic Hessian has been completed

        IF ( intype >= metype ) THEN
        END IF

!  the element types have all been specified

        IF ( intype >= meuses ) THEN
          IF ( .NOT. endelt ) THEN
            endelt = .TRUE.

!  if the last element has no explicit internal representation,
!  use its elemental representation

            IF ( neltyp > 0 ) THEN
              IF ( ETYPES( neltyp ) /= cqsqr .AND.                            &
                   ETYPES( neltyp ) /= cqprod ) THEN
                IF ( .NOT. inrep ) THEN
                  DO k = IELV( neltyp ), neln
                    ninn = ninn + 1
                    IF ( ninn > nimax ) THEN
                      status = - 16
                      GO TO 700
                    END IF
                    INAMES( ninn ) = ENAMES( k )
                  END DO
                ELSE
                  IF ( ninn - IINV( neltyp ) >= neln - IELV( neltyp ) ) THEN
                    status = 76
                    IF ( out > 0 ) WRITE( out, 2760 )
                    GO TO 800
                  END IF
                END IF
              END IF
              IF ( neltyp >= nlmax ) THEN
                status = - 3
                GO TO 700
              END IF
            END IF
            IELV( neltyp + 1 ) = neln + 1
            IINV( neltyp + 1 ) = ninn + 1
            IEPA( neltyp + 1 ) = nepn + 1
          END IF
        END IF

!  the nonlinear elements have all been specified

        IF ( intype >= mgtype ) THEN

!  check if there are any nonlinear variables

          IF ( nnlvrs < 0 ) nnlvrs = n - nlvars
          IF ( .NOT. endelu ) THEN
            endelu = .TRUE.

!  check that the nonlinear elements have been completely specified
!  first check the parameter values have been set

            IF ( nelnum > 0 ) THEN
              DO 153 j = 1, nelnum
                k = ITYPEE( j )
                ip = IEPA( k ) - 1
                k1 = IEPA( k + 1 ) - IEPA( k )
                k2 = ISTEP( j ) - 1
                DO i = 1, k1
                  IF ( EPVALU( k2 + i ) == biginf ) THEN
                    status = 28
                    IF ( out > 0 ) WRITE( out, 2280 )                          &
                       LNAMES( j ), EPNAME( ip + i )
                  END IF
                END DO

!  now check the elemental variables have been set

                is = IELV( k ) - 1
                k1 = IELV( k + 1 ) - IELV( k )
                k2 = ISTAEV( j ) - 1
                DO i = 1, k1
                  IF ( IELVAR( k2 + i ) == 0 ) THEN
                    status = 16
                    IF ( out > 0 ) WRITE( out, 2160 )                          &
                       LNAMES( j ), ENAMES( is + i )
                  END IF
                END DO
  153         CONTINUE
            END IF
            IF ( status /= 0 ) RETURN
          END IF
          ISTEP( nelnum + 1 ) = nlisep + 1
          ISTAEV( nelnum + 1 ) = nlisev + 1
        END IF

!  the group types have all been specified

         IF ( intype >= mguses .AND. ngrtyp > 0 ) THEN

!  check that the argument for the last group-type has been set

           IF ( .NOT. endgrt ) THEN
             endgrt = .TRUE.
             IF ( .NOT. setana ) THEN
               status = 25
               IF ( out > 0 ) WRITE( out, 2250 )
               RETURN
             END IF
             IF ( ngrtyp >= ngrmax ) THEN
               status = - 4
               GO TO 700
             END IF
             IGPA( ngrtyp + 1 ) = ngpn + 1
           END IF
         END IF
         IF ( intype == mendat ) THEN

!  check that the groups have been completely specified by
!  checking that the parameter values have been set

           nlisgp = 0
           DO j = 1, ng
             k = ITYPEG( j )
             IF ( k < 0 ) THEN
               k = - k - 1
               ITYPEG( j ) = k
               ISTGP( j ) = nlisgp + 1
               IF ( k /= 0 ) THEN
                 k1 = IGPA( k + 1 ) - IGPA( k )
                 IF ( k1 > 0 ) THEN
                   ip = IGPA( k ) - 1
                   k2 = ISTGP( j ) - 1
                   status = 34
                   DO i = 1, k1
                     nlisgp = nlisgp + 1
                     GPVALU( nlisgp ) = biginf
                     IF ( out > 0 ) WRITE( out, 2340 )                         &
                       GNAMES( j ), GPNAME( ip + i )
                   END DO 
                 END IF 
               END IF 
             ELSE IF ( k == 0 ) THEN
               ISTGP( j ) = nlisgp + 1
             ELSE
               ISTGP( j ) = nlisgp + 1
               ip = IGPA( k ) - 1
               k1 = IGPA( k + 1 ) - IGPA( k )
               k2 = ISTGP( j ) - 1
               DO i = 1, k1
                 nlisgp = nlisgp + 1
                 GPVALU( nlisgp ) = GPTEMP( k2 + i )
                 IF ( GPVALU( nlisgp ) == biginf ) THEN
                   status = 34
                   IF ( out > 0 ) WRITE( out, 2340 )                           &
                     GNAMES( j ), GPNAME( ip + i )
                 END IF
               END DO
             END IF
           END DO
           ISTGP( ng + 1 ) = nlisgp + 1
           IF ( status /= 0 ) RETURN

!  sort the list of elements for each group, so that the
!  elements for group i precede those for group i + 1, i = 1, ng - 1

           IF ( neling > 0 ) THEN
             CALL REORDA( ng, neling, IELING( 1, 1 ), IELING( 1, 2 ),          &
                          WEIGHT, ISTADG, IWK )
           ELSE
             ISTADG( : ng + 1 ) = 1
           END IF
         END IF

!  indicator card is endata
!  -------------------------

         IF ( intype == mendat ) GO TO 900
         GO TO 100
      END IF

!  a data card has been found. Rread the character fields 1, 2, 3 and 5 
!  from the new data line

      field1 = NULINE(  2:  3 )
      field2 = NULINE(  5: 14 )
      field3 = NULINE( 15: 24 )
      field5 = NULINE( 40: 49 )

!  start of a do-loop
! ===================

      IF ( field1 == 'DO' ) THEN
        IF ( level >= 3 ) THEN
          status = 13
          IF ( out > 0 ) WRITE( out, 2130 )
          GO TO 800
        END IF

!  this is the first level of the loop

        IF ( level == 0 ) THEN
          doloop = .TRUE.
          narray = 0
          NINSTR( 1 ) = 0
          NINSTR( 2 ) = 0
          NINSTR( 3 ) = 0

!  this is the second or third level of the loop

        ELSE
          NINSTR( level ) = NINSTR( level ) + 1
          IF ( NINSTR( level ) > maxins ) THEN
            status = - 11
            GO TO 700
          END IF
          IF ( debug .AND. out > 0 ) WRITE( out, 4010 ) level, NINSTR( level )
          INSTR( 1, NINSTR( level ), level ) = 1
        END IF

!  record the location of the do-loop variable in the array inlist

        field = FIELD2( 1 : 10 ) // 'II'
        CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
        IF ( ifree <= 0 ) THEN
          IF ( ifree == 0 ) GO TO 700
          ifree = - ifree
        ELSE
          nusein = nusein + 1
          IF ( nusein > nindex ) THEN
            status = - 21
            GO TO 700
          END IF
          INLIST( ifree ) = nusein
          NAMIIN( nusein ) = FIELD( 1: 7 )
        END IF
        IF ( level == 0 ) THEN
          LOOP( 1 ) = INLIST( ifree )
        ELSE
          INSTR( 2, NINSTR( level ), level ) = INLIST( ifree )
        END IF

!  record the starting value of the do-loop variable

        field = FIELD3( 1 : 10 ) // 'II'
        CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
        IF ( ifield <= 0 ) THEN
          status = 3
          IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1: 10 )
          GO TO 800
        END IF
        IF ( level == 0 ) THEN
          LOOP( 2 ) = INLIST( ifield )
        ELSE
          INSTR( 3, NINSTR( level ), level ) = INLIST( ifield )
        END IF

!  record the finishing value of the do-loop variable and
!  set the incremental value of the do-loop variable to 1

        field = FIELD5( 1 : 10 ) // 'II'
        CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
        IF ( ifield <= 0 ) THEN
          status = 3
          IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
          GO TO 800
        END IF
        IF ( level == 0 ) THEN
          LOOP( 3 ) = INLIST( ifield )
          LOOP( 4 ) = - 1
        ELSE
          INSTR( 4, NINSTR( level ), level ) = INLIST( ifield )
          INSTR( 5, NINSTR( level ), level ) = - 1
        END IF
        level = level + 1
        GO TO 100
      END IF

!  a do-loop variable is to have a non-trivial increment

      IF ( field1 == 'DI' ) THEN

!  record the location of the do-loop variable in the array inlist

        adddoloop = .FALSE.
        IF ( level == 1 ) THEN
          adddoloop = ( FIELD2( 1: 10 ) == NAMIIN( LOOP( 1 ) ) )
        ELSE IF( level > 1 ) THEN
          adddoloop = ( FIELD2( 1: 10 ) ==                                     &
            NAMIIN( INSTR( 2, NINSTR( level - 1 ), level - 1 ) ) )
        ENDIF
        IF ( adddoloop ) THEN
          IF ( debug .AND. out > 0 .AND. level > 1 )                           &
            WRITE( out, 4030 ) level - 1, NINSTR( level - 1 )
          field = FIELD3( 1 : 10 ) // 'II'
          CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
          IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
            GO TO 800
          END IF
          IF ( level == 1 ) THEN
            LOOP( 4 ) = INLIST( ifield )
          ELSE
            INSTR( 5, NINSTR( level - 1 ), level - 1 ) = INLIST( ifield )
          END IF
        END IF
        GO TO 100
      END IF

!  end of one or more do-loops
! ============================

      IF ( field1 /= 'OD' .AND. field1 /= 'ND' ) GO TO 341

!  terminate the current level of loop

      IF ( field1 == 'OD' ) THEN
        NINSTR( level ) = NINSTR( level ) + 1
        IF ( NINSTR( level ) > maxins ) THEN
           status = - 11
           GO TO 700
        END IF
        INSTR( 1, NINSTR( level ), level ) = 2
        IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) level, NINSTR( level )
        level = level - 1
      ELSE
        DO i = level, 1, - 1
          NINSTR( level ) = NINSTR( level ) + 1
          IF ( NINSTR( level ) > maxins ) THEN
            status = - 11
            GO TO 700
          END IF
          INSTR( 1, NINSTR( level ), level ) = 2
          IF ( debug .AND. out > 0 ) WRITE( out, 4020 ) level, NINSTR( level )
          level = level - 1
        END DO
      END IF

!  execute do-loop instructions
! =============================

      IF ( level /= 0 ) GO TO 339

!  execute level-1 do-loop instructions

        lev1s = INDVAL( LOOP( 2 ) )
        lev1e = INDVAL( LOOP( 3 ) )
        IF ( LOOP( 4 ) <= 0 ) THEN
          lev1i = 1
        ELSE
          lev1i = INDVAL( LOOP( 4 ) )
        END IF

!  mock do-loop

          lev1 = lev1s
  220     CONTINUE
          IF ( .NOT. ( ( lev1i > 0 .AND. lev1 <= lev1e ) .OR.                  &
               ( lev1i < 0 .AND. lev1 >= lev1e ) ) ) GO TO 337
          l1 = 0 ; l2 = 0 ; l3 = 0

!  set the loop value

          INDVAL( LOOP( 1 ) ) = lev1
          IF ( debug .AND. out > 0 ) WRITE( out, 5000 )                        &
            1, NAMIIN( LOOP( 1 ) ), lev1

!  execute the remaining list of level-1 instructions

  230     CONTINUE
          l1 = l1 + 1

!  see if the level-1 loop is to be terminated

          IF ( INSTR( 1, l1, 1 ) == 2 ) GO TO 300

!  see if a level-2 loop is to be started

          IF ( INSTR( 1, l1, 1 ) /= 1 ) GO TO 295

!  execute level-2 do-loop instructions

          lev2s = INDVAL( INSTR( 3, l1, 1 ) )
          lev2e = INDVAL( INSTR( 4, l1, 1 ) )
          IF ( INSTR( 5, l1, 1 ) <= 0 ) THEN
            lev2i = 1
          ELSE
            lev2i = INDVAL( INSTR( 5, l1, 1 ) )
          END IF
          level2 = l2
          levl3a = l3

!  mock do-loop

          lev2 = lev2s
  240     CONTINUE
          l2 = level2
          IF ( .NOT. ( lev2i > 0 .AND. lev2 <= lev2e ) .OR.                    &
             ( lev2i < 0 .AND. lev2 >= lev2e ) ) GO TO 292
          l3 = levl3a

!  set the loop value

          INDVAL( INSTR( 2, l1, 1 ) ) = lev2
          IF ( debug .AND. out > 0 ) WRITE( out, 5000 )                        &
                  2, NAMIIN( INSTR( 2, l1, 1 ) ), lev2

!  execute the remaining list of level-2 instructions

  250     CONTINUE
          l2 = l2 + 1

!  see if the level-2 loop is to be terminated

          IF ( INSTR( 1, l2, 2 ) == 2 ) GO TO 290

!  see if a level-3 loop is to be started

          IF ( INSTR( 1, l2, 2 ) /= 1 ) GO TO 283

!  execute level-3 do-loop instructions

          lev3s = INDVAL( INSTR( 3, l2, 2 ) )
          lev3e = INDVAL( INSTR( 4, l2, 2 ) )
          IF ( INSTR( 5, l2, 2 ) <= 0 ) THEN
            lev3i = 1
          ELSE
            lev3i = INDVAL( INSTR( 5, l2, 2 ) )
          END IF
          level3 = l3

!  mock do-loop

          lev3 = lev3s
  260     CONTINUE
          l3 = level3
          IF ( .NOT.  ( lev3i > 0 .AND. lev3 <= lev3e ) .OR.                   &
                      ( lev3i < 0 .AND. lev3 >= lev3e ) ) GO TO 281

!  set the loop value

          INDVAL( INSTR( 2, l2, 2 ) ) = lev3
          IF ( debug .AND. out > 0 )                                           &
             WRITE( out, 5000 ) 3, NAMIIN( INSTR( 2, l2, 2 ) ), lev3

!  execute the remaining list of level-3 instructions

  270     CONTINUE
          l3 = l3 + 1

!  see if the level-3 loop is to be terminated

          IF ( INSTR( 1, l3, 3 ) == 2 ) GO TO 280

!  execute level-3 index instructions

          IF ( INSTR( 1, l3, 3 ) >= 21 .AND. INSTR( 1, l3, 3 ) <= 50 ) THEN
            CALL GETIIN( nindex, INDVAL, nrlndx, REALVL, INSTR( 1, l3, 3 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 3, l3,               &
              NAMIIN( INSTR( 2, l3, 3 ) ), INDVAL( INSTR( 2, l3, 3 ) )
          END IF
          IF ( INSTR( 1, l3, 3 ) >= 51 .AND. INSTR( 1, l3, 3 ) <= 99 ) THEN
            CALL GETRIN( nindex, nrlndx, INDVAL, REALVL, RVALUE( l3, 3 ),      &
                         INSTR( 1, l3, 3 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 ) WRITE( out, 5020 ) 3, l3,               &
                NAMRIN( INSTR( 2, l3, 3 ) ), REALVL( INSTR( 2, l3, 3 ) )
          END IF
          IF ( INSTR( 1, l3, 3 ) >= 100 ) THEN
            narray = INSTR( 2, l3, 3 )
            CALL GETLIN( nindex, nrlndx, INDVAL, IARRAY( 1, 1, narray ),       &
                         VARRAY( 1, narray ), ARRAY( 1, narray ),              &
                         CARRAY( 1, narray ), FARRAY( narray ), REALVL,        &
                         NAMIIN, novals, INSTR( 1, l3, 3 ), field1,            &
                         field2, field3, value4, field5, value6, out,          &
                         status, length, KEY, ITABLE, INLIST )
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
                   3, l3, field1, field2, field3, value4, field5, value6
            ijump = 3
            GO TO 400
          END IF
          GO TO 270
  280     CONTINUE
          lev3 = lev3 + lev3i
          GO TO 260

!  the do-loop is not executed. find the next relevant instruction

  281     CONTINUE
          l3 = l3 + 1
          IF ( INSTR( 1, l3, 3 ) /= 2 ) GO TO 281

!  end of level-3 do-loop

  283     CONTINUE

!  execute level-2 index instructions

          IF ( INSTR( 1, l2, 2 ) >= 21 .AND. INSTR( 1, l2, 2 ) <= 50 ) THEN
            CALL GETIIN( nindex, INDVAL, nrlndx, REALVL, INSTR( 1, l2, 2 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 2, l2,               &
                  NAMIIN( INSTR( 2, l2, 2 ) ), INDVAL( INSTR( 2, l2, 2 ) )
          END IF
          IF ( INSTR( 1, l2, 2 ) >= 51 .AND. INSTR( 1, l2, 2 ) <= 99 ) THEN
            CALL GETRIN( nindex, nrlndx, INDVAL, REALVL, RVALUE( l2, 2 ),      &
                         INSTR( 1, l2, 2 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 ) WRITE( out, 5020 ) 2, l2,               &
               NAMRIN( INSTR( 2, l2, 2 ) ), REALVL( INSTR( 2, l2, 2 ) )
          END IF
          IF ( INSTR( 1, l2, 2 ) >= 100 ) THEN
            narray = INSTR( 2, l2, 2 )
            CALL GETLIN( nindex, nrlndx, INDVAL, IARRAY( 1, 1, narray ),       &
                         VARRAY( 1, narray ), ARRAY( 1, narray ),              &
                         CARRAY( 1, narray ), FARRAY( narray ), REALVL,        &
                         NAMIIN, novals, INSTR( 1, l2, 2 ), field1,            &
                         field2, field3, value4, field5, value6, out,          &
                         status, length, KEY, ITABLE, INLIST )
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
                  2, l2, field1, field2, field3, value4, field5, value6
            ijump = 2
            GO TO 400
          END IF
          GO TO 250
  290     CONTINUE
          lev2 = lev2 + lev2i
          GO TO 240

!  the do-loop is not executed. find the next relevant instruction

  292     CONTINUE
          l2 = l2 + 1
          IF ( INSTR( 1, l2, 2 ) /= 2 ) GO TO 292
          level2 = l2

!  end of level-2 do-loop

  295     CONTINUE

!  execute level-1 index instructions

          IF ( INSTR( 1, l1, 1 ) >= 21 .AND. INSTR( 1, l1, 1 ) <= 50 ) THEN
            CALL GETIIN( nindex, INDVAL, nrlndx, REALVL, INSTR( 1, l1, 1 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 1, l1,               &
              NAMIIN( INSTR( 2, l1, 1 ) ), INDVAL( INSTR( 2, l1, 1 ) )
          END IF
          IF ( INSTR( 1, l1, 1 ) >= 51 .AND. INSTR( 1, l1, 1 ) <= 99 ) THEN
            CALL GETRIN( nindex, nrlndx, INDVAL, REALVL, RVALUE( l1, 1 ),      &
                         INSTR( 1, l1, 1 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 ) WRITE( out, 5020 ) 1, l1,               &
                NAMRIN( INSTR( 2, l1, 1 ) ), REALVL( INSTR( 2, l1, 1 ) )
          END IF
          IF ( INSTR( 1, l1, 1 ) >= 100 ) THEN
            narray = INSTR( 2, l1, 1 )
            CALL GETLIN( nindex, nrlndx, INDVAL, IARRAY( 1, 1, narray ),       &
                          VARRAY( 1, narray ), ARRAY( 1, narray ),             &
                          CARRAY( 1, narray ), FARRAY( narray ), REALVL,       &
                          NAMIIN, novals, INSTR( 1, l1, 1 ), field1,           &
                          field2, field3, value4, field5, value6, out,         &
                          status, length, KEY, ITABLE, INLIST )
             IF ( status > 0 ) GO TO 800
             IF ( status < 0 ) GO TO 700
             IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                     &
               1, l1, field1, field2, field3, value4, field5, value6
             ijump = 1
             GO TO 400
          END IF
          GO TO 230
  300     CONTINUE
          lev1 = lev1 + lev1i
          GO TO 220
  337     CONTINUE   

!  end of level-1 do-loop

          doloop = .FALSE.
  339   CONTINUE
        GO TO 100
  341 CONTINUE

!  construct a list of do-loop instructions: 1) arithmetic instructions
! =====================================================================

      IF ( doloop ) THEN
        NINSTR( level ) = NINSTR( level ) + 1
        IF ( NINSTR( level ) > maxins ) THEN
          status = - 11
          GO TO 700
        END IF

!  an arithmetic instruction is to be processed

        IF ( field1 == 'IE' .OR. field1 == 'IA' .OR.                           &
             field1 == 'IS' .OR. field1 == 'IM' .OR.                           &
             field1 == 'ID' .OR. field1 == 'IR' .OR.                           &
             field1 == 'I=' .OR. field1 == 'I+' .OR.                           &
             field1 == 'I-' .OR. field1 == 'I*' .OR.                           &
             field1 == 'I/' .OR. field1 == 'RE' .OR.                           &
             field1 == 'RA' .OR. field1 == 'RS' .OR.                           &
             field1 == 'RM' .OR. field1 == 'RD' .OR.                           &
             field1 == 'RI' .OR. field1 == 'RF' .OR.                           &
             field1 == 'R=' .OR. field1 == 'R+' .OR.                           &
             field1 == 'R-' .OR. field1 == 'R*' .OR.                           &
             field1 == 'R/' .OR. field1 == 'R(' ) THEN
          CALL PROCAI( nindex, nrlndx, length, nusein, nusere,                 &
                       status, out, level, NINSTR( level ),                    &
                       debug, RVALUE( NINSTR( level ), level ),                &
                       INLIST, ITABLE, NAMIIN, NAMRIN,                         &
                       INSTR( 1, NINSTR( level ), level ), KEY,                &
                       field1, field2, field3, field5,                         &
                       NULINE( 25: 36 ) )
          IF ( status > 0 ) GO TO 800
          IF ( status < 0 ) GO TO 700

!  construct a list of do-loop instructions: 2) array definitions
! ===============================================================

        ELSE
          IF ( FIELD1( 1: 1 ) /= 'X' .AND. FIELD1( 1: 1 ) /= 'Z'               &
               .AND. FIELD1( 1: 1 ) /= 'A' ) THEN
             status = 6
             IF ( out > 0 ) WRITE( out, 2060 )
             GO TO 800
          END IF
          narray = narray + 1
          IF ( narray > maxara ) THEN
             status = - 12
             GO TO 700
          END IF
          CALL PROCAD( nindex, nrlndx, level, NINSTR( level ),                 &
                       nusere, length, narray, intype, status, out,            &
                       debug, grp1st,                                          &
                       field1, field2, field3, field5,                         &
                       NULINE( 25 : 36 ), NULINE( 50 : 61 ),                   &
                       INLIST,                                                 &
                       INSTR( 1, NINSTR( level ), level ), ITABLE,             &
                       IARRAY( 1, 1, narray ), VARRAY( 1, narray ),            &
                       FARRAY( narray ), NAMIIN, NAMRIN,                       &
                       ARRAY( 1, narray ), CARRAY( 1, narray ), KEY )
          IF ( status > 0 ) GO TO 800
          IF ( status < 0 ) GO TO 700
        END IF

!  the array definition is complete

        GO TO 100

!  execute a non-do-loop instruction
! ==================================

      ELSE

!  the instruction is an array instruction

        IF ( field1 == 'IE' .OR. field1 == 'IA' .OR.                           &
             field1 == 'IS' .OR. field1 == 'IM' .OR.                           &
             field1 == 'ID' .OR. field1 == 'IR' .OR.                           &
             field1 == 'I=' .OR. field1 == 'I+' .OR.                           &
             field1 == 'I-' .OR. field1 == 'I*' .OR.                           &
             field1 == 'I/' .OR. field1 == 'RE' .OR.                           &
             field1 == 'RA' .OR. field1 == 'RS' .OR.                           &
             field1 == 'RM' .OR. field1 == 'RD' .OR.                           &
             field1 == 'RI' .OR. field1 == 'RF' .OR.                           &
             field1 == 'R=' .OR. field1 == 'R+' .OR.                           &
             field1 == 'R-' .OR. field1 == 'R*' .OR.                           &
             field1 == 'R/' .OR. field1 == 'R(' ) THEN

!  1) an arithmetic instruction. decode the instruction

          CALL PROCAI( nindex, nrlndx, length, nusein, nusere,                 &
                         status, out, 0, 1, debug, RVALUE( 1, 1 ),             &
                         INLIST, ITABLE, NAMIIN, NAMRIN,                       &
                         INSTR( 1, 1, 1 ), KEY,                                &
                         field1, field2, field3, field5,                       &
                         NULINE( 25: 36 ) )
          IF ( status > 0 ) GO TO 800
          IF ( status < 0 ) GO TO 700

!  execute the instruction

          IF ( FIELD1( 1 : 1 ) == 'I' ) THEN
            CALL GETIIN( nindex, INDVAL, nrlndx, REALVL, INSTR( 1, 1, 1 ) )
            IF ( debug .AND. out > 0 ) WRITE( out, 5010 ) 0, 1,                &
                NAMIIN( INSTR( 2, 1, 1 ) ), INDVAL( INSTR( 2, 1, 1 ) )
          ELSE
            CALL GETRIN( nindex, nrlndx, INDVAL, REALVL,                       &
                         RVALUE( 1, 1 ), INSTR( 1, 1, 1 ), status )
            IF ( status > 0 ) GO TO 800
            IF ( debug .AND. out > 0 )                                         &
              WRITE( out, 5020 ) 0, 1, NAMRIN( INSTR( 2, 1, 1 ) ),             &
              REALVL( INSTR( 2, 1, 1 ) )
          END IF
          GO TO 100
        ELSE
          IF ( FIELD1( 1 : 1 ) == 'X' .OR. FIELD1( 1: 1 ) == 'Z' .OR.          &
               FIELD1( 1 : 1 ) == 'A' ) THEN

!  2) an array definition. decode the instruction

            CALL PROCAD( nindex, nrlndx, 0, 1, nusere,                         &
                         length, 1, intype, status, out,                       &
                         debug, grp1st,                                        &
                         field1, field2, field3, field5,                       &
                         NULINE( 25 : 36 ), NULINE( 50 : 61 ),                 &
                         INLIST, INSTR( 1, 1, 1 ), ITABLE,                     &
                         IARRAY( 1, 1, 1 ), VARRAY( 1, 1 ),                    &
                         FARRAY( 1 ), NAMIIN, NAMRIN,                          &
                         ARRAY( 1, 1 ), CARRAY( 1, 1 ), KEY )
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700

!  execute the instruction

            CALL GETLIN( nindex, nrlndx, INDVAL,                               &
                         IARRAY( 1, 1, 1 ), VARRAY( 1, 1 ),                    &
                         ARRAY( 1, 1 ), CARRAY( 1, 1 ),                        &
                         FARRAY( 1 ), REALVL, NAMIIN, novals,                  &
                         INSTR( 1, 1, 1 ), field1,                             &
                         field2, field3, value4,                               &
                         field5, value6, out, status,                          &
                         length, KEY, ITABLE, INLIST )
            IF ( status > 0 ) GO TO 800
            IF ( status < 0 ) GO TO 700
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
               0, 1, field1, field2, field3, value4, field5, value6
               GO TO 400

!  the instruction is not an array instruction
!  check to see if there is are any numerical values to be read

          ELSE
            novals = 0
            IF ( ( field3 /= '          ' .AND.                                &
                   NULINE( 15: 15 ) /= '$' ) .OR. intype == mobbnd ) THEN
              novals = novals + 1
              IF (   intype <= mqhess .OR. intype == mobbnd .OR.               &
                ( ( intype == meuses .OR. intype == mguses )                   &
                    .AND. field1 == 'P ' ) .OR. ( intype == mguses             &
                          .AND. field1 == 'E ' ) ) THEN
                IF ( intype == mguses .AND. field1 == 'E '.AND.                &
                  NULINE( 25: 36 ) == '            ' ) THEN
                  value4 = one
                ELSE
                   CALL GETVAL( NULINE( 25: 36 ), value4 )
                END IF
                IF ( intype == mrange ) value4 = ABS( value4 )
              END IF
              IF ( field5 /= '          ' .AND. NULINE( 40: 40 ) /= '$' ) THEN
                novals = novals + 1
                IF (  intype <= mqhess .OR.                                    &
                    ( ( intype == meuses .OR. intype == mguses )               &
                         .AND. field1 == 'P ' ) .OR. ( intype ==               &
                              mguses .AND. field1 == 'E ' ) ) THEN
                  IF ( intype == mguses .AND. field1 == 'E ' .AND.             &
                        NULINE( 50: 61 ) == '            ' ) THEN
                     value6 = one
                  ELSE
                    CALL GETVAL( NULINE( 50: 61 ), value6 )
                  END IF
                  IF ( intype == mrange ) value6 = ABS( value6 )
                  IF ( intype < mconst ) THEN
                    IF ( value6 == zero ) novals = 1
                  END IF
                END IF
              END IF

!  remove fields with numerical values of zero

              IF ( intype < mconst ) THEN
                IF ( value4 == zero ) THEN
                  IF ( novals == 2 ) THEN
                    value4 = value6
                    field3 = field5
                  END IF
                  novals = novals - 1
                END IF
              END IF
              IF ( field3 == '''SCALE''   ' .OR.  field3 == ' ''SCALE''  ') THEN
                novals = 0
                value4 = ABS( value4 )
              END IF
            END IF
            IF ( debug .AND. out > 0 ) WRITE( out, 5060 )                      &
              0, 1, field1, field2, field3, value4, field5, value6
          END IF
        END IF
      END IF
  400 CONTINUE

!  execute real parameter array card

      IF ( field1 == 'AE' .OR. field1 == 'AA' .OR.                             &
           field1 == 'AS' .OR. field1 == 'AM' .OR.                             &
           field1 == 'AD' .OR. field1 == 'AI' .OR.                             &
           field1 == 'AF' .OR. field1 == 'A=' .OR.                             &
           field1 == 'A+' .OR. field1 == 'A-' .OR.                             &
           field1 == 'A*' .OR. field1 == 'A/' .OR.                             &
           field1 == 'A(' ) THEN
        CALL PROCAA( nindex, length, nusere, status, out,                      &
                     nrlndx, INLIST, ITABLE,                                   &
                     NAMRIN, KEY, INDVAL, REALVL,                              &
                     field1, field2, field3, field5, value4 )
        IF ( status > 0 ) GO TO 800
        IF ( status < 0 ) GO TO 700
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF

!  branch depending on the current indicator card
! ===============================================

      GO TO ( 100, 100, 100, 100, 420, 420, 420, 430, 430, 430,                &
              430, 430, 430, 440, 450, 455, 455, 455, 455, 455,                &
              455, 460, 470, 480, 490, 500, 900 ), intype

!  indicator card is groups/rows/constraints
!  ------------------------------------------

  420 CONTINUE
      IF ( grp1st ) THEN
        CALL SGRP1( ng, ngmax, nomax, length, nobj, novals, INLIST, IDROWS,    &
                    ISTATE, ITYPEG, ITABLE, RDROWS, RSCALE, field1, field2,    &
                    field3, value4, field5, value6, GNAMES, ONAMES, KEY,       &
                    out, status )
      ELSE
        CALL SGRP2( ng, ngmax, nomax, la, length, nnza, nobj, novals, INLIST,  &
                    IDROWS, ISTATE, ITYPEG, ITABLE, ICOORD, A, RDROWS,         &
                    RSCALE, field1, field2, field3, value4, field5, value6,    &
                    GNAMES, ONAMES, KEY, out, status )
      END IF
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is columns/variables, constants/rhs/rhs' or ranges
!  ------------------------------------------------------------------

  430 CONTINUE
      IF ( intype == mcols  ) colfie = 'VA'
      IF ( intype == mconst ) colfie = 'CO'
      IF ( intype == mrange ) colfie = 'RA'
      IF ( grp1st .OR. intype /= mcols ) THEN
        CALL SVAR2( nmax, ngmax, la, length, nnza, nvar, novals, nrlndx,       &
                    intype == mcols, colfie, ICOORD, ISTATE, ITYPEV, INLIST,   &
                    ITABLE, A, CSCALE, REALVL, DFAULT, field1, field2,         &
                    field3, value4, field5, value6, VNAMES, KEY, out, status )
      ELSE
        CALL SVAR1( nmax, length, nvar, colfie, ITYPEV, INLIST, ITABLE,        &
                    CSCALE, field2, field3, value4, VNAMES, KEY, status )
      END IF
      IF ( status == 0 ) THEN
         IF ( doloop ) GO TO 600
         GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is bounds
!  -------------------------

  440 CONTINUE
      CALL SBOUND( nmax, nbmax, length, nlvars, nbnd, ncol, nrlndx, defaut,    &
                   INLIST, ITABLE, BND, REALVL, field1, field2, field3,        &
                   value4, field5, BNAMES, BNDFLT, KEY, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is start point
!  ------------------------------

  450 CONTINUE
      CALL SSTART( nmax, ngmax, nsmax, length, nlvars, ng, nstart, ncol,       &
                   nrlndx, defaut, INLIST, ITABLE, VSTART, CSTART, REALVL,     &
                   field1, field2, field3, value4, field5, value6, SNAMES,     &
                   novals, KEY, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is qhess
!  ------------------------

  455 CONTINUE
      CALL SQHESS( negmax, ngmax, nlmax, nelmax, nevmax, netmax, nomax, nimax, &
                   length, ng, nobj, ngrupe, novals, neln, ninn, nepn, neltyp, &
                   nlisep, nlisev, nelnum, neling, qgroup, qsqr, qprod, IELV,  &
                   IINV, IEPA, ITYPEG, IELING, ISTAEV, IELVAR, INLIST, ITABLE, &
                   ISTATE, ITYPEE, ISTEP, iptype, istype, inrep, field1,       &
                   field2, field3, value4, field5, value6, ENAMES, GNAMES,     &
                   ETYPES, ONAMES, INAMES, lnames, WEIGHT, KEY, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is element type
!  -------------------------------

  460 CONTINUE
      CALL SETYPE( nlmax, nimax, netmax, nepmax, length, novals, neln, ninn,   &
                   nepn, neltyp, inrep, IELV, IINV, IEPA, INLIST, ITABLE,      &
                   field1, field2, field3, field5, ENAMES, INAMES, EPNAME,     &
                   ETYPES, KEY, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is element uses
!  --------------------------------

  470 CONTINUE
      CALL SEUSES( nlmax, nelmax, netmax, nevmax, nlisev, nlisep, novals,      &
                   nepmax, nepvmx, length, nelnum, nelmnt, nmax, n, elmnt,     &
                   IELV, IEPA, ITYPEE, IELVAR, INLIST, ITABLE, ISTAEV, ISTEP,  &
                   delset, detype, field1, field2, field3, value4, field5,     &
                   value6, EPVALU, ENAMES, lnames, EPNAME, VNAMES, KEY, out,   &
                   status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is group type
!  -----------------------------

  480 CONTINUE
      CALL SGTYPE( ngrmax, ngpmax, novals, length, ngrtyp, ngpn, setana,       &
                    INLIST, IGPA, ITABLE, field1, field2, field3, field5,      &
                    ANAMES, GTYPES, GPNAME, KEY, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is group uses
!  -----------------------------

  490 CONTINUE
      CALL SGUSES( negmax, ngpmax, ngrmax, ngmax, ngpvmx, length, ng, ngrupe,  &
                   nlisgp, novals, neling, ndtype, strtgu, grupe, IGPA,        &
                   ITYPEG, IELING, INLIST, ITABLE, ISTGP, ISTATE, dgrset,      &
                   dgtype, field1, field2, field3, value4, field5, value6,     &
                   GPTEMP, GPNAME, WEIGHT, KEY, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  indicator card is object bound
!  -------------------------------

  500 CONTINUE
      CALL SOBBND( nobbnd, nobmax, nrlndx, length, INLIST, ITABLE, FBOUND,     &
                   REALVL, field1, field2, value4, field5, OBNAME, KEY,        &
                   single, out, status )
      IF ( status == 0 ) THEN
        IF ( doloop ) GO TO 600
        GO TO 100
      END IF
      IF ( status > 0 ) GO TO 800
      GO TO 700

!  branch back into do loops at appropriate point

  600 CONTINUE
      IF ( ijump == 1 ) THEN
        GO TO 230
      ELSE IF ( ijump == 2 ) THEN
        GO TO 250
      ELSE
        GO TO 270
      END IF

!  insufficient space

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 ) INCRSE( - status )
      RETURN

!  data card inconsistency

  800 CONTINUE
      IF ( out > 0 ) THEN
        IF ( doloop ) THEN
          WRITE( out, 2960 )                                                   &
            lineno, field1, field2, field3, value4, field5, value6
        ELSE
          WRITE( out, 2990 ) lineno, nuline
        END IF
      END IF
      GO TO 960

!  missing/incomplete data card

  810 CONTINUE
      IF ( .NOT. defnam ) THEN
        status = 74
        IF ( out > 0 ) WRITE( out, 2740 )
      ELSE
        status = 75
        IF ( out > 0 ) WRITE( out, 2750 )
      END IF
      GO TO 960

!  successful return

  900 CONTINUE
      status = 0
      IF ( debug .AND. out > 0 ) THEN
        WRITE( out, 3000 ) ( GNAMES( j ), j = 1, ng )
        WRITE( out, 3010 ) ( VNAMES( j ), j = 1, n )
        WRITE( out, 3020 )                                                    &
            ( ICOORD( j, 1 ), ICOORD( j, 2 ), A( j ), j = 1, nnza )
        WRITE( out, 3030 )                                                    &
         ( ( i, j, BND( 1, j, i ), BND( 2, j, i ), j = 1, nlvars ),           &
             i =1, nbnd )
        WRITE( out, 3100 )                                                    &
         ( ( i, j, VSTART( j, i ), j = 1, nlvars ), i = 1, nstart )
        WRITE( out, 3040 ) ( RSCALE( j ), j = 1, ng )
        WRITE( out, 3050 ) ( CSCALE( j ), j = 1, n )
        IF ( neltyp > 0 )                                                     &
           WRITE( out, 3060 ) ( ETYPES( i ), IELV( i + 1 ) - IELV( I),        &
                                 IINV( i + 1 ) - IINV( i ),                   &
                                 IEPA( i + 1 ) - IEPA( i ),                   &
                                 i = 1, neltyp )
        IF ( ngrtyp > 0 )                                                     &
           WRITE( out, 3110 ) ( GTYPES( i ), ANAMES( i ),                     &
                                 IGPA( i + 1 ) - IGPA( i ),                   &
                                 i = 1, ngrtyp )
        WRITE( out, 3070 )
        DO i = 1, ng
          k1 = ISTADG( i )
          k2 = ISTADG( i + 1 ) - 1
          is = ITYPEG( i )
          IF ( k1 <= k2 ) THEN
            IF ( is == 0 ) THEN
              DO k = k1, k2
                l = IELING( k, 1 )
                WRITE( out, 3080 ) GNAMES( i ), 'TRIVIAL   ', LNAMES( l ),     &
                  ETYPES( ITYPEE( l ) ), ( VNAMES( IELVAR( j ) ),              &
                  j = ISTAEV( l ), ISTAEV( l + 1 ) - 1 )
              END DO
            ELSE
              DO k = k1, k2
                l = IELING( k, 1 )
                WRITE( out, 3080 ) GNAMES( i ), GTYPES( is ), LNAMES( l ),     &
                  ETYPES( ITYPEE( l ) ), ( VNAMES( IELVAR( j ) ),              &
                  j = ISTAEV( l ), ISTAEV( l + 1 ) - 1 )
              END DO
            END IF
          ELSE
            IF ( is == 0 ) THEN
              WRITE( out, 3090 ) GNAMES( i ), 'TRIVIAL   '
            ELSE
              WRITE( out, 3090 ) GNAMES( i ), GTYPES( is )
            END IF
          END IF
        END DO
      END IF
  960 CONTINUE
      IF ( debug .AND. out > 0 ) THEN
        DO i = 1, nusein
           WRITE( out, 4000 ) i, NAMIIN( i ), INDVAL( i )
        END DO
        DO i = 1, nusere
           WRITE( out, 4100 ) i, NAMRIN( i ), REALVL( i )
        END DO
      END IF
      RETURN

!  non-executable statements

 1000 FORMAT( A65 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from GPSMPS - insufficient space.',                    &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from GPSMPS - first card is not NAME ' )
 2020 FORMAT( ' ** Exit from GPSMPS - indicator card not recognised ' )
 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2060 FORMAT( ' ** Exit from GPSMPS - non array defn. within do-loop ' )
 2130 FORMAT( ' ** Exit from GPSMPS - do loop level greater than 3 ' )
 2160 FORMAT( ' ** Exit from GPSMPS - element ', A10, ' variable ',            &
              A10, ' not set ' )
 2210 FORMAT( ' ** Exit from GPSMPS -',                                        &
              ' groups and variables sections mixed')
 2250 FORMAT( ' ** Exit from GPSMPS - no group-type arg. given ' )
 2280 FORMAT( ' ** Exit from GPSMPS - element ', A10, ' parameter ',           &
              A10, ' not set ' )
 2340 FORMAT( ' ** Exit from GPSMPS - group ', A10, ' parameter ',             &
              A10, ' not set ' )
 2380 FORMAT( ' ** Exit from GPSMPS - do loop not completed ' )
 2740 FORMAT( ' ** Exit from GPSMPS - data file empty ' )
 2750 FORMAT( ' ** Exit from GPSMPS - data file incomplete.',                  &
              ' No ENDATA card ' )
 2760 FORMAT( ' ** Exit from GPSMPS - #internal vars >= #elementals' )
 2960 FORMAT( /, ' From within do loop ending on line ', i5,                   &
              ', current line is ', /,                                         &
              2X, A2, 1X, A10, A10, 1P, D12.4, 3X, A10, D12.4 )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( /, ' Row names ', /, ' --------- ', /, 8( 1X, A8 ) )
 3010 FORMAT( /, ' Column names ', /, ' ------------', /, 8( 1X, A8 ) )
 3020 FORMAT( /, 3('  Col   Row    Value  '),                                  &
              /, 3('  ---   ---    -----  '),                                  &
              /, ( 3( 2I5, 1P, D12.4 ) ) )
 3030 FORMAT( /, 2(' No. var.  Lower bnd   upper bnd '),                       &
              /, 2(' --- ----  ---------   --------- '),                       &
              /,  ( 2( I3, I6, 1P, 2E12.4 ) ) )
 3040 FORMAT( /, ' Row scaling ', /, ( 1P, D12.4 ) )
 3050 FORMAT( /, ' Column scaling ', /, ( 1P, D12.4 ) )
 3060 FORMAT( /, '    Element type No. el. vars. No. in. vars.',               &
                 ' No. parameters ',                                           &
              /, '    ------------ ------------- ------------- ',              &
                 ' -------------- ', /, ( 5X, A10, I12, 2( 2X, I12 ) ) )
 3070 FORMAT( /, ' Group      Gr. type    Element   El. type',                 &
                 '     Variables ',                                            &
              /, ' -----      --------    -------   --------',                 &
                 '     --------- ' )
 3080 FORMAT( 1X, A10, 1X, A10, 2X, A10, A10, 4X, 5A10,                        &
              /, ( 48X, 5A10 ) )
 3090 FORMAT( 1X, A10, 1X, A10, 2X, '   -    ' )
 3100 FORMAT( /, 2(' No. var.  Start point '),                                 &
              /, 2(' --- ----  ----------- '),                                 &
              /,  ( 2( i3, i6, 1P, D12.4 ) ) )
 3110 FORMAT( /, '    Group type   Argument   No. parameters',                 &
              /, '    ----------   --------   -------------- ',                &
              /, ( 5X, 2A10, I14 ) )
 4000 FORMAT( ' Int. par. num. ', i5, ' Name = ', A10, ' Value = ', I12)
 4010 FORMAT( ' Level-', i1, ' Instruction ', i4, ' Starting do-loop ' )
 4020 FORMAT( ' Level-', i1, ' Instruction ', i4, ' Ending do-loop ' )
 4030 FORMAT( ' Level-', i1, ' Instruction ', i4,                              &
              ' Incrementing do-loop ' )
 4100 FORMAT( ' Real par. num. ', i5, ' Name = ', A10,' Value = ',             &
                1P, D12.4)
 5000 FORMAT( /, ' Level-', i1, ' loop index ', A10, ' = ', I12 )
 5010 FORMAT( ' Level-', i1, ' instruction ', i3,                              &
              ' Index ', A10, ' = ', I12 )
 5020 FORMAT( ' Level-', i1, ' instruction ', i3,                              &
              ' Index ', A10, ' = ', 1P, D12.4 )
 5060 FORMAT( ' Level-', i1, ' instruction ', i3, ' Set line ', /,             &
              '    field1 = ', A12, ' field2 = ', A10, ' field3 = ',           &
              A10, /, '    value4 = ', 1P, D12.4, ' field5 = ', A10,           &
              ' value6 = ', 1P, D12.4 )

!  end of subroutine GPSMPS

      END SUBROUTINE GPSMPS

!-*-*-*-*-*-*- S I F D E C O D E   S G R P 1   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SGRP1( ng, ngmax, nomax, length, nobj, novals,                &
                         INLIST, IDROWS, ISTATE, ITYPEG, ITABLE,               &
                         RDROWS, RSCALE,                                       &
                         field1, field2, field3, value4, field5, value6,       &
                         GNAMES, ONAMES, KEY, out, status )
      INTEGER :: out, status, length
      INTEGER :: ng, ngmax, nomax
      INTEGER :: nobj, novals
      REAL ( KIND = wp ) :: value4, value6
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INLIST( length ), IDROWS( 2, ngmax )
      INTEGER :: ISTATE( ngmax ), ITYPEG( ngmax )
      INTEGER :: ITABLE ( length )
      REAL ( KIND = wp ) :: RDROWS( 2, ngmax ), RSCALE( ngmax )
      CHARACTER ( len = 10 ) :: GNAMES( ngmax ), ONAMES( nomax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  --------------------------------------------------------
!  indicator card is groups/rows/constraints

!  the groups section appears before the variables section
!  --------------------------------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, is, j
      CHARACTER ( len = 12 ) :: field

!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new group name in the hash-table

      CALL HASH_insert( length, 12, field2 // 'GR', KEY, ITABLE, ifree )
      IF ( ifree <= 0 ) THEN
         IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
         END IF

!  the group has appeared before as the j-th in the list

         j = INLIST( - ifree )
      ELSE

!  mark any objective function rows as special

         IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                          &
              field1 == 'DN' .OR. field1 == 'XN' .OR.                          &
              field1 == 'ZN' ) THEN
            nobj = nobj + 1
            IF( nobj > nomax ) THEN
               status = - 5
               RETURN
            END IF
            ONAMES ( nobj ) = field2
         END IF

!  the group is the ng-th encountered

         ng = ng + 1
         IF ( ng >= ngmax ) THEN
            status = - 6
            RETURN
         END IF

!  record the position of the new group in the table, record its
!  name and initialise its type as trivial

         j = ng
         INLIST( ifree ) = ng
         GNAMES( ng ) = field2
         ITYPEG( ng ) = - 1
         is = 0

!  record the status, istate, of the group. istate( ng ) = :
!  1 the group is an objective function type
!  2 the group is an equality function type
!  3 the group is a less-than-or-equal-to type
!  4 the group is a greater-than-or-equal-to type
!  5 the group is of d-type and an objective function type
!  6 the group is of d-type and an equality function type
!  7 the group is of d-type and a less-than-or-equal-to type
!  8 the group is of d-type and a greater-than-or-equal-to type

         IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                          &
              field1 == 'XN' .OR. field1 == 'ZN' ) is = 1
         IF ( field1 == 'E ' .OR. field1 == ' E' .OR.                          &
              field1 == 'XE' .OR. field1 == 'ZE' ) is = 2
         IF ( field1 == 'L ' .OR. field1 == ' L' .OR.                          &
              field1 == 'XL' .OR. field1 == 'ZL' ) is = 3
         IF ( field1 == 'G ' .OR. field1 == ' G' .OR.                          &
              field1 == 'XG' .OR. field1 == 'ZG' ) is = 4
         IF ( field1 == 'DN' ) is = 5
         IF ( field1 == 'DE' ) is = 6
         IF ( field1 == 'DL' ) is = 7
         IF ( field1 == 'DG' ) is = 8
         IF ( is == 0 ) THEN
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
         ENDIF
         ISTATE( ng ) = is
      END IF

!  include group scale factors

      IF ( field3 == '''SCALE''   ' .OR. field3                                &
           == ' ''SCALE''  ' ) RSCALE( j ) = value4

!  mark 'd'-type groups and record their multiplicative factors
!  idrows(1, ), idrows(2, ) give the numbers of the groups referred
!  to by the new d-type group and rdrows(1, ) and rdrows(2, ) give
!  the multiplicative factors

      IF ( FIELD1( 1: 1 ) == 'D' ) THEN
         IF ( ISTATE( j ) <= 4 ) THEN
            status = 22
            IF ( out > 0 ) WRITE( out, 2220 )
            RETURN
         END IF
         DO 10 i = 1, 2
            IF ( i > novals ) THEN
               IDROWS( i, j ) = 1
               RDROWS( i, j ) = zero
            ELSE

!  check that the groups referred to when constructing a d-type
!  group already exist

               IF ( i == 1 ) THEN
                  field = field3 // 'GR'
               ELSE
                  field = field5 // 'GR'
               END IF
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield > 0 ) THEN
                  IDROWS( i, j ) = INLIST( ifield )
                  IF ( i == 1 ) THEN
                     RDROWS( i, j ) = value4
                  ELSE
                     RDROWS( i, j ) = value6
                  END IF
               ELSE
                  status = 4
                  IF ( out > 0 ) WRITE( out, 2040 ) FIELD( 1: 10 )
                  RETURN
               END IF
            END IF
   10    CONTINUE
      END IF
      status = 0
      RETURN

!  non-executable statements

 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',         &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in GROUPS section ' )
 2220 FORMAT( ' ** Exit from GPSMPS -',                                        &
              ' conflicting field 1 on GROUPS card')

!  end of subroutine SGRP1

      END SUBROUTINE SGRP1

!-*-*-*-*-*-*- S I F D E C O D E   S G R P 2   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SGRP2 ( ng, ngmax, nomax, la, length,                         &
                         nnza, nobj, novals, INLIST, IDROWS,                   &
                         ISTATE, ITYPEG, ITABLE, ICOORD,                       &
                         A, RDROWS, RSCALE,                                    &
                         field1, field2, field3, value4, field5, value6,       &
                         GNAMES, ONAMES, KEY, out, status )
      INTEGER :: out, status, la, length
      INTEGER :: ng, ngmax, nomax
      INTEGER :: nnza, nobj, novals
      REAL ( KIND = wp ) :: value4, value6
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INLIST( length ), IDROWS( 2, ngmax )
      INTEGER :: ISTATE( ngmax ), ITYPEG( ngmax )
      INTEGER :: ITABLE ( length )
      INTEGER :: ICOORD( la, 2 )
      REAL ( KIND = wp ) :: A( la ), RDROWS( 2, ngmax ), RSCALE( ngmax )
      CHARACTER ( len = 10 ) :: GNAMES( ngmax ), ONAMES( nomax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ------------------------------------------------------
!  indicator card is groups/rows/constraints

!  the groups section appears after the variables section
!  ------------------------------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, is, j
      CHARACTER ( len = 12 ) :: field

!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new group name in the hash-table

      CALL HASH_insert( length, 12, field2 // 'GR', KEY, ITABLE, ifree )

!  the name already exists. it is the j-th name in the list

      IF ( ifree <= 0 ) THEN
         IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
         END IF
         j = INLIST( - ifree )
      ELSE

!  mark any objective function rows as special

         IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                          &
              field1 == 'DN' .OR. field1 == 'XN' .OR.                          &
              field1 == 'ZN' ) THEN
            nobj = nobj + 1
            IF( nobj > nomax ) THEN
               status = - 5
               RETURN
            END IF
            ONAMES ( nobj ) = field2
         END IF

!  the group is the ng-th encountered

         ng = ng + 1
         j = ng
         IF ( ng >= ngmax ) THEN
            status = - 6
            RETURN
         END IF

!  record the position of the new group in the table, record its
!  name and initialise its type as trivial

         INLIST( ifree ) = ng
         GNAMES( ng ) = field2
         ITYPEG( ng ) = - 1
         is = 0

!  record the status, istate, of the group. istate( ng ) = :
!  1 the group is an objective function type
!  2 the group is an equality function type
!  3 the group is a less-than-or-equal-to type
!  4 the group is a greater-than-or-equal-to type
!  5 the group is of d-type and an objective function type
!  6 the group is of d-type and an equality function type
!  7 the group is of d-type and a less-than-or-equal-to type
!  8 the group is of d-type and a greater-than-or-equal-to type

         IF ( field1 == 'N ' .OR. field1 == ' N' .OR.                          &
              field1 == 'XN' .OR. field1 == 'ZN' ) is = 1
         IF ( field1 == 'E ' .OR. field1 == ' E' .OR.                          &
              field1 == 'XE' .OR. field1 == 'ZE' ) is = 2
         IF ( field1 == 'L ' .OR. field1 == ' L' .OR.                          &
              field1 == 'XL' .OR. field1 == 'ZL' ) is = 3
         IF ( field1 == 'G ' .OR. field1 == ' G' .OR.                          &
              field1 == 'XG' .OR. field1 == 'ZG' ) is = 4
         IF ( field1 == 'DN' ) is = 5
         IF ( field1 == 'DE' ) is = 6
         IF ( field1 == 'DL' ) is = 7
         IF ( field1 == 'DG' ) is = 8
         IF ( is == 0 ) THEN
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
         ENDIF
         ISTATE( ng ) = is
      END IF

!  include group scale factors

      IF ( field3 == '''SCALE''   ' .OR. field3                                &
           == ' ''SCALE''  ' ) THEN
         RSCALE( j ) = value4
         RETURN
      END IF

!  mark 'd'-type groups and record their multiplicative factors
!  idrows(1, ), idrows(2, ) give the numbers of the groups referred
!  to by the new d-type group and rdrows(1, ) and rdrows(2, ) give
!  the multiplicative factors

      IF ( FIELD1( 1: 1 ) == 'D' ) THEN
         IF ( ISTATE( j ) <= 4 ) THEN
            status = 22
            IF ( out > 0 ) WRITE( out, 2220 )
            RETURN
         END IF
         DO 10 i = 1, 2
            IF ( i > novals ) THEN
               IDROWS( i, j ) = 1
               RDROWS( i, j ) = zero
            ELSE

!  check that the groups referred to when constructing a d-type
!  group already exist

               IF ( i == 1 ) THEN
                  field = field3 // 'GR'
               ELSE
                  field = field5 // 'GR'
               END IF
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield > 0 ) THEN
                  IDROWS( i, j ) = INLIST( ifield )
                  IF ( i == 1 ) THEN
                     RDROWS( i, j ) = value4
                  ELSE
                     RDROWS( i, j ) = value6
                  END IF
               ELSE
                  status = 4
                  IF ( out > 0 ) WRITE( out, 2040 ) FIELD( 1: 10 )
                  RETURN
               END IF
            END IF
   10    CONTINUE
      ELSE
         IF ( novals > 0 ) THEN

!  check that data has not been specified for a 'd'-group

            IF ( ISTATE( j ) >= 5 ) THEN
               status = 8
               IF ( out > 0 ) WRITE( out, 2080 )
               RETURN
            END IF

!  entries for the linear element for group j are to be specified
!  find the variable numbers for the input entries

            DO 20 i = 1, novals
               IF ( i == 1 ) THEN
                  field = field3 // 'VA'
               ELSE
                  field = field5 // 'VA'
               END IF
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield > 0 ) THEN

!  the nnza-th nonzero has been specified

                  nnza = nnza + 1
                  IF ( nnza > la ) THEN
                     status = - 2
                    RETURN
                  END IF

!  the nonzero is for group icoord(,1) and variable icoord(,2)
!  its value is given in a( )

                  ICOORD( nnza, 1 ) = j
                  ICOORD( nnza, 2 ) = INLIST( ifield )
                  IF ( i == 1 ) THEN
                     A( nnza ) = value4
                  ELSE
                     A( nnza ) = value6
                  END IF
               ELSE

!  the variable name is unknown

                  status = 5
                  IF ( out > 0 ) WRITE( out, 2050 ) FIELD( 1 : 10 )
                  RETURN
               END IF
   20       CONTINUE
         END IF
      END IF
      status = 0
      RETURN

!  non-executable statements

 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',         &
              ' name is ', A10 )
 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',        &
              ' name is ', A10 )
 2080 FORMAT( ' ** Exit from GPSMPS - ''D'' group/row contains data ' )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in GROUPS section ' )
 2220 FORMAT( ' ** Exit from GPSMPS -',                                        &
              ' conflicting field 1 on GROUPS card')

!  end of subroutine SGRP2

      END SUBROUTINE SGRP2

!-*-*-*-*-*-*- S I F D E C O D E   S V A R 1   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SVAR1( nmax, length, nvar, colfie, ITYPEV, INLIST, ITABLE,    &
                        CSCALE, field2, field3, value4, VNAMES, KEY, status )
      INTEGER :: status, length, nmax, nvar
      REAL ( KIND = wp ) :: value4
      CHARACTER ( len = 2 ) :: colfie
      CHARACTER ( len = 10 ) :: field2, field3
      INTEGER :: INLIST( length ),  ITABLE ( length )
      INTEGER :: ITYPEV( nmax )
      REAL ( KIND = wp ) :: CSCALE( nmax )
      CHARACTER ( len = 10 ) :: VNAMES( nmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  -----------------------------------------------------------------
!  indicator card is columns/variables, constants/rhs/rhs' or ranges

!  the variables section before the groups section
!  -----------------------------------------------------------------

!  local variables

      INTEGER :: ifree, j

!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new variable name in the hash-table
!  if it has not already been entered

      CALL HASH_insert( length, 12, field2 // colfie, KEY, ITABLE, ifree )
      IF ( ifree <= 0 ) THEN

!  the variable name already exists. it is the j-th named variable

         IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
         END IF
         j = INLIST( - ifree )
      ELSE

!  the variable name is new. the variable is the nvar-th encountered and
!  it occurs in position ifree in the table

         nvar = nvar + 1
         IF ( nvar > nmax ) THEN
            status = - 7
            RETURN
         END IF
         j = nvar
         INLIST( ifree ) = nvar
         VNAMES( nvar ) = field2
      END IF

!  include column scale factors if they are allowed

      IF ( field3 == '''SCALE''   ' .OR. field3 == ' ''SCALE''  ' ) THEN
         CSCALE( j ) = value4
      END IF

!  mark zero-one and integer variables

      IF ( field3 == '''ZERO-ONE''' ) ITYPEV( j ) = 1
      IF ( field3 == '''INTEGER'' ' ) ITYPEV( j ) = 2
      status = 0
      RETURN

!  end of subroutine SVAR1

      END SUBROUTINE SVAR1

!-*-*-*-*-*-*- S I F D E C O D E   S V A R 2   S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE SVAR2( nmax, ngmax, la, length, nnza, nvar, novals,           &
                        nrlndx, varsec, colfie, ICOORD, ISTATE, ITYPEV,        &
                        INLIST, ITABLE, A, CSCALE, REALVL, DFAULT,             &
                        field1, field2, field3, value4, field5, value6,        &
                        VNAMES, KEY, out, status )
      INTEGER :: out, status, la, length
      INTEGER :: nmax, ngmax, nnza, nvar, novals, nrlndx
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: varsec
      CHARACTER ( len = 2 ) :: field1, colfie
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: ICOORD( la, 2 ), ISTATE( ngmax )
      INTEGER :: INLIST( length ), ITABLE ( length )
      INTEGER :: ITYPEV( nmax )
      REAL ( KIND = wp ) :: A( la ), CSCALE( nmax )
      REAL ( KIND = wp ) :: REALVL( nrlndx ), DFAULT( nmax )
      CHARACTER ( len = 10 ) :: VNAMES( nmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  -----------------------------------------------------------------
!  indicator card is columns/variables, constants/rhs/rhs' or ranges

!  the variables section appears after the groups section
!  -----------------------------------------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, j
      CHARACTER ( len = 12 ) :: field

!  ignore 'marker' cards

      IF ( field3 == '''MARKER''  ' ) RETURN

!  find a place to insert the new variable name in the hash-table
!  if it has not already been entered

      CALL HASH_insert( length, 12, field2 // colfie, KEY, ITABLE, ifree )

!  the variable name already exists. it is the j-th named variable

      IF ( ifree <= 0 ) THEN
         IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
         END IF
         j = INLIST( - ifree )
      ELSE

!  the variable name is new. the variable is the nvar-th encountered and
!  it occurs in position ifree in the table

         nvar = nvar + 1
         IF ( nvar > nmax ) THEN
            status = - 7
            RETURN
         END IF
         j = nvar
         INLIST( ifree ) = nvar
         VNAMES( nvar ) = field2
         IF ( colfie == 'CO' ) DFAULT( nvar ) = zero
         IF ( colfie == 'RA' ) DFAULT( nvar ) = biginf
      END IF

!  include column scale factors if they are allowed

      IF ( field3 == '''SCALE''   ' .OR.                                       &
           field3 == ' ''SCALE''  ' ) THEN
         IF ( .NOT. varsec ) THEN
            status = 7
            IF ( out > 0 ) WRITE( out, 2070 )
         ELSE
            CSCALE( j ) = value4
         END IF
         RETURN
      END IF

!  mark zero-one and integer variables

      IF ( field3 == '''ZERO-ONE''' ) THEN
         IF ( .NOT. varsec ) THEN
            status = 7
            IF ( out > 0 ) WRITE( out, 2060 )
         ELSE
            ITYPEV( j ) = 1
         END IF
         RETURN
      END IF
      IF ( field3 == '''INTEGER'' ' ) THEN
         IF ( .NOT. varsec ) THEN
            status = 7
            IF ( out > 0 ) WRITE( out, 2060 )
         ELSE
            ITYPEV( j ) = 2
         END IF
         RETURN
      END IF

!  a nontrivial default value has been specified for a constant or
!  range vector

      IF ( field3 == '''DEFAULT'' ' .AND. .NOT. varsec ) THEN
         IF ( field1 == 'Z ' ) THEN
            CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI',                &
                         KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
               RETURN
            END IF
            value4 = REALVL( INLIST( ifield ) )
         END IF
         DFAULT( nvar ) = value4
      ELSE

!  find the group numbers for the input nonzero(s)

         IF ( novals > 0 ) THEN
            DO 10 i = 1, novals
               IF ( i == 1 ) THEN
                  field = field3 // 'GR'
               ELSE
                  field = field5 // 'GR'
               END IF
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield > 0 ) THEN

!  check that data has not been specified for a 'd'-group

                  IF ( ISTATE( INLIST( ifield ) ) >= 5                         &
                       .AND. varsec ) THEN
                     status = 8
                     IF ( out > 0 ) WRITE( out, 2080 )
                     RETURN
                  END IF

!  the nnza-th nonzero has been specified

                  nnza = nnza + 1
                  IF ( nnza > la ) THEN
                     status = - 2
                     RETURN
                  END IF

!  the nonzero is for group icoord(,1) and variable icoord(,2)
!  its value is given in a( )

                  ICOORD( nnza, 1 ) = INLIST( ifield )
                  ICOORD( nnza, 2 ) = j
                  IF ( i == 1 ) THEN
                     A( nnza ) = value4
                  ELSE
                     A( nnza ) = value6
                  END IF
               ELSE

!  the group name is unknown

                  status = 4
                  IF ( out > 0 ) WRITE( out, 2040 ) FIELD( 1: 10 )
                  RETURN
               END IF
   10       CONTINUE
         END IF
      END IF
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',         &
              ' name is ', A10 )
 2060 FORMAT( ' ** Exit from GPSMPS - type given for RHS or RANGES ' )
 2070 FORMAT( ' ** Exit from GPSMPS - scale given for RHS or RANGES ' )
 2080 FORMAT( ' ** Exit from GPSMPS - ''D'' group/row contains data ' )

!  end of subroutine SVAR2

      END SUBROUTINE SVAR2

!-*-*-*-*-*- S I F D E C O D E   S B O U N D    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SBOUND( nmax, nbmax, length, nlvars, nbnd, ncol, nrlndx,      &
                         defaut, INLIST, ITABLE, BND, REALVL, field1, field2,  &
                         field3, value4, field5, BNAMES, BNDFLT, KEY, out,     &
                         status )
      INTEGER :: out, status, length, nmax, nbmax, nlvars, nbnd, ncol, nrlndx
      LOGICAL :: defaut
      REAL ( KIND = wp ) :: value4
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INLIST( length ), ITABLE ( length )
      REAL ( KIND = wp ) :: BND( 2, nmax, nbmax ), REALVL( nrlndx )
      REAL ( KIND = wp ) :: BNDFLT( 2, nbmax )
      CHARACTER ( len = 10 ) :: BNAMES( nbmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ------------------------
!  indicator card is bounds
!  ------------------------

!  local variables

      INTEGER :: i, ifield

!  the first pair of bound vectors are to be assigned

      IF ( nbnd == 0 ) THEN
         nbnd = 1
         IF ( nbnd > nbmax ) THEN
            status = - 13
            RETURN
         END IF
         BNAMES( nbnd ) = field2
         defaut = .TRUE.
         BNDFLT( 1, nbnd ) = zero
         BNDFLT( 2, nbnd ) = biginf
         DO 10 i = 1, nlvars
            BND( 1, i, nbnd ) = zero
            BND( 2, i, nbnd ) = biginf
   10    CONTINUE
      END IF

!  a new pair of bound vectors are to be assigned

      IF ( field2 /= BNAMES( nbnd ) ) THEN
         nbnd = nbnd + 1
         IF ( nbnd > nbmax ) THEN
            status = - 13
            RETURN
         END IF
         BNAMES( nbnd ) = field2
         defaut = .TRUE.
         BNDFLT( 1, nbnd ) = zero
         BNDFLT( 2, nbnd ) = biginf
         DO 20 i = 1, nlvars
            BND( 1, i, nbnd ) = zero
            BND( 2, i, nbnd ) = biginf
   20    CONTINUE
      END IF

!  ensure that default values are assigned first

      IF ( field3 == '''DEFAULT'' ' ) THEN
         IF ( .NOT. defaut ) THEN
            status = 20
            IF ( out > 0 ) WRITE( out, 2200 )
            RETURN
         END IF
         IF ( field1 == 'ZL' .OR. field1 == 'ZU' .OR.                          &
              field1 == 'ZX' ) THEN
            CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI',                &
                         KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
               RETURN
            END IF
            value4 = REALVL( INLIST( ifield ) )
         END IF
         IF ( field1 == 'LO' .OR. field1 == 'UP' .OR.                          &
              field1 == 'FX' .OR. field1 == 'FR' .OR.                          &
              field1 == 'MI' .OR. field1 == 'PL' .OR.                          &
              field1 == 'XL' .OR. field1 == 'XU' .OR.                          &
              field1 == 'XX' .OR. field1 == 'XR' .OR.                          &
              field1 == 'XM' .OR. field1 == 'XP' .OR.                          &
              field1 == 'ZL' .OR. field1 == 'ZU' .OR.                          &
              field1 == 'ZX' ) THEN

!  assign default lower bounds for variables

            IF ( ( FIELD1( 2 : 2 ) == 'L' .AND.                                &
                   field1 /= 'PL' ) .OR.                                       &
                 FIELD1( 2 : 2 ) == 'X' .OR.                                   &
                 FIELD1( 2 : 2 ) == 'R' .OR.                                   &
                 FIELD1( 2 : 2 ) == 'M' .OR.                                   &
                 field1 == 'LO' .OR. field1 == 'MI' ) THEN

!  a finite lower bound is specified

               IF ( FIELD1( 2 : 2 ) == 'L' .OR.                                &
                    FIELD1( 2 : 2 ) == 'X' .OR.                                &
                    field1 == 'LO' ) THEN
                  BNDFLT( 1, nbnd ) = value4
                  DO 30 i = 1, nlvars
                     BND( 1, i, nbnd ) = value4
   30             CONTINUE

!  an infinite lower bound is specified

               ELSE
                  BNDFLT( 1, nbnd ) = - biginf
                  DO 40 i = 1, nlvars
                     BND( 1, i, nbnd ) = - biginf
   40             CONTINUE
                  IF ( FIELD1( 2 : 2 ) == 'M' .OR.                             &
                       field1 == 'MI' ) THEN
                     BNDFLT( 2, nbnd ) = zero
                     DO 41 i = 1, nlvars
                        BND( 2, i, nbnd ) = zero
   41                CONTINUE
                  END IF
               END IF
            END IF

!  assign default upper bounds for variables

            IF ( FIELD1( 2 : 2 ) == 'U' .OR.                                   &
                 FIELD1( 2 : 2 ) == 'X' .OR.                                   &
                 FIELD1( 2 : 2 ) == 'R' .OR.                                   &
                 FIELD1( 2 : 2 ) == 'P' ) THEN

!  a finite upper bound is specified

               IF ( FIELD1( 2 : 2 ) == 'U' .OR.                                &
                    FIELD1( 2 : 2 ) == 'X' .OR.                                &
                    field1 == 'UP' ) THEN
                  IF ( ( FIELD1( 2 : 2 ) == 'U' .OR.                           &
                         field1 == 'UP' )                                      &
                       .AND. value4 == zero                                    &
                       .AND. BNDFLT( 1, nbnd ) == zero                         &
                       .AND. BNDFLT( 2, nbnd ) == biginf ) THEN
                     BNDFLT( 1, nbnd ) = - biginf
                     DO 51 i = 1, nlvars
                        BND( 1, i, nbnd ) = - biginf
   51                CONTINUE
                  END IF
                  BNDFLT( 2, nbnd ) = value4
                  DO 50 i = 1, nlvars
                     BND( 2, i, nbnd ) = value4
   50             CONTINUE

!  an infinite upper bound is specified

               ELSE
                  BNDFLT( 2, nbnd ) = biginf
                  DO 60 i = 1, nlvars
                     BND( 2, i, nbnd ) = biginf
   60             CONTINUE
               END IF
            END IF

!  field 1 is not recognised

         ELSE
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
         END IF
      ELSE

!  an individual bound is to be assigned

         defaut = .FALSE.
         IF ( field1 == 'LO' .OR. field1 == 'XL' .OR.                          &
              field1 == 'UP' .OR. field1 == 'XU' .OR.                          &
              field1 == 'FX' .OR. field1 == 'XX' .OR.                          &
              field1 == 'FR' .OR. field1 == 'XR' .OR.                          &
              field1 == 'MI' .OR. field1 == 'XM' .OR.                          &
              field1 == 'PL' .OR. field1 == 'XP' .OR.                          &
              field1 == 'ZL' .OR. field1 == 'ZU' .OR.                          &
              field1 == 'ZX' ) THEN

!  find which variable is being assigned

            CALL HASH_search( length, 12, FIELD3//'VA', KEY, ITABLE, ifield )
            IF ( ifield > 0 ) THEN
               ncol = INLIST( ifield )

!  assign a lower bound for this variable

               IF ( field1 == 'LO' .OR. field1 == 'XL' .OR.                    &
                    field1 == 'FX' .OR. field1 == 'XX' .OR.                    &
                    field1 == 'FR' .OR. field1 == 'XR' .OR.                    &
                    field1 == 'MI' .OR. field1 == 'XM' .OR.                    &
                    field1 == 'ZL' .OR. field1 == 'ZX' ) THEN

!  a finite lower bound is specified

                  IF ( field1 == 'LO' .OR. field1 == 'XL' .OR.                 &
                       field1 == 'ZL' .OR. field1 == 'ZX' .OR.                 &
                       field1 == 'FX' .OR. field1 == 'XX' ) THEN
                     BND( 1, ncol, nbnd ) = value4

!  an infinite lower bound is specified

                  ELSE
                     IF ( ( field1 == 'MI' .OR. field1 == 'XM' )               &
                            .AND. BND( 1, ncol, nbnd ) == zero                 &
                            .AND. BND( 2, ncol, nbnd ) == biginf )             &
                        BND( 2, ncol, nbnd ) = zero
                     BND( 1, ncol, nbnd ) = - biginf
                  END IF
               END IF

!  assign an upper bound for the variable

               IF ( field1 == 'UP' .OR. field1 == 'XU' .OR.                    &
                    field1 == 'FX' .OR. field1 == 'XX' .OR.                    &
                    field1 == 'FR' .OR. field1 == 'XR' .OR.                    &
                    field1 == 'PL' .OR. field1 == 'XP' .OR.                    &
                    field1 == 'ZU' .OR. field1 == 'ZX' ) THEN

!  a finite upper bound is specified

                  IF ( field1 == 'UP' .OR. field1 == 'XU' .OR.                 &
                       field1 == 'ZU' .OR. field1 == 'ZX' .OR.                 &
                       field1 == 'FX' .OR. field1 == 'XX' ) THEN
                     IF ( ( field1 == 'UP' .OR. field1 == 'XU' .OR.            &
                            field1 == 'ZU' ) .AND. value4 == zero              &
                            .AND. BND( 1, ncol, nbnd ) == zero                 &
                            .AND. BND( 2, ncol, nbnd ) == biginf )             &
                          BND( 1, ncol, nbnd ) = - biginf
                     BND( 2, ncol, nbnd ) = value4


!  an infinite upper bound is specified

                  ELSE
                     BND( 2, ncol, nbnd ) = biginf
                  END IF
               END IF
            ELSE
               status = 5
               IF ( out > 0 ) WRITE( out, 2050 ) field3
               RETURN
            END IF

!  field 1 is not recognised

         ELSE
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
         END IF
      END IF
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',        &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in BOUNDS section ' )
 2200 FORMAT( ' ** Exit from GPSMPS - default specified out of order ' )

!  end of subroutine SBOUND

      END SUBROUTINE SBOUND

!-*-*-*-*-*- S I F D E C O D E   S S T A R T    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SSTART( nmax, ngmax, nsmax, length, nlvars, ng,               &
                         nstart, ncol, nrlndx, defaut, INLIST, ITABLE,         &
                         VSTART, CSTART, REALVL,                               &
                         field1, field2, field3, value4, field5, value6,       &
                         SNAMES, novals, KEY, out, status )
      INTEGER :: out, status, length, novals, ng, ngmax
      INTEGER :: nmax, nsmax, nlvars, nstart, ncol, nrlndx
      LOGICAL :: defaut
      REAL ( KIND = wp ) :: value4, value6
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INLIST( length ), ITABLE ( length )
      REAL ( KIND = wp ) :: VSTART( nmax, nsmax ), CSTART( ngmax, nsmax )
      REAL ( KIND = wp ) :: REALVL( nrlndx )
      CHARACTER ( len = 10 ) :: SNAMES( nsmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  -----------------------------
!  indicator card is start point
!  -----------------------------

!  local variables

      INTEGER :: i, ifield
      CHARACTER ( len = 10 ) :: field

!  the starting vector is  to be assigned

      IF ( nstart == 0 ) THEN
         nstart = 1
         IF ( nstart > nsmax ) THEN
            status = - 8
            RETURN
         END IF
         SNAMES( nstart ) = field2
         defaut = .TRUE.
         DO 10 i = 1, nlvars
            VSTART( i, nstart ) = zero
   10    CONTINUE
         DO 20 i = 1, ng
            CSTART( i, nstart ) = zero
   20    CONTINUE
      END IF

!  a new starting vector is to be assigned

      IF ( field2 /= SNAMES( nstart ) ) THEN
         nstart = nstart + 1
         IF ( nstart > nsmax ) THEN
            status = - 8
            RETURN
         END IF
         SNAMES( nstart ) = field2
         defaut = .TRUE.

!  assign a default value of zero to the variables

         DO 30 i = 1, nlvars
            VSTART( i, nstart ) = zero
   30    CONTINUE

!  assign a default value of zero to the lagrange multipliers

         DO 40 i = 1, ng
            CSTART( i, nstart ) = zero
   40    CONTINUE
      END IF

!  ensure that default values are assigned first

      IF ( field3 == '''DEFAULT'' ' ) THEN
         IF ( .NOT. defaut ) THEN
            status = 20
            IF ( out > 0 ) WRITE( out, 2200 )
            RETURN
         END IF
         IF ( FIELD1( 1: 1 ) == 'Z' ) THEN
            CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI',                &
                         KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
               RETURN
            END IF
            value4 = REALVL( INLIST( ifield ) )
         END IF

!  assign default values to the starting point

         IF ( field1 == '  ' .OR. field1 == 'V ' .OR.                          &
              field1 == 'X ' .OR. field1 == 'Z ' .OR.                          &
              field1 == 'XV' .OR. field1 == 'ZV' ) THEN
            DO 50 i = 1, nlvars
               VSTART( i, nstart ) = value4
   50       CONTINUE
         END IF

!  assign default values to the lagrange multipliers

         IF ( field1 == '  ' .OR. field1 == 'M ' .OR.                          &
              field1 == 'X ' .OR. field1 == 'Z ' .OR.                          &
              field1 == 'XM' .OR. field1 == 'ZM' ) THEN
            DO 60 i = 1, ng
               CSTART( i, nstart ) = value4
   60       CONTINUE
         END IF
      ELSE

!  an individual starting value is to be assigned

         IF ( field1 == 'X ' .OR. field1 == '  ' .OR.                          &
              field1 == 'Z ' ) THEN
            defaut = .FALSE.

!  find which value is, or values are, being assigned

            DO 70 i = 1, novals
               IF ( i == 1 ) THEN
                  field = field3
               ELSE
                  field = field5
               END IF

!  see if the name belongs to a variable

               CALL HASH_search( length, 12, field // 'VA',                         &
                            KEY, ITABLE, ifield )
               IF ( ifield > 0 ) THEN
                  ncol = INLIST( ifield )

!  assign the starting value for this variable

                  IF ( i == 1 ) THEN
                     VSTART( ncol, nstart ) = value4
                  ELSE
                     VSTART( ncol, nstart ) = value6
                  END IF
               ELSE

!  see if the name belongs to a group

                  CALL HASH_search( length, 12, field // 'GR',                      &
                               KEY, ITABLE, ifield )
                  IF ( ifield > 0 ) THEN
                     ncol = INLIST( ifield )

!  assign the starting value for the lagrange multiplier for this group

                     IF ( i == 1 ) THEN
                        CSTART( ncol, nstart ) = value4
                     ELSE
                        CSTART( ncol, nstart ) = value6
                     END IF
                  ELSE
                     status = 5
                     IF ( i == 1 ) THEN
                        IF ( out > 0 ) WRITE( out, 2050 ) field3
                     ELSE
                        IF ( out > 0 ) WRITE( out, 2050 ) field5
                     END IF
                     RETURN
                  END IF
               END IF
   70       CONTINUE
         ELSE

!  an individual starting value for a variable is to be assigned

            IF ( field1 == 'V ' .OR. field1 == 'XV' .OR.                       &
                 field1 == 'ZV' ) THEN
               defaut = .FALSE.

!  find which value is, or values are, being assigned

               DO 80 i = 1, novals
                  IF ( i == 1 ) THEN
                     field = field3
                  ELSE
                     field = field5
                  END IF

!  see if the name belongs to a variable

                  CALL HASH_search( length, 12, field // 'VA',                      &
                               KEY, ITABLE, ifield )
                  IF ( ifield > 0 ) THEN
                     ncol = INLIST( ifield )

!  assign the starting value for this variable

                     IF ( i == 1 ) THEN
                        VSTART( ncol, nstart ) = value4
                     ELSE
                        VSTART( ncol, nstart ) = value6
                     END IF
                  ELSE
                     status = 4
                     IF ( i == 1 ) THEN
                        IF ( out > 0 ) WRITE( out, 2040 ) field3
                     ELSE
                        IF ( out > 0 ) WRITE( out, 2040 ) field5
                     END IF
                     RETURN
                  END IF
   80          CONTINUE
            ELSE

!  an individual starting lagrange multiplier value is to be assigned

               IF ( field1 == 'M ' .OR. field1 == 'XM' .OR.                    &
                    field1 == 'ZM' ) THEN
                  defaut = .FALSE.

!  find which value is, or values are, being assigned

                  DO 90 i = 1, novals
                     IF ( i == 1 ) THEN
                        field = field3
                     ELSE
                        field = field5
                     END IF

!  see if the name belongs to a group

                     CALL HASH_search( length, 12, field // 'GR',                   &
                                  KEY, ITABLE, ifield )
                     IF ( ifield > 0 ) THEN
                        ncol = INLIST( ifield )

!  assign the starting value for the lagrange multiplier for this group

                        IF ( i == 1 ) THEN
                           CSTART( ncol, nstart ) = value4
                        ELSE
                           CSTART( ncol, nstart ) = value6
                        END IF
                     ELSE
                        status = 5
                        IF ( i == 1 ) THEN
                           IF ( out > 0 ) WRITE( out, 2050 ) field3
                         ELSE
                           IF ( out > 0 ) WRITE( out, 2050 ) field5
                        END IF
                        RETURN
                     END IF
   90             CONTINUE

!  field 1 is not recognised

               ELSE
                  status = 10
                  IF ( out > 0 ) WRITE( out, 2100 ) field1
                  RETURN
               END IF
            END IF
         END IF
      END IF
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',         &
              ' name is ', A10 )
 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',        &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in START POINT section ' )
 2200 FORMAT( ' ** Exit from GPSMPS - default specified out of order ' )

!  end of subroutine SSTART

      END SUBROUTINE SSTART

!-*-*-*-*-*- S I F D E C O D E   S Q H E S S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SQHESS( negmax, ngmax, nlmax, nelmax, nevmax, netmax,         &
                         nomax, nimax, length, ng, nobj, ngrupe, novals,       &
                         neln, ninn, nepn, neltyp, nlisep, nlisev,             &
                         nelnum, neling, qgroup, qsqr, qprod, IELV,            &
                         IINV, IEPA, ITYPEG, IELING, ISTAEV, IELVAR,           &
                         INLIST, ITABLE, ISTATE, ITYPEE, ISTEP,                &
                         iptype, istype, inrep,                                &
                         field1, field2, field3, value4, field5, value6,       &
                         ENAMES, GNAMES, ETYPES, ONAMES, INAMES,               &
                         lnames, WEIGHT, KEY, out, status )
      INTEGER :: out, status, length, iptype, istype, nimax
      INTEGER :: negmax, ngmax, nobj, neln, ninn, nepn, neltyp
      INTEGER :: ng, neling, novals, ngrupe, nlisep, nlisev
      INTEGER :: nlmax, nelmax, nevmax, netmax, nomax, nelnum
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: qgroup, qsqr, qprod, inrep
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INLIST( length ), ITABLE ( length )
      INTEGER :: IELV  ( nlmax ), IINV  ( nlmax ), IEPA( nlmax )
      INTEGER :: IELING( negmax, 2 )
      INTEGER :: ITYPEG( ngmax ), ITYPEE( nelmax ), ISTEP( nelmax )
      INTEGER :: ISTATE( ngmax )
      INTEGER :: IELVAR( nevmax ), ISTAEV( nelmax )
      REAL ( KIND = wp ) :: WEIGHT( negmax )
      CHARACTER ( len = 10 ) :: GNAMES( ngmax ), ONAMES( nomax )
      CHARACTER ( len = 10 ) :: INAMES( nimax ), LNAMES( nelmax )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax ), ENAMES( netmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ---------------------------
!  indicator card is quadratic
!  ---------------------------

!  local variables

      INTEGER :: i, ifield, k, nevars
      INTEGER :: ncol1, ncol2, nterms, ifree
      REAL ( KIND = wp ) :: value
      CHARACTER ( len = 12 ) :: field

!  find the first variable

      CALL HASH_search( length, 12, field2 // 'VA', KEY, ITABLE, ifield )
      IF ( ifield > 0 ) THEN
         ncol1 = INLIST( ifield )
      ELSE
         status = 5
         IF ( out > 0 ) WRITE( out, 2050 ) field2
         RETURN
      END IF

!  find the second variable

      IF ( field1 == 'Z ' ) THEN
         nterms = 1
      ELSE
         nterms = novals
      END IF


      DO 110 i = 1, nterms
         IF ( i == 1 ) THEN
            field = field3 // 'VA'
            value = value4
         ELSE
            field = field5 // 'VA'
            value = value6
         END IF
         IF ( value /= 0.0D+0 ) THEN
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield > 0 ) THEN
               ncol2 = INLIST( ifield )
            ELSE
               status = 5
               IF ( out > 0 ) WRITE( out, 2050 ) FIELD( 1 : 10 )
               RETURN
            END IF
            IF ( .NOT. qgroup ) THEN

!  this is the first Hessian term. make it a new group
!  find a place to insert the new group name in the hash-table

               CALL HASH_insert( length, 12, cqgrou // 'GR', KEY,                   &
                            ITABLE, ifree )
               IF ( ifree <= 0 ) THEN
                  status = - 1
                  RETURN
               ELSE

!  mark this as an objective function group

                  nobj = nobj + 1
                  IF( nobj > nomax ) THEN
                     status = - 5
                     RETURN
                  END IF
                  ONAMES ( nobj ) =  cqgrou

!  the group is the ng-th encountered

                  ng = ng + 1
                  IF ( ng >= ngmax ) THEN
                     status = - 6
                     RETURN
                  END IF

!  record the position of the new group in the table, record its
!  name and initialise its type as trivial

                  ngrupe = ng
                  INLIST( ifree ) = ng
                  GNAMES( ng ) = cqgrou
                  ITYPEG( ng ) = 0
                  ISTATE( ng ) = 1
                  qgroup = .TRUE.
               END IF
            END IF
            IF ( ncol1 == ncol2 ) THEN

!  check if this is the first occurence of a diagonal term

               IF ( .NOT. qsqr ) THEN

!  check if this is the first element type

                  IF ( neltyp == 0 ) THEN
                     istype = 1
                     neln = 1
                     ninn = 0
                     nepn = 0
                     neltyp = 1
                  ELSE
                     istype = 2
                     neltyp = neltyp + 1
                     IF ( neltyp > nlmax ) THEN
                        status = - 3
                        RETURN
                     END IF
                     neln = neln + 1
                     IF ( neln > netmax ) THEN
                        status = - 14
                        RETURN
                     END IF
                  END IF

!  input the names of the element type

                  CALL HASH_insert( length, 12, cqsqr // 'ET', KEY,                 &
                               ITABLE, ifree )
                  IF ( ifree <= 0 ) THEN
                     IF ( ifree == 0 ) THEN
                        status = - 1
                        RETURN
                     END IF
                     status = 18
                     IF ( out > 0 ) WRITE( out, 2180 )
                     RETURN
                  END IF
                  INLIST( ifree ) = neltyp
                  IELV( neltyp ) = neln
                  IINV( neltyp ) = ninn + 1
                  IEPA( neltyp ) = nepn + 1
                  ETYPES( neltyp ) = cqsqr

!  insert the element variable

                  ENAMES( neln ) = 'X         '
                  ninn = ninn + 1
                  IF ( ninn > nimax ) THEN
                     status = - 16
                     RETURN
                  END IF
                  INAMES( ninn ) = ENAMES( neln )
                  inrep = .FALSE.
                  qsqr = .TRUE.
               END IF

!  the new element is the nelnum-th nonlinear element

               nelnum = nelnum + 1
               IF ( nelnum > nelmax ) THEN
                  status = - 9
                  RETURN
               END IF

!  insert the name into the table

               WRITE( UNIT = field, FMT = "( '%', I9, 'EL' )" )                &
                      123456789 - nelnum
               CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )

!  record the elements position in the table along with its name

               INLIST( ifree ) = nelnum
               LNAMES( nelnum ) = FIELD( 1 : 10 )

!  determine the number of the element type, k, the starting
!  addresses for the parameters and variables for the element
!  and the number of parameters and variables involved

               k = istype
               ITYPEE( nelnum ) = k
               ISTEP ( nelnum ) = nlisep + 1
               ISTAEV( nelnum ) = nlisev + 1
               nevars = 1
               IF ( nlisev + nevars > nevmax ) THEN
                  status = - 15
                  RETURN
               END IF
               IELVAR( nlisev + 1 ) = ncol1
               nlisev = nlisev + nevars

!  assign the element as neling in group ngrupe

               neling = neling + 1
               IF ( neling > negmax ) THEN
                  status = - 10
                  RETURN
               END IF

!  the element is ieling(,1) and the group is given by ieling(,2)

               IELING( neling, 1 ) = nelnum
               IELING( neling, 2 ) = ngrupe

!  the element is weighted by the constant weight()

               WEIGHT( neling ) = value
            ELSE
               IF ( .NOT. qprod ) THEN

!  check if this is the first element type

                  IF ( neltyp == 0 ) THEN
                     iptype = 1
                     neln = 1
                     ninn = 0
                     nepn = 0
                     neltyp = 1
                  ELSE
                     iptype = 2
                     neltyp = neltyp + 1
                     IF ( neltyp > nlmax ) THEN
                        status = - 3
                        RETURN
                     END IF
                     neln = neln + 1
                     IF ( neln > netmax ) THEN
                        status = - 14
                        RETURN
                     END IF
                  END IF

!  input the names of the element type

                  CALL HASH_insert( length, 12, cqprod // 'ET', KEY,                &
                               ITABLE, ifree )
                  IF ( ifree <= 0 ) THEN
                     IF ( ifree == 0 ) THEN
                        status = - 1
                        RETURN
                     END IF
                     status = 18
                     IF ( out > 0 ) WRITE( out, 2180 )
                     RETURN
                  END IF
                  INLIST( ifree ) = neltyp
                  IELV( neltyp ) = neln
                  IINV( neltyp ) = ninn + 1
                  IEPA( neltyp ) = nepn + 1
                  ETYPES( neltyp ) = cqprod

!  insert the element variable

                  ENAMES( neln ) = 'X         '
                  ninn = ninn + 1
                  IF ( ninn > nimax ) THEN
                     status = - 16
                     RETURN
                  END IF
                  INAMES( ninn ) = ENAMES( neln )
                  neln = neln + 1
                  IF ( neln > netmax ) THEN
                     status = - 14
                     RETURN
                  END IF
                  ENAMES( neln ) = 'Y         '
                  ninn = ninn + 1
                  IF ( ninn > nimax ) THEN
                     status = - 16
                     RETURN
                  END IF
                  INAMES( ninn ) = ENAMES( neln )
                  inrep = .FALSE.
                  qprod = .TRUE.
               END IF

!  the new element is the nelnum-th nonlinear element

               nelnum = nelnum + 1
               IF ( nelnum > nelmax ) THEN
                  status = - 9
                  RETURN
               END IF

!  insert the name into the table

               WRITE( UNIT = field, FMT = "( '%', I9, 'EL' )" )                &
                      123456789 - nelnum
               CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )

!  record the elements position in the table along with its name

               INLIST( ifree ) = nelnum
               LNAMES( nelnum ) = FIELD( 1 : 10 )

!  determine the number of the element type, k, the starting
!  addresses for the parameters and variables for the element
!  and the number of parameters and variables involved

               k = iptype
               ITYPEE( nelnum ) = k
               ISTEP ( nelnum ) = nlisep + 1
               ISTAEV( nelnum ) = nlisev + 1
               nevars = 2
               IF ( nlisev + nevars > nevmax ) THEN
                  status = - 15
                  RETURN
               END IF
               IELVAR( nlisev + 1 ) = ncol1
               IELVAR( nlisev + 2 ) = ncol2
               nlisev = nlisev + nevars

!  assign the element as neling in group ngrupe

               neling = neling + 1
               IF ( neling > negmax ) THEN
                  status = - 10
                  RETURN
               END IF

!  the element is ieling(,1) and the group is given by ieling(,2)

               IELING( neling, 1 ) = nelnum
               IELING( neling, 2 ) = ngrupe

!  the element is weighted by the constant weight()

               WEIGHT( neling ) = value
            END IF
         END IF 
  110 CONTINUE   
      status = 0
      RETURN

!  non-executable statements

 2050 FORMAT( ' ** Exit from GPSMPS - column/var name not recognised:',        &
              ' name is ', A10 )
 2180 FORMAT( ' ** Exit from GPSMPS - duplicate element-type name ' )

!  end of subroutine SQHESS

      END SUBROUTINE SQHESS

!-*-*-*-*-*- S I F D E C O D E   S E T Y P E    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SETYPE( nlmax, nimax, netmax, nepmax, length, novals, neln,   &
                         ninn, nepn, neltyp, inrep, IELV, IINV, IEPA, INLIST,  &
                         ITABLE, field1, field2, field3, field5, ENAMES,       &
                         INAMES, EPNAME, ETYPES, KEY, out, status )
      INTEGER :: out, status, length
      INTEGER :: nlmax, nimax, netmax, nepmax
      INTEGER :: novals, neln, ninn, neltyp, nepn
      LOGICAL :: inrep
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INLIST( length ), ITABLE ( length )
      INTEGER :: IELV  ( nlmax ), IINV  ( nlmax ), IEPA( nlmax )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax ), INAMES( nimax )
      CHARACTER ( len = 10 ) :: EPNAME( nepmax ), ENAMES( netmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ------------------------------
!  indicator card is element type
!  ------------------------------

!  local variables

      INTEGER :: i, ifree, k

!  check if this is the first element type

      IF ( neltyp == 0 ) THEN
         CALL HASH_insert( length, 12, field2 // 'ET', KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               RETURN
            END IF
            status = 18
            IF ( out > 0 ) WRITE( out, 2180 )
            RETURN
         END IF
         neltyp = 1
         neln = 0
         ninn = 0
         nepn = 0
         inrep = .FALSE.
         INLIST( ifree ) = neltyp
         IELV( neltyp ) = neln + 1
         IINV( neltyp ) = ninn + 1
         IEPA( neltyp ) = nepn + 1
         ETYPES( neltyp ) = field2
      END IF

!  check if the column is new

      IF ( field2 /= ETYPES( neltyp ) ) THEN
         IF ( ETYPES( neltyp ) /= cqsqr .AND.                                  &
              ETYPES( neltyp ) /= cqprod ) THEN

!  if the previous element has no explicit internal representation,
!  use its elemental representation

            IF ( .NOT. inrep ) THEN
               DO 10 k = IELV( neltyp ), neln
                  ninn = ninn + 1
                  IF ( ninn > nimax ) THEN
                     status = - 16
                     RETURN
                  END IF
                  INAMES( ninn ) = ENAMES( k )
   10          CONTINUE
            ELSE
              IF ( ninn - IINV( neltyp ) >=                                    &
                   neln - IELV( neltyp ) ) THEN
                 status = 76
                 IF ( out > 0 ) WRITE( out, 2760 )
                 RETURN
              END IF
            END IF
         END IF

!  record the name and starting address of the new element type

         CALL HASH_insert( length, 12, field2 // 'ET', KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               RETURN
            END IF
            status = 18
            IF ( out > 0 ) WRITE( out, 2180 )
            RETURN
         END IF
         neltyp = neltyp + 1
         inrep = .FALSE.
         IF ( neltyp > nlmax ) THEN
            status = - 3
            RETURN
         END IF
         INLIST( ifree ) = neltyp
         IELV( neltyp ) = neln + 1
         IINV( neltyp ) = ninn + 1
         IEPA( neltyp ) = nepn + 1
         ETYPES( neltyp ) = field2
      END IF

!  input the name of an internal variable

      IF ( field1 == 'IV' ) THEN
         IF ( novals > 0 ) THEN
            inrep = .TRUE.
            DO 30 i = 1, novals

!  check the name has not already been used in the current element

                DO 20 k = IINV( neltyp ), ninn
                   IF ( ( i == 1 .AND. field3 == INAMES( k ) )                 &
                        .OR. ( i == 2 .AND. field5 == INAMES( k ) )            &
                      ) THEN
                      status = 12
                      IF ( out > 0 ) WRITE( out, 2120 )
                      RETURN
                   END IF
   20           CONTINUE

!  the name is new. record it in the array inames

               ninn = ninn + 1
               IF ( ninn > nimax ) THEN
                  status = - 16
                  RETURN
               END IF
               IF ( i == 1 ) THEN
                  INAMES( ninn ) = field3
               ELSE
                  INAMES( ninn ) = field5
               END IF
   30       CONTINUE
         END IF
      ELSE

!  input the name of an elemental variable

         IF ( field1 == 'EV' ) THEN
            IF ( novals > 0 ) THEN
               DO 50 i = 1, novals

!  check the name has not already been used in the current element

                  DO 40 k = IELV( neltyp ), neln
                      IF ( ( i == 1 .AND. field3 == ENAMES( k ) )              &
                      .OR. ( i == 2 .AND. field5 == ENAMES( k ) )              &
                      ) THEN
                        status = 11
                        IF ( out > 0 ) WRITE( out, 2110 )
                        RETURN
                     END IF
   40             CONTINUE

!  the name is new. record it in the array enames

                  neln = neln + 1
                  IF ( neln > netmax ) THEN
                     status = - 14
                     RETURN
                  END IF
                  IF ( i == 1 ) THEN
                     ENAMES( neln ) = field3
                  ELSE
                     ENAMES( neln ) = field5
                  END IF
   50          CONTINUE
            END IF
         ELSE

!  input the name of an element parameter

            IF ( field1 == 'EP' ) THEN
               IF ( novals > 0 ) THEN
                  DO 70 i = 1, novals

!  check the name has not already been used in the current element

                     DO 60 k = IEPA( neltyp ), nepn
                         IF ( ( i == 1 .AND. field3 == EPNAME( k ) )           &
                         .OR. ( i == 2 .AND. field5 == EPNAME( k ) )           &
                         ) THEN
                           status = 23
                           IF ( out > 0 ) WRITE( out, 2230 )
                           RETURN
                        END IF
   60                CONTINUE

!  the name is new. record it in the array epname

                     nepn = nepn + 1
                     IF ( nepn > nepmax ) THEN
                        status = - 19
                        RETURN
                     END IF
                     IF ( i == 1 ) THEN
                        EPNAME( nepn ) = field3
                     ELSE
                        EPNAME( nepn ) = field5
                     END IF
   70             CONTINUE
               END IF

!  field1 not recognised

            ELSE
               status = 10
               IF ( out > 0 ) WRITE( out, 2100 ) field1
               RETURN
            END IF
         END IF
      END IF
      status = 0
      RETURN

!  non-executable statements

 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in ELEMENT TYPE section ' )
 2110 FORMAT( ' ** Exit from GPSMPS - duplicate elemental var. name ' )
 2120 FORMAT( ' ** Exit from GPSMPS - duplicate internal var. name ' )
 2180 FORMAT( ' ** Exit from GPSMPS - duplicate element-type name ' )
 2230 FORMAT( ' ** Exit from GPSMPS - duplicate elemental param. name ')
 2760 FORMAT( ' ** Exit from GPSMPS - #internal vars >= #elementals' )

!  end of subroutine SETYPE

      END SUBROUTINE SETYPE

!-*-*-*-*-*- S I F D E C O D E   S E U S E S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SEUSES( nlmax, nelmax, netmax, nevmax, nlisev, nlisep,        &
                         novals, nepmax, nepvmx, length, nelnum, nelmnt,       &
                         nmax, n, elmnt, IELV, IEPA, ITYPEE, IELVAR,           &
                         INLIST, ITABLE, ISTAEV, ISTEP, delset, detype,        &
                         field1, field2, field3, value4, field5, value6,       &
                         EPVALU, ENAMES, lnames, EPNAME, VNAMES,               &
                         KEY, out, status )
      INTEGER :: out, status, length
      INTEGER :: nmax, nlmax, nelmax, netmax, nevmax
      INTEGER :: nelnum, nlisep, nlisev
      INTEGER :: novals, nepvmx, nepmax, nelmnt, n
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: delset
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5, elmnt, detype
      INTEGER :: INLIST( length ), ITABLE ( length )
      INTEGER :: IELV( nlmax ), IEPA( nlmax )
      INTEGER :: IELVAR( nevmax ), ITYPEE( nelmax )
      INTEGER :: ISTAEV( nelmax ), ISTEP( nelmax )
      REAL ( KIND = wp ) :: EPVALU( nepvmx )
      CHARACTER ( len = 10 ) :: ENAMES( netmax ), LNAMES( nelmax )
      CHARACTER ( len = 10 ) :: EPNAME( nepmax ), VNAMES( nmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ------------------------------
!  indicator card is element uses
!  ------------------------------

!  local variables

      INTEGER :: i, ifree, ifield, ip, is, j, k, mlisep, mlisev
      INTEGER :: nepars, nevars
      CHARACTER ( len = 12 ) :: field

!  the current card defines a default type

      IF ( field2 == '''DEFAULT'' ' ) THEN
         IF ( delset ) THEN
            status = 26
            IF ( out > 0 ) WRITE( out, 2260 )
         END IF
         delset = .TRUE.
         detype = field3
      ELSE

!  if the element named in field2 is not that of the previous card,
!  determine the characteristics of the element

         IF ( elmnt /= field2 ) THEN

!  look the name up in the dictionary to see if it already exists

            CALL HASH_insert( length, 12, field2 // 'EL', KEY,                      &
                         ITABLE, ifree )

!  if the element name is recognised, recover its characteristics

            IF ( ifree <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  RETURN
               END IF
               nelmnt = INLIST( - ifree )
            ELSE

!  the new element is the nelnum-th nonlinear element

               nelnum = nelnum + 1
               IF ( nelnum > nelmax ) THEN
                  status = - 9
                  RETURN
               END IF

!  record the elements position in the table along with its name

               INLIST( ifree ) = nelnum
               LNAMES( nelnum ) = field2
               nelmnt = nelnum
            END IF

!  record the nonlinear element's name

            elmnt = field2

!  if the element has not yet been allocated a type, set it

            IF ( field1 == 'T ' .OR. field1 == 'XT'                            &
                 .OR. ifree > 0 ) THEN

!  record the element type

               IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
                  field = field3 // 'ET'
               ELSE
                  IF ( delset ) THEN
                     field = detype // 'ET'
                  ELSE

!  the element name is new. check that if a default element type
!  is required, a default has been set

                     status = 41
                     IF ( out > 0 ) WRITE( out, 2410 )
                     RETURN
                  END IF
               END IF

!  if the group is non-trivial, determine its characteristics

               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield <= 0 ) THEN
                  status = 9
                  IF ( out > 0 ) WRITE( out, 2090 ) field3
                  RETURN
               END IF

!  determine the number of the element type, k, the starting
!  addresses for the parameters and variables for the element
!  and the number of parameters and variables involved

               k = INLIST( ifield )
               ITYPEE( nelnum ) = k
               ISTEP ( nelnum ) = nlisep + 1
               ISTAEV( nelnum ) = nlisev + 1
               nepars = IEPA( k + 1 ) - IEPA( k )
               nevars = IELV( k + 1 ) - IELV( k )
               IF ( nlisev + nevars > nevmax ) THEN
                  status = - 15
                  RETURN
               END IF
               IF ( nlisep + nepars > nepvmx ) THEN
                  status = - 17
                  RETURN
               END IF

!  initialize the set of problem variables

               DO 10 i = 1, nevars
                  IELVAR( nlisev + i ) = 0
   10          CONTINUE

!  initialize the set of parameter values

               DO 20 i = 1, nepars
                  EPVALU( nlisep + i ) = biginf
   20          CONTINUE

!  find the starting addresses for the lists of the elements
!  parameters and variables

               nlisep = nlisep + nepars
               nlisev = nlisev + nevars
               IF ( field1 == 'T ' .OR. field1 == 'XT' ) RETURN
            END IF
         END IF

!  check that the cards are in the correct order

         IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
            status = 27
            IF ( out > 0 ) WRITE( out, 2270 )
            RETURN
         END IF

!  determine the number of the element type, k, the starting
!  addresses for the parameters and variables for the element
!  and the number of parameters and variables involved

         k = ITYPEE( nelmnt )
         nepars = IEPA( k + 1 ) - IEPA( k )
         nevars = IELV( k + 1 ) - IELV( k )
         mlisep = ISTEP( nelmnt ) - 1
         mlisev = ISTAEV( nelmnt ) - 1
         ip = IEPA( k ) - 1
         is = IELV( k ) - 1

!  the card contains names of elemental variables

         IF ( field1 == 'V ' .OR. field1 == 'ZV' ) THEN

!  the elemental variable is defined in field3

            DO 110 i = 1, nevars
               IF ( field3 == ENAMES( is + i ) ) GO TO 120
  110       CONTINUE

!  the elemental variable name is not recognised

            status = 15
            IF ( out > 0 ) WRITE( out, 2150 )
            RETURN
  120       CONTINUE

!  check that the variable has not already been set

            IF ( IELVAR( mlisev + i ) /= 0 ) THEN
               status = 30
               IF ( out > 0 ) WRITE( out, 2300 )
               RETURN
            END IF

!  search the table for the name of the input variable

            CALL HASH_insert( length, 12, FIELD5//'VA', KEY, ITABLE, ifree )
            IF ( ifree <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  RETURN
               END IF

!  the variable has appeared before. store its number

               IELVAR( mlisev + i ) = INLIST( - ifree )
            ELSE

!  the variable is completely new (and thus nonlinear)
!  it will be recorded as variable n

               n = n + 1
               IF ( n > nmax ) THEN
                  status = - 7
                  RETURN
               END IF

!  record the position of the new group in the table, record its
!  name, initialise its type as trivial and record its status as
!  an equality group

               INLIST( ifree ) = n
               VNAMES( n ) = field5
               IELVAR( mlisev + i ) = n
            END IF
         ELSE

!  the card contains names and values of elemental parameters

            IF ( field1 == 'P ' .OR. field1 == 'XP' .OR.                       &
                 field1 == 'ZP' ) THEN
               IF ( novals > 0 ) THEN
                  DO 230 j = 1, novals

!  check the name has not already been used in the current element
!  the parameter name occurs in field3 or field5

                     DO 210 i = 1, nepars
                        IF (                                                   &
                        ( j == 1 .AND. field3 == EPNAME( ip + i ) )            &
                   .OR. ( j == 2 .AND. field5 == EPNAME( ip + i ) )            &
                             ) GO TO 220
  210                CONTINUE

!  the elemental parameter name is not recognised

                     status = 28
                     IF ( out > 0 ) WRITE( out, 2280 )                       &
                        LNAMES( nelnum ), EPNAME( ip + i )
                     RETURN

!  the elemental parameter name is the i-th parameter in the list

  220                CONTINUE

!  check that the value has not already been set

                     IF ( EPVALU( mlisep + i ) < biginf ) THEN
                        status = 29
                        IF ( out > 0 ) WRITE( out, 2290 )
                        RETURN
                     END IF

!  read the associated value from field4 or field 6

                     IF ( j == 1 ) THEN
                        EPVALU( mlisep + i ) = value4
                     ELSE
                        EPVALU( mlisep + i ) = value6
                     END IF
  230             CONTINUE
               END IF

!  field1 not recognised

            ELSE
               status = 10
               IF ( out > 0 ) WRITE( out, 2100 ) field1
               RETURN
            END IF
         END IF
      END IF
      status = 0
      RETURN

!  non-executable statements

 2090 FORMAT( ' ** Exit from GPSMPS - element type not recognised:',           &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in ELEMENT USES section ' )
 2150 FORMAT( ' ** Exit from GPSMPS - element variable unrecognised ' )
 2260 FORMAT( ' ** Exit from GPSMPS - duplicate default element type ' )
 2270 FORMAT( ' ** Exit from GPSMPS - type for element already set ' )
 2280 FORMAT( ' ** Exit from GPSMPS - element ', A10, ' parameter ',           &
              A10, ' unrecognised ' )
 2290 FORMAT( ' ** Exit from GPSMPS - element parameter already set ' )
 2300 FORMAT( ' ** Exit from GPSMPS - element variable already set ' )
 2410 FORMAT( ' ** Exit from GPSMPS - element type unrecognised ' )

!  end of subroutine SEUSES

      END SUBROUTINE SEUSES

!-*-*-*-*-*- S I F D E C O D E   S G T Y P E    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SGTYPE( ngrmax, ngpmax, novals, length, ngrtyp, ngpn,         &
                         setana, INLIST, IGPA, ITABLE, field1, field2, field3, &
                         field5, ANAMES, GTYPES, GPNAME, KEY, out, status )
      INTEGER :: out, status, novals, length
      INTEGER :: ngrmax, ngpmax
      INTEGER :: ngrtyp, ngpn
      LOGICAL :: setana
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: ITABLE ( length ), IGPA( ngrmax )
      INTEGER :: INLIST( length )
      CHARACTER ( len = 10 ) :: ANAMES( ngrmax ), GTYPES( ngrmax )
      CHARACTER ( len = 10 ) :: GPNAME( NGPMAX)
      CHARACTER ( len = 12 ) :: KEY( length )

!  ----------------------------
!  indicator card is group type
!  ----------------------------

!  local variables

      INTEGER :: i, ifree, k

!  check if this is the first group type

      IF ( ngrtyp == 0 ) THEN
         CALL HASH_insert( length, 12, field2 // 'GT', KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               RETURN
            END IF
            status = 17
            IF ( out > 0 ) WRITE( out, 2170 )
            RETURN
         END IF
         ngrtyp = 1
         ngpn = 0
         setana = .FALSE.
         INLIST( ifree ) = ngrtyp
         IGPA( ngrtyp ) = ngpn + 1
         GTYPES( ngrtyp ) = field2
      END IF

!  check if the group-type is new

      IF ( field2 /= GTYPES( ngrtyp ) ) THEN

!  check that the argument for the previous group-type has been set

         IF ( .NOT. setana ) THEN
            status = 25
            IF ( out > 0 ) WRITE( out, 2250 )
            RETURN
         END IF

!  record the name and starting address of the new group type

         CALL HASH_insert( length, 12, field2 // 'GT', KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               RETURN
            END IF
            status = 17
            IF ( out > 0 ) WRITE( out, 2170 )
            RETURN
         END IF
         ngrtyp = ngrtyp + 1
         setana = .FALSE.
         IF ( ngrtyp > ngrmax ) THEN
            status = - 4
            RETURN
         END IF
         INLIST( ifree ) = ngrtyp
         IGPA( ngrtyp ) = ngpn + 1
         GTYPES( ngrtyp ) = field2
      END IF

!  input the name of the group-type argument

      IF ( field1 == 'GV' ) THEN
         setana = .TRUE.
         ANAMES( ngrtyp ) = field3
      ELSE

!  input the name of an group parameter

         IF ( field1 == 'GP' ) THEN
            IF ( novals > 0 ) THEN
               DO 20 i = 1, novals

!  check the name has not already been used in the current group

                  DO 10 k = IGPA( ngrtyp ), ngpn
                      IF ( ( i == 1 .AND. field3 == GPNAME( k ) )              &
                      .OR. ( i == 2 .AND. field5 == GPNAME( k ) )              &
                      ) THEN
                        status = 24
                        IF ( out > 0 ) WRITE( out, 2240 )
                        RETURN
                     END IF
   10             CONTINUE

!  the name is new. record it in the array gpname

                  ngpn = ngpn + 1
                  IF ( ngpn > ngpmax ) THEN
                     status = - 20
                     RETURN
                  END IF
                  IF ( i == 1 ) THEN
                     GPNAME( ngpn ) = field3
                  ELSE
                     GPNAME( ngpn ) = field5
                  END IF
   20          CONTINUE
            END IF

!  field1 not recognised

         ELSE
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
         END IF
      END IF

!  non-executable statements

 2170 FORMAT( ' ** Exit from GPSMPS - duplicate group-type name ' )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in GROUP TYPE section ' )
 2240 FORMAT( ' ** Exit from GPSMPS - duplicate group param. name ' )
 2250 FORMAT( ' ** Exit from GPSMPS - no group-type arg. given ' )
 
!  end of subroutine SGTYPE

      END SUBROUTINE SGTYPE

!-*-*-*-*-*- S I F D E C O D E   S G U S E S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SGUSES( negmax, ngpmax, ngrmax, ngmax, ngpvmx,                &
                         length, ng, ngrupe, nlisgp, novals, neling,           &
                         ndtype, strtgu, grupe, IGPA, ITYPEG, IELING,          &
                         INLIST, ITABLE, ISTGP, ISTATE, dgrset, dgtype,        &
                         field1, field2, field3, value4, field5, value6,       &
                         GPTEMP, GPNAME, WEIGHT,                               &
                         KEY, out, status )
      INTEGER :: out, status, length
      INTEGER :: negmax, ngpmax, ngrmax, ngmax, ngpvmx
      INTEGER :: ng, nlisgp, neling, novals, ngrupe, ndtype
      REAL ( KIND = wp ) :: value4, value6
      LOGICAL :: dgrset, strtgu
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5, grupe, dgtype
      INTEGER :: INLIST( length ), ITABLE ( length )
      INTEGER :: IGPA( ngrmax ), IELING( negmax, 2 )
      INTEGER :: ITYPEG( ngmax )
      INTEGER :: ISTGP( ngmax ), ISTATE( ngmax )
      REAL ( KIND = wp ) :: GPTEMP( ngpvmx ), WEIGHT( negmax )
      CHARACTER ( len = 10 ) :: GPNAME( ngpmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ----------------------------
!  indicator card is group uses
!  ----------------------------

!  local variables

      INTEGER :: i, ifield, ip, j, k, mlisgp, ngpars
      CHARACTER ( len = 12 ) :: field

!  the current card defines a default type

      IF ( field2 == '''DEFAULT'' ' ) THEN
         IF ( dgrset ) THEN
            status = 42
            IF ( out > 0 ) WRITE( out, 2420 )
            RETURN
         END IF
         dgrset = .TRUE.
         dgtype = field3

!  find the number allocated to the group type

         CALL HASH_search( length, 12, dgtype // 'GT', KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            status = 19
            IF ( out > 0 ) WRITE( out, 2190 ) field3
            RETURN
         END IF

!  reset the defaults for each of the groups allocated in previous
!  sections

         ndtype = INLIST( ifield )
         DO 10 i = 1, ng
            IF ( ITYPEG( i ) == - 1 ) ITYPEG( i ) = - ndtype - 1
   10    CONTINUE
         RETURN
      END IF

!  if the group named in field2 is not that of the previous card,
!  determine the characteristics of the group

      IF ( .NOT. strtgu .OR. grupe /= field2 ) THEN
         strtgu = .TRUE.

!  look the name up in the dictionary to see if it exists

         CALL HASH_search( length, 12, field2 // 'GR', KEY,                         &
                      ITABLE, ifield )
         IF ( ifield > 0 ) THEN
            ngrupe = INLIST( ifield )
            ISTATE( ngrupe ) = - ABS( ISTATE( ngrupe ) )
         ELSE

!  the group name is unknown

            IF ( out > 0 ) WRITE( out, 2040 ) field2
            status = 4
            RETURN
         END IF

!  record the group's name

         grupe = field2

!  record the group type

         IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
            field = field3 // 'GT'

!  if the group is non-trivial, determine its characteristics

            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 19
               IF ( out > 0 ) WRITE( out, 2190 ) field3
               RETURN
            END IF

!  determine the number of the group type, k, the starting
!  addresses for the parameters for the group and the
!  number of parameters involved

            k = INLIST( ifield )
            IF ( k == 0 ) THEN
              ngpars = 0
            ELSE
              ngpars = IGPA( k + 1 ) - IGPA( k )
            END IF
            ITYPEG( ngrupe ) = k
            ISTGP ( ngrupe ) = nlisgp + 1
            IF ( nlisgp + ngpars > ngpvmx ) THEN
               status = - 18
               RETURN
            END IF

!  initialize the set of parameter values

            DO 50 i = 1, ngpars
               GPTEMP( nlisgp + i ) = biginf
   50       CONTINUE

!  find the starting addresses for the lists of the group parameters

            nlisgp = nlisgp + ngpars
            RETURN
         ELSE

!  the group is new and of default type. determine the starting
!  addresses for the parameters for the group and the
!  number of parameters involved

            k = ndtype
            IF ( k == 0 ) THEN
              ngpars = 0
            ELSE
              ngpars = IGPA( k + 1 ) - IGPA( k )
            END IF
            ITYPEG( ngrupe ) = k
            ISTGP ( ngrupe ) = nlisgp + 1
            IF ( nlisgp + ngpars > ngpvmx ) THEN
               status = - 18
               RETURN
            END IF

!  initialize the set of parameter values

            DO 55 i = 1, ngpars
               GPTEMP( nlisgp + i ) = biginf
   55       CONTINUE

!  find the starting addresses for the lists of the group parameters

            nlisgp = nlisgp + ngpars
         END IF
      END IF

!  check that the cards are in the correct order

      IF ( field1 == 'T ' .OR. field1 == 'XT' ) THEN
         status = 31
         IF ( out > 0 ) WRITE( out, 2310 )
         RETURN
      END IF

!  the card contains names of nonlinear elements

      IF ( field1 == 'E ' .OR. field1 == 'XE' .OR.                             &
           field1 == 'ZE' ) THEN
         IF ( novals > 0 ) THEN

!  check the name has not already been used in the current element
!  the parameter name occurs in field3 or field5

            DO 110 i = 1, novals
               IF ( i == 1 ) THEN
                  field = field3 // 'EL'
               ELSE
                  field = field5 // 'EL'
               END IF
               CALL HASH_search( length, 12, field, KEY, ITABLE, IFIELD)
               IF ( ifield > 0 ) THEN

!  the neling-th element has been assigned

                  neling = neling + 1
                  IF ( neling > negmax ) THEN
                     status = - 10
                     RETURN
                  END IF

!  the element is ieling(,1) and the group is given by ieling(,2)

                  IELING( neling, 1 ) = INLIST( ifield )
                  IELING( neling, 2 ) = ngrupe

!  the element is weighted by the constant weight()

                  IF ( i == 1 ) THEN
                     WEIGHT( neling ) = value4
                  ELSE
                     WEIGHT( neling ) = value6
                  END IF

!  the element name is unknown

               ELSE
                  status = 43
                  IF ( out > 0 ) WRITE( out, 2430 )
                  RETURN
               END IF
  110       CONTINUE
         END IF
      ELSE

!  the card contains names and values of elemental parameters

         IF ( field1 == 'P ' .OR. field1 == 'XP' .OR.                          &
              field1 == 'ZP' ) THEN

!  determine the number of the group type, k, the starting
!  addresses for the parameters for the group and the
!  number of parameters involved

            IF ( ITYPEG( ngrupe ) < 0 )                                        &
              ITYPEG( ngrupe ) = - ITYPEG( ngrupe ) - 1
            ITYPEG( ngrupe ) = ABS( ITYPEG( ngrupe ) )
            k = ITYPEG( ngrupe )
            ngpars = IGPA( k + 1 ) - IGPA( k )
            ip = IGPA( k ) - 1
            mlisgp = ISTGP( ngrupe ) - 1
            IF ( novals > 0 ) THEN
               DO 230 j = 1, novals

!  check the name has not already been used in the current element
!  the parameter name occurs in field3 or field5

                  DO 210 i = 1, ngpars
                     IF (                                                      &
                     ( j == 1 .AND. field3 == GPNAME( ip + i ) )               &
                .OR. ( j == 2 .AND. field5 == GPNAME( ip + i ) )               &
                          ) GO TO 220
  210             CONTINUE

!  the group parameter name is not recognised

                  status = 33
                  IF ( out > 0 ) WRITE( out, 2330 )
                  RETURN

!  the group parameter name is the i-th parameter in the list

  220             CONTINUE

!  check that the value has not already been set

                  IF ( GPTEMP( mlisgp + i ) < biginf ) THEN
                     status = 32
                     IF ( out > 0 ) WRITE( out, 2320 )
                     RETURN
                  END IF

!  read the associated value from field4 or field 6

                  IF ( j == 1 ) THEN
                     GPTEMP( mlisgp + i ) = value4
                  ELSE
                     GPTEMP( mlisgp + i ) = value6
                  END IF
  230          CONTINUE
            END IF

!  field1 not recognised

         ELSE
            status = 10
            IF ( out > 0 ) WRITE( out, 2100 ) field1
            RETURN
         END IF
      END IF
      status = 0
      RETURN

!  non-executable statements

 2040 FORMAT( ' ** Exit from GPSMPS - group/row name not recognised:',         &
              ' name is ', A10 )
 2100 FORMAT( ' ** Exit from GPSMPS - field 1 ', A2,                           &
              '  not recognised in GROUP USES section' )
 2190 FORMAT( ' ** Exit from GPSMPS - group type not recognised:',             &
              ' name is ', A10 )
 2310 FORMAT( ' ** Exit from GPSMPS - type for group already set ' )
 2320 FORMAT( ' ** Exit from GPSMPS - group parameter already set ' )
 2330 FORMAT( ' ** Exit from GPSMPS - group parameter unrecognised ' )
 2420 FORMAT( ' ** Exit from GPSMPS - default group type already set ' )
 2430 FORMAT( ' ** Exit from GPSMPS - element name not recognised ' )

!  end of subroutine SGUSES

      END SUBROUTINE SGUSES

!-*-*-*-*-*- S I F D E C O D E   S O B B N D    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE SOBBND( nobbnd, nobmax, nrlndx, length, INLIST, ITABLE,       &
                         FBOUND, REALVL, field1, field2, value4, field5,       &
                         OBNAME, KEY, single, out, status )
      INTEGER :: out, status, length, nobbnd, nobmax, nrlndx
      REAL ( KIND = wp ) :: value4
      LOGICAL :: single
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field5
      INTEGER :: INLIST( length ), ITABLE ( length )
      REAL ( KIND = wp ) :: FBOUND( 2, nobmax ), REALVL( nrlndx )
      CHARACTER ( len = 10 ) :: OBNAME( nobmax )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ------------------------------
!  indicator card is object bound
!  ------------------------------

!  local variables

      INTEGER :: ifree, ifield, j
      REAL ( KIND = wp ) :: big

      IF ( single ) THEN
         big = 9.0D-1 * HUGE( 1.0_sp )
      ELSE
         big = 9.0D-1 * HUGE( one )
      END IF

!  find a place to insert the objective bound name in the hash-table

      CALL HASH_insert( length, 12, field2 // 'OB', KEY, ITABLE, ifree )

!  the name already exists. it is the j-th name in the list

      IF ( ifree <= 0 ) THEN
         IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
         END IF
         j = INLIST( - ifree )
      ELSE

!  the objective function bound is the nobbnd-th specified

         nobbnd = nobbnd + 1
         IF( nobbnd > nobmax ) THEN
            status = - 23
            RETURN
         END IF
         j = nobbnd

!  record the default bounds

         FBOUND( 1, nobbnd ) = - big
         FBOUND( 2, nobbnd ) = big

!  record the position of the new bound in the table and record its

         INLIST( ifree ) = nobbnd
         OBNAME( nobbnd ) = field2
      END IF

!  record the bound given

      IF ( FIELD1( 1: 1 ) == 'Z' ) THEN
         CALL HASH_search( length, 12,  FIELD5( 1 : 10 ) // 'RI',                   &
                      KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD5( 1 : 10 )
            RETURN
         END IF
         value4 = REALVL( INLIST( ifield ) )
      END IF
      IF ( field1 == 'XL' .OR. field1 == 'ZL' .OR.                             &
           field1 == 'LO' ) FBOUND( 1, j ) = value4
      IF ( field1 == 'XU' .OR. field1 == 'ZU' .OR.                             &
           field1 == 'UP' ) FBOUND( 1, j ) = value4
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )

!  end of subroutine SOBBND

      END SUBROUTINE SOBBND

!-*-*-*-*-*- S I F D E C O D E   P R O C A I    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE PROCAI( nindex, nrlndx, length, nusein, nusere,               &
                         status, out, level, ninstr,                           &
                         debug, rvalue, INLIST, ITABLE,                        &
                         NAMIIN, NAMRIN, INSTR, KEY,                           &
                         field1, field2, field3, field5, field4 )
      INTEGER :: nindex, nrlndx, length, nusein, nusere
      INTEGER :: status, out, level, ninstr
      LOGICAL :: debug
      REAL ( KIND = wp ) :: rvalue
      CHARACTER ( len =  2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      CHARACTER ( len = 12 ) :: field4
      INTEGER :: INSTR( 5 ), INLIST( length ), ITABLE ( length )
      CHARACTER ( len = 10 ) :: NAMIIN( nindex ), NAMRIN( nrlndx )
      CHARACTER ( len = 12 ) :: KEY( length )

!  --------------------------------------------------------------------
!  construct a list of do-loop integer and real arithmetic instructions
!  --------------------------------------------------------------------

!  local variables

      INTEGER :: i, ifield, ifree
      CHARACTER ( len = 12 ) :: field

!  decide what sort of instruction is to be performed: integer instructions

      IF ( field1 == 'IE' .OR. field1 == 'IA' .OR.                             &
           field1 == 'IS' .OR. field1 == 'IM' .OR.                             &
           field1 == 'ID' .OR. field1 == 'IR' .OR.                             &
           field1 == 'I=' .OR. field1 == 'I+' .OR.                             &
           field1 == 'I-' .OR. field1 == 'I*' .OR.                             &
           field1 == 'I/' ) THEN
         IF ( field1 == 'IE' ) INSTR( 1 ) = 21
         IF ( field1 == 'IA' ) INSTR( 1 ) = 22
         IF ( field1 == 'IS' ) INSTR( 1 ) = 23
         IF ( field1 == 'IM' ) INSTR( 1 ) = 24
         IF ( field1 == 'ID' ) INSTR( 1 ) = 25
         IF ( field1 == 'IR' ) INSTR( 1 ) = 26
         IF ( field1 == 'I=' ) INSTR( 1 ) = 31
         IF ( field1 == 'I+' ) INSTR( 1 ) = 32
         IF ( field1 == 'I-' ) INSTR( 1 ) = 33
         IF ( field1 == 'I*' ) INSTR( 1 ) = 34
         IF ( field1 == 'I/' ) INSTR( 1 ) = 35
         IF ( field1 == 'IE' .OR. field1 == 'IA' .OR.                          &
              field1 == 'IS' .OR. field1 == 'IM' .OR.                          &
              field1 == 'ID' ) THEN

!  read the integer value, ivalue, from field 4

            CALL GETINT( field4, INSTR( 4 ) )
         ELSE

!  obtain the integer value, ivalue, as the value of the index in
!  field 5, first ensuring that the index exists

            IF ( field1 /= 'I=' .AND. field1 /= 'IR' ) THEN
               field = FIELD5( 1 : 10 ) // 'II'
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield <= 0 ) THEN
                  status = 3
                  IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 4 ) = INLIST( ifield )
            END IF
         END IF

!  if a definition is to be made from a previously defined index,
!  ensure that the index exists

         IF ( field1 /= 'IE' ) THEN
            IF ( field1 /= 'IR' ) THEN
               field = FIELD3( 1 : 10 ) // 'II'
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield <= 0 ) THEN
                  status = 3
                  IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 3 ) = INLIST( ifield )
            ELSE
               field = FIELD3( 1 : 10 ) // 'RI'
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield <= 0 ) THEN
                  status = 3
                  IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 3 ) = INLIST( ifield )
            END IF
         END IF

!  record the address of the index which is to be set

         field = FIELD2( 1 : 10 ) // 'II'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               RETURN
            END IF
            ifree = - ifree
         ELSE
            nusein = nusein + 1
            IF ( nusein > nindex ) THEN
               status = - 21
               RETURN
            END IF
            INLIST( ifree ) = nusein
            NAMIIN( nusein ) = FIELD( 1 : 10 )
         END IF
         INSTR( 2 ) = INLIST( ifree )

!  print details of the instruction

         IF ( debug .AND. out > 0 ) THEN
            IF ( INSTR( 1 ) == 21 )                                            &
               WRITE( out, 4030 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) == 22 )                                            &
               WRITE( out, 4040 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) == 23 )                                            &
               WRITE( out, 4041 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) == 24 )                                            &
               WRITE( out, 4050 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ), INSTR( 4 )
            IF ( INSTR( 1 ) == 25 )                                            &
               WRITE( out, 4051 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), INSTR( 4 ), NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 26 )                                            &
               WRITE( out, 4055 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 31 )                                            &
               WRITE( out, 4059 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 32 )                                            &
               WRITE( out, 4060 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ),                     &
               NAMIIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) == 33 )                                            &
               WRITE( out, 4061 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 4 ) ),                     &
               NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 34 )                                            &
               WRITE( out, 4070 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ),                     &
               NAMIIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) == 35 )                                            &
               WRITE( out, 4071 ) level, ninstr,                               &
               NAMIIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) ),                     &
               NAMIIN( INSTR( 4 ) )
         END IF
      ELSE

!  real instructions

         IF ( field1 == 'RE' ) INSTR( 1 ) = 51
         IF ( field1 == 'RA' ) INSTR( 1 ) = 52
         IF ( field1 == 'RS' ) INSTR( 1 ) = 53
         IF ( field1 == 'RM' ) INSTR( 1 ) = 54
         IF ( field1 == 'RD' ) INSTR( 1 ) = 55
         IF ( field1 == 'RI' ) INSTR( 1 ) = 56
         IF ( field1 == 'RF' ) INSTR( 1 ) = 57
         IF ( field1 == 'R=' ) INSTR( 1 ) = 61
         IF ( field1 == 'R+' ) INSTR( 1 ) = 62
         IF ( field1 == 'R-' ) INSTR( 1 ) = 63
         IF ( field1 == 'R*' ) INSTR( 1 ) = 64
         IF ( field1 == 'R/' ) INSTR( 1 ) = 65
         IF ( field1 == 'R(' ) INSTR( 1 ) = 67
         IF ( field1 == 'RE' .OR. field1 == 'RA' .OR.                          &
              field1 == 'RS' .OR. field1 == 'RM' .OR.                          &
              field1 == 'RD' .OR. field1 == 'RF' ) THEN

!  read the real value, rvalue, from field 4

            CALL GETVAL( field4, rvalue )
         END IF
         IF ( field1 == 'R+' .OR. field1 == 'R-' .OR.                          &
              field1 == 'R*' .OR. field1 == 'R/' .OR.                          &
              field1 == 'R(' ) THEN

!  obtain the real value, rvalue, as the value associated with the real
!  index in field 5, first ensuring that the index exists

            field = FIELD5( 1 : 10 ) // 'RI'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            INSTR( 4 ) = INLIST( ifield )
         END IF

!  if a definition is to be made from a previously defined index,
!  ensure that the index exists

         IF ( field1 == 'RI' ) THEN
            field = FIELD3( 1 : 10 ) // 'II'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            INSTR( 3 ) = INLIST( ifield )
         ELSE
            IF ( field1 /= 'RF' .AND. field1 /= 'R(' .AND.                     &
                 field1 /= 'RE' ) THEN
               field = FIELD3( 1 : 10 ) // 'RI'
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield <= 0 ) THEN
                  status = 3
                  IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
                  RETURN
               END IF
               INSTR( 3 ) = INLIST( ifield )
            ELSE

!  the value is to be obtained using a special function. determine
!  which one

               IF ( field1 /= 'RE' ) THEN
                  DO i = 1, nfunct
                    IF ( FIELD3( 1 : 10 ) == FUNCTN( i ) ) GO TO 20
                  END DO
                  status = 39
                  IF ( out > 0 ) WRITE( out, 2390 ) FIELD3( 1 : 10)
                  RETURN
   20             CONTINUE
                  INSTR( 3 ) = i
               END IF
            END IF
         END IF

!  record the address of the index which is to be set

         field = FIELD2( 1 : 10 ) // 'RI'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               RETURN
            END IF
            ifree = - ifree
         ELSE
            nusere = nusere + 1
            IF ( nusere > nrlndx ) THEN
               status = - 22
               RETURN
            END IF
            INLIST( ifree ) = nusere
            NAMRIN( nusere ) = FIELD( 1 : 10 )
         END IF
         INSTR( 2 ) = INLIST( ifree )

!  print details of the instruction

         IF ( debug .AND. out > 0 ) THEN
            IF ( INSTR( 1 ) == 51 )                                            &
               WRITE( out, 4130 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), rvalue
            IF ( INSTR( 1 ) == 52 )                                            &
               WRITE( out, 4140 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ), rvalue
            IF ( INSTR( 1 ) == 53 )                                            &
               WRITE( out, 4141 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ), rvalue
            IF ( INSTR( 1 ) == 54 )                                            &
               WRITE( out, 4150 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ), rvalue
            IF ( INSTR( 1 ) == 55 )                                            &
               WRITE( out, 4151 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), rvalue, NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 56 )                                            &
               WRITE( out, 4180 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMIIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 57 )                                            &
               WRITE( out, 4110 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), FUNCTN( INSTR( 3 ) ), rvalue
            IF ( INSTR( 1 ) == 61 )                                            &
               WRITE( out, 4159 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 62 )                                            &
               WRITE( out, 4160 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ),                     &
               NAMRIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) == 63 )                                            &
               WRITE( out, 4161 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 4 ) ),                     &
               NAMRIN( INSTR( 3 ) )
            IF ( INSTR( 1 ) == 64 )                                            &
               WRITE( out, 4170 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ),                     &
               NAMRIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) == 65 )                                            &
               WRITE( out, 4171 ) level, ninstr,                               &
               NAMRIN( INSTR( 2 ) ), NAMRIN( INSTR( 3 ) ),                     &
               NAMRIN( INSTR( 4 ) )
            IF ( INSTR( 1 ) == 67 )                                            &
               WRITE( out, 4120 ) level, ninstr, NAMRIN( INSTR( 2 ) ),         &
               FUNCTN( INSTR( 3 ) ), NAMRIN( INSTR( 4 ) )
         END IF
      END IF
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2390 FORMAT( ' ** Exit from GPSMPS - specified function name ', A10,          &
              ' not recognised ' )
 4030 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', i6 )
 4040 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to the value ', i6 )
 4041 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from the value ', i6 )
 4050 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' by the value ', i6 )
 4051 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing the value ', i6, ' by ', A10 )
 4055 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the integer equivalent of ', A10 )
 4059 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to ', A10 )
 4060 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to ', A10 )
 4061 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from ', A10 )
 4070 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' and ', A10 )
 4071 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing ', A10, ' by ', A10 )
 4110 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', A6, '(', 1P, D12.4, ')' )
 4120 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', A6, '(', A10, ')' )
 4130 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the value ', 1P, D12.4 )
 4140 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to the value ', 1P, D12.4 )
 4141 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from the value ', 1P, D12.4 )
 4150 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' by the value ', 1P, D12.4 )
 4151 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing the value ', 1P, D12.4, ' by ', A10 )
 4159 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to ', A10 )
 4160 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by adding ', A10, ' to ', A10 )
 4161 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by subtracting ', A10, ' from ', A10 )
 4170 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by multiplying ', A10, ' and ', A10 )
 4171 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' by dividing ', A10, ' by ', A10 )
 4180 FORMAT( ' Level ', i2, ' instruction ', i4, ' set ', A10,                &
              ' to the fl. pt. value of ', A10 )

!  end of subroutine PROCAI

      END SUBROUTINE PROCAI

!-*-*-*-*-*- S I F D E C O D E   P R O C A D    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE PROCAD( nindex, nrlndx, level, ninstr, nusere,                &
                         length, narray, intype, status, out,                  &
                         debug, grp1st,                                        &
                         field1, field2, field3, field5,                       &
                         field4, field6,                                       &
                         INLIST, INSTR, ITABLE, IARRAY,                        &
                         VARRAY, farray, NAMIIN, NAMRIN,                       &
                         ARRAY, CARRAY, KEY )
      INTEGER :: nindex, nrlndx, level, length, status, out
      INTEGER :: ninstr, nusere, intype, narray
      LOGICAL :: debug, grp1st
      CHARACTER ( len =  2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      CHARACTER ( len = 12 ) :: field4, field6
      INTEGER :: INSTR( 5 ), IARRAY( 5, 3 )
      INTEGER :: INLIST( length ), ITABLE ( length )
      REAL ( KIND = wp ) :: VARRAY( 2 )
      CHARACTER ( len =  2 ) :: farray
      CHARACTER ( len = 10 ) :: NAMIIN( nindex ), NAMRIN( nrlndx )
      CHARACTER ( len = 10 ) :: ARRAY( 3 ), CARRAY( 2 )
      CHARACTER ( len = 12 ) :: KEY( length )

!  ---------------------------------------------
!  construct a list of do-loop array definitions
!  ---------------------------------------------

!  local variables

      INTEGER :: i, kindar, ifree

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1
      INTEGER, PARAMETER :: mfixed = 2
      INTEGER, PARAMETER :: mfree = 3
      INTEGER, PARAMETER :: mname =  4
      INTEGER, PARAMETER :: mrows =  5
      INTEGER, PARAMETER :: mgroup =  6
      INTEGER, PARAMETER :: mcnstr =  7
      INTEGER, PARAMETER :: mcols =  8
      INTEGER, PARAMETER :: mvars =  9
      INTEGER, PARAMETER :: mconst = 10
      INTEGER, PARAMETER :: mrhs = 11
      INTEGER, PARAMETER :: mrhsp = 12
      INTEGER, PARAMETER :: mrange = 13
      INTEGER, PARAMETER :: mbound = 14
      INTEGER, PARAMETER :: mstart = 15
      INTEGER, PARAMETER :: mqhess = 16
      INTEGER, PARAMETER :: mquadr = 17
      INTEGER, PARAMETER :: mquads = 18
      INTEGER, PARAMETER :: mquado = 19
      INTEGER, PARAMETER :: mqsect = 20
      INTEGER, PARAMETER :: mqmatr = 21
      INTEGER, PARAMETER :: metype = 22
      INTEGER, PARAMETER :: meuses = 23
      INTEGER, PARAMETER :: mgtype = 24
      INTEGER, PARAMETER :: mguses = 25
      INTEGER, PARAMETER :: mobbnd = 26
      INTEGER, PARAMETER :: mendat = 27

!  determine how much information must be saved by determining
!  the kind of array definition being made

      kindar = - 1

!  real index array definitions

      IF ( field1 == 'AE' .OR. field1 == 'AA' .OR.                             &
           field1 == 'AS' .OR. field1 == 'AM' .OR.                             &
           field1 == 'AD' .OR. field1 == 'AI' .OR.                             &
           field1 == 'A=' .OR. field1 == 'A+' .OR.                             &
           field1 == 'A-' .OR. field1 == 'A*' .OR.                             &
           field1 == 'A/' .OR. field1 == 'AF' .OR.                             &
           field1 == 'A(' ) THEN
         IF ( field1 == 'AE' ) kindar = 107
         IF ( field1 == 'AF' ) kindar = 108
         IF ( field1 == 'A(' ) kindar = 105
         IF ( field1 == 'AA' ) kindar = 101
         IF ( field1 == 'AS' ) kindar = 101
         IF ( field1 == 'AM' ) kindar = 101
         IF ( field1 == 'AD' ) kindar = 101
         IF ( field1 == 'A=' ) kindar = 103
         IF ( field1 == 'A+' ) kindar = 104
         IF ( field1 == 'A-' ) kindar = 104
         IF ( field1 == 'A*' ) kindar = 104
         IF ( field1 == 'A/' ) kindar = 104
         IF ( field1 == 'AI' ) kindar = 106
      ELSE

!  groups section

         IF ( intype == mrows  .OR. intype == mgroup .OR.                      &
              intype == mcnstr ) THEN
            IF ( FIELD1( 2: 2 ) == 'N' .OR.                                    &
                 FIELD1( 2: 2 ) == 'G' .OR.                                    &
                 FIELD1( 2: 2 ) == 'L' .OR.                                    &
                 FIELD1( 2: 2 ) == 'E' ) THEN
               IF ( grp1st ) THEN
                  IF ( FIELD3( 1: 7 ) == '''SCALE''' ) THEN
                     IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                        kindar = 115
                     ELSE
                        kindar = 108
                     END IF
                  ELSE
                     kindar = 100
                  END IF
               ELSE
                  IF ( FIELD3( 1: 7 ) == '''SCALE''' ) THEN
                     IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                        kindar = 115
                     ELSE
                        kindar = 108
                     END IF
                  ELSE
                     IF ( FIELD3( 1: 10 ) == '          ' ) THEN
                        kindar = 100
                     ELSE
                        IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                           kindar = 113
                        ELSE
                           IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                              kindar = 101
                           ELSE
                              kindar = 102
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF

!  variables section

         IF ( intype == mcols  .OR. intype == mvars ) THEN
            IF ( grp1st ) THEN
               IF ( FIELD3( 1: 7 ) == '''SCALE''' ) THEN
                  IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                     kindar = 115
                  ELSE
                     kindar = 108
                  END IF
               ELSE IF ( field3 == '''ZERO-ONE''' .OR.                         &
                         field3 == '''INTEGER'' ' ) THEN
                  kindar = 106
               ELSE
                  IF ( FIELD3( 1: 10 ) == '          ' ) THEN
                     kindar = 100
                  ELSE
                     IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                        kindar = 113
                     ELSE
                        IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                           kindar = 101
                        ELSE
                           kindar = 102
                        END IF
                     END IF
                  END IF
               END IF
            ELSE
               IF ( FIELD3( 1: 7 ) == '''SCALE''' ) THEN
                  IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                     kindar = 115
                  ELSE
                     kindar = 108
                  END IF
               ELSE IF ( field3 == '''ZERO-ONE''' .OR.                         &
                         field3 == '''INTEGER'' ' ) THEN
                  kindar = 106
               ELSE
                  kindar = 100
               END IF
            END IF
         END IF

!  constants section

         IF ( intype == mconst .OR. intype == mrhs  .OR.                       &
              intype == mrhsp  ) THEN
            IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
               kindar = 116
            ELSE
               IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                  kindar = 111
               ELSE
                  kindar = 112
               END IF
            END IF
         END IF

!  ranges section

         IF ( intype == mrange ) THEN
            IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
               kindar = 116
            ELSE
               IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                  kindar = 111
               ELSE
                  kindar = 112
               END IF
            END IF
         END IF

!  bounds section

         IF ( intype == mbound ) THEN
            IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
               kindar = 116
            ELSE
               IF ( FIELD1( 2: 2 ) == 'R' .OR.                                 &
                    FIELD1( 2: 2 ) == 'M' .OR.                                 &
                    FIELD1( 2: 2 ) == 'P' ) kindar = 110
               IF ( FIELD1( 2: 2 ) == 'L' .OR.                                 &
                    FIELD1( 2: 2 ) == 'U' .OR.                                 &
                    FIELD1( 2: 2 ) == 'X' ) kindar = 111
            END IF
         END IF

!  start point section

         IF ( intype == mstart ) THEN
            IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
               kindar = 116
            ELSE
               IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                  kindar = 111
               ELSE
                  kindar = 112
               END IF
            END IF
         END IF

!  Hessian section

         IF ( intype == mquadr .OR. intype == mquads .OR.                      &
              intype == mquado .OR. intype == mqsect .OR.                      &
              intype == mqhess .OR. intype == MQMATR) THEN
            IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
               kindar = 113
            ELSE
               IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                  kindar = 101
               ELSE
                  kindar = 102
               END IF
            END IF
         END IF

!  element uses section

         IF ( intype == meuses ) THEN
            IF ( FIELD1( 2: 2 ) == 'T' ) kindar = 106
            IF ( FIELD1( 2: 2 ) == 'V' ) kindar = 105
            IF ( FIELD1( 2: 2 ) == 'P' ) THEN
               IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                  kindar = 115
               ELSE
                  IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                     kindar = 108
                  ELSE
                     kindar = 109
                  END IF
               END IF
            END IF
         END IF

!  group uses section

         IF ( intype == mguses ) THEN
            IF ( FIELD1( 2: 2 ) == 'T' ) kindar = 106
            IF ( FIELD1( 2: 2 ) == 'E' ) THEN
               IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                  kindar = 113
               ELSE
                  IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                     kindar = 101
                  ELSE
                     kindar = 102
                     IF ( FIELD6( 1: 12 ) == '            ' )                  &
                          FIELD6( 1: 3 ) = '1.0'
                  END IF
                  IF ( FIELD4( 1: 12 ) == '            ' )                     &
                       FIELD4( 1: 3 ) = '1.0'
               END IF
            END IF
            IF ( FIELD1( 2: 2 ) == 'P' ) THEN
               IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
                  kindar = 115
               ELSE
                  IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                     kindar = 108
                  ELSE
                     kindar = 109
                  END IF
               END IF
            END IF
         END IF

!  ranges section

         IF ( intype == mobbnd ) THEN
            IF ( FIELD1( 1: 1 )  == 'Z' ) THEN
               kindar = 116
            ELSE
               IF ( FIELD5( 1: 10 ) == '          ' ) THEN
                  kindar = 111
               ELSE
                  kindar = 112
               END IF
            END IF
         END IF
      END IF

!  check that the type of array definition has been recognised

      IF ( kindar < 0 ) THEN
         IF ( out > 0 ) WRITE( out, 2140 )
         status = 14
         RETURN
      ELSE
         farray = field1
         INSTR( 1 ) = kindar
         INSTR( 2 ) = narray
      END IF

!  an array name occurs in field 2. interpret the contents of this
!  field

      IF ( ( kindar >= 100 .AND. kindar <= 109 ) .OR.                          &
           ( kindar >= 113 .AND. kindar <= 115 ) ) THEN
         CALL INTFIE( length, 12, KEY, ITABLE, INLIST,                         &
                      field2, ARRAY( 1 ),                                      &
                      IARRAY( 1, 1 ), out, status )
         IF ( status /= 0 ) RETURN
         IF ( debug .AND. out > 0 ) WRITE( out, 4080 ) level,                  &
                          ninstr, ARRAY( 1 ),                                  &
                         ( NAMIIN( IARRAY( 2 + i, 1 ) ),                       &
                            i = 1, IARRAY( 2, 1 ) )

!  if the array name is just a scalar name, record its name

         IF ( field1 == 'AE' .OR. field1 == 'AA' .OR.                          &
              field1 == 'AS' .OR. field1 == 'AM' .OR.                          &
              field1 == 'AD' .OR. field1 == 'AI' .OR.                          &
              field1 == 'A=' .OR. field1 == 'A+' .OR.                          &
              field1 == 'A-' .OR. field1 == 'A*' .OR.                          &
              field1 == 'A/' .OR. field1 == 'AF' .OR.                          &
              field1 == 'A(' ) THEN
            IF ( IARRAY( 2, 1 ) == 0 ) THEN
               CALL HASH_insert( length, 12, ARRAY( 1 ) // 'RI',                    &
                            KEY, ITABLE, ifree )
               IF ( ifree <= 0 ) THEN
                  IF ( ifree == 0 ) THEN
                     status = - 1
                     RETURN
                  END IF
               ELSE
                  nusere = nusere + 1
                  IF ( nusere > nrlndx ) THEN
                     status = - 22
                     RETURN
                  END IF
                  INLIST( ifree ) = nusere
                  NAMRIN( nusere ) = ARRAY( 1 )
               END IF
            END IF
         END IF
      END IF

!  an array name occurs in field 3. interpret the contents of this
!  field

      IF ( ( kindar >= 101 .AND. kindar <= 104 ) .OR.                          &
           ( kindar >= 110 .AND. kindar <= 112 ) .OR.                          &
             kindar == 113 .OR.  kindar == 116 ) THEN
         CALL INTFIE( length, 12, KEY, ITABLE, INLIST,                         &
                      field3, ARRAY( 2 ),                                      &
                      IARRAY( 1, 2 ), out, status )
         IF ( status /= 0 ) RETURN
         IF ( debug .AND. out > 0 ) WRITE( out, 4090 ) level,                  &
                          ninstr, ARRAY( 2 ),                                  &
                         ( NAMIIN( IARRAY( 2 + i, 2 ) ),                       &
                            i = 1, IARRAY( 2, 2 ) )
      END IF

!  an array name occurs in field 5. interpret the contents of this
!  field

      IF ( kindar == 102 .OR.  kindar == 104 .OR.                              &
           kindar == 105 .OR.  kindar == 112 .OR.                              &
         ( kindar >= 113 .AND. kindar <= 116 ) ) THEN
         CALL INTFIE( length, 12, KEY, ITABLE, INLIST,                         &
                      field5, ARRAY( 3 ),                                      &
                      IARRAY( 1, 3 ), out, status )
         IF ( status /= 0 ) RETURN
         IF ( debug .AND. out > 0 ) WRITE( out, 4100 ) level,                  &
                          ninstr, ARRAY( 3 ),                                  &
                         ( NAMIIN( IARRAY( 2 + i, 3 ) ),                       &
                           i = 1, IARRAY( 2, 3 ) )
      END IF

!  an name occurs in field 2

      IF ( ( kindar >= 110 .AND. kindar <= 112 ) .OR.                          &
             kindar == 116 ) THEN
         CARRAY( 1 ) = field2
         IF ( debug .AND. out > 0 ) WRITE( out, 4110 ) level,                  &
                          ninstr, CARRAY( 1 )
      END IF

!  an name occurs in field 3

      IF ( kindar == 105 .OR. kindar == 106 .OR.                               &
           kindar == 108 .OR. kindar == 109 .OR.                               &
           kindar == 115 ) THEN
         CARRAY( 1 ) = field3
         IF ( debug .AND. out > 0 ) WRITE( out, 4120 ) level,                  &
                          ninstr, CARRAY( 1 )
      END IF

!  an name occurs in field 5

      IF ( kindar == 109 ) THEN
         CARRAY( 2 ) = field5
         IF ( debug .AND. out > 0 ) WRITE( out, 4130 ) level,                  &
                          ninstr, CARRAY( 2 )
      END IF

!  a numerical value occurs in field 4

      IF ( kindar == 101 .OR. kindar == 102 .OR.                               &
           kindar == 107 .OR. kindar == 108 .OR.                               &
           kindar == 109 .OR. kindar == 111 .OR.                               &
           kindar == 112 ) THEN
         CALL GETVAL( field4, VARRAY( 1 ) )
         IF ( debug .AND. out > 0 ) WRITE( out, 4140 ) level,                  &
                          ninstr, VARRAY( 1 )
      END IF

!  a numerical value occurs in field 6

      IF ( kindar == 102 .OR. kindar == 109 .OR.                               &
           kindar == 112 ) THEN
         CALL GETVAL( field6, VARRAY( 2 ) )
         IF ( debug .AND. out > 0 ) WRITE( out, 4150 ) level,                  &
                          ninstr, VARRAY( 2 )
      END IF
      RETURN

!  non-executable statements

 2140 FORMAT( ' ** Exit from GPSMPS - type of array defn. unrecognised')
 4080 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 2 array ',           &
                A10, ' indices ', 3( A10, 1X ) )
 4090 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 3 array ',           &
                A10, ' indices ', 3( A10, 1X ) )
 4100 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 5 array ',           &
                A10, ' indices ', 3( A10, 1X ) )
 4110 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 2 name ', A10)
 4120 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 3 name ', A10)
 4130 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 5 name ', A10)
 4140 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 4 value ',           &
              1P, D12.4 )
 4150 FORMAT( ' Level ', i2, ' instruction ', i4, ' field 6 value ',           &
              1P, D12.4 )

!  end of subroutine PROCAD

      END SUBROUTINE PROCAD

!-*-*-*-*-*- S I F D E C O D E   P R O C A A    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE PROCAA( nindex, length, nusere, status, out, nrlndx, INLIST,  &
                         ITABLE, NAMRIN, KEY, INDVAL, REALVL, field1, field2,  &
                         field3, field5, rvalue )
      INTEGER :: nindex, length, nusere, status, out
      INTEGER :: nrlndx
      REAL ( KIND = wp ) :: rvalue
      CHARACTER ( len =  2 ) :: field1
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INLIST( length ), ITABLE ( length )
      INTEGER :: INDVAL( nindex )
      REAL ( KIND = wp ) :: REALVL( nrlndx )
      CHARACTER ( len = 10 ) :: NAMRIN( nrlndx )
      CHARACTER ( len = 12 ) :: KEY( length )

!  --------------------------------------------------------------------------
!  construct and execute a list of do-loop real array arithmetic instructions
!  --------------------------------------------------------------------------

!  local variables

      INTEGER :: i, ifield, ifree, instr2, instr3, instr4
      CHARACTER ( len = 12 ) :: field

      IF ( field1 == 'A+' .OR. field1 == 'A-' .OR.                             &
           field1 == 'A*' .OR. field1 == 'A/' .OR.                             &
           field1 == 'A(' ) THEN

!  obtain the real value, rvalue, as the value associated with the real
!  index in field 5, first ensuring that the index exists

         field = FIELD5( 1 : 10 ) // 'RI'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
            RETURN
         END IF
         instr4 = INLIST( ifield )
      END IF

!  if a definition is to be made from a previously defined index,
!  ensure that the index exists

      IF ( field1 == 'AI' ) THEN
         field = FIELD3( 1 : 10 ) // 'II'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            status = 3
            IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
            RETURN
         END IF
         instr3 = INLIST( ifield )
      ELSE
         IF ( field1 /= 'AF' .AND. field1 /= 'A(' .AND.                        &
              field1 /= 'AE' ) THEN
            field = FIELD3( 1 : 10 ) // 'RI'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            instr3 = INLIST( ifield )
         ELSE

!  the value is to be obtained using a special function. determine
!  which one

            IF ( field1 /= 'AE' ) THEN
               DO i = 1, nfunct
                 IF ( FIELD3( 1 : 10 ) == FUNCTN( i ) ) GO TO 20
               END DO
               status = 39
               IF ( out > 0 ) WRITE( out, 2390 ) FIELD3( 1 : 10 )
               RETURN
   20          CONTINUE
               instr3 = i
            END IF
         END IF
      END IF

!  record the address of the index which is to be set

      field = FIELD2( 1 : 10 ) // 'RI'
      CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
      IF ( ifree <= 0 ) THEN
         IF ( ifree == 0 ) THEN
            status = - 1
            RETURN
         END IF
         ifree = - ifree
      ELSE
         nusere = nusere + 1
         IF ( nusere > nrlndx ) THEN
            status = - 22
            RETURN
         END IF
         INLIST( ifree ) = nusere
         NAMRIN( nusere ) = FIELD( 1 : 10 )
      END IF
      instr2 = INLIST( ifree )
      IF ( field1 == 'AE' ) REALVL( instr2 ) = rvalue
      IF ( field1 == 'AA' ) REALVL( instr2 ) =                                 &
                              rvalue + REALVL( instr3 )
      IF ( field1 == 'AS' ) REALVL( instr2 ) =                                 &
                              rvalue - REALVL( instr3 )
      IF ( field1 == 'AM' ) REALVL( instr2 ) =                                 &
                              rvalue * REALVL( instr3 )
      IF ( field1 == 'AD' ) REALVL( instr2 ) =                                 &
                              rvalue / REALVL( instr3 )
      IF ( field1 == 'AI' ) REALVL( instr2 ) =                                 &
                              FLOAT( INDVAL( instr3 ) )
      IF ( field1 == 'AF' ) CALL RINTRN( REALVL( instr2 ),                     &
                                           rvalue, instr3, status )
      IF ( field1 == 'A=' ) REALVL( instr2 ) = REALVL( instr3 )
      IF ( field1 == 'A+' ) REALVL( instr2 ) =                                 &
                              REALVL( instr3 ) + REALVL( instr4 )
      IF ( field1 == 'A-' ) REALVL( instr2 ) =                                 &
                              REALVL( instr3 ) - REALVL( instr4 )
      IF ( field1 == 'A*' ) REALVL( instr2 ) =                                 &
                              REALVL( instr3 ) * REALVL( instr4 )
      IF ( field1 == 'A/' ) REALVL( instr2 ) =                                 &
                              REALVL( instr3 ) / REALVL( instr4 )
      IF ( field1 == 'A(' ) CALL RINTRN( REALVL( instr2 ),                     &
                              REALVL( instr4 ), instr3, status )
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2390 FORMAT( ' ** Exit from GPSMPS - specified function name ', A10,          &
              ' not recognised ' )

!  end of subroutine PROCAA

      END SUBROUTINE PROCAA

!-*-*-*-*-*- S I F D E C O D E   G E T F I E    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETFIE( nindex, INDVAL, IARRAY, array, field, status )
      INTEGER ::NINDEX, status
      INTEGER ::INDVAL( nindex )
      INTEGER ::IARRAY( 5 )
      CHARACTER ( len = 10 ) :: array, field

!  -----------------------------------------------------------
!  construct an expanded array name from its constituent parts
!  -----------------------------------------------------------

!  local variables

      INTEGER ::I, indces, ivalue, j, ndigit
      CHARACTER ( len = 9 ) :: field9

      j = IARRAY( 1 )
      field = ARRAY( 1: j )
      j = j + 1
      indces = IARRAY( 2 )
      DO 10 i = 1, indces
         ivalue = INDVAL( IARRAY( 2 + i ) )
         IF ( ivalue == 0 ) THEN
            ndigit = 1
         ELSE
            ndigit = INT( LOG10( ABS( FLOAT( ivalue ) ) ) ) + 1
            IF ( ivalue < 0 ) ndigit = ndigit + 1
         END IF
         IF ( ( i < indces .AND. j + ndigit > 10 ) .OR.                        &
              ( i == indces .AND. j + ndigit > 11 ) ) THEN
            status = 35
            RETURN
         END IF
         WRITE( UNIT = field9, FMT = 2000 ) ivalue
         FIELD( J: j + ndigit - 1 ) = FIELD9( 10 - NDIGIT: 9 )
         j = j + ndigit
         IF ( i < indces ) THEN
            FIELD( J: j ) = ','
            j = j + 1
         END IF
  10  CONTINUE
      status = 0
      RETURN

!  non-executable statements

 2000 FORMAT( I9 )

!  end of subroutine GETFIE

      END SUBROUTINE GETFIE

!-*-*-*-*-*- S I F D E C O D E   G E T I I N    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETIIN( nindex, INDVAL, nrlndx, REALVL, INSTR )
      INTEGER :: nindex, nrlndx
      INTEGER :: INDVAL( nindex ), INSTR( 4 )
      REAL ( KIND = wp ) :: REALVL( nrlndx )

!  ----------------------------------
!  execute integer index instructions
!  ----------------------------------

      IF ( INSTR( 1 ) == 21 ) INDVAL( INSTR( 2 ) ) = INSTR( 4 )
      IF ( INSTR( 1 ) == 22 ) INDVAL( INSTR( 2 ) ) =                           &
         INSTR( 4 ) + INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 23 ) INDVAL( INSTR( 2 ) ) =                           &
         INSTR( 4 ) - INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 24 ) INDVAL( INSTR( 2 ) ) =                           &
         INSTR( 4 ) * INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 25 ) INDVAL( INSTR( 2 ) ) =                           &
         INSTR( 4 ) / INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 26 ) INDVAL( INSTR( 2 ) ) =                           &
         INT( REALVL( INSTR( 3 ) ) )
      IF ( INSTR( 1 ) == 31 ) INDVAL( INSTR( 2 ) ) =                           &
         INDVAL( INSTR( 3 ) )
      IF ( INSTR( 1 ) == 32 ) INDVAL( INSTR( 2 ) ) =                           &
         INDVAL( INSTR( 3 ) ) + INDVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) == 33 ) INDVAL( INSTR( 2 ) ) =                           &
         INDVAL( INSTR( 3 ) ) - INDVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) == 34 ) INDVAL( INSTR( 2 ) ) =                           &
         INDVAL( INSTR( 3 ) ) * INDVAL( INSTR( 4 ) )
      IF ( INSTR( 1 ) == 35 ) INDVAL( INSTR( 2 ) ) =                           &
         INDVAL( INSTR( 3 ) ) / INDVAL( INSTR( 4 ) )
      RETURN

!  end of subroutine GETIIN

      END SUBROUTINE GETIIN

!-*-*-*-*-*- S I F D E C O D E   G E T R I N    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETRIN( nindex, nrlndx, INDVAL, REALVL,                       &
                         rvalue, INSTR, status )
      INTEGER :: nindex, nrlndx, status
      REAL ( KIND = wp ) ::    rvalue
      INTEGER :: INSTR( 4 ), INDVAL( nindex )
      REAL ( KIND = wp ) ::    REALVL( nrlndx )

!  -------------------------------
!  execute real index instructions
!  -------------------------------

!  local variables

      INTEGER :: i

      status = 0
      i = INSTR( 1 ) - 50
      GO TO ( 110, 120, 130, 140, 150, 160, 170, 300, 300, 300,                &
              210, 220, 230, 240, 250, 300, 270, 300, 300 ), i
  110 CONTINUE
      REALVL( INSTR( 2 ) ) = rvalue
      RETURN
  120 CONTINUE
      REALVL( INSTR( 2 ) ) = rvalue + REALVL( INSTR( 3 ) )
      RETURN
  130 CONTINUE
      REALVL( INSTR( 2 ) ) = rvalue - REALVL( INSTR( 3 ) )
      RETURN
  140 CONTINUE
      REALVL( INSTR( 2 ) ) = rvalue * REALVL( INSTR( 3 ) )
      RETURN
  150 CONTINUE
      REALVL( INSTR( 2 ) ) = rvalue / REALVL( INSTR( 3 ) )
      RETURN
  160 CONTINUE
      REALVL( INSTR( 2 ) ) = FLOAT( INDVAL( INSTR( 3 ) ) )
      RETURN
  170 CONTINUE
      CALL RINTRN( REALVL( INSTR( 2 ) ), rvalue, INSTR( 3 ), status )
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
      CALL RINTRN( REALVL( INSTR( 2 ) ),                                       &
                   REALVL( INSTR( 4 ) ), INSTR( 3 ), status )
      RETURN
  300 CONTINUE
      RETURN

!  end of subroutine GETRIN

      END SUBROUTINE GETRIN

!-*-*-*-*-*- S I F D E C O D E   R I N T R N    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE RINTRN( evalue, value, ivalue, status )
      INTEGER :: ivalue, status
      REAL ( KIND = wp ) :: value, evalue

!  -------------------------------------------------------
!  returns the value of the appropriate intrinsic function
!  -------------------------------------------------------

!  local variables

      REAL ( KIND = wp ) :: dvalue

      dvalue = DBLE( value )
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120,               &
              130, 140 ), ivalue
   10 CONTINUE
      evalue = ABS( dvalue )
      RETURN
   20 CONTINUE
      IF ( value < 0.0 ) THEN
         status = 40
         WRITE( 6, 2400 ) value, 'SQRT  '
      ELSE
         evalue = SQRT( dvalue )
      END IF
      RETURN
   30 CONTINUE
      evalue = EXP( dvalue )
      RETURN
   40 CONTINUE
      IF ( value <= 0.0 ) THEN
         status = 40
         WRITE( 6, 2400 ) value, 'LOG   '
      ELSE
         evalue = LOG( dvalue )
      END IF
      RETURN
   50 CONTINUE
      IF ( value <= 0.0 ) THEN
         status = 40
         WRITE( 6, 2400 ) value, 'LOG10 '
      ELSE
         evalue = LOG10( dvalue )
      END IF
      RETURN
   60 CONTINUE
      evalue = SIN( dvalue )
      RETURN
   70 CONTINUE
      evalue = COS( dvalue )
      RETURN
   80 CONTINUE
      evalue = TAN( dvalue )
      RETURN
   90 CONTINUE
      IF ( ABS( value ) > 1.0 ) THEN
         status = 40
         WRITE( 6, 2400 ) value, 'ASIN  '
      ELSE
         evalue = ASIN( dvalue )
      END IF
      RETURN
  100 CONTINUE
      IF ( ABS( value ) > 1.0 ) THEN
         status = 40
         WRITE( 6, 2400 ) value, 'ACOS  '
      ELSE
         evalue = ACOS( dvalue )
      END IF
      RETURN
  110 CONTINUE
      evalue = ATAN( dvalue )
      RETURN
  120 CONTINUE
      evalue = SINH( dvalue )
      RETURN
  130 CONTINUE
      evalue = COSH( dvalue )
      RETURN
  140 CONTINUE
      evalue = TANH( dvalue )
      RETURN
 2400 FORMAT( ' ** Exit from GPSMPS - argument value ', 1P, D9.1,              &
              ' is illegal for function ', A6 )

!  end of subroutine RINTRN

      END SUBROUTINE RINTRN

!-*-*-*-*-*- S I F D E C O D E   G E T I N T    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETINT( field, ivalue )
      INTEGER :: ivalue
      CHARACTER ( len = 12 ) :: field

!  ------------------------------------------------------------
!  read the integer number ivalue stored in the character field
!  ------------------------------------------------------------

!  local variables

      INTEGER :: i, j
      CHARACTER ( len = 12 ) :: field2

!  right-shift the field, eliminating blanks

      field2 = '            '
            j = 12
      DO 10 i = 12, 1, - 1
         IF ( FIELD( i : i ) == ' ' ) GO TO 10
         FIELD2( j : j ) = FIELD( i : i )
         j = j - 1
   10 CONTINUE
      READ( UNIT = field2, FMT = 2000 ) ivalue
      RETURN

!  non-executable statements

 2000 FORMAT( I12 )

!  end of subroutine GETINT

      END SUBROUTINE GETINT

!-*-*-*-*-*- S I F D E C O D E   G E T L I N    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETLIN( nindex, nrlndx, INDVAL, IARRAY, VARRAY,               &
                         array, CARRAY, farray, REALVL, NAMIIN, novals,        &
                         kindar, field1, field2, field3, value4,               &
                         field5, value6, out, status,                          &
                         length, KEY, ITABLE, INLIST )
      INTEGER :: nindex, nrlndx, kindar, novals, out, status
      INTEGER :: length
      REAL ( KIND = wp ) :: value4, value6
      CHARACTER ( len =  2 ) :: field1, farray
      CHARACTER ( len = 10 ) :: field2, field3, field5
      INTEGER :: INDVAL( nindex ), IARRAY( 5, 3 )
      INTEGER :: INLIST ( length ), ITABLE ( length )
      REAL ( KIND = wp ) :: VARRAY( 2 ), REALVL( nrlndx )
      CHARACTER ( len =  7 ) :: NAMIIN( nindex )
      CHARACTER ( len = 10 ) :: ARRAY( 3 ), CARRAY( 2 )
      CHARACTER ( len = 12 ) :: KEY   ( length )

!  -----------------------------------------------------------------
!  translate the contents of an array card to its scalar values
!  the expected contents are at the control of the parameter kindar
!  as follows: (n=name,a=array name, v=numerical value,
!               r=real index array value)

!  kindar       field2    field3    field4     field5     field6
!  ------       ------    ------    ------     ------     ------
!   100            a
!   101            a         a         v
!   102            a         a         v          a          v
!   103            a         a
!   104            a         a                    a
!   105            a         n                    a
!   106            a         n
!   107            a                   v
!   108            a         n         v
!   109            a         n         v          n          v
!   110            n         a
!   111            n         a         v
!   112            n         a         v          a          v
!   113            a         a          < ------- r
!   114            a                    < ------- r
!   115            a         n          < ------- r
!   116            n         a          < ------- r
!  -----------------------------------------------------------------

!  local variables

      INTEGER :: i, ifield
      CHARACTER ( len = 12 ) :: field

      field1 = farray
      novals = 0

!  an array name occurs in field 2. interpret the contents of this
!  field

      IF ( ( kindar >= 100 .AND. kindar <= 109 ) .OR.                          &
           ( kindar >= 113 .AND. kindar <= 115 ) ) THEN
         CALL GETFIE( nindex, INDVAL, IARRAY( 1, 1 ),                          &
                      ARRAY( 1 ), field2, status )
         IF ( status /= 0 ) THEN
            IF ( out > 0 ) WRITE( out, 2350 )                                  &
               ARRAY( 1 )( 1: IARRAY( 1, 1 ) ),                                &
               ( NAMIIN( IARRAY( 2 + i, 1 ) ),                                 &
                 INDVAL( IARRAY( 2 + i, 1 ) ), i = 1, IARRAY( 2, 1 ) )
            status = 35
            RETURN
         END IF
      ELSE
         field2 = '          '
      END IF

!  an array name occurs in field 3. interpret the contents of this
!  field

      IF ( ( kindar >= 101 .AND. kindar <= 104 ) .OR.                          &
           ( kindar >= 110 .AND. kindar <= 112 ) .OR.                          &
             kindar == 113 .OR.  kindar == 116 ) THEN
         CALL GETFIE( nindex, INDVAL, IARRAY( 1, 2 ),                          &
                      ARRAY( 2 ), field3, status )
         IF ( status /= 0 ) THEN
            IF ( out > 0 ) WRITE( out, 2350 )                                  &
               ARRAY( 2 )( 1: IARRAY( 1, 2 ) ),                                &
               ( NAMIIN( IARRAY( 2 + i, 2 ) ),                                 &
                 INDVAL( IARRAY( 2 + i, 2 ) ), i = 1, IARRAY( 2, 2 ) )
            status = 35
            RETURN
         END IF
      ELSE
         field3 = '          '
      END IF
      IF ( kindar == 103 ) novals = 1

!  an array name occurs in field 5. interpret the contents of this
!  field

      IF ( kindar == 102 .OR.  kindar == 104 .OR.                              &
           kindar == 105 .OR.  kindar == 112 .OR.                              &
         ( kindar >= 113 .AND. kindar <= 116 ) ) THEN
         CALL GETFIE( nindex, INDVAL, IARRAY( 1, 3 ),                          &
                      ARRAY( 3 ), field5, status )
         IF ( status /= 0 ) THEN
            IF ( out > 0 ) WRITE( out, 2350 )                                  &
               ARRAY( 3 )( 1: IARRAY( 1, 3 ) ),                                &
               ( NAMIIN( IARRAY( 2 + i, 3 ) ),                                 &
                 INDVAL( IARRAY( 2 + i, 3 ) ), i = 1, IARRAY( 2, 3 ) )
            status = 35
            RETURN
         END IF
      ELSE
         field5 = '          '
      END IF
      IF ( kindar == 104 ) novals = 2

!  an name occurs in field 2

      IF ( ( kindar >= 110 .AND. kindar <= 112 ) .OR.                          &
             kindar == 116 ) THEN
         field2 = CARRAY( 1 )
      END IF

!  an name occurs in field 3

      IF ( kindar == 105 .OR. kindar == 106 .OR.                               &
           kindar == 108 .OR. kindar == 109 .OR.                               &
           kindar == 115 ) THEN
         field3 = CARRAY( 1 )
      END IF

!  an name occurs in field 5

      IF ( kindar == 109 ) THEN
         field5 = CARRAY( 2 )
      END IF

!  a numerical value occurs in field 4

      IF ( kindar == 101 .OR. kindar == 102 .OR.                               &
           kindar == 107 .OR. kindar == 108 .OR.                               &
           kindar == 109 .OR. kindar == 111 .OR.                               &
           kindar == 112 ) THEN
         value4 = VARRAY( 1 )
         novals = 1
      ELSE

!  a real index value is to be places in field 4

         IF ( kindar == 113 .OR. kindar == 114 .OR.                            &
              kindar == 115 .OR. kindar == 116 ) THEN
            field = FIELD5( 1 : 10 ) // 'RI'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1 : 10 )
               RETURN
            END IF
            value4 = REALVL( INLIST( ifield ) )
            novals = 1
         ELSE
            value4 = 0.0D+0
         END IF
      END IF

!  a numerical value occurs in field 6

      IF ( kindar == 102 .OR. kindar == 109 .OR.                               &
           kindar == 112 ) THEN
         value6 = VARRAY( 2 )
         novals = 2
      ELSE
         value6 = 0.0D+0
      END IF
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2350 FORMAT( ' ** Exit from GPSMPS - expanded array name > 10 chars.',        &
              ' Array name = ', A9, /, ( '    Index ', A10,                    &
              ' has the value ', i6, : ) )

!  end of subroutine GETLIN

      END SUBROUTINE GETLIN

!-*-*-*-*-*- S I F D E C O D E   G E T V A L    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETVAL( field, value )
      REAL ( KIND = wp ) :: value
      CHARACTER ( len = 12 ) :: field

!  --------------------------------------------------------
!  read the real number value stored in the character field
!  --------------------------------------------------------

      READ( UNIT = field, FMT = "( BN, F12.0 )" ) value
      RETURN

!  end of subroutine GETVAL

      END SUBROUTINE GETVAL

!-*-*-*-*-*- S I F D E C O D E   I N T F I E    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE INTFIE( length, nchar, KEY, ITABLE, INLIST,                   &
                         fielda, array, IARRAY, out, status )
      INTEGER :: length, nchar, out, status
      CHARACTER ( len = 10 ) :: fielda, array
      CHARACTER ( len = 12 ) :: KEY   ( length )
      INTEGER :: IARRAY( 5 )
      INTEGER :: INLIST ( length )
      INTEGER :: ITABLE ( length )

!  --------------------------------
!  interpret the contents of fielda
!  --------------------------------

!  local variables

      INTEGER :: i, ifield, indces, j
      CHARACTER ( len = 12 ) :: field

!  first find the array name by searching the field for the string '('

      DO 10 i = 1, 10
         IF ( FIELDA( I: i ) == '(' ) GO TO 20
   10 CONTINUE
!     if ( iout > 0 ) write( iout, 2370 ) fielda
!     inform = 37
      IARRAY( 1 ) = 10
      IARRAY( 2 ) = 0
      array = fielda
      status = 0
      RETURN

!  the string '(' occurs in position i

   20 CONTINUE
      j = i - 1
      IARRAY( 1 ) = j
      array = FIELDA( 1: j )
      indces = 0

!  now find the array indices. search for one of the strings ')' or ','

   30 CONTINUE
         j = i + 1
         DO 40 i = j, 10
            IF ( FIELDA( I: i ) == ',' .OR.                                    &
                 FIELDA( I: i ) == ')' ) GO TO 50
   40    CONTINUE
         IF ( out > 0 ) WRITE( out, 2370 ) fielda
         status = 37
         RETURN

!  the string ',' or ')' occurs in position j

   50    CONTINUE
         IF ( i /= j ) THEN
            indces = indces + 1
            IF ( indces > 3 ) THEN
               IF ( out > 0 ) WRITE( out, 2360 )
               status = 36
               RETURN
            END IF
            FIELD( 1: 12 ) = '          II'
            FIELD( 1: i - j ) = FIELDA( J: i - 1 )

!  check that the array index exists and determine its address

            CALL HASH_search( length, nchar, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               status = 3
               IF ( out > 0 ) WRITE( out, 2030 ) FIELD( 1: 10 )
               RETURN
            END IF
            IARRAY( 2 + indces ) = INLIST( ifield )
         END IF
         IF ( FIELDA( I: i ) == ',' ) GO TO 30

!  the array definition is complete

      IARRAY( 2 ) = indces
      status = 0
      RETURN

!  non-executable statements

 2030 FORMAT( ' ** Exit from GPSMPS - index parameter name ', A10,             &
              ' not recognised ' )
 2370 FORMAT( ' ** Exit from GPSMPS - incorrect array name', A10,              &
              ' in do-loop ')
 2360 FORMAT( ' ** Exit from GPSMPS - > 3 array name indices ' )

!  end of subroutine INTFIE

      END SUBROUTINE INTFIE

!-*-*-*-*-*-*- S I F D E C O D E   O N L Y 1    F U N C T I O N -*-*-*-*-*-*-*-

      FUNCTION ONLY1( num )
      INTEGER :: only1
      INTEGER :: num

!  -------------------------------------------------
!  returns the value 1 if num is one and 2 otherwise
!  -------------------------------------------------

      ONLY1 = 2
      IF ( num == 1 ) ONLY1 = 1
      RETURN

!  end of FUNCTION ONLY1

      END FUNCTION ONLY1

!-*-*-*-*-*- S I F D E C O D E   I N L A N C    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE INLANC( n, nlvars, ng, nelnum, nobj, length,                  &
                         lstadg, lelvar, lstaev, lntvar, licna, lstada,        &
                         la, lb, lbnds, lintre, liwk, lwk, nmax,               &
                         ngmax, nbmax, nsmax, nlmax, nelmax, negmax,           &
                         nobmax, ngrmax, ngpvmx, nepvmx, nomax, nlisgp,        &
                         nbnd, nnza, nconst, nstart, nrange, nobjgr, nobbnd,   &
                         neltyp, ngrtyp, pname, nameob, namerh, namera,        &
                         namebn, namest, nameof, ISTADG, IELVAR, ISTAEV,       &
                         INTVAR, ICNA, ISTADA, ICOORD, INLIST, ITABLE,         &
                         ISTATE, IDROWS, IELV, IINV, IGPA, IELING,             &
                         ISTEP, ISTGP, ITYPEE, ITYPEG, ITYPEV, IWK,            &
                         A, BND, VSTART, CSTART, RSCALE, CSCALE,               &
                         RDROWS, DFAULT, WEIGHT, BNDFLT, WK,                   &
                         GPVALU, EPVALU, FBOUND, ABYROW, B, BL, BU, X,         &
                         CLMULT, ESCALE, GSCALE, VSCALE, INTREP, GXEQX,        &
                         KEY, GNAMES, VNAMES, BNAMES, SNAMES, ONAMES,          &
                         ETYPES, GTYPES, OBNAME, ialgor, iauto,                &
                         out, outda, single, status, debug  )

      INTEGER :: n, ng, length, lstadg, lelvar, lstaev, lntvar
      INTEGER :: lstada, la, lb, lbnds, lintre, liwk, lwk
      INTEGER :: nmax, ngmax, nbmax, nsmax, nlmax, nlvars
      INTEGER :: negmax, ngrmax, ngpvmx, nepvmx, nlisgp, nomax
      INTEGER :: nbnd, nnza, nconst, nstart, nrange, nobjgr
      INTEGER :: nelmax, ialgor, out, outda, status, nobbnd
      INTEGER :: neltyp, ngrtyp, licna, nobmax, nobj, nelnum
      INTEGER :: iauto
      LOGICAL :: single, debug
      CHARACTER ( len = 8 ) :: pname
      CHARACTER ( len = 10 ) :: nameob, namerh, namera, namebn, namest,NAMEOF
      INTEGER :: ISTADG( lstadg ), IELVAR( lelvar ), ICNA( licna )
      INTEGER :: ISTAEV( lstaev ), INTVAR( lntvar )
      INTEGER :: ISTADA( lstada ), ITABLE( length )
      INTEGER :: ICOORD( la, 2  ), INLIST( length )
      INTEGER :: ISTATE( ngmax  ), IDROWS( 2, ngmax )
      INTEGER :: IELV  ( nlmax  ), IINV  ( nlmax  )
      INTEGER :: ISTEP ( nelmax ), ISTGP ( ngmax  )
      INTEGER :: IELING( negmax ), IGPA  ( ngrmax ) 
      INTEGER :: ITYPEE( nelmax ), ITYPEG( ngmax ), IWK( liwk )
      INTEGER :: ITYPEV( nmax )
      REAL ( KIND = wp ) :: A( la ),  RDROWS( 2, ngmax ), WEIGHT( negmax )
      REAL ( KIND = wp ) :: BND( 2, nmax, nbmax ), VSTART( nmax, nsmax )
      REAL ( KIND = wp ) :: BNDFLT( 2, nbmax ), CSTART( ngmax, nsmax )
      REAL ( KIND = wp ) :: RSCALE( ngmax ), CSCALE( nmax )
      REAL ( KIND = wp ) :: GPVALU( ngpvmx ), EPVALU( nepvmx )
      REAL ( KIND = wp ) :: DFAULT( nmax ), FBOUND( 2, nobmax ), WK( lwk )
      REAL ( KIND = wp ) :: ABYROW( la ), B( lb ), BL( lbnds ), BU( lbnds )
      REAL ( KIND = wp ) :: X( nmax ), GSCALE( ngmax ), ESCALE( negmax )
      REAL ( KIND = wp ) :: VSCALE( nmax ), CLMULT( ngmax )
      LOGICAL :: INTREP( lintre ), GXEQX( ngmax )
      CHARACTER ( len = 12 ) :: KEY( length  )
      CHARACTER ( len = 10 ) :: GNAMES( ngmax ), BNAMES( nbmax )
      CHARACTER ( len = 10 ) :: ONAMES( nomax ), OBNAME( nobmax )
      CHARACTER ( len = 10 ) :: VNAMES( nmax  ), SNAMES( nsmax )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax ), GTYPES( ngrmax )

!  ---------------------------------------------------------------------------
!  convert the output from gpsmps into a form suitable for any of the LANCELOT
!  programs SBMIN ( ialgor = 1 ), AUGLG ( ialgor = 2 ) or BARIA ( ialgor = 3 )
!  ---------------------------------------------------------------------------

!  local variables

      INTEGER :: i, ic, ifield, ig, irow, is, itype, j, jbnd, k
      INTEGER :: k1, k2, nelv, ninv, ng1, nel, ngpv, ngr
      INTEGER :: jstart, nel1, jcol, jconst, jrange
      INTEGER :: nnz, nslack, jobbnd
      REAL ( KIND = wp ) :: avalue, rrow
      REAL ( KIND = wp ) :: OBFBND( 2 )
      CHARACTER ( len = 12 ) :: field

      IF ( liwk <  ng .OR. lwk < MAX( n, nlisgp ) ) THEN
        status = - 1
        IF ( out  > 0 ) WRITE( out, 2000 )
        GO TO 800
      END IF

!  decide which optimization method to use
!  ---------------------------------------

      IF ( ialgor <= 0 ) THEN
         ialgor = 1
         DO 10 ig = 1, ng
            IF ( ABS( ISTATE( ig ) ) >= 2 ) ialgor = 2
   10    CONTINUE   
      END IF
      DO 20 ig = 1, ng
         ISTATE( ig ) = ABS( ISTATE( ig ) )
   20 CONTINUE   

!  select the bounds on the variables
!  -----------------------------------

      IF ( nbnd == 1 ) THEN
         jbnd = 1
      ELSE

!  find the key word in the list of bounds

         DO 110 jbnd = 1, nbnd
            IF ( namebn == BNAMES( jbnd ) ) GO TO 120
  110    CONTINUE
         status = 47
         IF ( out > 0 ) WRITE( out, 2470 ) namebn
         GO TO 800
  120    CONTINUE
      END IF

!  the required vector of bounds is column jbnd of bnd. copy this
!  vector into bl( ) and bu( ). record the scale factors

      DO 130 j = 1, nlvars
         BL( j ) = BND( 1, j, jbnd )
         BU( j ) = BND( 2, j, jbnd )
         VSCALE( j ) = CSCALE( j )
  130 CONTINUE

!  the bounds on the nonlinear variables are set to default values

      DO 140 j = nlvars + 1, n
         BL( j ) = BNDFLT( 1, jbnd )
         BU( j ) = BNDFLT( 2, jbnd )
  140 CONTINUE

!  select the constant/rhs and ranges
!  -----------------------------------

!  find the named constant (r.h.s.) vector in the list

      IF ( nconst == 1 ) THEN
         jconst = nlvars + 1
      ELSE
         field = namerh // 'CO'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
         IF ( ifield > 0 ) THEN
            jconst = INLIST( ifield )
         ELSE
            status = 46
            IF ( out > 0 ) WRITE( out, 2460 ) namerh
            GO TO 800
         END IF
      END IF

!  find the named range vector in the list

      IF ( nrange > 0 ) THEN
         IF ( nrange == 1 ) THEN
            jrange = nlvars + nconst + 1
         ELSE
            field = namera // 'RA'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield > 0 ) THEN
               jrange = INLIST( ifield )
            ELSE
               status = 48
               IF ( out > 0 ) WRITE( out, 2480 ) namera
               GO TO 800
            END IF
         END IF
      ELSE
         jrange = 0
      END IF

!  initialize the vector of constants, b, as its default value

      DO 210 i = 1, ng
        IF ( GNAMES ( i ) == cqgrou ) THEN
         B( i ) = zero
         BL( n + i ) = zero
         BU( n + i ) = biginf
         GSCALE( i ) = one
        ELSE
         B( i ) = DFAULT( jconst )

!  initialize lower and upper bounds on slack variables as zero
!  and the default respectively

         BL( n + i ) = zero
         IF ( nrange == 0 ) THEN
            BU( n + i ) = biginf
         ELSE
            BU( n + i ) = DFAULT( jrange )
         END IF

!  record the group scale factors

         GSCALE( i ) = one / RSCALE( i )
        END IF
  210 CONTINUE

!  sweep through the entries of a. look for entries in columns
!  jconst and jrange. subsequently remove all constant/rhs and
!  range columns to leave only entries corresponding to linear
!  elements

      nnz = 0
      DO 220 k = 1, nnza
         i = ICOORD( k, 1 )
         j = ICOORD( k, 2 )
         avalue = A( k )

!  see if the entry belongs to the selected constant/rhs vector

         IF ( j == jconst ) B( i ) = avalue

!  see if the entry belongs to the selected range vector

         IF ( j == jrange ) BU( n + i ) = avalue

!  check if the entry belongs to a linear element

         IF ( j <= nlvars ) THEN
            nnz = nnz + 1

!  record the coordinates and value of the entry from the linear
!  element

            ICOORD( nnz, 1 ) = i
            ICOORD( nnz, 2 ) = j
            A( nnz ) = avalue
         END IF
  220 CONTINUE
      nnza = nnz

!  the matrix is stored in coordinate form. resort it so that
!  it is stored by rows

      IF ( nnza > 0 ) CALL REORDA( ng, nnza, ICOORD( 1, 2 ),                   &
                                      ICOORD( 1, 1 ), A, ISTADA, IWK )

!  decode the 'd'-groups/rows. set the workspace array wk to zero

      nnz = 0
      nslack = 0
      DO 310 i = 1, n
         WK( i ) = zero
  310 CONTINUE

!  gxeqx is true if the group function is trivial

      DO 315 ig = 1, ng
        GXEQX( ig ) = ITYPEG( ig ) == 0
  315 CONTINUE

!  set the coefficients of the linear elements
!  --------------------------------------------

!  consider the groups in order

      DO 390 ig = 1, ng
         k1 = ISTADA( ig )
         ISTADA( ig ) = nnz + 1
         IF ( ISTATE( ig ) <= 4 ) THEN

!  first pass: determine the nonzeros in the row

            DO 320 k = k1, ISTADA( ig + 1 ) - 1
               j = ICOORD( k, 2 )
               IF ( WK( j ) /= zero ) THEN
                  WK( j ) = WK( j ) + A( k )
               ELSE
                  WK( j ) = A( k )
               END IF
  320       CONTINUE

!  second pass: only record nonzeros

            DO 325 k = k1, ISTADA( ig + 1 ) - 1
               j = ICOORD( k, 2 )
               IF ( WK( j ) /= zero ) THEN
                  nnz = nnz + 1
                  ICNA( nnz ) = j
                  ABYROW( nnz ) = WK( j )
                  WK( j ) = zero
!               else
!                 write(6,*) ' duplicate or zero removed ', j, a( k )
               END IF
  325       CONTINUE
         ELSE

!  the ig-th group is a 'd'-group. construct the new group from its
!  two donors. consider each donor row in turn. form the new row in wk

            DO 340 i = 1, 2
               irow = IDROWS( i, ig )
               rrow = RDROWS( i, ig )
               DO 330 k = ISTADA( irow ), ISTADA( irow + 1 ) - 1
                  ic = ICNA( k )
                  IF ( ic <= nlvars ) WK( ic ) = WK( ic ) +                    &
                                   ABYROW( k ) * rrow
  330          CONTINUE
  340       CONTINUE

!  move the new row into abyrow, resetting wk to zero as we proceed

            DO 360 i = 1, 2
               irow = IDROWS( i, ig )
               DO 350 k = ISTADA( irow ), ISTADA( irow + 1 ) - 1
                  ic = ICNA( k )
                  IF ( ic <= nlvars ) THEN
                     IF ( WK( ic ) /= zero ) THEN
                        nnz = nnz + 1
                        ICNA( nnz ) = ic
                        ABYROW( nnz ) = WK( ic )
                        WK( ic ) = zero
                     END IF
                  END IF
  350          CONTINUE
  360       CONTINUE
         END IF

!  if the group is of type 'l' or 'g', insert a slack variable in
!  the linear element

         is = ISTATE( ig )
         IF ( is > 4 ) is = is - 4
         IF ( ialgor <= 2 ) THEN
            IF ( is /= 1 .AND. is /= 2 ) THEN
               IF ( .NOT. GXEQX( ig ) ) THEN
                 WRITE( out, 1990 )
                 status = 51
                 GO TO 800
               END IF
               nnz = nnz + 1
               nslack = nslack  + 1
               jcol = n + nslack
               ICNA( nnz ) = jcol
               IF ( is == 3 ) THEN
                  ABYROW( nnz ) =   one
               ELSE
                  ABYROW( nnz ) = - one
               END IF

!  give the slack variable the same name as its corresponding group

               VNAMES( jcol ) = GNAMES( ig )

!  assign the correct bounds for the slack variable

               BL( jcol ) = BL( n + ig )
               BU( jcol ) = BU( n + ig )
            END IF
         ELSE
            IF ( is == 3 ) BL( n + ig ) = - BU( n + ig )
            IF ( is == 1 .OR. is == 2 .OR. is == 3 )                           &
                             BU( n + ig ) = zero
         END IF
         ISTATE( ig ) = is
  390 CONTINUE

!  reset the number of variables to include the slacks

      n = n + nslack
      nnza = nnz + 1
      ng1 = ng + 1
      ISTADA( ng1 ) = nnza
      IF ( debug .AND. out > 0 ) WRITE( out, 3020 )                          &
          ( ( i, ICNA( k ), ABYROW( k ), k = ISTADA( i ),                      &
              ISTADA( i + 1 ) - 1 ), i = 1, ng )

!  select the starting point for the minimization
!  ----------------------------------------------

      IF ( nstart == 1 ) THEN
         jstart = 1
      ELSE

!  find the key word in the list of starting points

         DO 410 jstart = 1, nstart
            IF ( namest == SNAMES( jstart ) ) GO TO 420
  410    CONTINUE
         status = 49
         IF ( out > 0 ) WRITE( out, 2490 ) namest
         GO TO 800
  420    CONTINUE
      END IF

!  record the starting point

      DO 430 j = 1, nlvars
         X( j ) = VSTART( j, jstart )
  430 CONTINUE

!  initialize all slack and nonlinear variables as zero, with weight 1

      DO 440 j = nlvars + 1, n
         X( j ) = zero
         VSCALE( j ) = one
  440 CONTINUE

!  record the lagrange multipliers, if any

      IF ( ialgor >= 2 ) THEN
         DO 450 ig = 1, ng
            CLMULT( ig ) = CSTART( ig, jstart )
  450    CONTINUE
      END IF

!  nonlinear element information
!  ------------------------------

!  the parameter values for the nonlinear elements may be unordered
!  order the parameters so that those for group i precede those
!  from group i+1. place the reordered set in wk

      j = 1
      DO 520 ig = 1, ng
         k = ITYPEG( ig )
         ISTGP( ig ) = j
         IF ( k > 0 ) THEN
            k1 = IGPA( k + 1 ) - IGPA( k )
            k2 = ISTGP( ig ) - 1
            DO 510 i = 1, k1
               WK( j ) = GPVALU( k2 + i )
               j = j + 1
  510       CONTINUE
         END IF
  520 CONTINUE
      ISTGP( ng1 ) = j

!  overwrite gpvalu with wk

      DO 530 i = 1, j - 1
         GPVALU( i ) = WK( i )
  530 CONTINUE

!  record the scale factors for the nonlinear elements

      DO 540 i = 1, ISTADG( ng1 ) - 1
         ESCALE( i ) = WEIGHT( i )
  540 CONTINUE
      DO 550 i = 1, nelnum

!  determine whether the nonlinear elements have internal
!  representations

         itype = ITYPEE( i )
         nelv = IELV( itype + 1 ) - IELV( itype )
         ninv = IINV( itype + 1 ) - IINV( itype )
         INTREP( i ) = ninv < nelv

!  store the number of internal variables for each element

         INTVAR( i ) = ninv
  550 CONTINUE
      IF ( ialgor >= 2 ) THEN

!  select the objective function group
!  ------------------------------------

!  find the named objective function group in the list
!  mark the remaining objective function groups for removal

         IF ( oneobj ) THEN
            nobjgr = 0
            DO 610 i = 1, nobj
               field = ONAMES( i ) // 'GR'
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               k = INLIST( ifield )
               IF ( nameof == ONAMES( i ) ) THEN
                  nobjgr = k
                  IF ( out > 0 .AND. debug )                                   &
                       WRITE( out, 3010 ) ONAMES( i ), k
               ELSE
                  ISTADG( k ) = - ISTADG( k )
                  IF ( out > 0 .AND. debug )                                   &
                       WRITE( out, 3000 ) ONAMES( i )
               END IF
  610       CONTINUE

!  remove redundant group information

             IF ( nobj > 1 .OR.                                                &
                ( nobj == 1 .AND. nobjgr == 0 ) ) THEN
               nnz = 1
               nel = 1
               ngpv = 1
               ngr = 0
               DO 650 i = 1, ng
                  IF ( ISTADG( i ) > 0 ) THEN
                     ngr = ngr + 1
                     IF ( i == nobjgr ) nobjgr = ngr

!  shift the group status, name, type, constant,
!  triviality indicator, lagrange multiplier and weight

                     IF ( ialgor >= 2 ) ISTATE( ngr ) = ISTATE( i )
                     GNAMES( ngr ) = GNAMES( i )
                     ITYPEG( ngr ) = ITYPEG( i )
                     B( ngr ) = B( i )
                     GXEQX( ngr ) = GXEQX( i )
                     IF ( ialgor >= 2 ) CLMULT( ngr ) = CLMULT( i )
                     GSCALE( ngr ) = GSCALE( i )

!  shift the list of elements and weights in the i-th group

                     k1 = ISTADG( i )
                     ISTADG( ngr ) = nel
                     DO 620 k = k1, ABS( ISTADG( i + 1 ) ) - 1
                        IELING( nel ) = IELING( k )
                        ESCALE( nel ) = ESCALE( k )
                        nel = nel + 1
  620                CONTINUE

!  shift the list of parameters in the i-th group

                     k1 = ISTGP( i )
                     ISTGP( ngr ) = ngpv
                     DO 630 k = k1, ISTGP( i + 1 ) - 1
                        GPVALU( ngpv ) = GPVALU( k )
                        ngpv = ngpv + 1
  630             CONTINUE

!  shift the list of coefficients and positions of the nonzeros
!  for the linear element in the i-th group

                     k1 = ISTADA( i )
                     ISTADA( ngr ) = nnz
                     DO 640 k = k1, ISTADA( i + 1 ) - 1
                        A( nnz ) = A( k )
                        ICNA( nnz ) = ICNA( k )
                        nnz = nnz + 1
  640                CONTINUE
                  END IF
  650          CONTINUE
               ng = ngr
               ISTADG( ng + 1 ) = nel
               ISTGP ( ng + 1 ) = ngpv
               ISTADA( ng + 1 ) = nnz
            END IF
         END IF
      END IF

!  set the required lower and upper bounds on the objective function

      IF ( nobbnd == 0 ) THEN
         OBFBND( 1 ) = - biginf
         OBFBND( 2 ) =   biginf
      ELSE

!  find the key word in the list of starting points

         DO 660 jobbnd = 1, nobbnd
            IF ( nameob == OBNAME( jobbnd ) ) GO TO 670
  660    CONTINUE
         status = 50
         IF ( out > 0 ) WRITE( out, 2500 ) nameob
         GO TO 800
  670    CONTINUE
         OBFBND( 1 ) = FBOUND( 1, jobbnd )
         OBFBND( 2 ) = FBOUND( 2, jobbnd )
      END IF

!  if no output is required, exit

      IF ( outda <= 0 ) GO TO 900
      nel1 = nelnum + 1
      WRITE( outda, 3180 ) n,  ng, nelnum,     ISTADG( ng1  ) - 1,             &
                            ISTAEV( nel1 ) - 1, ISTADA( ng1  ) - 1,            &
                            ISTGP ( ng1  ) - 1, ISTEP ( nel1 ) - 1,            &
                            neltyp, ngrtyp 

!  print out problem data. output the number of variables, groups and
!  elements and, perhaps, the identity of the objective function group

      WRITE( outda, 3100 ) ialgor, pname, iauto
      IF ( ialgor == 2 ) WRITE( outda, 3170 ) nslack, nobjgr

!  output the starting addresses of the elements in each group,
!  of the parameters used for each group and
!  of the nonzeros of the linear element in each group

      WRITE( outda, 3110 ) ( ISTADG( i ), i = 1, ng1 )
      WRITE( outda, 3110 ) ( ISTGP ( i ), i = 1, ng1 )
      WRITE( outda, 3110 ) ( ISTADA( i ), i = 1, ng1 )

!  output the starting addresses of the variables and parameters
!  in each element

      WRITE( outda, 3110 ) ( ISTAEV( i ), i = 1, nel1 )
      WRITE( outda, 3110 ) ( ISTEP( i ), i = 1, nel1 )

!  output the group type of each group and its status

      WRITE( outda, 3110 ) ( ITYPEG( i ), i = 1, ng )
      IF ( ialgor >= 2 ) WRITE( outda, 3110 )( ISTATE( i ), i = 1,NG)

!  output the element type of each element

      WRITE( outda, 3110 ) ( ITYPEE( i ), i = 1, nelnum )

!  output the element type of each element
!  and its number of internal variables

      WRITE( outda, 3110 ) ( INTVAR( i ), i = 1, nelnum )

!  output the identity of each individual element

      WRITE( outda, 3110 ) ( IELING( i ), i = 1, ISTADG( ng1 ) - 1 )

!  output the variables in each group's elements

      WRITE( outda, 3110 ) ( IELVAR( i ), i = 1, ISTAEV( nel1 ) - 1 )

!  output the column addresses of the nonzeros in each linear element

      nnza = ISTADA( ng1 ) - 1
      WRITE( outda, 3110 ) ( ICNA( i ), i = 1, nnza )

!  write single precision format

      IF ( single ) THEN

!  output the values of the nonzeros in each linear element, the
!  constant term in each group, the lower and upper bounds on
!  the variables and the starting point for the minimization

         WRITE( outda, 3121 ) ( ABYROW( i ), i = 1, nnza )
         WRITE( outda, 3121 ) ( B( i ), i = 1, ng )
         IF ( ialgor <= 2 ) THEN
            WRITE( outda, 3121 ) ( BL( i ), i = 1, n )
            WRITE( outda, 3121 ) ( BU( i ), i = 1, n )
         ELSE
            WRITE( outda, 3121 ) ( BL( i ), i = 1, n + ng )
            WRITE( outda, 3121 ) ( BU( i ), i = 1, n + ng )
         END IF
         WRITE( outda, 3121 ) ( X( i ), i = 1, n )
         IF ( ialgor >= 2 ) WRITE( outda, 3121 )( CLMULT( i ), i = 1, ng )

!  output the parameters in each group

         WRITE( outda, 3121 ) ( GPVALU( i ), i = 1, ISTGP( ng1 ) - 1 )

!  output the parameters in each individual element

         WRITE( outda, 3121 ) ( EPVALU( i ), i = 1, ISTEP( nel1 ) - 1 )

!  output the scale factors for the nonlinear elements

         WRITE( outda, 3121 ) ( ESCALE( i ), i = 1, ISTADG( ng1 ) - 1 )

!  output the scale factors for the groups

         WRITE( outda, 3121 ) ( GSCALE( i ), i = 1, ng )

!  output the scale factors for the variables

         WRITE( outda, 3121 ) ( VSCALE( i ), i = 1, n )

!  output the lower and upper bounds on the objective function

         WRITE( outda, 3161 ) OBFBND( 1 ), OBFBND( 2 )

!  write REAL ( KIND = wp ) :: format

      ELSE

!  output the values of the nonzeros in each linear element, the
!  constant term in each group, the lower and upper bounds on
!  the variables and the starting point for the minimization

         WRITE( outda, 3120 ) ( ABYROW( i ), i = 1, nnza )
         WRITE( outda, 3120 ) ( B( i ), i = 1, ng )
         IF ( ialgor <= 2 ) THEN
            WRITE( outda, 3120 ) ( BL( i ), i = 1, n )
            WRITE( outda, 3120 ) ( BU( i ), i = 1, n )
         ELSE
            WRITE( outda, 3120 ) ( BL( i ), i = 1, n + ng )
            WRITE( outda, 3120 ) ( BU( i ), i = 1, n + ng )
         END IF
         WRITE( outda, 3120 ) ( X( i ), i = 1, n )
         IF ( ialgor >= 2 ) WRITE( outda, 3120 )( CLMULT( i ), i = 1, ng )

!  output the parameters in each group

         WRITE( outda, 3120 ) ( GPVALU( i ), i = 1, ISTGP( ng1 ) - 1 )

!  output the parameters in each individual element

         WRITE( outda, 3120 ) ( EPVALU( i ), i = 1, ISTEP( nel1 ) - 1 )

!  output the scale factors for the nonlinear elements

         WRITE( outda, 3120 ) ( ESCALE( i ), i = 1, ISTADG( ng1 ) - 1 )

!  output the scale factors for the groups

         WRITE( outda, 3120 ) ( GSCALE( i ), i = 1, ng )

!  output the scale factors for the variables

         WRITE( outda, 3120 ) ( VSCALE( i ), i = 1, n )

!  output the lower and upper bounds on the objective function

         WRITE( outda, 3160 ) OBFBND( 1 ), OBFBND( 2 )
      END IF

!  output a logical array which says whether an element has internal
!  variables

      WRITE( outda, 3130 ) ( INTREP( i ), i = 1, nelnum )

!  output a logical array which says whether a group is trivial

      WRITE( outda, 3130 ) ( GXEQX( i ), i = 1, ng )

!  output the names given to the groups and to the variables

      WRITE( outda, 3140 ) ( GNAMES( i ), i = 1, ng )
      WRITE( outda, 3140 ) ( VNAMES( i ), i = 1, n )

!  output the names given to the element and group types

      WRITE( outda, 3140 ) ( ETYPES( i ), i = 1, neltyp )
      WRITE( outda, 3140 ) ( GTYPES( i ), i = 1, ngrtyp )

!  output the type of each variable

      WRITE( outda, 3110 ) ( ITYPEV( i ), i = 1, n )
      GO TO 900

!  incorrect data specified

  800 CONTINUE
      RETURN

!  successful return

  900 CONTINUE
      status = 0
      RETURN

!  non-executable statements

 1990 FORMAT( ' ** Exit from INLANC - ', /,                                    &
              ' Although the manual may suggest otherwise,',                   &
              ' non-trivial',/,                                                &
              ' groups are not allowed for inequality constraints',/)
 2000 FORMAT( ' ** Exit from INLANC - increase one or more of ngpvmx, ',       &
              ' nelmax and ngmax ' )
 2460 FORMAT( ' ** Exit from INLANC - constant name ', A8,                     &
              ' not recognised ' )
 2470 FORMAT( ' ** Exit from INLANC - bound name ', A8,                        &
              ' not recognised ' )
 2480 FORMAT( ' ** Exit from INLANC - range name ', A8,                        &
              ' not recognised ' )
 2490 FORMAT( ' ** Exit from INLANC - start point name ', A8,                  &
              ' not recognised ' )
 2500 FORMAT( ' ** Exit from INLANC - obj. bound name ', A8,                   &
              ' not recognised ' )
 3000 FORMAT( ' Group ', A10, ' removed as a redundant objective ' )
 3010 FORMAT( /, ' Objective function ', A10, ' is group number ', I8 )
 3020 FORMAT( /, 3('  Row   Col    Value  '),                                  &
              /, 3('  ---   ---    -----  '),                                  &
              /, ( 3( 2I5, 1P, D12.4 ) ) )
 3100 FORMAT( i2, A8, i2 )
 3110 FORMAT( ( 10I8 ) )
 3120 FORMAT( ( 1P, 4D16.8 ) )
 3121 FORMAT( ( 1P, 4E16.8 ) )
 3130 FORMAT( ( 72L1 ) )
 3140 FORMAT( ( 8A10 ) )
 3160 FORMAT( 1P, 2D16.8 )
 3161 FORMAT( 1P, 2E16.8 )
 3170 FORMAT( 2I8 )
 3180 FORMAT( 10I8 )

!  end of subroutine INLANC

      END SUBROUTINE INLANC

!-*-*-*-*-*- S I F D E C O D E   P R I N T P    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE PRINTP( nmax, ngmax, nlmax,                                   &
                         nelmax, netmax,                                       &
                         nevmax, nepmax, ngrmax, negmax, nepvmx,               &
                         ngpvmx, ngpmax, lstada, licna, liwk,                  &
                         n, ng, nlvars, nelnum,                                &
                         ISTATE, ISTADG, IELVAR, ITYPEG, ITYPEE,               &
                         IELV, IINV, IEPA, IGPA,                               &
                         ISTADA, ICNA, ISTGP, ISTEP, ISTEV,                    &
                         IELING, ITYPEV, IWK,                                  &
                         ABYROW, B, BL, BU, X, EPVALU, GPVALU,                 &
                         GSCALE, ESCALE, VSCALE,                               &
                         pname, VNAMES, GNAMES,                                &
                         lnames, ETYPES, ENAMES,                               &
                         ANAMES, EPNAME, GPNAME, GTYPES,                       &
                         out, print_level )
      INTEGER :: out, print_level
      INTEGER :: nmax, ngmax, nlmax, nelmax, netmax
      INTEGER :: nepmax, nevmax, ngrmax, liwk
      INTEGER :: n, ng
      INTEGER :: nlvars, nelnum, licna, lstada
      INTEGER :: negmax, nepvmx, ngpvmx, ngpmax
      REAL ( KIND = wp ) :: EPVALU( nepvmx ), GPVALU( ngpvmx )
      INTEGER :: IELING( negmax )
      INTEGER :: ISTATE( ngmax ), ISTADA( lstada ), ICNA( licna )
      INTEGER :: IELV  ( nlmax ), IINV  ( nlmax )
      INTEGER :: IELVAR( nevmax ), ITYPEV( nmax )
      INTEGER :: ISTADG(  ngmax ), ITYPEG( ngmax ), ITYPEE( nelmax )
      INTEGER :: IEPA( nlmax ), IGPA( ngrmax ), IWK( liwk )
      INTEGER :: ISTEP( nelmax ), ISTEV( nelmax ), ISTGP( ngmax )
      CHARACTER ( len = 8 ) :: pname
      CHARACTER ( len = 10 ) :: GNAMES( ngmax ), VNAMES( nmax )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax ), LNAMES( nelmax )
      CHARACTER ( len = 10 ) :: ENAMES( netmax )
      CHARACTER ( len = 10 ) :: ANAMES( ngrmax ), GTYPES( ngrmax )
      CHARACTER ( len = 10 ) :: EPNAME( nepmax ), GPNAME( ngpmax )
      REAL ( KIND = wp ) :: B( ngmax ), BL( nmax ), BU( nmax ), X( nmax )
      REAL ( KIND = wp ) :: GSCALE( ngmax ), ESCALE( negmax ), VSCALE( nmax )
      REAL ( KIND = wp ) :: ABYROW( licna )

!  --------------------------------------------------------------------
!  print details of the problem previously specified in an SIF file

! the level of printing performed is determined by the value of iprint
!  possible values are:

!  >= 1, a simple summary of the problem name, the number of variables,
!        groups and elements
!  >= 2, a list of the variables used
!  >= 3, a breakdown of the groups. a list of the nonlinear elements
!        used, the types of each group, the status of the group and
!        a statement that the group uses or does not use a linear
!        element
! = 4, further details of each group. the name of the group-type
!        variable and a list of the values associated with each
!        parameter
!  >= 5, details of each element. the numbers of elemental and
!        internal variables and the numbers of parameters
!  >= 6, further details of each element. the names of the
!        elemental variables together with their associated
!        problem variables, a list of the values associated
!        with each parameter and the variables involved in
!        the linear element
!  >= 7, details of the coefficients of the linear elements
!  >= 8, full details of the variables used including their lower
!        and upper bounds and starting values
!  >= 9, all of the above
!  --------------------------------------------------------------------

!  local variables

      INTEGER :: i, iel, ig, is, j, k, k1, k2, k3, k4, k5, k6, l, ieltyp

!  pararmeter definitions

      CHARACTER ( len = 3 ), DIMENSION( 3 ), PARAMETER :: VARTYP               &
        = (/ '   ', '0-1', 'int' /)
      CHARACTER ( len = 12 ), DIMENSION( 4 ), PARAMETER :: CONSTRAINT_status   &
        = (/ 'an objective', 'an equality ', 'a negativity', 'a positivity' /)

      IF ( out <= 0 ) RETURN
      IF ( print_level >= 1 ) THEN
        IWK( : nelnum ) = 0
        WRITE( out, 2000 ) pname, n, n - nlvars, ng, nelnum

!  list of variables

         IF ( print_level >= 2 ) THEN
             WRITE( out, 2010 ) ( VNAMES( j ), j = 1, nlvars )
             IF ( nlvars < n )                                                 &
                WRITE( out, 2020 ) ( VNAMES( j ), j = nlvars + 1, n )

!  group details

            IF ( print_level >= 3 ) THEN
               DO 500 ig = 1, ng
                  is = ITYPEG( ig )
                  IF ( is == 0 ) THEN
                     IF ( ISTATE( ig ) == 1 ) THEN
                       WRITE( out, 2030 ) ig, GNAMES( ig ),                    &
                       CONSTRAINT_status( 1 ), 'TRIVIAL   ', GSCALE( ig )
                     ELSE
                       WRITE( out, 2030 ) ig, GNAMES( ig ),                    &
                       CONSTRAINT_status( ISTATE( ig ) ),                      &
                       'TRIVIAL   ', GSCALE( ig )
                     END IF
                  ELSE
                     IF ( ABS( ISTATE( ig ) ) == 1 ) THEN
                       WRITE( out, 2030 ) ig, GNAMES( ig ),                    &
                       CONSTRAINT_status( 1 ), GTYPES( is ), GSCALE( ig )
                     ELSE
                       WRITE( out, 2030 ) ig, GNAMES( ig ),                    &
                       CONSTRAINT_status( ISTATE( ig ) ),                      &
                       GTYPES( is ), GSCALE( ig )
                     END IF
                  END IF
                  k1 = ISTADG( ig )
                  k2 = ISTADG( ig + 1 ) - 1
                  l = k2 - k1 + 1
                  IF ( k1 <= k2 ) THEN
                     IF ( k1 == k2 ) THEN
                        WRITE( out, 2060 ) LNAMES( IELING( k1 ) )
                     ELSE
                        WRITE( out, 2070 ) l,                                 &
                             ( LNAMES( IELING( k ) ), k = k1, k2 )
                     END IF
                  ELSE
                    WRITE( out, 2080 )
                  END IF

!  further group details

                  IF ( print_level == 4 .OR. print_level >= 7 ) THEN
                     IF ( is > 0 ) THEN
                        k3 = IGPA( is ) - 1
                        k4 = ISTGP( ig ) - 1
                        l = ISTGP( ig + 1 ) - k4 - 1
                        IF ( is > 0 ) THEN
                           IF ( l == 1 ) THEN
                              WRITE( out, 2090 )                               &
                                ANAMES( is ), 'is ', l, '. '
                           ELSE
                              WRITE( out, 2090 )                               &
                                ANAMES( is ), 'are', l, 's.'
                           END IF
                        END IF
                        IF ( l > 0 )                                           &
                           WRITE( out, 2100 ) ( GPNAME( k3 + i ),              &
                                  GPVALU( k4 + i ), i = 1, l )
                     END IF
                  END IF

!  element details

                  IF ( print_level >= 5 ) THEN
                     DO 400 k = k1, k2
                        iel = IELING( k )
                        ieltyp = ITYPEE( iel )
                        IF ( IWK( iel ) == 0 ) THEN
                           WRITE( out, 2110 ) LNAMES( iel ),                   &
                                  iel, ETYPES( ieltyp ),                       &
                                  IELV( ieltyp + 1 ) - IELV( ieltyp ),         &
                                  IINV( ieltyp + 1 ) - IINV( ieltyp ),         &
                                  IEPA( ieltyp + 1 ) - IEPA( ieltyp ),         &
                                  ESCALE( k )
                           IF ( print_level < 6 ) IWK( iel ) = 1
                        ELSE
                           WRITE( out, 2120 ) LNAMES( iel ), iel,              &
                                  ESCALE( k )
                        END IF

!  further element details

                        IF ( print_level >= 6 ) THEN
                           IF ( IWK( iel ) == 0 ) THEN
                              IWK( iel ) = 1
                              k3 = IELV ( ieltyp ) - 1
                              k4 = ISTEV( iel ) - 1
                              l = ISTEV( iel + 1 ) - k4 - 1
                              WRITE( out, 2130 ) ( ENAMES( k3 + i ),           &
                                     VNAMES( IELVAR( k4 + i ) ),               &
                                                          i = 1, l )
                              k3 = IEPA( ieltyp )  - 1
                              k4 = ISTEP( iel ) - 1
                              l = ISTEP( iel + 1 ) - k4 - 1
                              IF ( l > 0 )                                     &
                                WRITE( out, 2150 ) ( EPNAME( k3 + i ),         &
                                       EPVALU( k4 + i ), i = 1, l )
                           END IF
                        END IF
  400                CONTINUE
                  END IF

!  linear element details

                  k5 = ISTADA( ig )
                  k6 = ISTADA( ig + 1 ) - 1
                  IF ( print_level == 6 ) THEN
                     IF ( k5 <= k6 ) THEN
                        IF ( k5 == k6 ) THEN
                           WRITE( out, 2040 ) k6 - k5 + 1, '. '
                           IF ( print_level >= 6 ) WRITE( out, 2140 ) ' ',     &
                             ( VNAMES( ICNA( k ) ), k = k5, k6 )
                        ELSE
                           WRITE( out, 2040 ) k6 - k5 + 1, 's.'
                           IF ( print_level >= 6 ) WRITE( out, 2140 ) 's',     &
                             ( VNAMES( ICNA( k ) ), k = k5, k6 )
                        END IF
                     ELSE
                        IF ( ABS( B( ig ) ) < 1.0D-12 ) THEN
                           WRITE( out, 2050 )
                        ELSE
                           WRITE( out, 2160 )
                        END IF
                     END IF
                  ELSE

!  further linear element details

                     IF ( k5 <= k6 ) THEN
                        IF ( k5 == k6 ) THEN
                           WRITE( out, 2040 ) k6 - k5 + 1, '. '
                           IF ( print_level >= 6 ) WRITE( out, 2200 ) ' ',     &
                             ') ', ( VNAMES( ICNA( k ) ),                      &
                               ABYROW( k ), k = k5, k6 )
                        ELSE
                           WRITE( out, 2040 ) k6 - k5 + 1, 's.'
                           IF ( print_level >= 6 ) WRITE( out, 2200 ) 's',     &
                             's)', ( VNAMES( ICNA( k ) ),                      &
                               ABYROW( k ), k = k5, k6 )
                        END IF
                        IF ( ABS( B( ig ) ) < 1.0D-12 ) THEN
                           WRITE( out, 2230 )
                        ELSE
                           WRITE( out, 2220 ) B( ig )
                        END IF
                     ELSE
                        IF ( ABS( B( ig ) ) < 1.0D-12 ) THEN
                           WRITE( out, 2050 )
                        ELSE
                           WRITE( out, 2210 ) B( ig )
                        END IF
                     END IF
                  END IF
  500          CONTINUE
            END IF
         END IF
      END IF
      IF ( print_level >= 8 ) THEN
         WRITE( out, 2170 )
         DO 510 i = 1, n
            WRITE( out, 2180 ) i, VNAMES( i ), BL( i ), X( i ),                &
                                BU( i ), VSCALE( i ),                          &
                                VARTYP( ITYPEV( i ) + 1 )
  510    CONTINUE
      END IF
      RETURN

!  non-executable statements

 2000 FORMAT( /, ' Problem name ', A8, /,                                      &
              /, ' There are ', I8, ' VARIABLES of which ', I8,                &
                 ' are artificials',                                           &
              /, ' There are ', I8, ' GROUPS',                                 &
              /, ' There are ', I8, ' NONLINEAR ELEMENTS ' )
 2010 FORMAT( /, ' Names of problem variables ',                               &
              /, ' ----- -- ------- --------- ',                               &
              /, 7( 1X, A10 ) )
 2020 FORMAT( /, ' Names of artificial variables ',                            &
              /, ' ----- -- ---------- --------- ',                            &
              /, 7( 1X, A10 ) )
 2030 FORMAT( /, ' Group ', I8, ' is named ', A10, /, '  * It is ',            &
              A12, ' group of type ', A10,                                     &
              /, '  * The group is scaled by the factor ', 1P, D12.4 )
 2040 FORMAT( '  * The group has a LINEAR ELEMENT with ', i6,                  &
              ' variable', A2 )
 2050 FORMAT( '  * The group has no LINEAR ELEMENT. ' )
 2060 FORMAT( '  * The group uses a single NONLINEAR',                         &
                 ' ELEMENT.  This is element ', A10 )
 2070 FORMAT( '  * The group uses ', i5, ' NONLINEAR',                         &
              ' ELEMENTS. These are elements ', A10,                           &
               /, ( 3X, 6( 1X, A10 ) ) )
 2080 FORMAT( '  * The group uses no NONLINEAR ELEMENTS. ' )
 2090 FORMAT( '  * The group-type argument is ', A10, ' and there ', A3,       &
                 I8, ' parameter', A2 )
 2100 FORMAT( ( '    * Group parameter ', A10, ' has the value ',              &
                1P, D12.4, '.' ) )
 2110 FORMAT(  '  * Group uses nonlinear element ', A10,                       &
               ' number ', i5, ' of type ', A10, /,                            &
               '    * No. elemental variables =', I8,                          &
               '. No. internal variables =', I8, '.',                          &
               /, '    * No. parameter values =', I8, '.', /,                  &
               '    * The element is scaled by the factor ', 1P, D12.4 )
 2120 FORMAT( '  * Group uses nonlinear element ', A10,                        &
              ' number ', i5, ' described above. ', /,                         &
              '    * The element is scaled by the factor ', 1P, D12.4 )
 2130 FORMAT( ( '    * Elemental variable ', A10, '  is assigned',             &
                ' problem variable ', A10 ) )
 2140 FORMAT( '    * The linear element uses variable', A1, 1X, 3A10,          &
              /, ( 6X, 6A10 ) )
 2150 FORMAT( ( '    * Elemental parameter ', A10, ' has the value ',          &
                1P, D12.4, '.' ) )
 2160 FORMAT( '  * The group has a constant LINEAR ELEMENT. ' )
 2170 FORMAT( /, '  #  variable name ',                                        &
                 'lower bound start value upper bound scale factor',           &
              ' type', /, '   -  ------------- ',                              &
                 '----------- ----------- ----------- ------------',           &
              ' ----' )
 2180 FORMAT( i5, 2X, A10, 1X, 1P, 4D12.4, 3X, A3 )
 2200 FORMAT( '    * The linear element uses variable', A1,                    &
              ' (with coefficient', A2,                                        &
              /, ( 6X, 3( A10, 1X, 1P, D10.2, 1X ) ) )
 2210 FORMAT( '    * The group has a constant LINEAR ELEMENT with',            &
              ' coefficient ', 1P, D10.2 )
 2220 FORMAT( '    * The constant term for the LINEAR ELEMENT has',            &
              ' coefficient ', 1P, D10.2 )
 2230 FORMAT( '    * There is no constant term for the LINEAR ELEMENT' )

!  end of subroutine PRINTP

      END SUBROUTINE PRINTP

!-*-*-  S I F D E C O D E   M A K E _ e l f u n    S U B R O U T I N E  -*-*-

      SUBROUTINE MAKE_elfun( input, out, outfn, outra, status, nlmax,          &
                             nimax, netmax, ninmax, numax, neltyp,             &
                             pname, ENAMES, INAMES, RENAME, INNAME, LONAME,    &
                             MINAME, EXNAME, ETYPES, LDEFND, length, ITABLE,   &
                             KEY, IELV, IINV, INLIST, EPNAME, IEPA, nepmax,    &
                             debug, ijump, U, SETVEC, nsetvc, single, nuline,  &
                             gotlin, print_level )
      INTEGER :: input, out, outfn, outra, print_level, length, status
      INTEGER :: nlmax, nimax, netmax, neltyp, ninmax, nsetvc, numax, nepmax
      LOGICAL :: debug, single, gotlin
      INTEGER :: ITABLE( length ), ijump ( nlmax  )
      INTEGER :: IELV  ( nlmax  ), IINV  ( nlmax  )
      INTEGER :: INLIST( length )
      INTEGER :: IEPA  ( nlmax  )
      LOGICAL :: LDEFND( nlmax  ), SETVEC( nsetvc )
      REAL ( KIND = wp ) ::   U     ( numax  )
      CHARACTER ( len = 12 ) :: KEY   ( length )
      CHARACTER ( len = 8 ) :: pname
      CHARACTER ( len = 10 ) :: EPNAME( nepmax ), EXNAME( ninmax )
      CHARACTER ( len = 10 ) :: INAMES( nimax  ), RENAME( ninmax )
      CHARACTER ( len = 10 ) :: LONAME( ninmax ), INNAME( ninmax )
      CHARACTER ( len = 10 ) :: MINAME( ninmax ), ENAMES( netmax )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax  )
      CHARACTER ( len = max_record_length ) :: nuline

!  ----------------------------------------------------------------
!  make a function evaluation subroutine and a range transformation
!  subroutine from a gps function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  ELEMENTS     problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  define the transformation from the elemental to the
!               internal variables for all elements with internal vars
!               set function and derivative values and make
!               element specific parameter assignments
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in 
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of inform indicate that insufficient
!  array space has been allowed, as follows:

!    inform = - 1  when length not large enough
!    inform = - 2  when max( ninnam, nrenam, nlonam, nenam, nminam )
!                  > ninmax
!    inform = - 11 when numax not large enough
!    inform = - 12 when nsetvc not large enough
!  ----------------------------------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mtemp = 5, mglob = 6, mindiv = 7, mendat = 8
      INTEGER, PARAMETER :: iires = 32
      INTEGER, PARAMETER :: nincrs = 12
      INTEGER, PARAMETER :: maxnul = 20
      INTEGER, DIMENSION( mendat ), PARAMETER :: LENIND                        &
        = (/ 0, 12, 11, 8, 11, 7, 11, 6 /)
      CHARACTER ( len = 12 ), DIMENSION( mendat ), PARAMETER :: INDIC8         &
        = (/ '            ', 'FIXED FORMAT', 'FREE FORMAT ', 'ELEMENTS    ',   &
             'TEMPORARIES ', 'GLOBALS     ', 'INDIVIDUALS ', 'ENDATA      '  /)
      CHARACTER ( len = 8 ), DIMENSION( iires ), PARAMETER :: FIELDI           &
        = (/ 'ELFUN   ', 'LFUVAL  ', 'FUVALS  ', 'XVALUE  ', 'NCALCF  ',       &
             'ITYPEE  ', 'ISTAEV  ', 'IELVAR  ', 'INTVAR  ', 'ISTADH  ',       &
             'ICALCF  ', 'IFFLAG  ', 'IELEMN  ', 'IELTYP  ', 'IHSTRT  ',       &
             'ILSTRT  ', 'IGSTRT  ', 'EPVALU  ', 'ISTEPA  ', 'IPSTRT  ',       &
             'JCALCF  ', 'LTYPEE  ', 'LSTAEV  ', 'LELVAR  ', 'LNTVAR  ',       &
             'LSTADH  ', 'LSTEPA  ', 'LCALCF  ', 'LFVALU  ', 'LXVALU  ',       &
             'LEPVLU  ', 'IFSTAT  ' /)
      CHARACTER ( len = 6 ), DIMENSION( nincrs ), PARAMETER :: INCRSE          &
        = (/ 'LENGTH', 'NINMAX', '      ', '      ', '      ', '      ',       &
             '      ', '      ', '      ', '      ', 'NUMAX ', 'NSETVC'  /)

!  local variables

      INTEGER :: ifield, ifree, ihvar, ivar, intype, nminam
      INTEGER :: jvar, k, k1, k2, nh, nhess, nvars, isetty
      INTEGER :: i, niname, ninnam, ninvar, nloop, nrenam, nexnam
      INTEGER :: itype, j, is, js, novals, nelv, ninv, nn, nename
      INTEGER :: npname, lineno, ilines, nlines, nlonam
      LOGICAL :: nointe, defnam, endpar, endgen, firstl, setran
      LOGICAL :: startf, startg, starth, startp, qprod
      LOGICAL :: endoff, endofg, endofh, fixed, nomorg
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 8 ) :: field3
      CHARACTER ( len = 10 ) :: field2
      CHARACTER ( len = 12 ) :: field, header
      CHARACTER ( len = 41 ) :: field7
      CHARACTER ( len = max_record_length ) :: blnkln
      REAL ( KIND = wp ), DIMENSION( 2 ) :: VALUES
      CHARACTER ( len = 8 ), DIMENSION( 2 ) :: FIELDS
      CHARACTER ( len = 65 ), DIMENSION( maxnul ) :: NULINA

      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnam = 0 ; nrenam = 0 ; nlonam = 0 ; nminam = 0 ; nexnam = 0
      lineno = 0 ; nloop = neltyp + 1 ; intype = 1 ; ilines = 0 ; nlines = 0

!  set initial values for logical variables

      defnam = .FALSE. ; endpar = .FALSE. ; starth = .FALSE. ; startp = .FALSE.
      endgen = .FALSE. ; firstl = .TRUE. ; nointe = .FALSE. ; fixed = .TRUE.
      gotlin = .FALSE.

!  create a dictionary of the internal variable names used

      niname = IINV( neltyp + 1 ) - 1
      DO 20 i = 1, niname
         field = INAMES( i ) // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = INAMES( i )
         END IF
   20 CONTINUE

!  include the names of the elemental variables used in this dictionary

      nename = IELV( neltyp + 1 ) - 1
      DO 30 i = 1, nename
         field = ENAMES( i ) // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = ENAMES( i )
         END IF
   30 CONTINUE

!  include the names of the elemental parameters used
!  in this dictionary

      npname = IEPA( neltyp + 1 ) - 1
      DO 40 i = 1, npname
         field = EPNAME( i ) // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = EPNAME( i )
         END IF
   40 CONTINUE

!  find which element types have an internal representation

      isetty = 0
      DO 50 itype = 1, neltyp
         LDEFND( itype ) = .FALSE.
         IF ( IELV( itype + 1 ) - IELV( itype ) ==                             &
              IINV( itype + 1 ) - IINV( itype ) ) THEN
            IJUMP( itype ) = 99998
            nointe = .TRUE.
         ELSE
            IJUMP( itype ) = itype
         END IF
   50 CONTINUE

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( i : i ) = ' '
      END DO

!  read next line

  100 CONTINUE
      IF ( ilines + 1 > nlines ) THEN

!  read next line from the input file

         lineno = lineno + 1
         nuline = blnkln
         IF ( fixed ) THEN
            READ( input, 1000, END = 590, ERR = 590 ) nuline
            IF ( out > 0 .AND. debug ) WRITE( out, 2990 )                      &
                 lineno, nuline
         ELSE
            READ( input, 1010, END = 590, ERR = 590 ) nuline
            IF ( out > 0 .AND. debug ) WRITE( out, 2970 )                      &
                 lineno, nuline

!  if the card is in free format, translate it into fixed format

            CALL FREE_format( nuline, max_record_length, mendat, INDIC8,       &
                              LENIND, NULINA, maxnul, nlines, .FALSE.,         &
                              status, out )
            IF ( status > 0 ) GO TO 800
            IF ( nlines > 0 ) THEN

!  if there are non-blank lines on the free format card, read the first

               ilines = 1
               nuline = blnkln
               nuline = NULINA( ilines )
               IF ( out > 0 .AND. debug ) WRITE( out, 2980 )                   &
                    lineno, ilines, nuline
            ELSE

!  there are only blank lines on the free format card

               GO TO 100
            END IF
         END IF
      ELSE

!  read next line from the last encountered free format card

         ilines = ilines + 1
         nuline = blnkln
         nuline = NULINA( ilines )
         IF ( out > 0 .AND. debug ) WRITE( out, 2980 )                         &
              lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1: 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1: 1 ) /= ' ' ) THEN

!  ignore comment cards

         IF ( NULINE( 1: 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

         IF ( header == INDIC8( mfixed ) ) THEN
            fixed = .TRUE.
            GO TO 100
         END IF

!  check if we have entered free-format input

         IF ( header == INDIC8( mfree ) ) THEN
            fixed = .FALSE.
            GO TO 100
         END IF

!  check that the first encountered indicator card is the elements card

         IF ( .NOT. defnam  ) THEN
            IF ( header /= INDIC8( mname ) ) THEN
               IF ( neltyp > 0 ) GO TO 930
               IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
               gotlin = .TRUE.
               GO TO 600
            ELSE

!  indicator card is elements
!  ---------------------------

               IF ( pname  /= NULINE( 15: 22 ) ) THEN
                  status = 51
                  IF ( out > 0 ) WRITE( out, 2510 )
                  GO TO 800
               ELSE
                  defnam = .TRUE.

!  -------- set up subroutine call for range routine

                  IF ( single ) THEN
                     WRITE( outra, 4001 ) pname, TRIM( version )
                  ELSE
                     WRITE( outra, 4000 ) pname, TRIM( version )
                  END IF
                  IF ( neltyp > 1 ) THEN
                     WRITE( outra, 4040 ) ( IJUMP( i ), i = 1, neltyp )
                     WRITE( outra, 4050 )
                  END IF
                  GO TO 100
               END IF
            END IF
         END IF

!  an indicator card has been found

         DO 110 i = intype, mendat
            IF ( header == INDIC8( i ) ) THEN
               intype = i
               GO TO 120
            END IF
  110    CONTINUE

!  the indicator card is not recognised

         status = 2
         IF ( out > 0 ) WRITE( out, 2020 )
         GO TO 800
  120    CONTINUE

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

         IF ( intype >= mglob .AND. .NOT. endpar ) THEN
            endpar = .TRUE.

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

            DO 130 i = 1, iires
               field = FIELDI( i ) // '  PF'
               CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
               IF ( ifree <= 0 ) THEN
                  IF ( ifree == 0 ) THEN
                     status = - 1
                     GO TO 700
                  END IF
                  status = 59
                  IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- set up subroutine call and reserved parameter declarations

            IF ( single ) THEN
               WRITE( outfn, 3001 ) FIELDI( 1 )(1:6),                          &
         FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),              &
       ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                    &
         FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                 &
         FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                                 &
         FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                 &
       ( FIELDI(  i )(1:6), i = 22, 32 ),                                      &
         FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                 &
         FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                 &
         FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                 &
         FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                 &
         FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                 &
         FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                 &
         FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                 &
         FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                 &
         FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                 &
         FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                 &
         pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),         &
         FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                 &
         FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            ELSE   
               WRITE( outfn, 3000 ) FIELDI( 1 )(1:6),                          &
         FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),              &
       ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                    &
         FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                 &
         FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                                 &
         FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                 &
       ( FIELDI(  i )(1:6), i = 22, 32 ),                                      &
         FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                 &
         FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                 &
         FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                 &
         FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                 &
         FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                 &
         FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                 &
         FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                 &
         FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                 &
         FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                 &
         FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                 &
         pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),         &
         FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                 &
         FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            END IF

! --------- insert integer declarations

            IF ( ninnam > 0 )                                                  &
               WRITE( outfn, 3010 ) ( INNAME( i ), i = 1, ninnam )

! --------- insert real declarations

            IF ( nrenam > 0 ) THEN
               IF ( single ) THEN
                  WRITE( outfn, 3019 ) ( RENAME( i ), i = 1, nrenam )
               ELSE
                  WRITE( outfn, 3020 ) ( RENAME( i ), i = 1, nrenam )
               END IF
            END IF

! --------- insert logical declarations

            IF ( nlonam > 0 )                                                  &
               WRITE( outfn, 3023 ) ( LONAME( i ), i = 1, nlonam )

! --------- insert intrinsic declarations

            IF ( nminam > 0 )                                                  &
               WRITE( outfn, 3021 ) ( MINAME( i ), i = 1, nminam )

! --------- insert external declarations

            IF ( nexnam > 0 )                                                  &
               WRITE( outfn, 3022 ) ( EXNAME( i ), i = 1, nexnam )
            WRITE( outfn, 3009 ) FIELDI( 32 )(1:6)
         END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

         IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
            endgen = .TRUE.

! --------- start loop over elements

            WRITE( outfn, 3050 ) nloop,                                        &
                   FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),                       &
                   FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),                       &
                   FIELDI( 21 )(1:6),                                          &
                   FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),                       &
                   FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),                       &
                   FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),                       &
                   FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),                       &
                   FIELDI( 13 )(1:6),                                          &
                   FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),                       &
                   FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
            IF ( neltyp > 1 ) THEN
               WRITE( outfn, 3051 )                                            &
                  FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),                        &
                  FIELDI( 13 )(1:6), ( i, i = 1, neltyp )
               WRITE( outfn, 3052 ) FIELDI( 14 )(1:6)
            END IF

!  make sure that quadratic Hessian terms are included

            DO 140 itype = 1, MIN( 2, neltyp )

!  diagonal term

               IF ( ETYPES( itype ) == cqsqr ) THEN
                  WRITE( outfn, 3060 ) ETYPES( itype )
                  IF ( neltyp > 1 ) WRITE( outfn, 3061 ) itype
                  IF ( single ) THEN
                    WRITE( outfn, 3053 ) 'E', 'E'
                  ELSE
                    WRITE( outfn, 3053 ) 'D', 'D'
                  END IF
                  LDEFND( itype ) = .TRUE.
                  isetty = isetty + 1
                  IF ( isetty < neltyp ) WRITE( outfn, 3191 ) nloop

!  off-diagonal term

               ELSE IF ( ETYPES( itype ) == cqprod ) THEN
                  WRITE( outfn, 3060 ) ETYPES( itype )
                  IF ( neltyp > 1 ) WRITE( outfn, 3061 ) itype
                  IF ( single ) THEN
                    WRITE( outfn, 3054 ) 'E', 'E', 'E'
                  ELSE
                    WRITE( outfn, 3054 ) 'D', 'D', 'D'
                  END IF
                  LDEFND( itype ) = .TRUE.
                  isetty = isetty + 1
                  IF ( isetty < neltyp ) WRITE( outfn, 3191 ) nloop
               END IF
  140       CONTINUE   
         END IF

!  indicator card is endata
!  -------------------------

         IF ( intype == mendat ) GO TO 900
         GO TO 100
      ELSE

!  check that the first non commment card is the elements indicator card

         IF ( .NOT. defnam  ) THEN
            IF ( neltyp > 0 ) GO TO 930
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
            gotlin = .TRUE.
            GO TO 600
         END IF

!  a data card has been found
!  read the character fields 1 and 2 from the card

         field1 = NULINE(  2:  3 )
         field2 = NULINE(  5: 14 )
         IF ( intype == mindiv .AND. field1 == 'R ' ) THEN

!  read the character fields 3 and 5 from the card

            FIELDS( 1 ) = NULINE( 15: 22 )
            FIELDS( 2 ) = NULINE( 40: 47 )

!  check to see if there is are any numerical values to be read

            novals = 0
            IF ( FIELDS( 1 ) /= '        ' .AND.                               &
                 NULINE( 15: 15 ) /= '[' ) THEN
               novals = 1
               CALL GETVAL( NULINE( 25: 36 ), VALUES( 1 ) )
               IF ( FIELDS( 2 ) /= '        ' .AND.                            &
                 NULINE( 40: 40 ) /= '[' ) THEN
                  novals = 2
                  CALL GETVAL( NULINE( 50: 61 ), VALUES( 2 ) )

!  remove fields with numerical values of zero

                  IF ( VALUES( 2 ) == zero ) THEN
                     novals = 1
                  END IF
               END IF
               IF ( VALUES( 1 ) == zero ) THEN
                  IF ( novals == 2 ) THEN
                     VALUES( 1 ) = VALUES( 2 )
                     FIELDS( 1 ) = FIELDS( 2 )
                  END IF
                  novals = novals - 1
               END IF
            END IF
         ELSE

!  read the character fields 3 and 7 from the card

            field3 = NULINE( 15: 22 )
            field7 = NULINE( 25: 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

            IF ( FIELD1( 1: 1 ) == 'A' .OR.                                    &
                 FIELD1( 1: 1 ) == 'F' .OR.                                    &
                 FIELD1( 1: 1 ) == 'G' ) THEN
               IF ( field3 /= '       ' ) THEN
                  status = 72
                  IF ( out > 0 ) WRITE( out, 2720 )
                  GO TO 800
               END IF
            END IF
         END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 200, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  200 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND.                           &
           field1 /= 'M ' .AND. field1 /= 'F ' .AND.                           &
           field1 /= 'L ' ) THEN
         status = 54
         IF ( out > 0 ) WRITE( out, 2540 )
         GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
         field = field2 // 'FU'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nexnam = nexnam + 1
            IF ( nexnam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            EXNAME( nexnam ) = field2
         END IF
      ELSE

!  check to see that the parameter name has not already been used

         field = field2 // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            IF ( field1 == 'R ' ) THEN
               nrenam = nrenam + 1
               IF ( nrenam > ninmax ) THEN
                  status = - 2
                  GO TO 700
               END IF
               RENAME( nrenam ) = field2
            ELSE
               IF ( field1 == 'M ' ) THEN
                  nminam = nminam + 1
                  IF ( nminam > ninmax ) THEN
                     status = - 2
                     GO TO 700
                  END IF
                  MINAME( nminam ) = field2
               ELSE
                  IF ( field1 == 'L ' ) THEN
                     nlonam = nlonam + 1
                     IF ( nlonam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     LONAME( nlonam ) = field2
                  ELSE
                     ninnam = ninnam + 1
                     IF ( ninnam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     INNAME( ninnam ) = field2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  indicator card is globals
!  --------------------------

  300 CONTINUE
      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR.                             &
           field1 == 'E ' ) THEN
         startp = .TRUE.

!  start a parameter assignment. check to see that the parameter has
!  been defined

         field = field2 // 'PF'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
            status = 57
            IF ( out > 0 ) WRITE( out, 2570 )
            GO TO 800
         END IF

! --------- make general parameter assignments

         IF ( field1 == 'A ' ) THEN
            WRITE( outfn, 3030 ) FIELD2(1:6), field7

! --------- make conditional parameter assignments

         ELSE   

!  check that the logical variable has been defined

            field = field3 // '  PF'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  GO TO 700
               END IF
               status = 57
               IF ( out > 0 ) WRITE( out, 2570 )
               GO TO 800
            END IF
            IF ( field1 == 'I ' ) THEN
               WRITE( outfn, 3031 ) FIELD2(1:6),                               &
                                     FIELD3(1:6), field7
            ELSE
               WRITE( outfn, 3032 ) FIELD2(1:6),                               &
                                     FIELD3(1:6), field7
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) == '+' .AND. startp ) THEN

! --------- continue a parameter assignment

            WRITE( outfn, 3040 ) field7
         ELSE
            status = 55
            IF ( out > 0 ) WRITE( out, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new element has been encountered

      IF ( field1 == 'T ' ) THEN

!  check to see if the range of a new element is to be defined

         IF ( firstl ) THEN

!  check if this is the first element

            firstl = .FALSE.
         ELSE

!  finish of the previous element, if any

            IF ( starth ) THEN
               IF ( .NOT. endofh ) THEN
                  DO 410 ihvar = 1, nhess

! --------- set a component of h

                     IF ( .NOT. SETVEC( ihvar ) ) THEN
                        IF ( single ) THEN
                           WRITE( outfn, 3162 ) FIELDI(  3 )(1:6),             &
                                  FIELDI( 15 )(1:6), ihvar
                        ELSE
                           WRITE( outfn, 3161 ) FIELDI(  3 )(1:6),             &
                                 FIELDI( 15 )(1:6), ihvar
                        END IF   
                     END IF   
  410             CONTINUE
                  endofh = .TRUE.
               END IF

! ---------- wind up h

               WRITE( outfn, 3180 )
            END IF
            IF ( startg ) THEN

!  set the remaining gradient components to zero

               IF ( .NOT. endofg ) THEN
                  DO 415 ivar = 1, ninvar

! --------- set a component of g

                     IF ( .NOT. SETVEC( ivar ) ) THEN
                        IF ( single ) THEN
                           WRITE( outfn, 3132 )                                &
                              FIELDI(  3 )(1:6),                               &
                              FIELDI( 17 )(1:6), ivar
                        ELSE
                           WRITE( outfn, 3131 )                                &
                              FIELDI(  3 )(1:6),                               &
                              FIELDI( 17 )(1:6), ivar
                        END IF   
                     END IF   
  415             CONTINUE
                  endofg = .TRUE.
               END IF   

! ---------- wind up f and g

            END IF
            IF ( startf ) THEN
               WRITE( outfn, 3190 )
            ELSE
               status = 61
               IF ( out > 0 ) WRITE( out, 2610 )
               GO TO 800
            END IF
            IF ( isetty < neltyp ) WRITE( outfn, 3191 ) nloop
         END IF

!  find itype, the element type

         field = field2 // 'ET'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )

!  the element type is unknown

         IF ( ifield <= 0 ) THEN
            status = 9
            IF ( out > 0 ) WRITE( out, 2090 )
            GO TO 800
         END IF

! --------- find type of current element

         itype = INLIST( ifield )
         WRITE( outfn, 3060 ) field2
         IF ( neltyp > 1 ) WRITE( outfn, 3061 ) itype
         IF ( LDEFND( itype ) ) THEN
            status = 67
            IF ( out > 0 ) WRITE( out, 2670 )
            GO TO 800
         ELSE
            LDEFND( itype ) = .TRUE.
            isetty = isetty + 1
         END IF

!  find the row and column dimensions (ninv and nelv, resp.) of the
!  transformation matrix u. u is stored in vector form by columns

         is = IINV( itype ) - 1
         js = IELV( itype ) - 1
         nelv = IELV( itype + 1 ) - IELV( itype )
         ninv = IINV( itype + 1 ) - IINV( itype )
         nn = ninv * nelv
         IF ( nn > numax ) THEN
            status = - 11
            GO TO 700
         END IF

! --------- find type of current element

         IF ( nelv > ninv ) WRITE( outra, 4060 ) field2, itype

!  initialize u as the zero matrix

         DO 420 i = 1, nn
            U( i ) = zero
  420    CONTINUE
         setran = nelv > ninv

! --------- set elemental variables

         k1 = IELV( itype )
         k2 = IELV( itype + 1 ) - 1
         DO 430 k = k1, k2
            ivar = k - k1 + 1
            WRITE( outfn, 3070 ) ENAMES( k ), FIELDI(  4 )(1:6),               &
                FIELDI(  8 )(1:6), FIELDI( 16 )(1:6), ivar
  430    CONTINUE

! --------- set elemental parameters

         k1 = IEPA( itype )
         k2 = IEPA( itype + 1 ) - 1
         DO 435 k = k1, k2
            ivar = k - k1 + 1
            WRITE( outfn, 3071 ) EPNAME( k ), FIELDI( 18 )(1:6),               &
                FIELDI( 20 )(1:6), ivar
  435    CONTINUE

!  find the number of internal variables and the required size of
!  the lower triangular portion of the Hessian matrix

         k1 = IINV( itype )
         k2 = IINV( itype + 1 ) - 1
         ninvar = k2 - k1 + 1
         nhess = ninvar * ( ninvar + 1 ) / 2
         nvars = 0
         nh = 0
         startp = .FALSE.
         startf = .FALSE.
         startg = .FALSE.
         starth = .FALSE.
         endoff = .TRUE.
         endofg = .TRUE.
         endofh = .TRUE.
         nomorg = .FALSE.
      ELSE
         IF ( field1 == 'R ' ) THEN

!  the range transformation matrix u is now defined, entry by entry
!  determine which internal variable is given in field2

            DO 440 i = 1, ninv
               IF ( field2 == INAMES( is + i ) ) GO TO 450
  440       CONTINUE

!  the internal variable name is unrecognised

            status = 65
            IF ( out > 0 ) WRITE( out, 2650 )
            GO TO 800

!  the internal variable is the i-th in the list

  450       CONTINUE

!  determine which elemental variable(s) occur in fields

            IF ( novals > 0 ) THEN
               DO 480 k = 1, novals
                  DO 460 j = 1, nelv
                     IF ( FIELDS( k ) == ENAMES( js + j ) ) GO TO 470
  460             CONTINUE

!  the elemental variable name is unrecognised

                  status = 66
                  IF ( out > 0 ) WRITE( out, 2660 )
                  GO TO 800

!  the elemental variable is the j-th in the list

  470             CONTINUE

!  insert the value of the new nonzero into u

                  U( ninv * ( j - 1 ) + i ) = VALUES( k )
  480          CONTINUE
            END IF
         ELSE
            IF ( FIELD1( 1: 1 ) == 'A' .OR. FIELD1( 1: 1 )                     &
                 == 'I' .OR. FIELD1( 1: 1 ) == 'E' ) THEN
               IF ( startf ) THEN
                  IF ( .NOT. endoff ) THEN
                     WRITE( outfn, 3120 )
                     endoff = .TRUE.
                  END IF
               END IF
               IF ( startg ) THEN
                  IF ( endofg .AND. .NOT. nomorg ) THEN
                     WRITE( outfn, 3150 ) FIELDI( 12 )(1:6)
                     nomorg = .TRUE.
                  END IF
               END IF

!  start a parameter assignment

               IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                  startp = .TRUE.

!  set up the transformations for the element

                  IF ( setran ) THEN
                      CALL OUTRAN( nelv, ninv, U, outfn, outra,                &
                                   ENAMES( js + 1 ), INAMES( is + 1 ),         &
                                   single )
                      setran = .FALSE.
                  END IF

!  check to see that the parameter has been defined

                  field = field2 // 'PF'
                  CALL HASH_search(LENGTH, 12, field, KEY, ITABLE, IFIELD)
                  IF ( ifield <= 0 ) THEN
                     status = 58
                     IF ( out > 0 ) WRITE( out, 2580 )
                     GO TO 800
                  END IF

! --------- make element-specific parameter assignments

                  IF ( FIELD1( 1: 1 ) == 'A' ) THEN
                     IF ( .NOT. startf ) THEN
                        WRITE( outfn, 3080 ) FIELD2(1:6), field7
                     ELSE
                        WRITE( outfn, 3083 ) FIELD2(1:6), field7
                     END IF

! --------- make conditional parameter assignments

                  ELSE   

!  check that the logical variable has been defined

                     field = field3 // '  PF'
                     CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
                     IF ( ifield <= 0 ) THEN
                        IF ( ifree == 0 ) THEN
                           status = - 1
                           GO TO 700
                        END IF
                        status = 58
                        IF ( out > 0 ) WRITE( out, 2580 )
                        GO TO 800
                     END IF
                     IF ( FIELD1( 1: 1 ) == 'I' ) THEN
                        IF ( .NOT. startf ) THEN
                           WRITE( outfn, 3081 ) FIELD2(1:6),                   &
                                                 FIELD3(1:6), field7
                        ELSE
                           WRITE( outfn, 3084 ) FIELD2(1:6),                   &
                                                 FIELD3(1:6), field7
                        END IF
                     ELSE
                     IF ( .NOT. startf ) THEN
                           WRITE( outfn, 3082 ) FIELD2(1:6),                   &
                                                 FIELD3(1:6), field7
                        ELSE
                           WRITE( outfn, 3085 ) FIELD2(1:6),                   &
                                                 FIELD3(1:6), field7
                        END IF
                     END IF   
                  END IF
               ELSE
                  IF ( FIELD1( 2: 2 ) == '+' ) THEN
                     IF ( startp ) THEN

! --------- continuation of a parameter assignment

                     IF ( .NOT. startf ) THEN
                        WRITE( outfn, 3090 ) field7
                     ELSE
                        WRITE( outfn, 3091 ) field7
                     END IF
                     ELSE
                        status = 56
                        IF ( out > 0 ) WRITE( out, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            ELSE
               startp = .FALSE.
               IF ( FIELD1( 1: 1 ) == 'F' ) THEN

!  set the function value

                  IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                     startf = .TRUE.
                     endoff = .FALSE.

!  set up the transformations for the element

                     IF ( setran ) THEN
                         CALL OUTRAN( nelv, ninv, U, outfn, outra,             &
                                   ENAMES( js + 1 ), INAMES( is + 1 ),         &
                                   single )
                         setran = .FALSE.
                     END IF

! --------- start f

                     WRITE( outfn, 3100 ) FIELDI( 12 )(1:6),                   &
                     FIELDI( 3 )(1:6), FIELDI( 13 )(1:6), field7
                  ELSE
                     IF ( FIELD1( 2: 2 ) == '+' ) THEN
                        IF ( startf ) THEN

! --------- continuation of f

                           WRITE( outfn, 3110 ) field7
                        ELSE
                           status = 56
                           IF ( out > 0 ) WRITE( out, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               ELSE
                  IF ( FIELD1( 1: 1 ) == 'G' ) THEN

!  no function value has been specified

                     IF ( .NOT. startf ) THEN
                        status = 61
                        IF ( out > 0 ) WRITE( out, 2610 )
                        GO TO 800
                     END IF

!  set the gradient values

                     IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                        IF ( .NOT. startg ) THEN
                           startg = .TRUE.
                           endofg = .FALSE.

!  use the logical array setvec to ensure that all gradients are set

                           IF ( ninvar > nsetvc ) THEN
                              status = - 12
                              GO TO 700
                           END IF
                           DO 510 i = 1, ninvar
                              SETVEC( i ) = .FALSE.
  510                      CONTINUE

! --------- start g

                           IF ( .NOT. endoff ) THEN
                              WRITE( outfn, 3120 )
                              endoff = .TRUE.
                           END IF
                        END IF

!  find which component is to be set

                        DO 520 k = k1, k2
                           ivar = k - k1 + 1
                           IF ( field2 == INAMES( k ) ) GO TO 525
  520                   CONTINUE

!  the component name is unrecognised

                        status = 60
                        IF ( out > 0 ) WRITE( out, 2600 )
                        GO TO 800
  525                   CONTINUE

! --------- set a component of g

                        IF ( SETVEC( ivar ) ) THEN
                           status = 69
                           IF ( out > 0 ) WRITE( out, 2690 )
                           GO TO 800
                        END IF
                        SETVEC( ivar ) = .TRUE.
                        nvars = nvars + 1
                        endofg = nvars == ninvar
                        WRITE( outfn, 3130 ) FIELDI(  3 )(1:6),                &
                               FIELDI( 17 )(1:6), ivar, field7
                     ELSE
                        IF ( FIELD1( 2: 2 ) == '+' ) THEN
                           IF ( startg .AND. .NOT. nomorg ) THEN

! --------- continuation of g

                              WRITE( outfn, 3140 ) field7
                           ELSE
                              status = 56
                              IF ( out > 0 ) WRITE( out, 2560 )
                              GO TO 800
                           END IF
                        END IF
                     END IF
                  ELSE
                     IF ( FIELD1( 1: 1 ) == 'H' ) THEN

!  set the Hessian values

                        IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                           IF ( .NOT. starth ) THEN

!  set the remaining gradient components to zero

                              IF ( .NOT. startg ) THEN
                                 DO 530 ivar = 1, ninvar

! --------- set a component of g

                                    IF ( single ) THEN
                                       WRITE( outfn, 3132 )                    &
                                          FIELDI(  3 )(1:6),                   &
                                          FIELDI( 17 )(1:6), ivar
                                    ELSE
                                       WRITE( outfn, 3131 )                    &
                                          FIELDI(  3 )(1:6),                   &
                                          FIELDI( 17 )(1:6), ivar
                                    END IF   
  530                            CONTINUE
                                 startg = .TRUE.
                                 endofg = .TRUE.
                              END IF
                              IF ( .NOT. endofg ) THEN
                                 DO 535 ivar = 1, ninvar

! --------- set a component of g

                                    IF ( .NOT. SETVEC( ivar ) ) THEN
                                       IF ( single ) THEN
                                          WRITE( outfn, 3132 )                 &
                                             FIELDI(  3 )(1:6),                &
                                             FIELDI( 17 )(1:6), ivar
                                       ELSE
                                          WRITE( outfn, 3131 )                 &
                                             FIELDI(  3 )(1:6),                &
                                             FIELDI( 17 )(1:6), ivar
                                       END IF   
                                    END IF   
  535                            CONTINUE
                                 endofg = .TRUE.
                              END IF
                              IF ( .NOT. nomorg ) THEN
                                 WRITE( outfn, 3150 )                          &
                                        FIELDI( 12 )(1:6)
                                 nomorg = .TRUE.
                              END IF
                              starth = .TRUE.
                              endofh = .FALSE.

!  use the logical array setvec to ensure that all Hessians are set

                              IF ( nhess > nsetvc ) THEN
                                 status = - 12
                                 GO TO 700
                              END IF
                              DO 540 i = 1, nhess
                                 SETVEC( i ) = .FALSE.
  540                         CONTINUE
                           END IF

! ---------  start h


!  find which component is to be set

                           DO 550 k = k1, k2
                              ivar = k - k1 + 1
                              IF ( field2 == INAMES( k ) ) GO TO 560
  550                      CONTINUE

!  the component name field2 is unrecognised

                           status = 71
                           IF ( out > 0 ) WRITE( out, 2710 )
                           GO TO 800
  560                      CONTINUE
                           DO 570 k = k1, k2
                              jvar = k - k1 + 1
                              IF ( field3 == INAMES( k ) ) GO TO 580
  570                      CONTINUE

!  the component name field3 is unrecognised

                           status = 71
                           IF ( out > 0 ) WRITE( out, 2710 )
                           GO TO 800
  580                      CONTINUE

!  find the address of the component of the Hessian. the matrix is
!  stored as an upper triangle by rows

                           IF ( ivar > jvar ) THEN
                              i = ivar
                              ivar = jvar
                              jvar = i
                           END IF
                           ihvar = ivar + jvar * ( jvar - 1 ) / 2

!  ensure that the component has not already been set

                           IF ( SETVEC( ihvar ) ) THEN
                              status = 70
                              IF ( out > 0 ) WRITE( out, 2700 )
                              GO TO 800
                           END IF
                           SETVEC( ihvar ) = .TRUE.
                           nh = nh + 1
                           endofh = nh == nhess

! --------- set a component of h

                           WRITE( outfn, 3160 ) FIELDI(  3 )(1:6),             &
                                  FIELDI( 15 )(1:6), ihvar, field7
                        ELSE
                           IF ( FIELD1( 2: 2 ) == '+' ) THEN
                              IF ( starth ) THEN

! --------- continuation of h

                                 WRITE( outfn, 3170 ) field7
                              ELSE
                                 status = 56
                                 IF ( out > 0 ) WRITE( out, 2560 )
                                 GO TO 800
                              END IF
                           END IF
                        END IF
                     ELSE
                        status = 56
                        IF ( out > 0 ) WRITE( out, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE 

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
         status = 52
         IF ( out > 0 ) WRITE( out, 2520 )
         RETURN
      END IF
      qprod = .FALSE.
      DO 591 itype = 1, MIN( 2, neltyp ) 
         IF ( ETYPES( itype ) /= cqsqr .AND.                                   &
              ETYPES( itype ) /= cqprod ) GO TO 930
         IF ( ETYPES( itype ) == cqprod ) qprod = .TRUE.
  591 CONTINUE   
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE 

!  write a dummy elfuns routine

      IF ( neltyp == 0 ) THEN
         IF ( single ) THEN
            WRITE( outfn, 3003 ) FIELDI( 1 )(1:6),                             &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),            &
         ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                  &
           FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),               &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                               &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                               &
         ( FIELDI(  i )(1:6), i = 22, 32 ),                                    &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                               &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                               &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                               &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                               &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                               &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                               &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                               &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                               &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                               &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), pname, TRIM( version )
         ELSE   
            WRITE( outfn, 3002 ) FIELDI( 1 )(1:6),                             &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),            &
         ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                  &
           FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),               &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                               &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                               &
         ( FIELDI(  i )(1:6), i = 22, 32 ),                                    &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                               &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                               &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                               &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                               &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                               &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                               &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                               &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                               &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                               &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), pname, TRIM( version )
         END IF
         WRITE( outfn, 3009 ) FIELDI( 32 )(1:6)
         WRITE( outfn, 3201 )
      ELSE
         IF ( single ) THEN
            WRITE( outfn, 3001 ) FIELDI( 1 )(1:6),                             &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),            &
         ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                  &
           FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),               &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6),            &
           FIELDI( 12 )(1:6), ( FIELDI(  i )(1:6), i = 22, 32 ),               &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                               &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                               &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                               &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                               &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                               &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                               &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                               &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                               &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                               &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                               &
           pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),       &
           FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                               &
           FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( qprod ) THEN
               WRITE( outfn, 3019 ) 'X     ', 'Y     '
            ELSE
               WRITE( outfn, 3019 ) 'X     '
            END IF
         ELSE   
            WRITE( outfn, 3000 ) FIELDI( 1 )(1:6),                             &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),            &
         ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                  &
           FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),               &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6),            &
           FIELDI( 12 )(1:6), ( FIELDI(  i )(1:6), i = 22, 32 ),               &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                               &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                               &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                               &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                               &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                               &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                               &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                               &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                               &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                               &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                               &
           pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),       &
           FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                               &
           FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( qprod ) THEN
               WRITE( outfn, 3020 ) 'X     ', 'Y     '
            ELSE
               WRITE( outfn, 3020 ) 'X     '
            END IF
         END IF
         WRITE( outfn, 3009 ) FIELDI( 32 )(1:6)
         WRITE( outfn, 3050 ) nloop,                                           &
                FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),                          &
                FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),                          &
                FIELDI( 21 )(1:6),                                             &
                FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),                          &
                FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),                          &
                FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),                          &
                FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),                          &
                FIELDI( 13 )(1:6),                                             &
                FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),                          &
                FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
         IF ( neltyp > 1 ) THEN
            WRITE( outfn, 3051 )                                               &
               FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),                           &
               FIELDI( 13 )(1:6), ( i, i = 1, neltyp )
            WRITE( outfn, 3052 ) FIELDI( 14 )(1:6)
         END IF

!  make sure that quadratic Hessian terms are included

         DO 640 itype = 1, MIN( 2, neltyp )

!  diagonal term

            IF ( ETYPES( itype ) == cqsqr ) THEN
               WRITE( outfn, 3060 ) ETYPES( itype )
               IF ( neltyp > 1 ) WRITE( outfn, 3061 ) itype
               IF ( single ) THEN
                 WRITE( outfn, 3053 ) 'E', 'E'
               ELSE
                 WRITE( outfn, 3053 ) 'D', 'D'
               END IF
               LDEFND( itype ) = .TRUE.
               isetty = isetty + 1
               IF ( isetty < neltyp ) WRITE( outfn, 3191 ) nloop

!  off-diagonal term

            ELSE IF ( ETYPES( itype ) == cqprod ) THEN
               WRITE( outfn, 3060 ) ETYPES( itype )
               IF ( neltyp > 1 ) WRITE( outfn, 3061 ) itype
               IF ( single ) THEN
                 WRITE( outfn, 3054 ) 'E', 'E', 'E'
               ELSE
                 WRITE( outfn, 3054 ) 'D', 'D', 'D'
               END IF
               LDEFND( itype ) = .TRUE.
               isetty = isetty + 1
               IF ( isetty < neltyp ) WRITE( outfn, 3191 ) nloop
            END IF
  640    CONTINUE   
         WRITE( outfn, 3200 ) nloop
      END IF

!  write a dummy range routine

      IF ( single ) THEN
         WRITE( outra, 4003 ) pname, TRIM( version )
      ELSE
         WRITE( outra, 4002 ) pname, TRIM( version )
      END IF
      WRITE( outra, 4080 )
      WRITE( outra, 4090 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 ) INCRSE( - status )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE
      IF ( .NOT. firstl ) THEN

!  finish of the previous element, if any

         IF ( starth ) THEN
            IF ( .NOT. endofh ) THEN
               DO 910 ihvar = 1, nhess

! --------- set a component of h

                  IF ( .NOT. SETVEC( ihvar ) ) THEN
                     IF ( single ) THEN
                        WRITE( outfn, 3162 ) FIELDI(  3 )(1:6),                &
                               FIELDI( 15 )(1:6), ihvar
                     ELSE
                        WRITE( outfn, 3161 ) FIELDI(  3 )(1:6),                &
                               FIELDI( 15 )(1:6), ihvar
                     END IF   
                  END IF   
  910          CONTINUE
               endofh = .TRUE.
            END IF

! ---------- wind up h

            WRITE( outfn, 3180 )
         END IF
         IF ( startg ) THEN

!  set the remaining gradient components to zero

            IF ( .NOT. endofg ) THEN
               DO 920 ivar = 1, ninvar

! --------- set a component of g

                  IF ( .NOT. SETVEC( ivar ) ) THEN
                     IF ( single ) THEN
                        WRITE( outfn, 3132 )                                   &
                           FIELDI(  3 )(1:6),                                  &
                           FIELDI( 17 )(1:6), ivar
                     ELSE
                        WRITE( outfn, 3131 )                                   &
                           FIELDI(  3 )(1:6),                                  &
                           FIELDI( 17 )(1:6), ivar
                     END IF   
                  END IF   
  920          CONTINUE
               endofg = .TRUE.
            END IF

! ---------- wind up f and g

         END IF
         IF ( startf ) THEN
            WRITE( outfn, 3190 )
         ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
         END IF
         IF ( isetty < neltyp ) WRITE( outfn, 3191 ) nloop
      END IF

! ---------- successful run. wind up output

      status = 0
      WRITE( outfn, 3200 ) nloop
      IF ( nointe ) WRITE( outra, 4070 )
      IF ( neltyp == 0 ) WRITE( outra, 4080 )
      WRITE( outra, 4090 )

!   check that all element types have been defined

  930 CONTINUE
      DO 940 itype = 1, neltyp
         IF ( .NOT. LDEFND( itype ) ) THEN
            status = 68
            IF ( out > 0 ) WRITE( out, 2680 ) ETYPES( itype )
         END IF
  940 CONTINUE
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_elfun - insufficient space.',                &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAKE_elfun - warning.',                           &
              ' First card not elements. ', /, '    A dummy',                  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_elfun - indicator card not recognised ' )
 2090 FORMAT( ' ** Exit from MAKE_elfun - element type not recognised ' )
 2510 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_elfun - data file incomplete.',              &
              ' No ENDATA card ' )
 2540 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' unrecognised field 1 in INDIVIDUALS section' )
 2570 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_elfun - repeated parameter name ', A8 )
 2600 FORMAT( ' ** Exit from MAKE_elfun - unknown component of gradient ' )
 2610 FORMAT( ' ** Exit from MAKE_elfun - function not set '  )
 2650 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' internal variable not recognised ' )
 2660 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' elemental variable not recognised ' )
 2670 FORMAT( ' ** Exit from MAKE_elfun - element type already defined ' )
 2680 FORMAT( ' ** Exit from MAKE_elfun - warning, element type ', A10,        &
              ' undefined ' )
 2690 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' gradient component already defined ' )
 2700 FORMAT( ' ** Exit from MAKE_elfun -',                                    &
              ' Hessian component already defined ' )
 2710 FORMAT( ' ** Exit from MAKE_elfun - unknown component of Hessian '  )
 2720 FORMAT( ' ** Exit from MAKE_elfun - field 3 not blank on',               &
              ' A, F or G card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER ', 5( A6, ', ' ), A6, /,                          &
              '      INTEGER ', A6 )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER ', 5( A6, ', ' ), A6, /,                          &
              '      INTEGER ', A6 )
 3002 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3003 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
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
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ') ', /,                      &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       IF ( ', A6, ' == 3 ) ',                                  &
                      A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3053 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3054 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= X * Y', /,                              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= Y', /,                           &
              '        FUVALS(IGSTRT+     2)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3060 FORMAT( 'C', /, 'C  Element type : ', A10, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE' )
 3070 FORMAT( '       ', A6, ' = ', A6, '(', A6, '(', A6, '+', i6, '))')
 3071 FORMAT( '       ', A6, ' = ', A6, '(', A6, '+', i6, ')')
 3080 FORMAT( '       ', A6, ' = ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        ', A6, '(', A6, ')= ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3130 FORMAT( '        ', A6, '(', A6, '+', i6, ')= ', A41 )
 3131 FORMAT( '        ', A6, '(', A6, '+', i6, ')= 0.0D+0' )
 3132 FORMAT( '        ', A6, '(', A6, '+', i6, ')= 0.0E+0' )
 3140 FORMAT( '     *                         ', A41 )
 3150 FORMAT( '        IF ( ', A6, ' == 3 ) THEN' )
 3160 FORMAT( '         ', A6, '(', A6, '+', i6, ')=', A41 )
 3161 FORMAT( '         ', A6, '(', A6, '+', i6, ')=0.0D+0' )
 3162 FORMAT( '         ', A6, '(', A6, '+', i6, ')=0.0E+0' )
 3170 FORMAT( '     *                         ', A41 )
 3180 FORMAT( '        END IF' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3200 FORMAT( i5,  ' CONTINUE', /, '      RETURN', /,                          &
              '      END' )
 3201 FORMAT( '      RETURN', /,                                               &
              '      END' )
 4000 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4001 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4002 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4003 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4040 FORMAT( '      GO TO (', 8( i5, :, ',' ), /,                             &
            ( '     *       ', 8( i5, :, ',' ) ) )
 4050 FORMAT( '     *        ', 48X, '), ITYPE' )
 4060 FORMAT( 'C', /, 'C  Element type : ', A10, /, 'C', /,                    &
              i5, ' CONTINUE', /,                                              &
              '      IF ( TRANSP ) THEN' )
 4070 FORMAT( 'C', /,                                                          &
              'C  Elements without internal variables.', /,                    &
              'C', /,                                                          &
              '99998 CONTINUE', /,                                             &
              '      DO 99999 i = 1, NELVAR', /,                               &
              '         W2( i ) = W1( i )', /,                                 &
              '99999 CONTINUE', /,                                             &
              '      RETURN' )
 4080 FORMAT( '      RETURN' )
 4090 FORMAT( '      END' )

!  end of subroutine MAKE_elfun

      END SUBROUTINE MAKE_elfun

!-*-  S I F D E C O D E   M A K E _ e l f u n _ a d    S U B R O U T I N E  -*-

      SUBROUTINE MAKE_elfun_ad( input, out, outff, outfd, outra, outem,        &
                                status, nlmax, nimax, netmax, ninmax, numax,   &
                                neltyp, pname, ENAMES, INAMES, RENAME, INNAME, &
                                LONAME, MINAME, EXNAME, ETYPES, LDEFND,        &
                                length, ITABLE, KEY, IELV, IINV, INLIST,       &
                                EPNAME, IEPA, nepmax, debug, ijump, U, single, &
                                nuline, gotlin, iauto, iad0, print_level )
      INTEGER :: input, out, outff, outra, outfd, outem, status
      INTEGER :: nlmax, nimax, netmax, neltyp, ninmax
      INTEGER :: nepmax, length, numax, print_level, iauto, iad0
      LOGICAL :: debug, single, gotlin 
      INTEGER :: ITABLE( length ), ijump ( nlmax  )
      INTEGER :: IELV( nlmax  ), IINV( nlmax  )
      INTEGER :: INLIST( length )
      INTEGER :: IEPA( nlmax  )
      LOGICAL :: LDEFND( nlmax  )
      REAL ( KIND = wp ) ::   U( numax )
      CHARACTER ( len = 12 ) :: KEY( length )
      CHARACTER ( len = 8  ) :: pname
      CHARACTER ( len = 10 ) :: EPNAME( nepmax ), EXNAME( ninmax )
      CHARACTER ( len = 10 ) :: INAMES( nimax  ), RENAME( ninmax )
      CHARACTER ( len = 10 ) :: LONAME( ninmax ), INNAME( ninmax )
      CHARACTER ( len = 10 ) :: MINAME( ninmax ), ENAMES( netmax )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax  )
      CHARACTER ( len = max_record_length ) :: nuline

!  --------------------------------------------------------------------
!  make a function evaluation subroutine, suitable for automatic
!  differentiation, and a range transformation subroutine from a 
!  gps function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  ELEMENTS     problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  define the transformation from the elemental to the
!               internal variables for all elements with internal
!               vars, set function and derivative values and make
!               element specific parameter assignments
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in 
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of inform indicate that insufficient
!  array space has been allowed, as follows:

!    inform = - 1  when length not large enough
!    inform = - 2  when max( ninnam, nrenam, nlonam, nenam, nminam )
!                  > ninmax
!    inform = - 11 when numax not large enough
!  -------------------------------------------------------------------

!  parameter definitions

      INTEGER, PARAMETER :: mblank = 1, mfixed = 2, mfree = 3, mname = 4
      INTEGER, PARAMETER :: mtemp = 5, mglob = 6, mindiv = 7, mendat = 8
      INTEGER, PARAMETER :: iires = 33
      INTEGER, PARAMETER :: nincrs = 12
      INTEGER, PARAMETER :: maxnul = 20
      INTEGER, DIMENSION( mendat ), PARAMETER :: LENIND                        &
        = (/ 0, 12, 11, 8, 11, 7, 11, 6 /)
      CHARACTER ( len = 12 ), DIMENSION( mendat ), PARAMETER :: INDIC8         &
        = (/ '            ', 'FIXED FORMAT', 'FREE FORMAT ', 'ELEMENTS    ',   &
             'TEMPORARIES ', 'GLOBALS     ', 'INDIVIDUALS ', 'ENDATA      '  /)
      CHARACTER ( len = 8 ), DIMENSION( iires ), PARAMETER :: FIELDI           &
        = (/ 'ELFUNF  ', 'LFUVAL  ', 'FUVALS  ', 'XVALUE  ', 'NCALCF  ',       &
             'ITYPEE  ', 'ISTAEV  ', 'IELVAR  ', 'INTVAR  ', 'ISTADH  ',       &
             'ICALCF  ', 'IFFLAG  ', 'IELEMN  ', 'IELTYP  ', 'IHSTRT  ',       &
             'ILSTRT  ', 'IGSTRT  ', 'EPVALU  ', 'ISTEPA  ', 'IPSTRT  ',       &
             'JCALCF  ', 'LTYPEE  ', 'LSTAEV  ', 'LELVAR  ', 'LNTVAR  ',       &
             'LSTADH  ', 'LSTEPA  ', 'LCALCF  ', 'LFVALU  ', 'LXVALU  ',       &
             'LEPVLU  ', 'IFSTAT  ', 'ELFUN   ' /)
      CHARACTER ( len = 6 ), DIMENSION( nincrs ), PARAMETER :: INCRSE          &
        = (/ 'LENGTH', 'NINMAX', '      ', '      ', '      ', '      ',       &
             '      ', '      ', '      ', '      ', 'NUMAX ', 'NSETVC'  /)

!  local variables

      INTEGER :: i, niname, ninnam, ninvar, nloop, nrenam, nexnam, nlonam
      INTEGER :: i1, i2, i3, i4, i5, i6
      INTEGER :: ifield, ifree, ihvar, ivar, intype, nminam, k, k1, k2, isetty
      INTEGER :: itype, j, is, js, novals, nelv, ninv, nn, nename, nlines
      INTEGER :: npname, lineno, ilines, maxnel, maxnin, ntem, nrenm1, nrenm2
      LOGICAL :: nointe, defnam, endpar, endgen, firstl, setran
      LOGICAL :: startf, startp, startv, qprod
      LOGICAL :: endoff, fixed, cqsqrt, cqprdt, loutff
      CHARACTER ( len = 4 ) :: ad0
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 6 ) :: xvar, yvar
      CHARACTER ( len = 8 ) :: field3
      CHARACTER ( len = 10 ) :: field2, ctemp
      CHARACTER ( len = 12 ) :: field, header
      CHARACTER ( len = 15 ) :: aorb
      CHARACTER ( len = 41 ) :: field7
      CHARACTER ( len = 72 ) :: ctem
      CHARACTER ( len = max_record_length ) :: blnkln
      REAL ( KIND = wp ), DIMENSION( 2 ) :: VALUES
      CHARACTER ( len = 8 ), DIMENSION( 2 ) :: FIELDS
      CHARACTER ( len = 65 ), DIMENSION( maxnul ) :: NULINA

      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnam = 0 ; nrenam = 0 ; nlonam = 0 ; nminam = 0 ; nexnam = 0 ; ntem = 0
      lineno = 0 ; nloop = neltyp + 1 ; intype = 1 ; ilines = 0 ; nlines = 0

!  set initial values for logical variables

      defnam = .FALSE. ; endpar = .FALSE. ; startp = .FALSE. ; fixed = .TRUE.
      endgen = .FALSE. ; firstl = .TRUE. ;  nointe = .FALSE. ; gotlin = .FALSE.

      loutff = outff > 0

!  assign unique names to variables from quadratic terms

      i1 = 0 ; i2 = 1 ; i3 = 1 ; i4 = 1 ; i5 = 1 ; i6 = 1
      cqsqrt = .FALSE. ; cqprdt = .FALSE.
      DO itype = 1, MIN( 2, neltyp )
        IF ( ETYPES( itype ) == cqsqr ) cqsqrt = .TRUE.
        IF ( ETYPES( itype ) == cqprod ) cqprdt = .TRUE.
      END DO
      IF ( cqprdt ) THEN
         yvar = NUNAME( i1, i2, i3, i4, i5, i6, iires,                         &
                        ninmax, nrenam, ninnam, nlonam,                        &
                        nminam, nexnam, nlmax, neltyp, '      ',               &
                        FIELDI, RENAME, INNAME, LONAME,                        &
                        MINAME, EXNAME, ETYPES )
      ELSE
         yvar = '      '
      END IF
      IF ( cqsqrt .OR.  cqprdt ) THEN
         xvar = NUNAME( i1, i2, i3, i4, i5, i6, iires,                         &
                        ninmax, nrenam, ninnam, nlonam,                        &
                        nminam, nexnam, nlmax, neltyp, yvar,                   &
                        FIELDI, RENAME, INNAME, LONAME,                        &
                        MINAME, EXNAME, ETYPES )
      ELSE
         xvar = '      '
      END IF

!  create a dictionary of the internal variable names used

      niname = IINV( neltyp + 1 ) - 1
      DO 20 i = 1, niname
         field = INAMES( i ) // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = INAMES( i )
         END IF
   20 CONTINUE

!  include the names of the elemental variables used in this dictionary

      nename = IELV( neltyp + 1 ) - 1
      DO 30 i = 1, nename
         field = ENAMES( i ) // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = ENAMES( i )
         END IF
   30 CONTINUE
!     netnam = nrenam

!  include the names of the elemental parameters used
!  in this dictionary

      npname = IEPA( neltyp + 1 ) - 1
      DO 40 i = 1, npname
         field = EPNAME( i ) // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = EPNAME( i )
         END IF
   40 CONTINUE

!  find which element types have an internal representation

      maxnin = 1
      maxnel = 1
      isetty = 0
      DO 50 itype = 1, neltyp
         LDEFND( itype ) = .FALSE.
         IF ( IELV( itype + 1 ) - IELV( itype ) ==                             &
              IINV( itype + 1 ) - IINV( itype ) ) THEN
            IJUMP( itype ) = 99998
            nointe = .TRUE.
         ELSE
            IJUMP( itype ) = itype
         END IF
         maxnin = MAX( maxnin, IINV( itype + 1 ) - IINV( itype ) )
         maxnel = MAX( maxnel, IELV( itype + 1 ) - IELV( itype ) )
   50 CONTINUE

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( I: i ) = ' '
      END DO

!  read next line

  100 CONTINUE
      IF ( ilines + 1 > nlines ) THEN

!  read next line from the input file

         lineno = lineno + 1
         nuline = blnkln
         IF ( fixed ) THEN
            READ ( input, 1000, END = 590, ERR = 590 ) nuline
            IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
         ELSE
            READ ( input, 1010, END = 590, ERR = 590 ) nuline
            IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

            CALL FREE_format( nuline, max_record_length, mendat, INDIC8,       &
                              LENIND, NULINA, maxnul, nlines, .FALSE.,         &
                              status, out )
            IF ( status > 0 ) GO TO 800
            IF ( nlines > 0 ) THEN

!  if there are non-blank lines on the free format card, read the first

               ilines = 1
               nuline = blnkln
               nuline = NULINA( ilines )
               IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines,   &
                 nuline
            ELSE

!  there are only blank lines on the free format card

               GO TO 100
            END IF
         END IF
      ELSE

!  read next line from the last encountered free format card

         ilines = ilines + 1
         nuline = blnkln
         nuline = NULINA( ilines )
         IF ( out > 0 .AND. debug ) WRITE( out, 2980 ) lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1: 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1: 1 ) /= ' ' ) THEN

!  ignore comment cards

         IF ( NULINE( 1: 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

         IF ( header == INDIC8( mfixed ) ) THEN
            fixed = .TRUE.
            GO TO 100
         END IF

!  check if we have entered free-format input

         IF ( header == INDIC8( mfree ) ) THEN
            fixed = .FALSE.
            GO TO 100
         END IF

!  check that the first encountered indicator card is the elements card

         IF ( .NOT. defnam  ) THEN
            IF ( header /= INDIC8( mname ) ) THEN
               IF ( neltyp > 0 ) GO TO 930
               IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
               gotlin = .TRUE.
               GO TO 600
            ELSE

!  indicator card is elements
!  ---------------------------

               IF ( pname  /= NULINE( 15: 22 ) ) THEN
                  status = 51
                  IF ( out > 0 ) WRITE( out, 2510 )
                  GO TO 800
               ELSE
                  defnam = .TRUE.

!  -------- set up subroutine call for range routine

                  IF ( single ) THEN
                     WRITE( outra, 4001 ) pname, TRIM( version )
                  ELSE
                     WRITE( outra, 4000 ) pname, TRIM( version )
                  END IF
                  IF ( neltyp > 1 ) THEN
                     WRITE( outra, 4040 ) ( IJUMP( i ), i = 1, neltyp )
                     WRITE( outra, 4050 )
                  END IF
                  GO TO 100
               END IF
            END IF
         END IF

!  an indicator card has been found

         DO 110 i = intype, mendat
            IF ( header == INDIC8( i ) ) THEN
               intype = i
               GO TO 120
            END IF
  110    CONTINUE

!  the indicator card is not recognised

         status = 2
         IF ( out > 0 ) WRITE( out, 2020 )
         GO TO 800
  120    CONTINUE

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

         IF ( intype >= mglob .AND. .NOT. endpar ) THEN
            endpar = .TRUE.

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

            DO 130 i = 1, iires
               field = FIELDI( i ) // '  PF'
               CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
               IF ( ifree <= 0 ) THEN
                  IF ( ifree == 0 ) THEN
                     status = - 1
                     GO TO 700
                  END IF
                  status = 59
                  IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- set up subroutine call and reserved parameter declarations

            IF ( iauto == 1 ) THEN
               IF ( single ) THEN
                  aorb = 'FORWARD_SINGLE '
               ELSE
                  aorb = 'FORWARD_DOUBLE '
               END IF
            ELSE
               IF ( single ) THEN
                  aorb = 'BACKWARD_SINGLE'
               ELSE
                  aorb = 'BACKWARD_DOUBLE'
               END IF
            END IF
            IF ( iad0 == 1 ) THEN
               ad0 = 'AD01'
            ELSE
               ad0 = 'AD02'
            END IF
            IF ( single ) THEN
               IF ( loutff ) WRITE( outff, 3001 ) FIELDI( 1 )(1:6),            &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),            &
         ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                  &
           FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),               &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6),            &
           FIELDI( 12 )(1:6), ( FIELDI(  i )(1:6), i = 22, 32 ),               &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                               &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                               &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                               &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                               &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                               &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                               &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                               &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                               &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                               &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                               &
           pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),       &
           FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                               &
           FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
               WRITE( outfd, 3005 ) FIELDI( 33 )(1:6),                         &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),             &
        ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                   &
          FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), ad0, aorb,                     &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                &
        ( FIELDI(  i )(1:6), i = 22, 32 ), FIELDI(  6 )(1:6),                  &
          FIELDI( 22 )(1:6), FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),             &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                &
          pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),        &
          FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                &
          FIELDI( 17 )(1:6), FIELDI( 20 )(1:6),                                &
          FIELDI( 21 )(1:6), maxnin
            ELSE   
               IF ( loutff ) WRITE( outff, 3000 ) FIELDI( 1 )(1:6),            &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),             &
        ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                   &
          FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6),             &
          FIELDI( 12 )(1:6), ( FIELDI(  i )(1:6), i = 22, 32 ),                &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                &
          FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                &
          pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),        &
          FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                &
          FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
               WRITE( outfd, 3004 ) FIELDI( 33 )(1:6),                         &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),             &
        ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                   &
          FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), ad0, aorb,                     &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                &
        ( FIELDI(  i )(1:6), i = 22, 32 ),                                     &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6), FIELDI(  7 )(1:6),             &
          FIELDI( 23 )(1:6), FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),             &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                &
          pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),        &
          FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                &
          FIELDI( 17 )(1:6), FIELDI( 20 )(1:6),                                &
          FIELDI( 21 )(1:6), maxnin
            END IF
            IF ( iad0 == 1 ) THEN
               WRITE( outfd, 3024 ) maxnel, maxnin
            ELSE
               WRITE( outfd, 3025 ) maxnel, maxnin
            END IF

! --------- insert integer declarations

            IF ( ninnam > 0 .AND. loutff )                                     &
               WRITE( outff, 3010 ) ( INNAME( i ), i = 1, ninnam )
            IF ( ninnam > 0 )                                                  &
               WRITE( outfd, 3010 ) ( INNAME( i ), i = 1, ninnam )

!  order the real values so that the list of variables which belong
!  to intrinsic or external functions follow those which do not

            IF ( nrenam > 0 ) THEN
               nrenm1 = 0
               nrenm2 = nrenam + 1
  140          CONTINUE
               IF ( nrenm1 + 1 == nrenm2 ) GO TO 180
               DO 150 i = 1, nminam
                  IF ( RENAME( nrenm1 + 1 ) == MINAME( i ) ) GO TO 170
  150          CONTINUE
               DO 160 i = 1, nexnam
                  IF ( RENAME( nrenm1 + 1 ) == EXNAME( i ) ) GO TO 170
  160          CONTINUE
               nrenm1 = nrenm1 + 1
               GO TO 140
  170          CONTINUE
               nrenm2 = nrenm2 - 1
               ctemp = RENAME( nrenm2 )
               RENAME( nrenm2 ) = RENAME( nrenm1 + 1 )
               RENAME( nrenm1 + 1 ) = ctemp
               GO TO 140
  180          CONTINUE

! --------- insert real declarations

               IF ( loutff ) THEN
                  IF ( single ) THEN
                    WRITE( outff, 3019 ) ( RENAME( i ), i = 1, nrenam )
                  ELSE
                    WRITE( outff, 3020 ) ( RENAME( i ), i = 1, nrenam )
                  END IF
               END IF
               IF ( iad0 == 1 ) THEN
                  IF ( nrenm1 > 0 ) WRITE( outfd, 3018 )                       &
                       ( RENAME( i ), i = 1, nrenm1 )
               ELSE
                  IF ( nrenm1 > 0 ) WRITE( outfd, 3017 )                       &
                       ( ad0, RENAME( i ), i = 1, nrenm1 )
               END IF
               IF ( nrenm2 <= nrenam ) WRITE( outfd, 3017 )                    &
                    ( ad0, RENAME( i ), i = nrenm2, nrenam )
            END IF

! --------- insert logical declarations

            IF ( nlonam > 0 .AND. loutff )                                     &
               WRITE( outff, 3023 ) ( LONAME( i ), i = 1, nlonam )
            IF ( nlonam > 0 )                                                  &
               WRITE( outfd, 3023 ) ( LONAME( i ), i = 1, nlonam )

! --------- insert intrinsic declarations

            IF ( nminam > 0 .AND. loutff )                                     &
               WRITE( outff, 3021 ) ( MINAME( i ), i = 1, nminam )

! --------- insert external declarations

            IF ( nexnam > 0 .AND. loutff )                                     &
               WRITE( outff, 3022 ) ( EXNAME( i ), i = 1, nexnam )
            IF ( nexnam > 0 )                                                  &
               WRITE( outfd, 3022 ) ( EXNAME( i ), i = 1, nexnam )

! --------- insert variables for quadratic terms (if any)

            IF ( xvar /= '      ' ) THEN
               IF ( yvar /= '      ' ) THEN
                  IF ( single ) THEN
                     IF ( loutff ) WRITE( outff, 3019 ) xvar, yvar
                     WRITE( outfd, 3019 ) xvar, yvar
                  ELSE
                     IF ( loutff ) WRITE( outff, 3020 ) xvar, yvar
                     WRITE( outfd, 3020 ) xvar, yvar
                  END IF
               ELSE
                  IF ( single ) THEN
                     IF ( loutff ) WRITE( outff, 3019 ) xvar
                     WRITE( outfd, 3019 ) xvar
                  ELSE
                     IF ( loutff ) WRITE( outff, 3020 ) xvar
                     WRITE( outfd, 3020 ) xvar
                  END IF
               END IF
            END IF
            IF ( loutff ) WRITE( outff, 3009 ) FIELDI( 32 )(1:6)
            WRITE( outfd, 3009 ) FIELDI( 32 )(1:6)
         END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

         IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
            endgen = .TRUE.

! --------- start loop over elements

            IF ( loutff ) WRITE( outff, 3050 ) nloop,                          &
                   FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),                       &
                   FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),                       &
                   FIELDI( 21 )(1:6),                                          &
                   FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),                       &
                   FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),                       &
                   FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),                       &
                   FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),                       &
                   FIELDI( 13 )(1:6),                                          &
                   FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),                       &
                   FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
            IF ( iad0 == 1 ) THEN
              WRITE( outfd, 3008 ) 
            ELSE
              WRITE( outfd, 3011 )
!             do i = 1, netnam
              DO i = 1, nrenm1
                 WRITE( outfd, 3016 ) ad0, RENAME( i )
              END DO
            END IF  
            WRITE( outfd, 3050 ) nloop,                                        &
                   FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),                       &
                   FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),                       &
                   FIELDI( 21 )(1:6),                                          &
                   FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),                       &
                   FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),                       &
                   FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),                       &
                   FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),                       &
                   FIELDI( 13 )(1:6),                                          &
                   FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),                       &
                   FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
            IF ( neltyp > 1 ) THEN
               IF ( loutff ) WRITE( outff, 3051 )                              &
                  FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),                        &
                  FIELDI( 13 )(1:6), ( i, i = 1, neltyp )
               WRITE( outfd, 3051 )                                            &
                  FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),                        &
                  FIELDI( 13 )(1:6), ( i, i = 1, neltyp )
               IF ( loutff ) WRITE( outff, 3052 ) FIELDI( 14 )(1:6)
               WRITE( outfd, 3052 ) FIELDI( 14 )(1:6)
            END IF

!  make sure that quadratic Hessian terms are included

            DO 190 itype = 1, MIN( 2, neltyp )

!  diagonal term

               IF ( ETYPES( itype ) == cqsqr ) THEN
                  IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
                  WRITE( outfd, 3060 ) ETYPES( itype )
                  IF ( neltyp > 1 ) THEN
                     IF ( loutff ) WRITE( outff, 3061 ) itype
                     WRITE( outfd, 3061 ) itype
                  END IF
                  IF ( single ) THEN
                     IF ( loutff ) WRITE( outff, 3055 ) 'E'
                     WRITE( outfd, 3057 ) xvar, 'E', xvar, xvar, xvar, 'E'
                  ELSE
                     IF ( loutff ) WRITE( outff, 3055 ) 'D'
                     WRITE( outfd, 3057 ) xvar, 'D', xvar, xvar, xvar, 'D'
                  END IF
                  LDEFND( itype ) = .TRUE.
                  isetty = isetty + 1
                  IF ( isetty < neltyp ) THEN
                     IF ( loutff ) WRITE( outff, 3191 ) nloop
                     WRITE( outfd, 3191 ) nloop
                  END IF

!  off-diagonal term

               ELSE IF ( ETYPES( itype ) == cqprod ) THEN
                  IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
                  WRITE( outfd, 3060 ) ETYPES( itype )
                  IF ( neltyp > 1 ) THEN
                     IF ( loutff ) WRITE( outff, 3061 ) itype
                     WRITE( outfd, 3061 ) itype
                  END IF
                  IF ( single ) THEN
                     IF ( loutff ) WRITE( outff, 3056 )
                     WRITE( outfd, 3058 )                                      &
                       xvar, yvar, xvar, yvar, yvar, xvar, 'E', 'E', 'E'
                  ELSE
                     IF ( loutff ) WRITE( outff, 3056 )
                     WRITE( outfd, 3058 )                                      &
                       xvar, yvar, xvar, yvar, yvar, xvar, 'D', 'D', 'D'
                  END IF
                  LDEFND( itype ) = .TRUE.
                  isetty = isetty + 1
                  IF ( isetty < neltyp ) THEN
                     IF ( loutff ) WRITE( outff, 3191 ) nloop
                     WRITE( outfd, 3191 ) nloop
                  END IF
               END IF
  190       CONTINUE   
         END IF

!  indicator card is endata
!  -------------------------

         IF ( intype == mendat ) GO TO 900
         GO TO 100
      ELSE

!  check that the first non commment card is the elements indicator card

         IF ( .NOT. defnam  ) THEN
            IF ( neltyp > 0 ) GO TO 930
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
            gotlin = .TRUE.
            GO TO 600
         END IF

!  a data card has been found
!  read the character fields 1 and 2 from the card

         field1 = NULINE(  2:  3 )
         field2 = NULINE(  5: 14 )
         IF ( intype == mindiv .AND. field1 == 'R ' ) THEN

!  read the character fields 3 and 5 from the card

            FIELDS( 1 ) = NULINE( 15: 22 )
            FIELDS( 2 ) = NULINE( 40: 47 )

!  check to see if there is are any numerical values to be read

            novals = 0
            IF ( FIELDS( 1 ) /= '        ' .AND.                               &
                 NULINE( 15: 15 ) /= '[' ) THEN
               novals = 1
               CALL GETVAL( NULINE( 25: 36 ), VALUES( 1 ) )
               IF ( FIELDS( 2 ) /= '        ' .AND.                            &
                 NULINE( 40: 40 ) /= '[' ) THEN
                  novals = 2
                  CALL GETVAL( NULINE( 50: 61 ), VALUES( 2 ) )

!  remove fields with numerical values of zero

                  IF ( VALUES( 2 ) == zero ) THEN
                     novals = 1
                  END IF
               END IF
               IF ( VALUES( 1 ) == zero ) THEN
                  IF ( novals == 2 ) THEN
                     VALUES( 1 ) = VALUES( 2 )
                     FIELDS( 1 ) = FIELDS( 2 )
                  END IF
                  novals = novals - 1
               END IF
            END IF
         ELSE

!  read the character fields 3 and 7 from the card

            field3 = NULINE( 15: 22 )
            field7 = NULINE( 25: 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

            IF ( FIELD1( 1: 1 ) == 'A' .OR.                                    &
                 FIELD1( 1: 1 ) == 'F' .OR.                                    &
                 FIELD1( 1: 1 ) == 'G' ) THEN
               IF ( field3 /= '       ' ) THEN
                  status = 72
                  IF ( out > 0 ) WRITE( out, 2720 )
                  GO TO 800
               END IF
            END IF
         END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 200, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  200 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND.                           &
           field1 /= 'M ' .AND. field1 /= 'F ' .AND.                           &
           field1 /= 'L ' ) THEN
         status = 54
         IF ( out > 0 ) WRITE( out, 2540 )
         GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
         field = field2 // 'FU'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nexnam = nexnam + 1
            IF ( nexnam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            EXNAME( nexnam ) = field2
         END IF
      ELSE

!  check to see that the parameter name has not already been used

         field = field2 // 'PF'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            IF ( field1 == 'R ' ) THEN
               nrenam = nrenam + 1
               IF ( nrenam > ninmax ) THEN
                  status = - 2
                  GO TO 700
               END IF
               RENAME( nrenam ) = field2
            ELSE
               IF ( field1 == 'M ' ) THEN
                  nminam = nminam + 1
                  IF ( nminam > ninmax ) THEN
                     status = - 2
                     GO TO 700
                  END IF
                  MINAME( nminam ) = field2
               ELSE
                  IF ( field1 == 'L ' ) THEN
                     nlonam = nlonam + 1
                     IF ( nlonam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     LONAME( nlonam ) = field2
                  ELSE
                     ninnam = ninnam + 1
                     IF ( ninnam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     INNAME( ninnam ) = field2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  indicator card is globals
!  --------------------------

  300 CONTINUE
      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR.                             &
           field1 == 'E ' ) THEN
         startp = .TRUE.

!  start a parameter assignment. check to see that the parameter has
!  been defined

         field = field2 // 'PF'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
            status = 57
            IF ( out > 0 ) WRITE( out, 2570 )
            GO TO 800
         END IF

! --------- make general parameter assignments

         IF ( field1 == 'A ' ) THEN
            IF ( loutff ) WRITE( outff, 3030 ) FIELD2(1:6), field7
            ntem = ntem + 1
            WRITE( outem, 3080 ) FIELD2(1:6), field7

! --------- make conditional parameter assignments

         ELSE   

!  check that the logical variable has been defined

            field = field3 // '  PF'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  GO TO 700
               END IF
               status = 57
               IF ( out > 0 ) WRITE( out, 2570 )
               GO TO 800
            END IF
            IF ( field1 == 'I ' ) THEN
               IF ( loutff ) WRITE( outff, 3031 ) FIELD2(1:6),                 &
                                     FIELD3(1:6), field7
               ntem = ntem + 1
               WRITE( outem, 3081 ) FIELD2(1:6),                               &
                                     FIELD3(1:6), field7
            ELSE
               IF ( loutff ) WRITE( outff, 3032 ) FIELD2(1:6),                 &
                                     FIELD3(1:6), field7
               ntem = ntem + 1
               WRITE( outem, 3082 ) FIELD2(1:6),                               &
                                     FIELD3(1:6), field7
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) == '+' .AND. startp ) THEN

! --------- continue a parameter assignment

           IF ( loutff ) WRITE( outff, 3040 ) field7
            ntem = ntem + 1
            WRITE( outem, 3040 ) field7
         ELSE
            status = 55
            IF ( out > 0 ) WRITE( out, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new element has been encountered

      IF ( field1 == 'T ' ) THEN

!  check to see if the range of a new element is to be defined

         IF ( firstl ) THEN

!  check if this is the first element

            firstl = .FALSE.
         ELSE

!  finish of the previous element, if any

            IF ( startf ) THEN
               IF ( .NOT. endoff ) THEN
                  IF ( loutff ) THEN
                     WRITE( outff, 3120 )
                     WRITE( outff, 3121 )
                  END IF
                  IF ( iad0 == 1 ) THEN
                     WRITE( outfd, 3122 )                                      &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),                  &
                        ninvar
                  ELSE
                     WRITE( outfd, 3123 )                                      &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),                  &
                        ninvar
                  END IF
                  WRITE( outfd, 3150 ) FIELDI( 12 )(1:6)
                  IF ( iad0 == 1 ) THEN
                    WRITE( outfd, 3151 ) 
                  ELSE
                    WRITE( outfd, 3152 ) 
                  END IF
                  DO 402 js = 1, ninvar
                     DO 401 is = 1, js
                        ihvar = ( js * ( js - 1 ) ) / 2 + is
                        IF ( is == js ) THEN
                           WRITE( outfd, 3163 )                                &
                              FIELDI(  3 )(1:6),                               &
                              FIELDI( 15 )(1:6), ihvar, ihvar
                        ELSE
                           WRITE( outfd, 3164 )                                &
                              FIELDI(  3 )(1:6),                               &
                              FIELDI( 15 )(1:6), ihvar, ihvar
                        END IF
  401                CONTINUE
  402             CONTINUE   
                  endoff = .TRUE.
               END IF
               IF ( loutff ) WRITE( outff, 3190 )
               WRITE( outfd, 3180 )
               WRITE( outfd, 3190 )
            ELSE
               status = 61
               IF ( out > 0 ) WRITE( out, 2610 )
               GO TO 800
            END IF
            IF ( isetty < neltyp .AND. loutff )                                &
               WRITE( outff, 3191 ) nloop
            IF ( isetty < neltyp ) WRITE( outfd, 3191 ) nloop
         END IF

!  find itype, the element type

         field = field2 // 'ET'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )

!  the element type is unknown

         IF ( ifield <= 0 ) THEN
            status = 9
            IF ( out > 0 ) WRITE( out, 2090 )
            GO TO 800
         END IF

! --------- find type of current element

         itype = INLIST( ifield )
         IF ( loutff ) WRITE( outff, 3060 ) field2
         WRITE( outfd, 3060 ) field2
         IF ( neltyp > 1 .AND. loutff ) WRITE( outff, 3061 ) itype
         IF ( neltyp > 1 ) WRITE( outfd, 3061 ) itype
         IF ( LDEFND( itype ) ) THEN
            status = 67
            IF ( out > 0 ) WRITE( out, 2670 )
            GO TO 800
         ELSE
            LDEFND( itype ) = .TRUE.
            isetty = isetty + 1
         END IF

!  find the row and column dimensions (ninv and nelv, resp.) of the
!  transformation matrix u. u is stored in vector form by columns

         is = IINV( itype ) - 1
         js = IELV( itype ) - 1
         nelv = IELV( itype + 1 ) - IELV( itype )
         ninv = IINV( itype + 1 ) - IINV( itype )
         nn = ninv * nelv
         IF ( nn > numax ) THEN
            status = - 11
            GO TO 700
         END IF

! --------- find type of current element

         IF ( nelv > ninv ) WRITE( outra, 4060 ) field2, itype

!  initialize u as the zero matrix

         DO 420 i = 1, nn
            U( i ) = zero
  420    CONTINUE
         setran = nelv > ninv

! --------- set elemental variables

         k1 = IELV( itype )
         k2 = IELV( itype + 1 ) - 1
         IF ( setran ) THEN
            IF ( iad0 == 1 ) THEN
               WRITE( outfd, 3230 ) nelv,                                      &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6),                    &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), nelv
            ELSE
               WRITE( outfd, 3231 ) nelv,                                      &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6),                    &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), nelv
            END IF
            DO 430 k = k1, k2
               ivar = k - k1 + 1
               IF ( loutff )                                                   &
               WRITE( outff, 3070 ) ENAMES( k ), FIELDI(  4 )(1:6),            &
                   FIELDI(  8 )(1:6), FIELDI( 16 )(1:6), ivar
               WRITE( outfd, 3220 ) ENAMES( k ), ivar
  430       CONTINUE
         ELSE
            IF ( iad0 == 1 ) THEN
               WRITE( outfd, 3210 ) FIELDI( 12 )(1:6), ninv,                   &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6),                    &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), ninv
            ELSE
               WRITE( outfd, 3211 ) FIELDI( 12 )(1:6), ninv,                   &
                      FIELDI(  4 )(1:6), FIELDI(  8 )(1:6),                    &
                      FIELDI( 16 )(1:6), FIELDI( 16 )(1:6), ninv
            END IF
            DO 431 k = k1, k2
               ivar = k - k1 + 1
               IF ( loutff )                                                   &
               WRITE( outff, 3070 ) ENAMES( k ), FIELDI(  4 )(1:6),            &
                   FIELDI(  8 )(1:6), FIELDI( 16 )(1:6), ivar
               WRITE( outfd, 3220 ) ENAMES( k ), ivar
  431       CONTINUE
         END IF

!  find the number of internal variables and the required size of
!  the lower triangular portion of the Hessian matrix

         k1 = IINV( itype )
         k2 = IINV( itype + 1 ) - 1
         ninvar = k2 - k1 + 1
         startp = .FALSE.
         startf = .FALSE.
         endoff = .TRUE.
         startv = .FALSE.
      ELSE
         IF ( field1 == 'R ' ) THEN

!  the range transformation matrix u is now defined, entry by entry
!  determine which internal variable is given in field2

            DO 440 i = 1, ninv
               IF ( field2 == INAMES( is + i ) ) GO TO 450
  440       CONTINUE

!  the internal variable name is unrecognised

            status = 65
            IF ( out > 0 ) WRITE( out, 2650 )
            GO TO 800

!  the internal variable is the i-th in the list

  450       CONTINUE

!  determine which elemental variable(s) occur in fields

            IF ( novals > 0 ) THEN
               DO 480 k = 1, novals
                  DO 460 j = 1, nelv
                     IF ( FIELDS( k ) == ENAMES( js + j ) ) GO TO 470
  460             CONTINUE

!  the elemental variable name is unrecognised

                  status = 66
                  IF ( out > 0 ) WRITE( out, 2660 )
                  GO TO 800

!  the elemental variable is the j-th in the list

  470             CONTINUE

!  insert the value of the new nonzero into u

                  U( ninv * ( j - 1 ) + i ) = VALUES( k )
  480          CONTINUE
            END IF
         ELSE
            IF ( FIELD1( 1: 1 ) == 'A' .OR. FIELD1( 1: 1 )                     &
                 == 'I' .OR. FIELD1( 1: 1 ) == 'E' ) THEN

!  finish the function assignment

               IF ( startf .AND. .NOT. endoff ) THEN
                  IF ( loutff ) THEN
                     WRITE( outff, 3120 )
                     WRITE( outff, 3121 )
                  END IF
                  IF ( iad0 == 1 ) THEN
                     WRITE( outfd, 3122 )                                      &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),                  &
                        ninvar
                  ELSE
                     WRITE( outfd, 3123 )                                      &
                        FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),                  &
                        FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),                  &
                        ninvar
                  END IF
                  WRITE( outfd, 3150 ) FIELDI( 12 )(1:6)
                  IF ( iad0 == 1 ) THEN
                    WRITE( outfd, 3151 ) 
                  ELSE
                    WRITE( outfd, 3152 ) 
                  END IF
                  DO 482 js = 1, ninvar
                     DO 481 is = 1, js
                        ihvar = ( js * ( js - 1 ) ) / 2 + is
                        IF ( is == js ) THEN
                           WRITE( outfd, 3163 )                                &
                              FIELDI(  3 )(1:6),                               &
                              FIELDI( 15 )(1:6), ihvar, ihvar
                        ELSE
                           WRITE( outfd, 3164 )                                &
                              FIELDI(  3 )(1:6),                               &
                              FIELDI( 15 )(1:6), ihvar, ihvar
                        END IF
  481                CONTINUE
  482             CONTINUE   
                  endoff = .TRUE.
               END IF
               IF ( .NOT. startf ) THEN

!  start a parameter assignment

                  IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                     startp = .TRUE.

!  set up the transformations for the element

                     IF ( setran ) THEN
                         CALL OUTRN2( nelv, ninv, U, outff, outfd,             &
                                      outra, ENAMES( js + 1 ),                 &
                                      INAMES( is + 1 ), single, ad0 )
                         setran = .FALSE.
                     END IF

! --------- set elemental parameters

                     k1 = IEPA( itype )
                     DO 483 k = k1, IEPA( itype + 1 ) - 1
                        ivar = k - k1 + 1
                        IF ( loutff )                                          &
                        WRITE( outff, 3071 ) EPNAME( k ),                      &
                        FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), ivar
                        WRITE( outfd, 3071 ) EPNAME( k ),                      &
                            FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), ivar
  483                CONTINUE

!  include the global parameters

                     IF ( .NOT. startv ) THEN
                        REWIND( outem )
                        DO 484 i = 1, ntem
                           READ( outem, 1000 ) ctem
                           WRITE( outfd, 1000 ) ctem
  484                   CONTINUE   
                        startv = .TRUE.
                     END IF

!  check to see that the parameter has been defined

                     field = field2 // 'PF'
                     CALL HASH_search(LENGTH, 12, field, KEY, ITABLE, IFIELD)
                     IF ( ifield <= 0 ) THEN
                        status = 58
                        IF ( out > 0 ) WRITE( out, 2580 )
                        GO TO 800
                     END IF

! --------- make element-specific parameter assignments

                     IF ( FIELD1( 1: 1 ) == 'A' ) THEN
                        IF ( .NOT. startf ) THEN
                           IF ( loutff )                                       &
                           WRITE( outff, 3080 ) FIELD2(1:6), field7
                           WRITE( outfd, 3080 ) FIELD2(1:6), field7
                        ELSE
                           IF ( loutff )                                       &
                           WRITE( outff, 3083 ) FIELD2(1:6), field7
                           WRITE( outfd, 3083 ) FIELD2(1:6), field7
                        END IF

! --------- make conditional parameter assignments

                     ELSE   

!  check that the logical variable has been defined

                        field = field3 // '  PF'
                        CALL HASH_search( length, 12, field, KEY, ITABLE,      &
                                          ifield )
                        IF ( ifield <= 0 ) THEN
                           IF ( ifree == 0 ) THEN
                              status = - 1
                              GO TO 700
                           END IF
                           status = 58
                           IF ( out > 0 ) WRITE( out, 2580 )
                           GO TO 800
                        END IF
                        IF ( FIELD1( 1: 1 ) == 'I' ) THEN
                           IF ( .NOT. startf ) THEN
                              IF ( loutff )                                    &
                              WRITE( outff, 3081 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                              WRITE( outfd, 3081 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                           ELSE
                              IF ( loutff )                                    &
                              WRITE( outff, 3084 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                              WRITE( outfd, 3084 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                           END IF
                        ELSE
                        IF ( .NOT. startf ) THEN
                              IF ( loutff )                                    &
                              WRITE( outff, 3082 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                              WRITE( outfd, 3082 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                           ELSE
                              IF ( loutff )                                    &
                              WRITE( outff, 3085 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                              WRITE( outfd, 3085 ) FIELD2(1:6),                &
                                                 FIELD3(1:6), field7
                           END IF
                        END IF   
                     END IF
                  ELSE
                     IF ( FIELD1( 2: 2 ) == '+' ) THEN
                        IF ( startp ) THEN

! --------- continuation of a parameter assignment

                           IF ( .NOT. startf ) THEN
                              IF ( loutff ) WRITE( outff, 3090 ) field7
                              WRITE( outfd, 3090 ) field7
                           ELSE
                              IF ( loutff ) WRITE( outff, 3091 ) field7
                              WRITE( outfd, 3091 ) field7
                           END IF
                        ELSE
                           status = 56
                           IF ( out > 0 ) WRITE( out, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               END IF
            ELSE
               startp = .FALSE.
               IF ( FIELD1( 1: 1 ) == 'F' ) THEN

!  set the function value

                  IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                     startf = .TRUE.
                     endoff = .FALSE.

!  set up the transformations for the element

                     IF ( setran ) THEN
                         CALL OUTRN2( nelv, ninv, U, outff, outfd,             &
                                      outra, ENAMES( js + 1 ),                 &
                                      INAMES( is + 1 ), single, ad0 )
                         setran = .FALSE.
                     END IF

! --------- set elemental parameters

                     k1 = IEPA( itype )
                     DO 485 k = k1, IEPA( itype + 1 ) - 1
                        ivar = k - k1 + 1
                        IF ( loutff )                                          &
                        WRITE( outff, 3071 ) EPNAME( k ),                      &
                        FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), ivar
                        WRITE( outfd, 3071 ) EPNAME( k ),                      &
                            FIELDI( 18 )(1:6), FIELDI( 20 )(1:6), ivar
  485                CONTINUE

!  include the global parameters

                     IF ( .NOT. startv ) THEN
                        REWIND( outem )
                        DO 486 i = 1, ntem
                           READ( outem, 1000 ) ctem
                           WRITE( outfd, 1000 ) ctem
  486                      CONTINUE   
                        startv = .TRUE.
                     END IF

! --------- start f

                     IF ( loutff )                                             &
                     WRITE( outff, 3100 ) FIELDI( 12 )(1:6),                   &
                     FIELDI( 3 )(1:6), FIELDI( 13 )(1:6), field7
                     WRITE( outfd, 3101 ) field7
                  ELSE
                     IF ( FIELD1( 2: 2 ) == '+' ) THEN
                        IF ( startf ) THEN

! --------- continuation of f

                           IF ( loutff ) WRITE( outff, 3110 ) field7
                           WRITE( outfd, 3110 ) field7
                        ELSE
                           status = 56
                           IF ( out > 0 ) WRITE( out, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               ELSE IF ( FIELD1( 1: 1 ) == 'G' .OR.                            &
                         FIELD1( 1: 1 ) == 'H' ) THEN
                  IF ( startf .AND. .NOT. endoff ) THEN
                     IF ( loutff ) THEN
                        WRITE( outff, 3120 )
                        WRITE( outff, 3121 )
                     END IF
                     IF ( iad0 == 1 ) THEN
                        WRITE( outfd, 3122 )                                   &
                           FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),               &
                           FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),               &
                           FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),               &
                           ninvar
                     ELSE
                        WRITE( outfd, 3123 )                                   &
                           FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),               &
                           FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),               &
                           FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),               &
                           ninvar
                     END IF
                     WRITE( outfd, 3150 ) FIELDI( 12 )(1:6)
                     IF ( iad0 == 1 ) THEN
                       WRITE( outfd, 3151 ) 
                     ELSE
                       WRITE( outfd, 3152 ) 
                     END IF
                     DO 512 js = 1, ninvar
                        DO 511 is = 1, js
                           ihvar = ( js * ( js - 1 ) ) / 2 + is
                           IF ( is == js ) THEN
                              WRITE( outfd, 3163 )                             &
                                 FIELDI(  3 )(1:6),                            &
                                 FIELDI( 15 )(1:6), ihvar, ihvar
                           ELSE
                              WRITE( outfd, 3164 )                             &
                                 FIELDI(  3 )(1:6),                            &
                                 FIELDI( 15 )(1:6), ihvar, ihvar
                           END IF
  511                   CONTINUE
  512                CONTINUE   
                     endoff = .TRUE.
                  END IF
               ELSE
                  status = 56
                  IF ( out > 0 ) WRITE( out, 2560 )
                  GO TO 800
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE 

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
         status = 52
         IF ( out > 0 ) WRITE( out, 2520 )
         RETURN
      END IF
!     if ( neltyp > 0 ) go to 930
      qprod = .FALSE.
      DO 591 itype = 1, MIN( 2, neltyp ) 
         IF ( ETYPES( itype ) /= cqsqr .AND.                                   &
              ETYPES( itype ) /= cqprod ) GO TO 930
         IF ( ETYPES( itype ) == cqprod ) qprod = .TRUE.
  591 CONTINUE   
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE 

!  write a dummy elfuns routine

      IF ( iauto == 1 ) THEN
         IF ( single ) THEN
            aorb = 'FORWARD_SINGLE '
         ELSE
            aorb = 'FORWARD_DOUBLE '
         END IF
      ELSE
         IF ( single ) THEN
            aorb = 'BACKWARD_SINGLE'
         ELSE
            aorb = 'BACKWARD_DOUBLE'
         END IF
      END IF
      IF ( iad0 == 1 ) THEN
         ad0 = 'AD01'
      ELSE
         ad0 = 'AD02'
      END IF
      IF ( neltyp == 0 ) THEN
         IF ( single ) THEN
            IF ( loutff ) WRITE( outff, 3003 ) FIELDI( 1 )(1:6),               &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),                               &
           FIELDI( 18 )(1:6), ( FIELDI(  i )(1:6), i = 5, 10 ),                &
           FIELDI( 19 )(1:6), FIELDI( 11 )(1:6),                               &
         ( FIELDI(  i )(1:6), i = 22, 31 ),                                    &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                               &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                               &
         ( FIELDI(  i )(1:6), i = 22, 32 ),                                    &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                               &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                               &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                               &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                               &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                               &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                               &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                               &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                               &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                               &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), pname, TRIM( version )
            WRITE( outfd, 3007 ) FIELDI( 33 )(1:6),                            &
           FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),                               &
           FIELDI( 18 )(1:6), ( FIELDI(  i )(1:6), i = 5, 10 ),                &
           FIELDI( 19 )(1:6), FIELDI( 11 )(1:6),                               &
         ( FIELDI(  i )(1:6), i = 22, 31 ),                                    &
           FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), ad0, aorb,                    &
           FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                               &
         ( FIELDI(  i )(1:6), i = 22, 32 ),                                    &
           FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                               &
           FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                               &
           FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                               &
           FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                               &
           FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                               &
           FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                               &
           FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                               &
           FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                               &
           FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                               &
           FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), pname, TRIM( version )
         ELSE   
            IF ( loutff ) WRITE( outff, 3002 ) FIELDI( 1 )(1:6),               &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),                                &
          FIELDI( 18 )(1:6), ( FIELDI(  i )(1:6), i = 5, 10 ),                 &
          FIELDI( 19 )(1:6), FIELDI( 11 )(1:6),                                &
        ( FIELDI(  i )(1:6), i = 22, 31 ),                                     &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                                &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                &
        ( FIELDI(  i )(1:6), i = 22, 32 ),                                     &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                &
          FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), pname, TRIM( version )
            WRITE( outfd, 3006 ) FIELDI( 33 )(1:6),                            &
          FIELDI(  3 )(1:6), FIELDI(  4 )(1:6),                                &
          FIELDI( 18 )(1:6), ( FIELDI(  i )(1:6), i = 5, 10 ),                 &
          FIELDI( 19 )(1:6), FIELDI( 11 )(1:6),                                &
        ( FIELDI(  i )(1:6), i = 22, 31 ),                                     &
          FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), ad0, aorb,                     &
          FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                &
        ( FIELDI(  i )(1:6), i = 22, 32 ),                                     &
          FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                &
          FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                &
          FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                &
          FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                &
          FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                &
          FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                &
          FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                &
          FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                &
          FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                &
          FIELDI( 18 )(1:6), FIELDI( 31 )(1:6), pname, TRIM( version )
         END IF
         IF ( loutff ) WRITE( outff, 3009 ) FIELDI( 32 )(1:6)
         WRITE( outfd, 3009 ) FIELDI( 32 )(1:6)
         IF ( loutff ) WRITE( outff, 3201 )
!        if ( iad0 == 2 ) then
!          write( ioutfd, 3203 )
!        else
            WRITE( outfd, 3201 )
!        end if
      ELSE
         IF ( single ) THEN
           IF ( loutff ) WRITE( outff, 3001 ) FIELDI( 1 )(1:6),                &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),                &
       ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                    &
        FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                  &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6),                &
       FIELDI( 12 )(1:6), ( FIELDI(  i )(1:6), i = 22, 32 ),                   &
       FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                   &
       FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                   &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                   &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                   &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                   &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                   &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                   &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                   &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                   &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                   &
       pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),           &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                   &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            WRITE( outfd, 3001 ) FIELDI( 33 )(1:6),                            &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),                &
       ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                    &
       FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                   &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                                   &
       FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                   &
       ( FIELDI(  i )(1:6), i = 22, 32 ),                                      &
       FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                   &
       FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                   &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                   &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                   &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                   &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                   &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                   &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                   &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                   &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                   &
       pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),           &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                   &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( qprod ) THEN
               IF ( loutff ) WRITE( outff, 3019 ) 'X     ', 'Y     '
               WRITE( outfd, 3019 ) 'X     ', 'Y     '
            ELSE
               IF ( loutff ) WRITE( outff, 3019 ) 'X     '
               WRITE( outfd, 3019 ) 'X     '
            END IF
         ELSE   
            IF ( loutff ) WRITE( outff, 3000 ) FIELDI( 1 )(1:6),               &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),                &
       ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                    &
       FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                   &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6),                                   &
       FIELDI(  5 )(1:6), FIELDI( 12 )(1:6),                                   &
       ( FIELDI(  i )(1:6), i = 22, 32 ), FIELDI(  6 )(1:6),                   &
       FIELDI( 22 )(1:6), FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                   &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                   &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                   &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                   &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                   &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                   &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                   &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                   &
       pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),           &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                   &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6),                                   &
       FIELDI( 21 )(1:6)
            WRITE( outfd, 3000 ) FIELDI( 33 )(1:6),                            &
       FIELDI(  3 )(1:6), FIELDI(  4 )(1:6), FIELDI( 18 )(1:6),                &
       ( FIELDI(  i )(1:6), i = 5, 10 ), FIELDI( 19 )(1:6),                    &
       FIELDI( 11 )(1:6), ( FIELDI(  i )(1:6), i = 22, 31 ),                   &
       FIELDI( 12 )(1:6), FIELDI( 32 )(1:6), FIELDI(  5 )(1:6),                &
       FIELDI( 12 )(1:6), ( FIELDI(  i )(1:6), i = 22, 32 ),                   &
       FIELDI(  6 )(1:6), FIELDI( 22 )(1:6),                                   &
       FIELDI(  7 )(1:6), FIELDI( 23 )(1:6),                                   &
       FIELDI(  8 )(1:6), FIELDI( 24 )(1:6),                                   &
       FIELDI(  9 )(1:6), FIELDI( 25 )(1:6),                                   &
       FIELDI( 10 )(1:6), FIELDI( 26 )(1:6),                                   &
       FIELDI( 19 )(1:6), FIELDI( 27 )(1:6),                                   &
       FIELDI( 11 )(1:6), FIELDI( 28 )(1:6),                                   &
       FIELDI(  3 )(1:6), FIELDI( 29 )(1:6),                                   &
       FIELDI(  4 )(1:6), FIELDI( 30 )(1:6),                                   &
       FIELDI( 18 )(1:6), FIELDI( 31 )(1:6),                                   &
       pname, TRIM( version ), FIELDI( 13 )(1:6), FIELDI( 14 )(1:6),           &
       FIELDI( 15 )(1:6), FIELDI( 16 )(1:6),                                   &
       FIELDI( 17 )(1:6), FIELDI( 20 )(1:6), FIELDI( 21 )(1:6)
            IF ( qprod ) THEN
               IF ( loutff ) WRITE( outff, 3020 ) 'X     ', 'Y     '
               WRITE( outfd, 3020 ) 'X     ', 'Y     '
            ELSE
               IF ( loutff ) WRITE( outff, 3020 ) 'X     '
               WRITE( outfd, 3020 ) 'X     '
            END IF
         END IF
         IF ( loutff ) WRITE( outff, 3009 ) FIELDI( 32 )(1:6)
         WRITE( outfd, 3009 ) FIELDI( 32 )(1:6)
         IF ( loutff ) WRITE( outff, 3050 ) nloop,                             &
                FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),                          &
                FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),                          &
                FIELDI( 21 )(1:6),                                             &
                FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),                          &
                FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),                          &
                FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),                          &
                FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),                          &
                FIELDI( 13 )(1:6),                                             &
                FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),                          &
                FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
         WRITE( outfd, 3050 ) nloop,                                           &
                FIELDI( 21 )(1:6), FIELDI(  5 )(1:6),                          &
                FIELDI( 13 )(1:6), FIELDI( 11 )(1:6),                          &
                FIELDI( 21 )(1:6),                                             &
                FIELDI( 16 )(1:6), FIELDI(  7 )(1:6),                          &
                FIELDI( 13 )(1:6), FIELDI( 17 )(1:6),                          &
                FIELDI(  9 )(1:6), FIELDI( 13 )(1:6),                          &
                FIELDI( 20 )(1:6), FIELDI( 19 )(1:6),                          &
                FIELDI( 13 )(1:6),                                             &
                FIELDI( 12 )(1:6), FIELDI( 15 )(1:6),                          &
                FIELDI( 10 )(1:6), FIELDI( 13 )(1:6)
         IF ( neltyp > 1 ) THEN
            IF ( loutff ) WRITE( outff, 3051 )                                 &
               FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),                           &
               FIELDI( 13 )(1:6), ( i, i = 1, neltyp )
            WRITE( outfd, 3051 )                                               &
               FIELDI( 14 )(1:6), FIELDI(  6 )(1:6),                           &
               FIELDI( 13 )(1:6), ( i, i = 1, neltyp )
           IF ( loutff ) WRITE( outff, 3052 ) FIELDI( 14 )(1:6)
            WRITE( outfd, 3052 ) FIELDI( 14 )(1:6)
         END IF

!  make sure that quadratic Hessian terms are included

         DO 640 itype = 1, MIN( 2, neltyp )

!  diagonal term

            IF ( ETYPES( itype ) == cqsqr ) THEN
               IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
               WRITE( outfd, 3060 ) ETYPES( itype )
               IF ( neltyp > 1 ) WRITE( outff, 3061 ) itype
               IF ( neltyp > 1 ) WRITE( outfd, 3061 ) itype
               IF ( single ) THEN
                 IF ( loutff ) WRITE( outff, 3055 ) 'E'
                 WRITE( outfd, 3053 ) 'E', 'E'
               ELSE
                 IF ( loutff ) WRITE( outff, 3055 ) 'D'
                 WRITE( outfd, 3053 ) 'D', 'D'
               END IF
               LDEFND( itype ) = .TRUE.
               isetty = isetty + 1
               IF ( isetty < neltyp ) THEN
                  IF ( loutff ) WRITE( outff, 3191 ) nloop
                  WRITE( outfd, 3191 ) nloop
               END IF

!  off-diagonal term

            ELSE IF ( ETYPES( itype ) == cqprod ) THEN
               IF ( loutff ) WRITE( outff, 3060 ) ETYPES( itype )
               WRITE( outfd, 3060 ) ETYPES( itype )
               IF ( neltyp > 1 ) THEN
                  IF ( loutff ) WRITE( outff, 3061 ) itype
                  WRITE( outfd, 3061 ) itype
               END IF
               IF ( single ) THEN
                 IF ( loutff ) WRITE( outff, 3056 )
                 WRITE( outfd, 3054 ) 'E', 'E', 'E'
               ELSE
                 IF ( loutff ) WRITE( outff, 3056 )
                 WRITE( outfd, 3054 ) 'D', 'D', 'D'
               END IF
               LDEFND( itype ) = .TRUE.
               isetty = isetty + 1
               IF ( isetty < neltyp ) THEN
                  IF ( loutff ) WRITE( outff, 3191 ) nloop
                  WRITE( outfd, 3191 ) nloop
               END IF
            END IF
  640    CONTINUE   
         IF ( loutff ) WRITE( outff, 3200 ) nloop
         IF ( iad0 == 2 ) THEN
           WRITE( outfd, 3202 ) nloop
         ELSE
           WRITE( outfd, 3200 ) nloop
         END IF
      END IF

!  write a dummy range routine

      IF ( single ) THEN
         WRITE( outra, 4003 ) pname, TRIM( version )
      ELSE
         WRITE( outra, 4002 ) pname, TRIM( version )
      END IF
      WRITE( outra, 4080 )
      WRITE( outra, 4090 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 ) INCRSE( - status )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE
      IF ( .NOT. firstl ) THEN

!  finish of the previous element, if any

         IF ( startf ) THEN
            IF ( .NOT. endoff ) THEN
               IF ( loutff ) THEN
                  WRITE( outff, 3120 )
                  WRITE( outff, 3121 )
               END IF
               IF ( iad0 == 1 ) THEN
                  WRITE( outfd, 3122 )                                         &
                     FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),                     &
                     FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),                     &
                     FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),                     &
                     ninvar
               ELSE
                  WRITE( outfd, 3123 )                                         &
                     FIELDI( 12 )(1:6), FIELDI( 3  )(1:6),                     &
                     FIELDI( 13 )(1:6), FIELDI( 3  )(1:6),                     &
                     FIELDI( 17 )(1:6), FIELDI( 17 )(1:6),                     &
                     ninvar
               END IF
               WRITE( outfd, 3150 ) FIELDI( 12 )(1:6)
               IF ( iad0 == 1 ) THEN
                 WRITE( outfd, 3151 ) 
               ELSE
                 WRITE( outfd, 3152 ) 
               END IF
               DO 902 js = 1, ninvar
                  DO 901 is = 1, js
                     ihvar = ( js * ( js - 1 ) ) / 2 + is
                     IF ( is == js ) THEN
                        WRITE( outfd, 3163 )                                   &
                           FIELDI(  3 )(1:6),                                  &
                           FIELDI( 15 )(1:6), ihvar, ihvar
                     ELSE
                        WRITE( outfd, 3164 )                                   &
                           FIELDI(  3 )(1:6),                                  &
                           FIELDI( 15 )(1:6), ihvar, ihvar
                     END IF
  901             CONTINUE
  902          CONTINUE   
               endoff = .TRUE.
            END IF
           IF ( loutff ) WRITE( outff, 3190 )
            WRITE( outfd, 3180 )
            WRITE( outfd, 3190 )
         ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
         END IF
         IF ( isetty < neltyp .AND. loutff )                                   &
            WRITE( outff, 3191 ) nloop
         IF ( isetty < neltyp ) WRITE( outfd, 3191 ) nloop
      END IF

! ---------- successful run. wind up output

      status = 0
      IF ( loutff ) WRITE( outff, 3200 ) nloop
      IF ( iad0 == 2 ) THEN
        WRITE( outfd, 3202 ) nloop
      ELSE
        WRITE( outfd, 3200 ) nloop
      END IF
      IF ( nointe ) WRITE( outra, 4070 )
      IF ( neltyp == 0 ) WRITE( outra, 4080 )
      WRITE( outra, 4090 )

!   check that all element types have been defined

  930 CONTINUE
      DO 940 itype = 1, neltyp
         IF ( .NOT. LDEFND( itype ) ) THEN
            status = 68
            IF ( out > 0 ) WRITE( out, 2680 ) ETYPES( itype )
         END IF
  940 CONTINUE
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_elfun_ad - insufficient space.',             &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAKE_elfun_ad - warning.',                        &
              ' First card not elements. ', /, '    A dummy',                  &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_elfun_ad - indicator card not recognised ' )
 2090 FORMAT( ' ** Exit from MAKE_elfun_ad - element type not recognised ' )
 2510 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_elfun_ad - data file incomplete.',           &
              ' No ENDATA card ' )
 2540 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' unrecognised field 1 in INDIVIDUALS section' )
 2570 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_elfun_ad - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKE_elfun_ad - function not set '  )
 2650 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' internal variable not recognised ' )
 2660 FORMAT( ' ** Exit from MAKE_elfun_ad -',                                 &
              ' elemental variable not recognised ' )
 2670 FORMAT( ' ** Exit from MAKE_elfun_ad - element type already defined ' )
 2680 FORMAT( ' ** Exit from MAKE_elfun_ad - warning, element type ', A10,     &
              ' undefined ' )
 2720 FORMAT( ' ** Exit from MAKE_elfun_ad - field 3 not blank on',            &
              ' A, F or G card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 2( A6, ', ' ), A6, ' )', /,               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', A6, '(', A6, ')', /,                                 &
        '      DOUBLE PRECISION ', A6, '(', A6, '), ',                         &
                                   A6, '(', A6, '), ',                         &
                                   A6, '(', A6, ')', /,                        &
        'C', /, 'C  Problem name : ', A8, /,                                   &
        'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,                  &
        '      INTEGER ', 5( A6, ', ' ), A6, /,                                &
        '      INTEGER ', A6 )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 5( A6, ', ' ), /,                         &
        '     *                   ', 2( A6, ', ' ), A6, ' )', /,               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', 5( A6, ', ' ),  A6, /,                               &
        '      INTEGER ', A6, /,                                               &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', 2( A6, '(', A6, '), ' ),                             &
                             A6, '(', A6, ')', /,                              &
        '      INTEGER ', A6, '(', A6, ')', /,                                 &
        '      REAL             ', A6, '(', A6, '), ',                         &
                                   A6, '(', A6, '), ',                         &
                                   A6, '(', A6, ')', /,                        &
        'C', /, 'C  Problem name : ', A8, /,                                   &
        'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,                  &
        '      INTEGER ', 5( A6, ', ' ), A6, /,                                &
        '      INTEGER ', A6 )
 3002 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3003 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3004 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
       '      USE HSL_', A4, '_', A15, /, '      INTEGER ',                    &
        5( A6, ', ' ),  A6, /, '      INTEGER ', 5( A6, ', ' ),  A6, /,        &
       '      INTEGER ', A6, /, '      INTEGER ', 2( A6, '(',                  &
       A6, '), ' ), A6, '(', A6, ')', /,                                       &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
       '      INTEGER ', 5( A6, ', ' ), A6, /, '      INTEGER ', A6, /,        &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      DOUBLE PRECISION, POINTER :: H_result( : ) ', /,          &
              '      DOUBLE PRECISION X_int( ', i6, ') ' )
 3005 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
       '      USE HSL_', A4, '_', A15, /, '      INTEGER ', 5( A6,             &
       ', ' ),  A6, /, '      INTEGER ', 5( A6, ', ' ),  A6, /,                &
       '      INTEGER ', A6, /, '      INTEGER ', 2( A6, '(', A6,              &
       '), ' ), A6, '(', A6, ')', /,                                           &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
       '      INTEGER ', 5( A6, ', ' ), A6, /, '      INTEGER ', A6, /,        &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      REAL, POINTER :: H_result( : ) ', /,                      &
              '      REAL X_int( ', i6, ') ' )
 3006 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      DOUBLE PRECISION ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3007 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 2( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', 5( A6, ', ' ),  A6, /,                         &
              '      INTEGER ', A6, /,                                         &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', 2( A6, '(', A6, '), ' ),                       &
                                   A6, '(', A6, ')', /,                        &
              '      INTEGER ', A6, '(', A6, ')', /,                           &
              '      REAL             ', A6, '(', A6, '), ',                   &
                                         A6, '(', A6, '), ',                   &
                                         A6, '(', A6, ')', /,                  &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3008 FORMAT( '      X_AD01_int = AD01_UNDEFINED' )
!3008 format( '      call ad01_undefine( x_ad01_int ) ' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, :, 4( ', ', A6, : ) ) )
!3011 format( '      nullify( data_ad02 )', /,
!    *        '      call ad02_initialize(ifflag-1, x_value(1),', /,
!    *        '     *          xvalue(ielvar(istaev(1)+1)),', /,
!    *        '     *                      data_ad02, 0)' )
 3011 FORMAT( '      CALL AD02_INITIALIZE_DATA(DATA_AD02, ERROR_AD02)' )
!3015 format( '       call ', a4, '_undefine( ', a6,
!    *        ', data_ad02 )' )
 3016 FORMAT( '      CALL ', A4, '_UNDEFINE( ', A6,                            &
              ', DATA_AD02 )' )
 3017 FORMAT( ( '      TYPE (', A4, '_REAL) :: ', A6 ) )
 3018 FORMAT( ( '      TYPE (AD01_REAL) :: ', A6,                              &
                ' = AD01_UNDEFINED' ) )
 3019 FORMAT( ( '      REAL             ', A6, :, 4( ', ', A6, : ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, :, 4( ', ', A6, : ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, :, 4( ', ', A6, : ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, :, 4( ', ', A6, : ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3024 FORMAT( '      TYPE (AD01_REAL) :: F_value = AD01_UNDEFINED', /,         &
              '      TYPE (AD01_REAL) :: X_value(', i6, '),',                  &
                   ' X_AD01_int(',I6, ')' )
 3025 FORMAT( '      INTEGER :: ERROR_AD02', /,                                &
              '      TYPE (AD02_REAL) :: F_value', /,                          &
              '      TYPE (AD02_REAL) :: X_value(', i6, '),',                  &
                   ' X_AD02_int(',I6, ')', /,                                  &
              '      TYPE (AD02_DATA), POINTER :: DATA_AD02' )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ') ', /,                      &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       ', A6, ' = ', A6, '(', A6, ') - 1', /,                   &
              '       IF ( ', A6, ' == 3 ) ',                                  &
                      A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3053 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3054 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= X * Y', /,                              &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= Y', /,                           &
              '        FUVALS(IGSTRT+     2)= X', /,                           &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3055 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * X * X', /,              &
              '       ELSE', /,                                                &
              '        WRITE(6,*) '' impossible value IFFLAG = '', ',          &
              'IFFLAG, '' in ELFUNF '' ', /,                                   &
              '       END IF' )
 3056 FORMAT( '       X = XVALUE(IELVAR(ILSTRT+     1))', /,                   &
              '       Y = XVALUE(IELVAR(ILSTRT+     2))', /,                   &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= X * Y', /,                              &
              '       ELSE', /,                                                &
              '        WRITE(6,*) '' impossible value IFFLAG = '', ',          &
              'IFFLAG, '' in ELFUNF '' ', /,                                   &
              '       END IF' )
 3057 FORMAT( '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     1))', /,            &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= 5.0', A1, '-1 * ', A6,                  &
              ' * ', A6, /,                                                    &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= ', A6, /,                        &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 1.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3058 FORMAT( '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     1))', /,            &
              '       ', A6, ' = XVALUE(IELVAR(ILSTRT+     2))', /,            &
              '       IF ( IFFLAG == 1 ) THEN', /,                             &
              '        FUVALS(IELEMN)= ', A6, ' * ', A6, /,                    &
              '       ELSE', /,                                                &
              '        FUVALS(IGSTRT+     1)= ', A6, /,                        &
              '        FUVALS(IGSTRT+     2)= ', A6, /,                        &
              '        IF ( IFFLAG == 3 ) THEN', /,                            &
              '         FUVALS(IHSTRT+     1)= 0.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     2)= 1.0', A1, '+0', /,              &
              '         FUVALS(IHSTRT+     3)= 0.0', A1, '+0', /,              &
              '        END IF', /,                                             &
              '       END IF' )
 3060 FORMAT( 'C', /, 'C  ELEMENT TYPE : ', A10, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE' )
 3070 FORMAT( '       ', A6, ' = ', A6, '(', A6, '(', A6, '+', i6, '))')
 3071 FORMAT( '       ', A6, ' = ', A6, '(', A6, '+', i6, ')')
 3080 FORMAT( '       ', A6, ' = ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        ', A6, '(', A6, ')= ', A41 )
 3101 FORMAT( '       F_value = ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3121 FORMAT( '        WRITE(6,*) '' impossible value IFFLAG = '', ',          &
              'IFFLAG, '' in ELFUNF '' ' )
 3122 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        CALL AD01_VALUE(F_value, ', A6, '(', A6, '))', /,       &
              '       ELSE',/,                                                 &
              '        CALL AD01_GRAD(F_value, ', A6, '(', A6, '+1:',          &
              A6, '+', i6, '))' )
 3123 FORMAT( '       IF ( ', A6, ' == 1 ) THEN', /,                           &
              '        CALL AD02_VALUE(F_value, ', A6, '(', A6, '),',          &
              ' ERROR_AD02)', /,                                               &
              '       ELSE',/,                                                 &
              '        CALL AD02_GRAD(F_value, ', A6, '(', A6, '+1:',          &
              A6, '+', i6, '),', /,                                            &
              '     *                 ERROR_AD02)' )
 3150 FORMAT( '        IF ( ', A6, ' == 3 ) THEN' )
 3151 FORMAT( '         CALL AD01_DERIVS(F_value, 2,',                         &
              ' H_index, H_result)' )
 3152 FORMAT( '         CALL AD02_DERIVS(F_value, 2,',                         &
              ' H_index, H_result, ERROR_AD02)' )
 3163 FORMAT( '         ', A6, '(', A6, '+', i6, ')=2.0*H_result(',            &
              i6, ')')
 3164 FORMAT( '         ', A6, '(', A6, '+', i6, ')=H_result(', i6, ')')
 3180 FORMAT( '        END IF' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3200 FORMAT( i5,  ' CONTINUE', /, '      RETURN', /,                          &
              '      END' )
 3201 FORMAT( '      RETURN', /,                                               &
              '      END' )
 3202 FORMAT( i5,  ' CONTINUE', /,                                             &
              '      CALL AD02_FINALIZE_DATA(DATA_AD02, ERROR_AD02)', /,       &
              '      RETURN', /,   '      END' )
 3210 FORMAT( '       CALL AD01_INITIALIZE(',A6,' - 1, X_value(:', i6,         &
                     '),', /, '     *', 22X,                                   &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')), 0) ' )
 3211 FORMAT( '       CALL AD02_INITIALIZE_COMP(',A6,' - 1, X_value(:',        &
                     i6, '),', /, '     *', 22X,                               &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')),', /,              &
              '     *                      DATA_AD02, ERROR_AD02, 0)' )
 3220 FORMAT( '       ', A6, ' = X_value(', i6, ')' )
 3230 FORMAT( '       CALL AD01_INITIALIZE(0, X_value(:', i6,                  &
                     '),', /, '     *', 22X,                                   &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')), 0) ' )
 3231 FORMAT( '       CALL AD02_INITIALIZE_COMP(0, X_value(:', i6,             &
                     '),', /, '     *', 22X,                                   &
                     A6,'(',A6,'(',A6,'+1:',A6,'+', i6, ')),', /,              &
              '     *                      DATA_AD02, ERROR_AD02, 0)' )
 4000 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4001 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C', /,                                                          &
              '      INTEGER I' )
 4002 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      DOUBLE PRECISION W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4003 FORMAT( '      SUBROUTINE RANGE( IELEMN, TRANSP, W1,',                   &
              ' W2, NELVAR, ninvar,', /,                                       &
              '     *                  itype, LW1, LW2 )', /,                  &
              '      INTEGER IELEMN, NELVAR, ninvar, itype,',                  &
              ' LW1, LW2', /,                                                  &
              '      LOGICAL TRANSP', /,                                       &
              '      REAL             W1( LW1 ), W2( LW2 )', /,                &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              'C  TRANSP = .FALSE. <=> W2 = U * W1', /,                        &
              'C  TRANSP = .TRUE.  <=> W2 = U(transpose) * W1', /,             &
              'C' )
 4040 FORMAT( '      GO TO (', 8( i5, :, ',' ), /,                             &
            ( '     *       ', 8( i5, :, ',' ) ) )
 4050 FORMAT( '     *        ', 48X, '), ITYPE' )
 4060 FORMAT( 'C', /, 'C  Element type : ', A10, /, 'C', /,                    &
              i5, ' CONTINUE', /,                                              &
              '      IF ( TRANSP ) THEN' )
 4070 FORMAT( 'C', /,                                                          &
              'C  Elements without internal variables.', /,                    &
              'C', /,                                                          &
              '99998 CONTINUE', /,                                             &
              '      DO 99999 i = 1, NELVAR', /,                               &
              '         W2( i ) = W1( i )', /,                                 &
              '99999 CONTINUE', /,                                             &
              '      RETURN' )
 4080 FORMAT( '      RETURN' )
 4090 FORMAT( '      END' )

!  end of subroutine MAKE_elfun_ad

      END SUBROUTINE MAKE_elfun_ad

!-*-*-*-  S I F D E C O D E   M A K E _ g r o u p   S U B R O U T I N E  -*-*-*-

      SUBROUTINE MAKE_group( input, out, outgr, status, ngrtyp,                &
                             ngrmax, nlmax, ninmax, pname, ANAMES,             &
                             RENAME, INNAME, LONAME, MINAME, EXNAME, GTYPES,   &
                             LDEFND, GPNAME, IGPA, ngpmax, debug, length,      &
                             ITABLE, KEY, INLIST, single, nuline, gotlin,      &
                             print_level )
      INTEGER :: input, out, outgr, status, length
      INTEGER :: nlmax, ngrtyp, ninmax, ngpmax, ngrmax
      INTEGER :: print_level
      LOGICAL :: gotlin, debug, single
      CHARACTER ( len = 8 ) :: pname
      CHARACTER ( len = max_record_length ) :: nuline
      INTEGER :: ITABLE( length ), IGPA( ngrmax )
      INTEGER :: INLIST( length )
      LOGICAL :: LDEFND( nlmax  )
      CHARACTER ( len = 12 ) :: KEY   ( length )
      CHARACTER ( len = 10 ) :: RENAME( ninmax ), INNAME( ninmax )
      CHARACTER ( len = 10 ) :: GPNAME( ngpmax ), EXNAME( ninmax )
      CHARACTER ( len = 10 ) :: LONAME( ninmax ), GTYPES( ngrmax )
      CHARACTER ( len = 10 ) :: ANAMES( ngrmax ), MINAME( ninmax )

!  -------------------------------------------------------------------
!  make a group function evaluation subroutine from a gps group 
!  function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  GROUPS       problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  set function and derivative values for each group-type
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in 
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of inform indicate that insufficient
!  array space has been allowed, as follows:

!    inform = - 1  when length not large enough
!    inform = - 2  when max( ninnam, nrenam, nlonam, nenam, nminam )
!                  > ninmax
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, ifield, ifree, itype, intype, ivar, k1, k2
      INTEGER :: ninnam, nloop, nrenam, nminam, npname
      INTEGER :: nexnam, nincrs, lineno, iires, k, ilines, nlines
      INTEGER :: mblank, mfixed, mfree, mname, mtemp, mglob
      INTEGER :: mindiv, mendat, maxnul, nlonam
      LOGICAL :: defnam, endpar, endgen, firstg
      LOGICAL :: setf, setg, seth, startp, fixed
      LOGICAL :: endf
      PARAMETER        ( iires = 20 )
      PARAMETER        ( nincrs = 2 )
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 6 ) :: INCRSE( nincrs )
      CHARACTER ( len = 8 ) :: field2, field3, FIELDI( iires )
      CHARACTER ( len = 12 ) :: field
      CHARACTER ( len = 41 ) :: field7

!  parameter definitions

      CHARACTER ( len = max_record_length ) :: blnkln
      PARAMETER        ( mblank =  1, mfixed =  2, mfree = 3  )
      PARAMETER        ( mname =  4, mtemp =  5 )
      PARAMETER        ( mglob =  6, mindiv =  7, mendat =  8 )
      INTEGER :: LENIND( mendat )
      CHARACTER ( len = 12 ) :: INDIC8( mendat ), header
      PARAMETER        ( maxnul = 20 )
      CHARACTER ( len = 65 ) :: NULINA( maxnul )

!  data declarations

      DATA INCRSE / 'LENGTH', 'NINMAX' /
      DATA INDIC8( mblank ) / '            ' /, LENIND( mblank ) / 0  / 
      DATA INDIC8( mfixed ) / 'FIXED FORMAT' /, LENIND( mfixed ) / 12 /
      DATA INDIC8( mfree  ) / 'FREE FORMAT ' /, LENIND( mfree  ) / 11 /
      DATA INDIC8( mname  ) / 'GROUPS      ' /, LENIND( mname  ) / 6  /
      DATA INDIC8( mtemp  ) / 'TEMPORARIES ' /, LENIND( mtemp  ) / 11 / 
      DATA INDIC8( mglob  ) / 'GLOBALS     ' /, LENIND( mglob  ) / 7  /
      DATA INDIC8( mindiv ) / 'INDIVIDUALS ' /, LENIND( mindiv ) / 11 / 
      DATA INDIC8( mendat ) / 'ENDATA      ' /, LENIND( mendat ) / 6  /
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

      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnam = 0
      nrenam = 0
      nlonam = 0
      nexnam = 0
      nminam = 0
      lineno = 0
      intype = 1
      ilines = 0
      nlines = 0

!  set initial values for logical variables

      defnam = .FALSE.
      endpar = .FALSE.
      seth = .FALSE.
      startp = .FALSE.
      endgen = .FALSE.
      firstg = .TRUE.
      fixed = .TRUE.

!  find which group-types are nontrivial

      DO 20 itype = 1, ngrtyp
         LDEFND( itype ) = .FALSE.
   20 CONTINUE

!  insert the list of group-type arguments into the dictionary

      DO 30 itype = 1, ngrtyp
         field = ANAMES( itype ) // 'PG'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = ANAMES( itype )
         END IF
   30 CONTINUE

!  include the names of the group parameters used
!  in this dictionary

      IF ( ngrtyp > 0 ) THEN
         npname = IGPA( ngrtyp + 1 ) - 1
         DO 40 i = 1, npname
            field = GPNAME( i ) // 'PG'
            CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
            IF ( ifree <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  GO TO 700
               END IF
            ELSE
               nrenam = nrenam + 1
               IF ( nrenam > ninmax ) THEN
                  status = - 2
                  GO TO 700
               END IF
               RENAME( nrenam ) = GPNAME( i )
            END IF
   40    CONTINUE
      END IF

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( I: i ) = ' '
      END DO

!  read next line

  100 CONTINUE
      IF ( ilines + 1 > nlines ) THEN

!  read next line from the input file

         lineno = lineno + 1
         IF ( fixed ) THEN
            IF ( gotlin ) THEN
               gotlin = .FALSE.
            ELSE
               nuline = blnkln
               READ ( input, 1000, END = 590, ERR = 590 ) nuline
            END IF
            IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
         ELSE
            IF ( gotlin ) THEN
               gotlin = .FALSE.
            ELSE
               nuline = blnkln
               READ ( input, 1010, END = 590, ERR = 590 ) nuline
            END IF
            IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

            CALL FREE_format( nuline, max_record_length, mendat, INDIC8,       &
                              LENIND, NULINA, maxnul, nlines, .FALSE.,         &
                              status, out )
            IF ( status > 0 ) GO TO 800
            IF ( nlines > 0 ) THEN

!  if there are non-blank lines on the free format card, read the first

               ilines = 1
               nuline = blnkln
               nuline = NULINA( ilines )
               IF ( out > 0 .AND. debug ) WRITE( out, 2980 )                   &
                    lineno, ilines, nuline
            ELSE

!  there are only blank lines on the free format card

               GO TO 100
            END IF
         END IF
      ELSE

!  read next line from the last encountered free format card

         ilines = ilines + 1
         nuline = blnkln
         nuline = NULINA( ilines )
         IF ( out > 0 .AND. debug ) WRITE( out, 2980 )                         &
              lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1: 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1: 1 ) /= ' ' ) THEN

!  ignore comment cards

         IF ( NULINE( 1: 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

         IF ( header == INDIC8( mfixed ) ) THEN
            fixed = .TRUE.
            GO TO 100
         END IF

!  check if we have entered free-format input

         IF ( header == INDIC8( mfree ) ) THEN
            fixed = .FALSE.
            GO TO 100
         END IF

!  check that the first encountered indicator card is the groups card

         IF ( .NOT. defnam  ) THEN
            IF ( header /= INDIC8( mname ) ) THEN
               IF ( ngrtyp > 0 ) GO TO 930
               IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
               gotlin = .TRUE.
               GO TO 600
            ELSE

!  indicator card is groups
!  -------------------------

               IF ( pname  /= NULINE( 15: 22 ) ) THEN
                  status = 51
                  IF ( out > 0 ) WRITE( out, 2510 )
                  GO TO 800
               ELSE
                  defnam = .TRUE.
                  GO TO 100
               END IF
            END IF
         END IF

!  an indicator card has been found

         DO 110 i = intype, mendat
            IF ( header == INDIC8( i ) ) THEN
               intype = i
               GO TO 120
            END IF
  110    CONTINUE

!  the indicator card is not recognised

         status = 2
         IF ( out > 0 ) WRITE( out, 2020 )
         GO TO 800
  120    CONTINUE

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

         IF ( intype >= mglob .AND. .NOT. endpar ) THEN
            endpar = .TRUE.
            nloop = ngrtyp + 1

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

            DO 130 i = 1, iires
               field = FIELDI( i ) // '  PG'
               CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
               IF ( ifree <= 0 ) THEN
                  IF ( ifree == 0 ) THEN
                     status = - 1
                     GO TO 700
                  END IF
                  status = 59
                  IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- set up subroutine call and reserved parameter declarations

            IF ( single ) THEN
               WRITE( outgr, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ),                               &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                 &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),        &
                          FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                 &
                          FIELDI(  8 )( 1 : 6 ),                               &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),        &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),        &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),        &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),        &
                          pname, TRIM( version )
            ELSE
               WRITE( outgr, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ),                               &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                 &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),        &
                          FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                 &
                          FIELDI(  8 )( 1 : 6 ),                               &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),        &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),        &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),        &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),        &
                          pname, TRIM( version )
            END IF
            IF ( ngrtyp == 0 ) THEN
              WRITE( outgr, 3009 ) FIELDI( 20 )( 1 : 6 )
              GO TO 910
            END IF
            WRITE( outgr, 3002 ) FIELDI(  9 )( 1 : 6 ),                        &
                          FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),        &
                          FIELDI( 14 )( 1 : 6 )

! --------- insert integer declarations

            IF ( ninnam > 0 )                                                  &
               WRITE( outgr, 3010 ) ( INNAME( i ), i = 1, ninnam )

! --------- insert real declarations

            IF ( nrenam > 0 ) THEN
               IF ( single ) THEN
                  WRITE( outgr, 3019 ) ( RENAME( i ), i = 1, nrenam )
               ELSE
                  WRITE( outgr, 3020 ) ( RENAME( i ), i = 1, nrenam )
               END IF
            END IF

! --------- insert logical declarations

            IF ( nlonam > 0 )                                                  &
               WRITE( outgr, 3023 ) ( LONAME( i ), i = 1, nlonam )

! --------- insert intrinsic declarations

            IF ( nminam > 0 )                                                  &
               WRITE( outgr, 3021 ) ( MINAME( i ), i = 1, nminam )

! --------- insert external declarations

            IF ( nexnam > 0 )                                                  &
               WRITE( outgr, 3022 ) ( EXNAME( i ), i = 1, nexnam )
            WRITE( outgr, 3009 ) FIELDI( 20 )( 1 : 6 )
         END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

         IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
            endgen = .TRUE.

! --------- start loop over groups

            WRITE( outgr, 3050 ) nloop,  FIELDI( 14 )( 1 : 6 ),                &
                   FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ),                                      &
                   FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), nloop,                               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 )
            IF ( ngrtyp > 1 ) THEN
               WRITE( outgr, 3051 ) ( i, i = 1, ngrtyp )
               WRITE( outgr, 3052 ) FIELDI(  9 )( 1 : 6 )
            END IF
         END IF

!  indicator card is endata
!  -------------------------

         IF ( intype == mendat ) GO TO 900
         GO TO 100
      ELSE

!  check that the first non comment card is the groups indicator card

         IF ( .NOT. defnam  ) THEN
            IF ( ngrtyp > 0 ) GO TO 930
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
            gotlin = .TRUE.
            GO TO 600
         END IF

!  a data card has been found
!  read the character fields 1, 2, 3 and 7 from the card

         field1 = NULINE(  2:  3 )
         field2 = NULINE(  5: 12 )
         field3 = NULINE( 15: 22 )
         field7 = NULINE( 25: 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

            IF ( FIELD1( 1: 1 ) == 'A' .OR.                                    &
                 FIELD1( 1: 1 ) == 'F' .OR.                                    &
                 FIELD1( 1: 1 ) == 'G' .OR.                                    &
                 FIELD1( 1: 1 ) == 'H' ) THEN
               IF ( ( FIELD1( 1: 1 ) /= 'A' .AND. field2 /=                    &
                    '       ' ) .OR.FIELD3 /= '       ' ) THEN
                  status = 73
                  IF ( out > 0 ) WRITE( out, 2730 )
                  GO TO 800
               END IF
            END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 290, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  290 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND.                           &
           field1 /= 'M ' .AND. field1 /= 'F ' .AND.                           &
           field1 /= 'L' ) THEN
         status = 54
         IF ( out > 0 ) WRITE( out, 2540 )
         GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
         field = field2 // '  GU'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nexnam = nexnam + 1
            IF ( nexnam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            EXNAME( nexnam ) = field2
         END IF
      ELSE

!  check to see that the parameter name has not already been used

         field = field2 // '  PG'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            IF ( field1 == 'R ' ) THEN
               nrenam = nrenam + 1
               IF ( nrenam > ninmax ) THEN
                  status = - 2
                  GO TO 700
               END IF
               RENAME( nrenam ) = field2
            ELSE
               IF ( field1 == 'M ' ) THEN
                  nminam = nminam + 1
                  IF ( nminam > ninmax ) THEN
                     status = - 2
                     GO TO 700
                  END IF
                  MINAME( nminam ) = field2
               ELSE
                  IF ( field1 == 'L ' ) THEN
                     nlonam = nlonam + 1
                     IF ( nlonam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     LONAME( nlonam ) = field2
                  ELSE
                     ninnam = ninnam + 1
                     IF ( ninnam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     INNAME( ninnam ) = field2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  indicator card is global
!  -------------------------

  300 CONTINUE
      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR.                             &
           field1 == 'E ' ) THEN
         startp = .TRUE.

!  start a parameter assignment. check to see that the parameter has
!  been defined

         field = field2 // '  PG'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
            status = 57
            IF ( out > 0 ) WRITE( out, 2570 )
            GO TO 800
         END IF

! --------- make general parameter assignments

         IF ( field1 == 'A ' ) THEN
            WRITE( outgr, 3030 ) FIELD2( 1 : 6 ), field7

! --------- make conditional parameter assignments

         ELSE   

!  check that the logical variable has been defined

            field = field3 // '  PG'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  GO TO 700
               END IF
               status = 57
               IF ( out > 0 ) WRITE( out, 2570 )
               GO TO 800
            END IF
            IF ( field1 == 'I ' ) THEN
               WRITE( outgr, 3031 ) FIELD2( 1 : 6 ),                           &
                                     FIELD3( 1 : 6 ), field7
            ELSE
               WRITE( outgr, 3032 ) FIELD2( 1 : 6 ),                           &
                                     FIELD3( 1 : 6 ), field7
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) == '+' .AND. startp ) THEN

! --------- continue a parameter assignment

            WRITE( outgr, 3040 ) field7
         ELSE
            status = 55
            IF ( out > 0 ) WRITE( out, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new group has been encountered

      IF ( field1 == 'T ' ) THEN
         IF ( firstg ) THEN

!  check if this is the first group-type

            firstg = .FALSE.
         ELSE

!  finish of the previous group, if any

            IF ( .NOT. seth ) THEN
               status = 63
               IF ( out > 0 ) WRITE( out, 2630 )
               GO TO 800
            END IF
            IF ( .NOT. setg ) THEN
               status = 62
               IF ( out > 0 ) WRITE( out, 2620 )
               GO TO 800
            END IF

! ---------- wind up f and g

            IF ( setf ) THEN
               WRITE( outgr, 3190 )
            ELSE
               status = 61
               IF ( out > 0 ) WRITE( out, 2610 )
               GO TO 800
            END IF
            IF ( itype < ngrtyp ) WRITE( outgr, 3191 ) nloop
         END IF

!  find itype, the group-type

         field = field2 // '  GT'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )

!  the group-type is unknown

         IF ( ifield <= 0 ) THEN
            status = 19
            IF ( out > 0 ) WRITE( out, 2190 )
            GO TO 800
         END IF

! --------- find type of current group

         itype = INLIST( ifield )
         WRITE( outgr, 3060 ) field2
         IF ( ngrtyp > 1 ) WRITE( outgr, 3061 ) itype
         WRITE( outgr, 3062 ) ANAMES( itype )( 1 : 6 ),                        &
                   FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )

! --------- set group parameters

         k1 = IGPA( itype )
         k2 = IGPA( itype + 1 ) - 1
         DO 435 k = k1, k2
            ivar = k - k1 + 1
            WRITE( outgr, 3063 ) GPNAME( k ), FIELDI( 11 )( 1 : 6 ),           &
                FIELDI( 13 )( 1 : 6 ), ivar
  435    CONTINUE
         IF ( LDEFND( itype ) ) THEN
            status = 64
            IF ( out > 0 ) WRITE( out, 2640 )
            GO TO 800
         ELSE
            LDEFND( itype ) = .TRUE.
         END IF

!  initialize logicals which determine whether the data has been
!  input in the correct order

         startp = .FALSE.
         setf = .FALSE.
         setg = .FALSE.
         seth = .FALSE.
         endf = .TRUE.
      ELSE
         IF ( FIELD1( 1: 1 ) == 'A' .OR. FIELD1( 1: 1 )                        &
              == 'I' .OR. FIELD1( 1: 1 ) == 'E' ) THEN
            IF ( setf ) THEN
               IF ( .NOT. endf ) THEN
                  WRITE( outgr, 3120 )
                  endf = .TRUE.
               END IF
            END IF

!  start a parameter assignment. check to see that the parameter has
!  been defined

            IF ( FIELD1( 2: 2 ) == ' ' ) THEN
               startp = .TRUE.
               field = field2 // '  PG'
               CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
               IF ( ifield <= 0 ) THEN
                  status = 57
                  IF ( out > 0 ) WRITE( out, 2570 )
                  GO TO 800
               END IF

! --------- make group-specific parameter assignments

               IF ( FIELD1( 1: 1 ) == 'A' ) THEN
                  IF ( .NOT. setf ) THEN
                     WRITE( outgr, 3080 ) FIELD2( 1 : 6 ), field7
                  ELSE
                     WRITE( outgr, 3083 ) FIELD2( 1 : 6 ), field7
                  END IF

! --------- make conditional parameter assignments

               ELSE   

!  check that the logical variable has been defined

                  field = field3 // '  PG'
                  CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
                  IF ( ifield <= 0 ) THEN
                     IF ( ifree == 0 ) THEN
                        status = - 1
                        GO TO 700
                     END IF
                     status = 58
                     IF ( out > 0 ) WRITE( out, 2580 )
                     GO TO 800
                  END IF
                  IF ( FIELD1( 1: 1 ) == 'I' ) THEN
                     IF ( .NOT. setf ) THEN
                        WRITE( outgr, 3081 ) FIELD2( 1 : 6 ),                  &
                                              FIELD3( 1 : 6 ), field7
                     ELSE
                        WRITE( outgr, 3084 ) FIELD2( 1 : 6 ),                  &
                                              FIELD3( 1 : 6 ), field7
                     END IF
                  ELSE
                     IF ( .NOT. setf ) THEN
                        WRITE( outgr, 3082 ) FIELD2( 1 : 6 ),                  &
                                              FIELD3( 1 : 6 ), field7
                     ELSE
                        WRITE( outgr, 3085 ) FIELD2( 1 : 6 ),                  &
                                              FIELD3( 1 : 6 ), field7
                     END IF
                  END IF   
               END IF
            ELSE
               IF ( FIELD1( 2: 2 ) == '+' ) THEN
                  IF ( startp ) THEN

! --------- continuation of a parameter assignment

                     IF ( .NOT. setf ) THEN
                        WRITE( outgr, 3090 ) field7
                     ELSE
                        WRITE( outgr, 3091 ) field7
                     END IF
                  ELSE
                     status = 56
                     IF ( out > 0 ) WRITE( out, 2560 )
                     GO TO 800
                  END IF
               END IF
            END IF
         ELSE
            startp = .FALSE.
            IF ( FIELD1( 1: 1 ) == 'F' ) THEN

!  set the function value

               IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                  setf = .TRUE.
                  endf = .FALSE.

! --------- start g

                  WRITE( outgr, 3100 ) FIELDI(  8 )( 1 : 6 ),                  &
                  FIELDI( 2 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ), field7
               ELSE
                  IF ( FIELD1( 2: 2 ) == '+' ) THEN
                     IF ( setf ) THEN

! --------- continuation of g

                        WRITE( outgr, 3110 ) field7
                     ELSE
                        status = 56
                        IF ( out > 0 ) WRITE( out, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            ELSE
               IF ( FIELD1( 1: 1 ) == 'G' ) THEN

!  no function value has been specified

                  IF ( .NOT. setf ) THEN
                     status = 61
                     IF ( out > 0 ) WRITE( out, 2610 )
                     GO TO 800
                  END IF

!  set the first derivative value

                  IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                     IF ( .NOT. setg ) THEN
                        setg = .TRUE.

! --------- start gdash

                        IF ( .NOT. endf ) THEN
                           WRITE( outgr, 3120 )
                           endf = .TRUE.
                        END IF
                     END IF
                     WRITE( outgr, 3130 ) FIELDI( 2 )( 1 : 6 ),                &
                            FIELDI( 10 )( 1 : 6 ), 2, field7
                  ELSE
                     IF ( FIELD1( 2: 2 ) == '+' ) THEN
                        IF ( setg ) THEN

! --------- continuation of gdash

                           WRITE( outgr, 3140 ) field7
                        ELSE
                           status = 56
                           IF ( out > 0 ) WRITE( out, 2560 )
                           GO TO 800
                        END IF
                     END IF
                  END IF
               ELSE
                  IF ( FIELD1( 1: 1 ) == 'H' ) THEN

!  set the second derivative value

                     IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                        IF ( .NOT. seth ) THEN

!  the first derivative has not been set

                           IF ( .NOT. setg ) THEN
                              status = 62
                              IF ( out > 0 ) WRITE( out, 2620 )
                              GO TO 800
                           END IF
                           seth = .TRUE.
                        END IF

! --------- set g2dash

                        WRITE( outgr, 3130 ) FIELDI( 2 )( 1 : 6 ),             &
                               FIELDI( 10 )( 1 : 6 ), 3, field7
                     ELSE
                        IF ( FIELD1( 2: 2 ) == '+' ) THEN
                           IF ( seth ) THEN

! --------- continuation of g2dash

                              WRITE( outgr, 3140 ) field7
                           ELSE
                              status = 56
                              IF ( out > 0 ) WRITE( out, 2560 )
                              GO TO 800
                           END IF
                        END IF
                     END IF
                  ELSE
                     status = 56
                     IF ( out > 0 ) WRITE( out, 2560 )
                     GO TO 800
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE 

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
         status = 52
         IF ( out > 0 ) WRITE( out, 2520 )
         RETURN
      END IF
      IF ( ngrtyp > 0 ) GO TO 930
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE 

!  write a dummy groups routine

      IF ( single ) THEN
         WRITE( outgr, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      ELSE
         WRITE( outgr, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      END IF
      WRITE( outgr, 3009 ) FIELDI( 20 )( 1 : 6 )
      WRITE( outgr, 3210 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 ) INCRSE( - status )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE
      IF ( .NOT. firstg ) THEN

!  finish of the previous group, if any

         IF ( .NOT. seth ) THEN
            status = 63
            IF ( out > 0 ) WRITE( out, 2630 )
            GO TO 800
         END IF
         IF ( .NOT. setg ) THEN
            status = 62
            IF ( out > 0 ) WRITE( out, 2620 )
            GO TO 800
         END IF

! ---------- wind up f and g

         IF ( setf ) THEN
            WRITE( outgr, 3190 )
         ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
         END IF
         IF ( itype < ngrtyp ) WRITE( outgr, 3191 ) nloop
      END IF

! ---------- end do loop

      WRITE( outgr, 3200 ) nloop
  910 CONTINUE

! ---------- successful run. wind up output

      WRITE( outgr, 3210 )
      status = 0

!   check that all element types have been defined

  930 CONTINUE
      DO 940 itype = 1, ngrtyp
         IF ( .NOT. LDEFND( itype ) ) THEN
            status = 53
            IF ( out > 0 ) WRITE( out, 2530 ) GTYPES( itype )
         END IF
  940 CONTINUE
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_group - insufficient space.',                &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAKE_group - warning.',                           &
              ' First card not groups. ', /, '    A dummy',                    &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_group - indicator card not recognised ' )
 2190 FORMAT( ' ** Exit from MAKE_group - group type not recognised:',         &
              ' name is ', A10 )
 2510 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' Name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' data file incomplete. No ENDATA card ' )
 2530 FORMAT( ' ** Exit from MAKE_group - warning, group type ', A8,           &
              ' undefined ' )
 2540 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' unrecognised field 1 in INDIVIDUAL section' )
 2570 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_group - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKE_group - function not set '  )
 2620 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' one or more first derivatives not set ' )
 2630 FORMAT( ' ** Exit from MAKE_group -',                                    &
              ' one or more second derivatives not set ' )
 2640 FORMAT( ' ** Exit from MAKE_group - group type already defined ' )
 2730 FORMAT( ' ** Exit from MAKE_group - field 2 or 3 not blank on',          &
              ' A, F, G or H card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      REAL             ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
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
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       IF ( ', A6, ' == 0 ) GO TO ', i5, /,                     &
              '       ', A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3060 FORMAT( 'C', /, 'C  Group type : ', A8, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE ' )
 3062 FORMAT( '       ', A6, '= ', A6, '(', A6, ')' )
 3063 FORMAT( '       ', A6, '= ', A6, '(', A6, '+', i6, ')' )
 3080 FORMAT( '       ', A6, '= ', A41 )
 3081 FORMAT( '       IF (', A6, ') ', A6, ' = ', A41 )
 3082 FORMAT( '       IF (.NOT.', A6, ') ', A6, '=', A41 )
 3083 FORMAT( '        ', A6, ' = ', A41 )
 3084 FORMAT( '        IF (', A6, ') ', A6, ' = ', A41 )
 3085 FORMAT( '        IF (.NOT.', A6, ')', A6, '=', A41 )
 3090 FORMAT( '     *          ', A41 )
 3091 FORMAT( '     *           ', A41 )
 3100 FORMAT( '       IF ( .NOT. ', A6, ' ) THEN', /,                          &
              '        ', A6, '(', A6, ',1)= ', A41 )
 3110 FORMAT( '     *                  ', A41 )
 3120 FORMAT( '       ELSE' )
 3130 FORMAT( '        ', A6, '(', A6, ',', i1, ')= ', A41 )
 3140 FORMAT( '     *                         ', A41 )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3200 FORMAT( i5,  ' CONTINUE' )
 3210 FORMAT( '      RETURN', /,  '      END' )

!  end of subroutine MAKE_group

      END SUBROUTINE MAKE_group

!-*-  S I F D E C O D E   M A K E _ g r o u p _ a d    S U B R O U T I N E  -*-

      SUBROUTINE MAKE_group_ad( input, out, outgf, outgd, outem, status,       &
                                 ngrtyp, ngrmax, nlmax, ninmax, pname,         &
                                 ANAMES, RENAME, INNAME, LONAME, MINAME,       &
                                 EXNAME, GTYPES, LDEFND, GPNAME, IGPA, ngpmax, &
                                 debug, length, ITABLE, KEY, INLIST, single,   &
                                 nuline, gotlin, iauto, iad0, print_level )
      INTEGER :: input, out, outgf, status, length
      INTEGER :: nlmax, ngrtyp, ninmax, ngpmax, ngrmax
      INTEGER :: print_level, outgd, outem, iauto, iad0
      LOGICAL :: gotlin, debug, single
      CHARACTER ( len = 8 ) ::  pname
      CHARACTER ( len = max_record_length ) :: nuline
      INTEGER :: ITABLE( length ), IGPA( ngrmax )
      INTEGER :: INLIST( length )
      LOGICAL :: LDEFND( nlmax  )
      CHARACTER ( len = 12 ) :: KEY   ( length )
      CHARACTER ( len = 10 ) :: RENAME( ninmax ), INNAME( ninmax )
      CHARACTER ( len = 10 ) :: GPNAME( ngpmax ), EXNAME( ninmax )
      CHARACTER ( len = 10 ) :: LONAME( ninmax ), GTYPES( ngrmax )
      CHARACTER ( len = 10 ) :: ANAMES( ngrmax ), MINAME( ninmax )

!  --------------------------------------------------------------------
!  make a group function evaluation subroutine, suitable for 
!  automatic differentiation from a gps function data file

!  function indicator cards
!  -------------------------

!  definition   purpose
!  ----------   --------
!  GROUPS       problem name
!  TEMPORARIES  names of additional parameters used in function defs
!  GLOBALS      general parameter assignments
!  INDIVIDUALS  set function and derivative values for each group-type
!  ENDATA       end of input data

!  data card description
!  ----------------------

!  see 'The SIF reference report', Chapter 7 in 
!       A. R. Conn, N. I. M. Gould and Ph. L. Toint,
!       LANCELOT A Fortran Package for Large-Scale Nonlinear Optimization
!       (RElease A), Springer Series in Computational Mathematics 17,
!       Springer Verlag 1992

!  see also http://www.cuter.rl.ac.uk/sifdec/Doc/sif.pdf
!  and      http://www.numerical.rl.ac.uk/lancelot/sif/sifhtml.html

!  returns with negative values of inform indicate that insufficient
!  array space has been allowed, as follows:

!    inform = - 1  when length not large enough
!    inform = - 2  when max( ninnam, nrenam, nlonam, nenam, nminam )
!                  > ninmax
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, ifield, ifree, itype, intype, ivar, k1, k2
      INTEGER :: ninnam, nloop, nrenam, nminam, npname, nrenm1
      INTEGER :: nexnam, nincrs, lineno, iires, k, ilines, nlines
      INTEGER :: mblank, mfixed, mfree, mname, mtemp, mglob, ntem
      INTEGER :: mindiv, mendat, maxnul, nlonam, nrenm2
!     INTEGER :: ngtnam
      LOGICAL :: defnam, endpar, endgen, firstg
      LOGICAL :: setf, startp, fixed, startv
      LOGICAL :: endf, loutgf
      PARAMETER        ( iires = 21 )
      PARAMETER        ( nincrs = 2 )
      CHARACTER ( len = 2 ) :: field1
      CHARACTER ( len = 4 ) :: ad0
      CHARACTER ( len = 6 ) :: INCRSE( nincrs )
      CHARACTER ( len = 8 ) :: field2, field3, FIELDI( iires )
      CHARACTER ( len = 10 ) :: ctemp
      CHARACTER ( len = 12 ) :: field
      CHARACTER ( len = 15 ) :: aorb
      CHARACTER ( len = 41 ) :: field7
      CHARACTER ( len = 72 ) :: ctem

!  parameter definitions

      CHARACTER ( len = max_record_length ) :: blnkln
      PARAMETER        ( mblank =  1, mfixed =  2, mfree = 3  )
      PARAMETER        ( mname =  4, mtemp =  5 )
      PARAMETER        ( mglob =  6, mindiv =  7, mendat =  8 )
      INTEGER :: LENIND( mendat )
      CHARACTER ( len = 12 ) :: INDIC8( mendat ), header
      PARAMETER        ( maxnul = 20 )
      CHARACTER ( len = 65 ) :: NULINA( maxnul )

!  data declarations

      DATA INCRSE / 'LENGTH', 'NINMAX' /
      DATA INDIC8( mblank ) / '            ' /, LENIND( mblank ) / 0  / 
      DATA INDIC8( mfixed ) / 'FIXED FORMAT' /, LENIND( mfixed ) / 12 /
      DATA INDIC8( mfree  ) / 'FREE FORMAT ' /, LENIND( mfree  ) / 11 /
      DATA INDIC8( mname  ) / 'GROUPS      ' /, LENIND( mname  ) / 6  /
      DATA INDIC8( mtemp  ) / 'TEMPORARIES ' /, LENIND( mtemp  ) / 11 / 
      DATA INDIC8( mglob  ) / 'GLOBALS     ' /, LENIND( mglob  ) / 7  /
      DATA INDIC8( mindiv ) / 'INDIVIDUALS ' /, LENIND( mindiv ) / 11 / 
      DATA INDIC8( mendat ) / 'ENDATA      ' /, LENIND( mendat ) / 6  /
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
!     data fieldi( 21 ) / 'groupd  ' /
      IF ( out > 0 ) WRITE( out, 2900 )

!  set initial values for integer variables

      ninnam = 0
      nrenam = 0
      nlonam = 0
      nexnam = 0
      nminam = 0
      lineno = 0
      intype = 1
      ilines = 0
      nlines = 0
      ntem = 0
      loutgf = outgf > 0

!  set initial values for logical variables

      defnam = .FALSE.
      endpar = .FALSE.
      startp = .FALSE.
      endgen = .FALSE.
      firstg = .TRUE.
      fixed = .TRUE.

!  find which group-types are nontrivial

      DO 20 itype = 1, ngrtyp
         LDEFND( itype ) = .FALSE.
   20 CONTINUE

!  insert the list of group-type arguments into the dictionary

      DO 30 itype = 1, ngrtyp
         field = ANAMES( itype ) // 'PG'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nrenam = nrenam + 1
            IF ( nrenam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            RENAME( nrenam ) = ANAMES( itype )
         END IF
   30 CONTINUE
!     ngtnam = nrenam

!  include the names of the group parameters used
!  in this dictionary

      IF ( ngrtyp > 0 ) THEN
         npname = IGPA( ngrtyp + 1 ) - 1
         DO 40 i = 1, npname
            field = GPNAME( i ) // 'PG'
            CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
            IF ( ifree <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  GO TO 700
               END IF
            ELSE
               nrenam = nrenam + 1
               IF ( nrenam > ninmax ) THEN
                  status = - 2
                  GO TO 700
               END IF
               RENAME( nrenam ) = GPNAME( i )
            END IF
   40    CONTINUE
      END IF

!  set a blank line

      DO i = 1, max_record_length
        BLNKLN( I: i ) = ' '
      END DO

!  read next line

  100 CONTINUE
      IF ( ilines + 1 > nlines ) THEN

!  read next line from the input file

         lineno = lineno + 1
         IF ( fixed ) THEN
            IF ( gotlin ) THEN
               gotlin = .FALSE.
            ELSE
               nuline = blnkln
               READ ( input, 1000, END = 590, ERR = 590 ) nuline
            END IF
            IF ( out > 0 .AND. debug ) WRITE( out, 2990 ) lineno, nuline
         ELSE
            IF ( gotlin ) THEN
               gotlin = .FALSE.
            ELSE
               nuline = blnkln
               READ ( input, 1010, END = 590, ERR = 590 ) nuline
            END IF
            IF ( out > 0 .AND. debug ) WRITE( out, 2970 ) lineno, nuline

!  if the card is in free format, translate it into fixed format

            CALL FREE_format( nuline, max_record_length, mendat, INDIC8,       &
                              LENIND, NULINA, maxnul, nlines, .FALSE.,         &
                              status, out )
            IF ( status > 0 ) GO TO 800
            IF ( nlines > 0 ) THEN

!  if there are non-blank lines on the free format card, read the first

               ilines = 1
               nuline = blnkln
               nuline = NULINA( ilines )
               IF ( out > 0 .AND. debug ) WRITE( out, 2980 )                   &
                    lineno, ilines, nuline
            ELSE

!  there are only blank lines on the free format card

               GO TO 100
            END IF
         END IF
      ELSE

!  read next line from the last encountered free format card

         ilines = ilines + 1
         nuline = blnkln
         nuline = NULINA( ilines )
         IF ( out > 0 .AND. debug ) WRITE( out, 2980 )                         &
              lineno, ilines, nuline
      END IF

!  consider the header part of the card

      header = NULINE( 1: 12 )

!  ignore blank lines

      IF ( header == INDIC8( mblank ) ) GO TO 100
      IF ( NULINE( 1: 1 ) /= ' ' ) THEN

!  ignore comment cards

         IF ( NULINE( 1: 1 ) == '*' ) GO TO 100

!  check if we have entered fixed-format input

         IF ( header == INDIC8( mfixed ) ) THEN
            fixed = .TRUE.
            GO TO 100
         END IF

!  check if we have entered free-format input

         IF ( header == INDIC8( mfree ) ) THEN
            fixed = .FALSE.
            GO TO 100
         END IF

!  check that the first encountered indicator card is the groups card

         IF ( .NOT. defnam  ) THEN
            IF ( header /= INDIC8( mname ) ) THEN
               IF ( ngrtyp > 0 ) GO TO 930
               IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010)
               gotlin = .TRUE.
               GO TO 600
            ELSE

!  indicator card is groups
!  -------------------------

               IF ( pname  /= NULINE( 15: 22 ) ) THEN
                  status = 51
                  IF ( out > 0 ) WRITE( out, 2510 )
                  GO TO 800
               ELSE
                  defnam = .TRUE.
                  GO TO 100
               END IF
            END IF
         END IF

!  an indicator card has been found

         DO 110 i = intype, mendat
            IF ( header == INDIC8( i ) ) THEN
               intype = i
               GO TO 120
            END IF
  110    CONTINUE

!  the indicator card is not recognised

         status = 2
         IF ( out > 0 ) WRITE( out, 2020 )
         GO TO 800
  120    CONTINUE

!  the parameter values have been completed. write out the
!  first part of the generated subroutine

         IF ( intype >= mglob .AND. .NOT. endpar ) THEN
            endpar = .TRUE.
            nloop = ngrtyp + 1

!  insert the list of reserved integer/real/logical variables into
!  the dictionary

            DO 130 i = 1, iires
               field = FIELDI( i ) // '  PG'
               CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
               IF ( ifree <= 0 ) THEN
                  IF ( ifree == 0 ) THEN
                     status = - 1
                     GO TO 700
                  END IF
                  status = 59
                  IF ( out > 0 ) WRITE( out, 2590 ) FIELDI( i )
                  GO TO 800
               END IF
  130       CONTINUE

!  -------- set up subroutine call and reserved parameter declarations

            IF ( iauto == 1 ) THEN
               IF ( single ) THEN
                  aorb = 'FORWARD_SINGLE '
               ELSE
                  aorb = 'FORWARD_DOUBLE '
               END IF
            ELSE
               IF ( single ) THEN
                  aorb = 'BACKWARD_SINGLE'
               ELSE
                  aorb = 'BACKWARD_DOUBLE'
               END IF
            END IF
            IF ( iad0 == 1 ) THEN
               ad0 = 'AD01'
            ELSE
               ad0 = 'AD02'
            END IF
            IF ( single ) THEN
               IF ( loutgf )                                                   &
                 WRITE( outgf, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),      &
                            FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ),                             &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),               &
                            FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),      &
                            FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),               &
                            FIELDI(  8 )( 1 : 6 ),                             &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),      &
                            FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),      &
                            FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),      &
                            FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),      &
                            FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),      &
                            pname, TRIM( version )
               WRITE( outgd, 3005 ) FIELDI( 21 )( 1 : 6 ),                     &
                        ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                    &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ),                               &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                 &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),        &
               ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                 &
                          FIELDI(  8 )( 1 : 6 ),                               &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),        &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),        &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),        &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),        &
                          pname, TRIM( version )
            ELSE
               IF ( loutgf )                                                   &
                 WRITE( outgf, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),      &
                            FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ),                             &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),               &
                            FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),      &
                            FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),      &
                          ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),               &
                            FIELDI(  8 )( 1 : 6 ),                             &
                            FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),      &
                            FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),      &
                            FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),      &
                            FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),      &
                            FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),      &
                            FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),      &
                            pname, TRIM( version )
               WRITE( outgd, 3004 ) FIELDI( 21 )( 1 : 6 ),                     &
                        ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                    &
                          FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ),                               &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                 &
                          FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),        &
               ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),        &
                        ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                 &
                          FIELDI(  8 )( 1 : 6 ),                               &
                          FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),        &
                          FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),        &
                          FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),        &
                          FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),        &
                          FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),        &
                          FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),        &
                          pname, TRIM( version )
            END IF
            IF ( iad0 == 1 ) THEN
               WRITE( outgd, 3006 )
            ELSE
               WRITE( outgd, 3007 )
            END IF
            IF ( ngrtyp == 0 ) THEN
               IF ( loutgf ) WRITE( outgf, 3009 ) FIELDI( 20 )( 1 : 6 )
               WRITE( outgd, 3009 ) FIELDI( 20 )( 1 : 6 )
               GO TO 910
            END IF
            IF ( loutgf ) WRITE( outgf, 3002 ) FIELDI(  9 )( 1 : 6 ),          &
                         FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),         &
                         FIELDI( 14 )( 1 : 6 )
            WRITE( outgd, 3002 ) FIELDI(  9 )( 1 : 6 ),                        &
                          FIELDI( 10 )( 1 : 6 ), FIELDI( 13 )( 1 : 6 ),        &
                          FIELDI( 14 )( 1 : 6 )

! --------- insert integer declarations

            IF ( ninnam > 0 .AND. loutgf )                                     &
               WRITE( outgf, 3010 ) ( INNAME( i ), i = 1, ninnam )
            IF ( ninnam > 0 )                                                  &
               WRITE( outgd, 3010 ) ( INNAME( i ), i = 1, ninnam )

!  order the real values so that the list of variables which belong
!  to intrinsic or external functions follow those which do not

            IF ( nrenam > 0 ) THEN
               nrenm1 = 0
               nrenm2 = nrenam + 1
  140          CONTINUE
               IF ( nrenm1 + 1 == nrenm2 ) GO TO 180
               DO 150 i = 1, nminam
                  IF ( RENAME( nrenm1 + 1 ) == MINAME( i ) ) GO TO 170
  150          CONTINUE
               DO 160 i = 1, nexnam
                  IF ( RENAME( nrenm1 + 1 ) == EXNAME( i ) ) GO TO 170
  160          CONTINUE
               nrenm1 = nrenm1 + 1
               GO TO 140
  170          CONTINUE
               nrenm2 = nrenm2 - 1
               ctemp = RENAME( nrenm2 )
               RENAME( nrenm2 ) = RENAME( nrenm1 + 1 )
               RENAME( nrenm1 + 1 ) = ctemp
               GO TO 140
  180          CONTINUE

! --------- insert real declarations

               IF ( single ) THEN
                  IF ( loutgf )                                                &
                  WRITE( outgf, 3019 ) ( RENAME( i ), i = 1, nrenam )
               ELSE
                  IF ( loutgf )                                                &
                  WRITE( outgf, 3020 ) ( RENAME( i ), i = 1, nrenam )
               END IF
               IF ( iad0 == 1 ) THEN
                  IF ( nrenm1 > 0 ) WRITE( outgd, 3018 )                       &
                       ( RENAME( i ), i = 1, nrenm1 )
               ELSE
                  IF ( nrenm1 > 0 ) WRITE( outgd, 3017 )                       &
                       ( ad0, RENAME( i ), i = 1, nrenm1 )
               END IF
               IF ( nrenm2 <= nrenam ) WRITE( outgd, 3017 )                    &
                    ( ad0, RENAME( i ), i = nrenm2, nrenam )
            END IF

! --------- insert logical declarations

            IF ( nlonam > 0 .AND. loutgf )                                     &
               WRITE( outgf, 3023 ) ( LONAME( i ), i = 1, nlonam )
            IF ( nlonam > 0 )                                                  &
               WRITE( outgd, 3023 ) ( LONAME( i ), i = 1, nlonam )

! --------- insert intrinsic declarations

            IF ( nminam > 0 .AND. loutgf )                                     &
               WRITE( outgf, 3021 ) ( MINAME( i ), i = 1, nminam )

! --------- insert external declarations

            IF ( nexnam > 0 .AND. loutgf )                                     &
               WRITE( outgf, 3022 ) ( EXNAME( i ), i = 1, nexnam )
            IF ( nexnam > 0 )                                                  &
               WRITE( outgd, 3022 ) ( EXNAME( i ), i = 1, nexnam )
            IF ( loutgf ) WRITE( outgf, 3009 ) FIELDI( 20 )( 1 : 6 )
            WRITE( outgd, 3009 ) FIELDI( 20 )( 1 : 6 )
         END IF

!  the general parameter assignments have been completed
!  continue with the construction of the generated subroutine

         IF ( intype >= mindiv .AND. .NOT. endgen ) THEN
            endgen = .TRUE.

! --------- start loop over groups

            IF ( loutgf )                                                      &
            WRITE( outgf, 3050 ) nloop,  FIELDI( 14 )( 1 : 6 ),                &
                   FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ),                                      &
                   FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), nloop,                               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 )
            IF ( iad0 == 2 ) THEN
              WRITE( outgd, 3011 )
!             do i = 1, ngtnam
              DO i = 1, nrenm1
                 WRITE( outgd, 3016 ) ad0, RENAME( i )
              END DO
            END IF
            WRITE( outgd, 3050 ) nloop,  FIELDI( 14 )( 1 : 6 ),                &
                   FIELDI(  5 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  7 )( 1 : 6 ), FIELDI( 14 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ),                                      &
                   FIELDI(  6 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),               &
                   FIELDI(  9 )( 1 : 6 ), nloop,                               &
                   FIELDI( 13 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),               &
                   FIELDI( 10 )( 1 : 6 )
            IF ( ngrtyp > 1 ) THEN
               IF ( loutgf ) WRITE( outgf, 3051 ) ( i, i = 1, ngrtyp )
               WRITE( outgd, 3051 ) ( i, i = 1, ngrtyp )
               IF ( loutgf ) WRITE( outgf, 3052 ) FIELDI(  9 )( 1 : 6 )
               WRITE( outgd, 3052 ) FIELDI(  9 )( 1 : 6 )
            END IF
         END IF

!  indicator card is endata
!  -------------------------

         IF ( intype == mendat ) GO TO 900
         GO TO 100
      ELSE

!  check that the first non comment card is the groups indicator card

         IF ( .NOT. defnam  ) THEN
            IF ( ngrtyp > 0 ) GO TO 930
            IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )
            gotlin = .TRUE.
            GO TO 600
         END IF

!  a data card has been found
!  read the character fields 1, 2, 3 and 7 from the card

         field1 = NULINE(  2:  3 )
         field2 = NULINE(  5: 12 )
         field3 = NULINE( 15: 22 )
         field7 = NULINE( 25: 65 )

!  check that field3 is blank on 'a', 'f' and 'g' cards

            IF ( FIELD1( 1: 1 ) == 'A' .OR.                                    &
                 FIELD1( 1: 1 ) == 'F' .OR.                                    &
                 FIELD1( 1: 1 ) == 'G' .OR.                                    &
                 FIELD1( 1: 1 ) == 'H' ) THEN
               IF ( ( FIELD1( 1: 1 ) /= 'A' .AND. field2 /=                    &
                    '       ' ) .OR.FIELD3 /= '       ' ) THEN
                  status = 73
                  IF ( out > 0 ) WRITE( out, 2730 )
                  GO TO 800
               END IF
            END IF
      END IF

!  branch on the value of intype

      GO TO ( 100, 100, 100, 100, 290, 300, 400, 900 ), intype

!  indicator card is temporaries
!  ------------------------------

  290 CONTINUE

!  check to see if the parameter is integer, real, logical or a function

      IF ( field1 /= 'I ' .AND. field1 /= 'R ' .AND.                           &
           field1 /= 'M ' .AND. field1 /= 'F ' .AND.                           &
           field1 /= 'L' ) THEN
         status = 54
         IF ( out > 0 ) WRITE( out, 2540 )
         GO TO 800
      END IF

!  if the parameter is a function, check to see that the name has
!  not already been used

      IF ( field1 == 'F ' ) THEN
         field = field2 // '  GU'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            nexnam = nexnam + 1
            IF ( nexnam > ninmax ) THEN
               status = - 2
               GO TO 700
            END IF
            EXNAME( nexnam ) = field2
         END IF
      ELSE

!  check to see that the parameter name has not already been used

         field = field2 // '  PG'
         CALL HASH_insert( length, 12, field, KEY, ITABLE, ifree )
         IF ( ifree <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
         ELSE
            IF ( field1 == 'R ' ) THEN
               nrenam = nrenam + 1
               IF ( nrenam > ninmax ) THEN
                  status = - 2
                  GO TO 700
               END IF
               RENAME( nrenam ) = field2
            ELSE
               IF ( field1 == 'M ' ) THEN
                  nminam = nminam + 1
                  IF ( nminam > ninmax ) THEN
                     status = - 2
                     GO TO 700
                  END IF
                  MINAME( nminam ) = field2
               ELSE
                  IF ( field1 == 'L ' ) THEN
                     nlonam = nlonam + 1
                     IF ( nlonam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     LONAME( nlonam ) = field2
                  ELSE
                     ninnam = ninnam + 1
                     IF ( ninnam > ninmax ) THEN
                        status = - 2
                        GO TO 700
                     END IF
                     INNAME( ninnam ) = field2
                  END IF
               END IF
            END IF
         END IF
      END IF
      GO TO 100

!  indicator card is global
!  -------------------------

  300 CONTINUE
      IF ( field1 == 'A ' .OR. field1 == 'I ' .OR.                             &
           field1 == 'E ' ) THEN
         startp = .TRUE.

!  start a parameter assignment. check to see that the parameter has
!  been defined

         field = field2 // '  PG'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
         IF ( ifield <= 0 ) THEN
            IF ( ifree == 0 ) THEN
               status = - 1
               GO TO 700
            END IF
            status = 57
            IF ( out > 0 ) WRITE( out, 2570 )
            GO TO 800
         END IF

! --------- make general parameter assignments

         IF ( field1 == 'A ' ) THEN
            IF ( loutgf ) WRITE( outgf, 3030 ) FIELD2( 1 : 6 ), field7
            ntem = ntem + 1
            WRITE( outem, 3080 ) FIELD2( 1 : 6 ), field7

! --------- make conditional parameter assignments

         ELSE   

!  check that the logical variable has been defined

            field = field3 // '  PG'
            CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
            IF ( ifield <= 0 ) THEN
               IF ( ifree == 0 ) THEN
                  status = - 1
                  GO TO 700
               END IF
               status = 57
               IF ( out > 0 ) WRITE( out, 2570 )
               GO TO 800
            END IF
            IF ( field1 == 'I ' ) THEN
               IF ( loutgf ) WRITE( outgf, 3031 ) FIELD2( 1 : 6 ),             &
                                     FIELD3( 1 : 6 ), field7
               ntem = ntem + 1
               WRITE( outem, 3081 ) FIELD2( 1 : 6 ),                           &
                                     FIELD3( 1 : 6 ), field7
            ELSE
               IF ( loutgf ) WRITE( outgf, 3032 ) FIELD2( 1 : 6 ),             &
                                     FIELD3( 1 : 6 ), field7
               ntem = ntem + 1
               WRITE( outem, 3082 ) FIELD2( 1 : 6 ),                           &
                                     FIELD3( 1 : 6 ), field7                  
            END IF   
         END IF
      ELSE
         IF ( FIELD1( 2: 2 ) == '+' .AND. startp ) THEN

! --------- continue a parameter assignment

            IF ( loutgf ) WRITE( outgf, 3040 ) field7
            ntem = ntem + 1
            WRITE( outem, 3040 ) field7
         ELSE
            status = 55
            IF ( out > 0 ) WRITE( out, 2550 )
            GO TO 800
         END IF
      END IF
      GO TO 100

!  indicator card is individuals
!  ------------------------------

  400 CONTINUE

!  check if a new group has been encountered

      IF ( field1 == 'T ' ) THEN
         IF ( firstg ) THEN

!  check if this is the first group-type

            firstg = .FALSE.
         ELSE

! ---------- wind up f and g

            IF ( setf ) THEN
               IF ( .NOT. endf ) THEN
                  IF ( iad0 == 1 ) THEN
                     WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),                &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  ELSE
                     WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),                &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  END IF
                  endf = .TRUE.
               END IF
               WRITE( outgd, 3190 )
            ELSE
               status = 61
               IF ( out > 0 ) WRITE( out, 2610 )
               GO TO 800
            END IF
            IF ( itype < ngrtyp .AND. loutgf )                                 &
               WRITE( outgf, 3191 ) nloop
            IF ( itype < ngrtyp ) WRITE( outgd, 3191 ) nloop
         END IF

!  find itype, the group-type

         field = field2 // '  GT'
         CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )

!  the group-type is unknown

         IF ( ifield <= 0 ) THEN
            status = 19
            IF ( out > 0 ) WRITE( out, 2190 )
            GO TO 800
         END IF

! --------- find type of current group

         itype = INLIST( ifield )
         IF ( loutgf ) WRITE( outgf, 3060 ) field2
         WRITE( outgd, 3060 ) field2
         IF ( ngrtyp > 1 .AND. loutgf ) WRITE( outgf, 3061 ) itype
         IF ( ngrtyp > 1 ) WRITE( outgd, 3061 ) itype
         IF ( loutgf ) WRITE( outgf, 3062 ) ANAMES( itype )( 1 : 6 ),          &
                   FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
         IF ( iad0 == 1 ) THEN
            WRITE( outgd, 3064 )                                               &
                      FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),            &
                      ANAMES( itype )( 1 : 6 )
         ELSE
            WRITE( outgd, 3065 )                                               &
                      FIELDI(  4 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),            &
                      ANAMES( itype )( 1 : 6 )
         END IF

! --------- set group parameters

         k1 = IGPA( itype )
         k2 = IGPA( itype + 1 ) - 1
         DO 435 k = k1, k2
            ivar = k - k1 + 1
            IF ( loutgf )                                                      &
            WRITE( outgf, 3063 ) GPNAME( k ), FIELDI( 11 )( 1 : 6 ),           &
                FIELDI( 13 )( 1 : 6 ), ivar
!           if ( iad0 == 2 ) write( ioutgd, 3015 ) ad0, gpname( k )
            WRITE( outgd, 3063 ) GPNAME( k ), FIELDI( 11 )( 1 : 6 ),           &
                FIELDI( 13 )( 1 : 6 ), ivar
  435    CONTINUE
         IF ( LDEFND( itype ) ) THEN
            status = 64
            IF ( out > 0 ) WRITE( out, 2640 )
            GO TO 800
         ELSE
            LDEFND( itype ) = .TRUE.
         END IF

!  initialize logicals which determine whether the data has been
!  input in the correct order

         startp = .FALSE.
         setf = .FALSE.
         endf = .TRUE.
         startv = .FALSE.
      ELSE
         IF ( FIELD1( 1: 1 ) == 'A' .OR. FIELD1( 1: 1 )                        &
              == 'I' .OR. FIELD1( 1: 1 ) == 'E' ) THEN

!  finish off the function assignment

            IF ( setf ) THEN
               IF ( .NOT. endf ) THEN
                  IF ( iad0 == 1 ) THEN
                     WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),                &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  ELSE
                     WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),                &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),          &
                        FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                  END IF
                  endf = .TRUE.
               END IF
            END IF

!  start a parameter assignment. check to see that the parameter has
!  been defined

            IF ( .NOT. setf ) THEN
               IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                  startp = .TRUE.

!  include the global parameters

                  IF ( .NOT. startv ) THEN
                     REWIND( outem )
                     DO 483 i = 1, ntem
                        READ( outem, 1000 ) ctem
                        WRITE( outgd, 1000 ) ctem
  483                CONTINUE   
                     startv = .TRUE.
                  END IF
                  field = field2 // '  PG'
                  CALL HASH_search( length, 12, field, KEY, ITABLE, ifield )
                  IF ( ifield <= 0 ) THEN
                     status = 57
                     IF ( out > 0 ) WRITE( out, 2570 )
                     GO TO 800
                  END IF

! --------- make group-specific parameter assignments

                  IF ( FIELD1( 1: 1 ) == 'A' ) THEN
                     IF ( .NOT. setf ) THEN
                        IF ( loutgf )                                          &
                        WRITE( outgf, 3080 ) FIELD2( 1 : 6 ), field7
                        WRITE( outgd, 3080 ) FIELD2( 1 : 6 ), field7
                     ELSE
                        IF ( loutgf )                                          &
                        WRITE( outgf, 3083 ) FIELD2( 1 : 6 ), field7
                        WRITE( outgd, 3083 ) FIELD2( 1 : 6 ), field7
                     END IF

! --------- make conditional parameter assignments

                  ELSE   

!  check that the logical variable has been defined

                     field = field3 // '  PG'
                     CALL HASH_search( length, 12, field, KEY, ITABLE, IFIELD)
                     IF ( ifield <= 0 ) THEN
                        IF ( ifree == 0 ) THEN
                           status = - 1
                           GO TO 700
                        END IF
                        status = 58
                        IF ( out > 0 ) WRITE( out, 2580 )
                        GO TO 800
                     END IF
                     IF ( FIELD1( 1: 1 ) == 'I' ) THEN
                        IF ( .NOT. setf ) THEN
                           IF ( loutgf )                                       &
                           WRITE( outgf, 3081 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                           WRITE( outgd, 3081 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                        ELSE
                           IF ( loutgf )                                       &
                           WRITE( outgf, 3084 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                           WRITE( outgd, 3084 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                        END IF
                     ELSE
                        IF ( .NOT. setf ) THEN
                           IF ( loutgf )                                       &
                           WRITE( outgf, 3082 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                           WRITE( outgd, 3082 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                        ELSE
                           IF ( loutgf )                                       &
                           WRITE( outgf, 3085 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                           WRITE( outgd, 3085 ) FIELD2( 1 : 6 ),               &
                                              FIELD3( 1 : 6 ), field7
                        END IF
                     END IF   
                  END IF
               ELSE
                  IF ( FIELD1( 2: 2 ) == '+' ) THEN
                     IF ( startp ) THEN

! --------- continuation of a parameter assignment

                        IF ( .NOT. setf ) THEN
                           IF ( loutgf )                                       &
                           WRITE( outgf, 3090 ) field7
                           WRITE( outgd, 3090 ) field7
                        ELSE
                           IF ( loutgf )                                       &
                           WRITE( outgf, 3091 ) field7
                           WRITE( outgd, 3091 ) field7
                        END IF
                     ELSE
                        status = 56
                        IF ( out > 0 ) WRITE( out, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            END IF
         ELSE
            startp = .FALSE.
            IF ( FIELD1( 1: 1 ) == 'F' ) THEN

!  set the function value

               IF ( FIELD1( 2: 2 ) == ' ' ) THEN
                  setf = .TRUE.
                  endf = .FALSE.

!  include the global parameters

                  IF ( .NOT. startv ) THEN
                     REWIND( outem )
                     DO 484 i = 1, ntem
                        READ( outem, 1000 ) ctem
                        WRITE( outgd, 1000 ) ctem
  484                CONTINUE   
                     startv = .TRUE.
                  END IF

! --------- start g

                  IF ( loutgf )                                                &
                  WRITE( outgf, 3100 ) FIELDI(  8 )( 1 : 6 ),                  &
                  FIELDI( 2 )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ), field7
                  WRITE( outgd, 3101 ) field7
               ELSE
                  IF ( FIELD1( 2: 2 ) == '+' ) THEN
                     IF ( setf ) THEN

! --------- continuation of g

                        IF ( loutgf ) WRITE( outgf, 3110 ) field7
                        WRITE( outgd, 3110 ) field7
                     ELSE
                        status = 56
                        IF ( out > 0 ) WRITE( out, 2560 )
                        GO TO 800
                     END IF
                  END IF
               END IF
            ELSE IF ( FIELD1( 1: 1 ) == 'G' .OR.                               &
                      FIELD1( 1: 1 ) == 'H' ) THEN
               IF ( setf ) THEN
                  IF ( .NOT. endf ) THEN
                     IF ( iad0 == 1 ) THEN
                        WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),             &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),       &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),       &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                     ELSE
                        WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),             &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),       &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),       &
                           FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
                     END IF
                     endf = .TRUE.
                  END IF
               END IF
            ELSE
              status = 56
              IF ( out > 0 ) WRITE( out, 2560 )
              GO TO 800
            END IF
         END IF
      END IF
      GO TO 100

!  the end of the input file has been reached before the endata card

  590 CONTINUE 

!  if the elements card has not been encountered, exit

      IF ( defnam ) THEN
         status = 52
         IF ( out > 0 ) WRITE( out, 2520 )
         RETURN
      END IF
      IF ( ngrtyp > 0 ) GO TO 930
      IF ( out > 0 .AND. print_level /= 0 ) WRITE( out, 2010 )

!  a dummy routine will be substituted

  600 CONTINUE 

!  write a dummy groups routine

      IF ( iauto == 1 ) THEN
         IF ( single ) THEN
            aorb = 'FORWARD_SINGLE '
         ELSE
            aorb = 'FORWARD_DOUBLE '
         END IF
      ELSE
         IF ( single ) THEN
            aorb = 'BACKWARD_SINGLE'
         ELSE
            aorb = 'BACKWARD_DOUBLE'
         END IF
      END IF
      IF ( iad0 == 1 ) THEN
         ad0 = 'AD01'
      ELSE
         ad0 = 'AD02'
      END IF
      IF ( single ) THEN
         IF ( loutgf )                                                         &
         WRITE( outgf, 3001 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
         WRITE( outgd, 3005 ) FIELDI( 21 )( 1 : 6 ),                           &
                  ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                          &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
         ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      ELSE
         IF ( loutgf )                                                         &
         WRITE( outgf, 3000 ) ( FIELDI( i )( 1 : 6 ), i = 1, 4 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
                    FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
         WRITE( outgd, 3004 ) FIELDI( 21 )( 1 : 6 ),                           &
                  ( FIELDI( i )( 1 : 6 ), i = 2, 4 ),                          &
                    FIELDI( 11 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 12 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ),                                     &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 19 ),                       &
                    FIELDI(  8 )( 1 : 6 ), FIELDI( 20 )( 1 : 6 ),              &
         ad0, aorb, FIELDI(  3 )( 1 : 6 ), FIELDI(  5 )( 1 : 6 ),              &
                  ( FIELDI(  i )( 1 : 6 ), i = 15, 20 ),                       &
                    FIELDI(  8 )( 1 : 6 ),                                     &
                    FIELDI(  6 )( 1 : 6 ), FIELDI( 15 )( 1 : 6 ),              &
                    FIELDI( 12 )( 1 : 6 ), FIELDI( 16 )( 1 : 6 ),              &
                    FIELDI(  7 )( 1 : 6 ), FIELDI( 17 )( 1 : 6 ),              &
                    FIELDI(  2 )( 1 : 6 ), FIELDI(  3 )( 1 : 6 ),              &
                    FIELDI(  4 )( 1 : 6 ), FIELDI( 18 )( 1 : 6 ),              &
                    FIELDI( 11 )( 1 : 6 ), FIELDI( 19 )( 1 : 6 ),              &
                    pname, TRIM( version )
      END IF
      IF ( loutgf ) WRITE( outgf, 3009 ) FIELDI( 20 )( 1 : 6 )
      WRITE( outgd, 3009 ) FIELDI( 20 )( 1 : 6 )
      IF ( loutgf ) WRITE( outgf, 3210 )
      WRITE( outgd, 3210 )
      status = 0
      RETURN

!  insufficient space to continue construction

  700 CONTINUE
      IF ( out > 0 ) WRITE( out, 2000 ) INCRSE( - status )
      RETURN

!  subroutine incomplete

  800 CONTINUE
      IF ( out > 0 ) WRITE( out, 2990 ) lineno, nuline
      RETURN

!  subroutine successfully completed

  900 CONTINUE
      IF ( .NOT. firstg ) THEN

! ---------- wind up f and g

         IF ( setf ) THEN
            IF ( .NOT. endf ) THEN
               IF ( iad0 == 1 ) THEN
                  WRITE( outgd, 3122 ) FIELDI( 8 )( 1 : 6 ),                   &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
               ELSE
                  WRITE( outgd, 3123 ) FIELDI( 8 )( 1 : 6 ),                   &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 ),             &
                     FIELDI( 2  )( 1 : 6 ), FIELDI( 10 )( 1 : 6 )
               END IF
               endf = .TRUE.
            END IF
            WRITE( outgd, 3190 )
         ELSE
            status = 61
            IF ( out > 0 ) WRITE( out, 2610 )
            GO TO 800
         END IF
         IF ( itype < ngrtyp .AND. loutgf )                                    &
            WRITE( outgf, 3191 ) nloop
         IF ( itype < ngrtyp ) WRITE( outgd, 3191 ) nloop
      END IF

! ---------- end do loop

      IF ( loutgf ) WRITE( outgf, 3200 ) nloop
      WRITE( outgd, 3200 ) nloop
      IF ( iad0 == 2 ) WRITE( outgd, 3192 )
  910 CONTINUE

! ---------- successful run. wind up output

      IF ( loutgf ) WRITE( outgf, 3210 )
      WRITE( outgd, 3210 )
      status = 0

!   check that all element types have been defined

  930 CONTINUE
      DO 940 itype = 1, ngrtyp
         IF ( .NOT. LDEFND( itype ) ) THEN
            status = 53
            IF ( out > 0 ) WRITE( out, 2530 ) GTYPES( itype )
         END IF
  940 CONTINUE
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 1010 FORMAT( A160 )
 2000 FORMAT( ' ** Exit from MAKE_group_ad - insufficient space.',             &
              ' Increase size of ', A6 )
 2010 FORMAT( ' ** Exit from MAKE_group_ad - warning.',                        &
              ' First card not groups. ', /, '    A dummy',                    &
              ' routine will be substituted ' )
 2020 FORMAT( ' ** Exit from MAKE_group_ad - indicator card not recognised ' )
 2190 FORMAT( ' ** Exit from MAKE_group_ad - group type not recognised:',      &
              ' name is ', A10 )
 2510 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' Name on card not that specified on input ' )
 2520 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' data file incomplete. No ENDATA card ' )
 2530 FORMAT( ' ** Exit from MAKE_group_ad - warning, group type ', A8,        &
              ' undefined ' )
 2540 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' unrecognised field 1 in TEMPORARIES section' )
 2550 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' unrecognised field 1 in GLOBALS section' )
 2560 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' unrecognised field 1 in INDIVIDUAL section' )
 2570 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' undefined parameter in GLOBALS section' )
 2580 FORMAT( ' ** Exit from MAKE_group_ad -',                                 &
              ' undefined parameter in INDIVIDUALS section' )
 2590 FORMAT( ' ** Exit from MAKE_group_ad - repeated parameter name ', A8 )
 2610 FORMAT( ' ** Exit from MAKE_group_ad - function not set '  )
 2640 FORMAT( ' ** Exit from MAKE_group_ad - group type already defined ' )
 2730 FORMAT( ' ** Exit from MAKE_group_ad - field 2 or 3 not blank on',       &
              ' A, F, G or H card ' )
 2900 FORMAT( ' ' )
 2970 FORMAT( ' Line ', i5, 4X, A160 )
 2980 FORMAT( ' Line ', i5, '.', i2, 1X, A65 )
 2990 FORMAT( ' Line ', i5, 4X, A65 )
 3000 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3001 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      REAL             ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C' )
 3002 FORMAT( '      INTEGER ', A6, ', ', A6, ', ', A6, ', ', A6 )
 3004 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      DOUBLE PRECISION ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      DOUBLE PRECISION, POINTER :: H_result( : ) ', /,          &
              '      DOUBLE PRECISION :: A_int( 1 ) ' )
 3005 FORMAT( '      SUBROUTINE ', A6, '( ', 5( A6, ', ' ), /,                 &
              '     *                   ', 5( A6, ', ' ), /,                   &
              '     *                   ', 4( A6, ', ' ), A6, ' )', /,         &
              '      USE HSL_', A4, '_', A15, /,                               &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      INTEGER ', 3( A6, ', ' ), A6, /,                          &
              '      LOGICAL ', A6, /,                                         &
              '      INTEGER ', A6, '(', A6, '), ', A6, '(', A6,               &
                             '), ', A6, '(', A6, ')', /,                       &
              '      REAL             ', A6, '(', A6, ',3), ',                 &
                                         A6, '(', A6, '), ', A6,               &
                                      '(', A6, ')', /,                         &
              'C', /, 'C  Problem name : ', A8, /,                             &
              'C', /, 'C  -- produced by SIFdecode ', A, /, 'C', /,            &
              '      INTEGER, POINTER :: H_index( : ) ', /,                    &
              '      REAL, POINTER :: H_result( : ) ', /,                      &
              '      REAL :: A_int( 1 ) ' )
 3006 FORMAT( '      TYPE (AD01_REAL) :: G_value = AD01_UNDEFINED', /,         &
              '      TYPE (AD01_REAL) :: A_value( 1 )' )
 3007 FORMAT( '      INTEGER :: ERROR_AD02', /,                                &
              '      TYPE (AD02_REAL) :: G_value', /,                          &
              '      TYPE (AD02_REAL) :: A_value( 1 )', /,                     &
              '      TYPE (AD02_DATA), POINTER :: DATA_AD02' )
 3009 FORMAT( '      ', A6, ' = 0' )
 3010 FORMAT( ( '      INTEGER ', A6, 4( :, ', ', A6 ) ) )
!3011 format( '      nullify( data_ad02 )', /,
!    *        '      call ad02_initialize(2, g_value,', /,
!    *        '     *                     gvalue(1,1),', /,
!    *        '     *                     data_ad02, 0)' )
 3011 FORMAT( '      CALL AD02_INITIALIZE_DATA(DATA_AD02, ERROR_AD02)' )
!3015 format( '       call ', a4, '_undefine( ', a6,
!    *        ', data_ad02 )' )
 3016 FORMAT( '      CALL ', A4, '_UNDEFINE( ', A6,                            &
              ', DATA_AD02 )' )
 3017 FORMAT( ( '      TYPE (', A4, '_REAL) :: ', A6 ) )
 3018 FORMAT( ( '      TYPE (AD01_REAL) :: ', A6,                              &
                ' = AD01_UNDEFINED' ) )
 3019 FORMAT( ( '      REAL             ', A6, 4( :, ', ', A6 ) ) )
 3020 FORMAT( ( '      DOUBLE PRECISION ', A6, 4( :, ', ', A6 ) ) )
 3021 FORMAT( ( '      INTRINSIC ', A6, 4( :, ', ', A6 ) ) )
 3022 FORMAT( ( '      EXTERNAL ', A6, 4( :, ', ', A6 ) ) )
 3023 FORMAT( ( '      LOGICAL ', A6, 4( :, ', ', A6 ) ) )
 3030 FORMAT( '      ', A6, ' = ', A41 )
 3031 FORMAT( '      IF (', A6, ') ', A6, ' = ', A41 )
 3032 FORMAT( '      IF (.NOT.', A6, ') ', /,                                  &
              '     *   ', A6, ' = ', A41 )
 3040 FORMAT( '     *         ', A41 )
 3050 FORMAT( '      DO ', i5, 1X, A6, ' = 1, ', A6, /,                        &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       ', A6, ' = ', A6, '(', A6, ')', /,                       &
              '       IF ( ', A6, ' == 0 ) GO TO ', i5, /,                     &
              '       ', A6, ' = ', A6, '(', A6, ') - 1' )
 3051 FORMAT( '       GO TO (', 8( i5, :, ',' ), /,                            &
            ( '     *        ', 8( i5, :, ',' ) ) )
 3052 FORMAT( '     *        ', 48X, '), ', A6 )
 3060 FORMAT( 'C', /, 'C  Group type : ', A8, /, 'C' )
 3061 FORMAT( i5, '  CONTINUE ' )
 3062 FORMAT( '       ', A6, '= ', A6, '(', A6, ')' )
 3063 FORMAT( '       ', A6, '= ', A6, '(', A6, '+', i6, ')' )
 3064 FORMAT( '       A_int( 1 ) = ', A6, '(', A6, ')', /,                     &
              '       CALL AD01_INITIALIZE(2, A_value( : 1 ),',                &
                      ' A_int( : 1 ), 0) ', /,                                 &
              '       ', A6, ' = A_value( 1 ) ' ) 
 3065 FORMAT( '       A_int( 1 ) = ', A6, '(', A6, ')', /,                     &
              '       CALL AD02_INITIALIZE_COMP(2, A_value( : 1 ),',           &
                      ' A_int( : 1 ),', /,                                     &
              '     *                      DATA_AD02, ERROR_AD02, 0)',         &
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
 3122 FORMAT( '       IF ( ', A6, ' ) THEN', /,                                &
              '        CALL AD01_HESSIAN(G_value, ',                           &
              A6, '(', A6, ',3), ', A6, '(', A6, ',2))', /,                    &
              '       ELSE',/,                                                 &
              '        CALL AD01_VALUE(G_value, ',A6,'(', A6, ',1))' )
 3123 FORMAT( '       IF ( ', A6, ' ) THEN', /,                                &
              '        CALL AD02_HESSIAN(G_value, ',                           &
              A6, '(', A6, ',3), ERROR_AD02,', /,                              &
              '     *                    ', A6, '(', A6, ',2))', /,            &
              '       ELSE',/,                                                 &
              '        CALL AD02_VALUE(G_value, ',A6,'(', A6, ',1),',          &
              ' ERROR_AD02)' )
 3190 FORMAT( '       END IF' )
 3191 FORMAT( '       GO TO', i6 )
 3192 FORMAT( '      CALL AD02_FINALIZE_DATA(DATA_AD02, ERROR_AD02)' )
 3200 FORMAT( i5,  ' CONTINUE' )
 3210 FORMAT( '      RETURN', /,  '      END' )

!  end of subroutine MAKE_group_ad

      END SUBROUTINE MAKE_group_ad

!-*-*-*-*-*- S I F D E C O D E   O U T R A N    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE OUTRAN( nelv, ninv, U, outfn, outra, ENAMES, INAMES, single )
      INTEGER :: nelv, ninv, outfn, outra
      LOGICAL :: single
      REAL ( KIND = wp ) :: U( ninv, nelv )
      CHARACTER ( len = 10 ) :: ENAMES( * ), INAMES( * )

!  --------------------------------------------------------------------
!  print out the gather and scatter part of the generated range routine
!  and the gather part of the generated function evaluation routine
!  --------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      REAL ( KIND = wp ) :: uij
      LOGICAL :: anynnz
      CHARACTER ( len = 6 ) :: evname, ivname

!  print out the scatter part

      DO 20 j = 1, nelv
         k = 0         
         anynnz = .FALSE.
         DO 10 i = 1, ninv
            uij = U( i, j )

!  ignore zero entries

            IF ( DABS( uij ) <= epsmch ) GO TO 10
            k = k + 1
            IF ( uij > zero ) THEN

!  the nonzero is positive

               IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4030 ) i
                  ELSE
                     WRITE( outra, 4040 ) j, i
                  END IF
               ELSE

!  nonzero has a value other than 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4050 ) i, uij
                  ELSE
                     WRITE( outra, 4060 ) j, i, uij
                  END IF
               END IF
            ELSE

!  the nonzero is negative

               IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4070 ) i
                  ELSE
                     WRITE( outra, 4080 ) j, i
                  END IF
               ELSE

!  nonzero has a value other than - 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4090 ) i, - uij
                  ELSE
                     WRITE( outra, 4100 ) j, i, - uij
                  END IF
               END IF
            END IF
            anynnz = .TRUE.
            IF ( MOD( k, 19 ) == 0 ) WRITE( outra, 4112 ) j, j
   10    CONTINUE
         IF ( .NOT. anynnz ) THEN
            IF ( single ) THEN
               WRITE( outra, 4111 ) j
            ELSE
               WRITE( outra, 4110 ) j
            END IF
         END IF
   20 CONTINUE

!  ----- the scatter has been completed; start the gather

      WRITE( outra, 4010 )

!  print out the gather part

      DO 40 i = 1, ninv
         k = 0
         anynnz = .FALSE.
         ivname = INAMES( i )(1:6)
         DO 30 j = 1, nelv
            evname = ENAMES( j )(1:6)
            uij = U( i, j )

!  ignore zero entries

            IF ( DABS( uij ) <= epsmch ) GO TO 30
            k = k + 1
            IF ( uij > zero ) THEN

!  the nonzero is positive

               IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

                  IF ( anynnz ) THEN
                     WRITE( outfn, 3030 ) evname
                     WRITE( outra, 4030 ) j
                  ELSE
                     WRITE( outfn, 3040 ) ivname, evname
                     WRITE( outra, 4040 ) i, j
                  END IF
               ELSE

!  nonzero has a value other than 1

                  IF ( anynnz ) THEN
                     WRITE( outfn, 3050 ) evname, uij
                     WRITE( outra, 4050 ) j, uij
                  ELSE
                     WRITE( outfn, 3060 ) ivname, evname, uij
                     WRITE( outra, 4060 ) i, j, uij
                  END IF
               END IF
             ELSE

!  the nonzero is negative

               IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

                  IF ( anynnz ) THEN
                     WRITE( outfn, 3070 ) evname
                     WRITE( outra, 4070 ) j
                  ELSE
                     WRITE( outfn, 3080 ) ivname, evname
                     WRITE( outra, 4080 ) i, j
                  END IF
               ELSE

!  nonzero has a value other than - 1

                  IF ( anynnz ) THEN
                     WRITE( outfn, 3090 ) evname, - uij
                     WRITE( outra, 4090 ) j, - uij
                  ELSE
                     WRITE( outfn, 3100 ) ivname, evname, - uij
                     WRITE( outra, 4100 ) i, j, - uij
                  END IF
               END IF
            END IF
            anynnz = .TRUE.
            IF ( MOD( k, 19 ) == 0 ) THEN
               WRITE( outfn, 3040 ) ivname, ivname
               WRITE( outra, 4112 ) i, i
            END IF
   30    CONTINUE
         IF ( .NOT. anynnz ) THEN
            IF ( single ) THEN
               WRITE( outfn, 3111 ) ivname
               WRITE( outra, 4111 ) i
            ELSE
               WRITE( outfn, 3110 ) ivname
               WRITE( outra, 4110 ) i
            END IF
         END IF
   40 CONTINUE

!  ----- the gather has been completed; wind up the element

      WRITE( outra, 4020 )
      RETURN

!  non-executable statements

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
 4030 FORMAT( '     *                 + W1(', i6, ' ) ' )
 4040 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) ' )
 4050 FORMAT( '     *                 + W1(', i6, ' ) * ', F12.5 )
 4060 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) * ', F12.5 )
 4070 FORMAT( '     *                 - W1(', i6, ' ) ' )
 4080 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) ' )
 4090 FORMAT( '     *                 - W1(', i6, ' ) * ', F12.5 )
 4100 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) * ', F12.5 )
 4110 FORMAT( '         W2(', i6, ' ) = 0.0D+0 ' )
 4111 FORMAT( '         W2(', i6, ' ) = 0.0E+0 ' )
 4112 FORMAT( '         W2(', i6, ' ) =   W2(', i6, ' ) ' )

!  end of subroutine OUTRAN

      END SUBROUTINE OUTRAN

!-*-*-*-*-*- S I F D E C O D E   O U T R N 2    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE OUTRN2( nelv, ninv, U, outff, outfd, outra,                   &
                         ENAMES, INAMES, single, ad0 )
      INTEGER :: nelv, ninv, outff, outfd, outra
      LOGICAL :: single
      CHARACTER ( len = 4 ) :: ad0
      REAL ( KIND = wp ) :: U( ninv, nelv )
      CHARACTER ( len = 10 ) :: ENAMES( * ), INAMES( * )

!  --------------------------------------------------------------------
!  print out the gather and scatter part of the generated range routine
!  and the gather part of the generated function evaluation routine
!  --------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      REAL ( KIND = wp ) :: uij
      LOGICAL :: anynnz, loutff
      CHARACTER ( len = 6 ) :: evname, ivname

      loutff = outff > 0

!  print out the scatter part

      DO 20 j = 1, nelv
         k = 0         
         anynnz = .FALSE.
         DO 10 i = 1, ninv
            uij = U( i, j )

!  ignore zero entries

            IF ( DABS( uij ) <= epsmch ) GO TO 10
            k = k + 1
            IF ( uij > zero ) THEN

!  the nonzero is positive

               IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4030 ) i
                  ELSE
                     WRITE( outra, 4040 ) j, i
                  END IF
               ELSE

!  nonzero has a value other than 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4050 ) i, uij
                  ELSE
                     WRITE( outra, 4060 ) j, i, uij
                  END IF
               END IF
            ELSE

!  the nonzero is negative

               IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4070 ) i
                  ELSE
                     WRITE( outra, 4080 ) j, i
                  END IF
               ELSE

!  nonzero has a value other than - 1

                  IF ( anynnz ) THEN
                     WRITE( outra, 4090 ) i, - uij
                  ELSE
                     WRITE( outra, 4100 ) j, i, - uij
                  END IF
               END IF
            END IF
            anynnz = .TRUE.
            IF ( MOD( k, 19 ) == 0 ) WRITE( outra, 4112 ) j, j
   10    CONTINUE
         IF ( .NOT. anynnz ) THEN
            IF ( single ) THEN
               WRITE( outra, 4111 ) j
            ELSE
               WRITE( outra, 4110 ) j
            END IF
         END IF
   20 CONTINUE

!  ----- the scatter has been completed; start the gather

      WRITE( outra, 4010 )

!  print out the gather part

      DO 40 i = 1, ninv
         k = 0
         anynnz = .FALSE.
         ivname = INAMES( i )(1:6)
         DO 30 j = 1, nelv
            evname = ENAMES( j )(1:6)
            uij = U( i, j )

!  ignore zero entries

            IF ( DABS( uij ) <= epsmch ) GO TO 30
            k = k + 1
            IF ( uij > zero ) THEN

!  the nonzero is positive

               IF ( DABS( uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value 1

                  IF ( anynnz ) THEN
                     IF ( loutff ) WRITE( outff, 3030 ) evname
                     WRITE( outfd, 3030 ) evname
                     WRITE( outra, 4030 ) j
                  ELSE
                     IF ( loutff ) WRITE( outff, 3040 ) ivname, evname
                     WRITE( outfd, 3041 ) ad0, i, evname
                     WRITE( outra, 4040 ) i, j
                  END IF
               ELSE

!  nonzero has a value other than 1

                  IF ( anynnz ) THEN
                     IF ( loutff ) WRITE( outff, 3050 ) evname, uij
                     WRITE( outfd, 3050 ) evname, uij
                     WRITE( outra, 4050 ) j, uij
                  ELSE
                     IF ( loutff )                                             &
                        WRITE( outff, 3060 ) ivname, evname, uij
                     WRITE( outfd, 3061 ) ad0, i, evname, uij
                     WRITE( outra, 4060 ) i, j, uij
                  END IF
               END IF
             ELSE

!  the nonzero is negative

               IF ( DABS( - uij - one ) <= epsmch ) THEN

!  special case if nonzero has the value - 1

                  IF ( anynnz ) THEN
                     IF ( loutff ) WRITE( outff, 3070 ) evname
                     WRITE( outfd, 3070 ) evname
                     WRITE( outra, 4070 ) j
                  ELSE
                     IF ( loutff ) WRITE( outff, 3080 ) ivname, evname
                     WRITE( outfd, 3081 ) ad0, i, evname
                     WRITE( outra, 4080 ) i, j
                  END IF
               ELSE

!  nonzero has a value other than - 1

                  IF ( anynnz ) THEN
                     IF ( loutff ) WRITE( outff, 3090 ) evname, - uij
                     WRITE( outfd, 3090 ) evname, - uij
                     WRITE( outra, 4090 ) j, - uij
                  ELSE
                     IF ( loutff )                                             &
                        WRITE( outff, 3100 ) ivname, evname, - uij
                     WRITE( outfd, 3101 ) ad0, i, evname, - uij
                     WRITE( outra, 4100 ) i, j, - uij
                  END IF
               END IF
            END IF
            anynnz = .TRUE.
            IF ( MOD( k, 19 ) == 0 ) THEN
               IF ( loutff ) WRITE( outff, 3040 ) ivname, ivname
               WRITE( outfd, 3041 ) ad0, i, ivname
               WRITE( outra, 4112 ) i, i
            END IF
   30    CONTINUE
         IF ( .NOT. anynnz ) THEN
            IF ( single ) THEN
              IF ( loutff ) WRITE( outff, 3120 ) ivname
               WRITE( outfd, 3121 ) ad0, i
               WRITE( outra, 4111 ) i
            ELSE
              IF ( loutff ) WRITE( outff, 3110 ) ivname
               WRITE( outfd, 3111 ) ad0, i
               WRITE( outra, 4110 ) i
            END IF
         END IF
         IF ( ad0 == 'AD01' ) THEN
            WRITE( outfd, 3150 ) i, i
         ELSE
            WRITE( outfd, 3151 ) i, i
         END IF
   40 CONTINUE

!  ----- the gather has been completed; wind up the element

      WRITE( outra, 4020 )
      IF ( ad0 == 'AD01' ) THEN
         WRITE( outfd, 3130 ) ninv, ninv
      ELSE
         WRITE( outfd, 3131 ) ninv, ninv
      END IF
      DO 50 i = 1, ninv
         ivname = INAMES( i )(1:6)
         WRITE( outfd, 3140 ) ivname, i
   50 CONTINUE   
      RETURN

!  non-executable statements

 3030 FORMAT( '     *          + ', A6 )
 3040 FORMAT( '       ', A6, ' =   ', A6 )
 3041 FORMAT( '       X_', A4, '_int(', i6, ') =   ', A6 )
 3050 FORMAT( '     *          + ', A6, ' * ', F12.5 )
 3060 FORMAT( '       ', A6, ' =   ', A6, ' * ', F12.5 )
 3061 FORMAT( '       X_', A4, '_int(', i6, ') =   ', A6, ' * ', F12.5 )
 3070 FORMAT( '     *          - ', A6 )
 3080 FORMAT( '       ', A6, ' = - ', A6 )
 3081 FORMAT( '       X_', A4, '_int(', i6, ') = - ', A6 )
 3090 FORMAT( '     *          - ', A6, ' * ', F12.5 )
 3100 FORMAT( '       ', A6, ' = - ', A6, ' * ', F12.5 )
 3101 FORMAT( '       X_', A4, '_int(', i6, ') = - ', A6, ' * ', F12.5 )
 3110 FORMAT( '       ', A6, ' = 0.0D+0 ' )
 3111 FORMAT( '       X_', A4, '_int(', i6, ') = 0.0D+0 ' )
 3120 FORMAT( '       ', A6, ' = 0.0E+0 ' )
 3121 FORMAT( '       X_', A4, '_int(', i6, ') = 0.0E+0 ' )
 3130 FORMAT( '       CALL AD01_INITIALIZE(IFFLAG - 1, X_value(:', i6,         &
                     '),', /, '     *', 22X, 'X_int(:', i6, '), 0) ')
 3131 FORMAT( '       CALL AD02_INITIALIZE_COMP(IFFLAG - 1, X_value(:',        &
                      i6, '),', /, '     *', 22X, 'X_int(:', i6, '),',         &
              ' DATA_AD02, ERROR_AD02, 0)' )
 3140 FORMAT( '       ', A6, ' = X_value(', i6, ')' )
 3150 FORMAT( '       CALL AD01_VALUE(X_AD01_int(', i6,                        &
                      '), X_int(', i6, '))' )
 3151 FORMAT( '       CALL AD02_VALUE(X_AD02_int(', i6,                        &
                      '), X_int(', i6, '), ERROR_AD02)' )
 4010 FORMAT( '      ELSE' )
 4020 FORMAT( '      END IF', /, '      RETURN' )
 4030 FORMAT( '     *                 + W1(', i6, ' ) ' )
 4040 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) ' )
 4050 FORMAT( '     *                 + W1(', i6, ' ) * ', F12.5 )
 4060 FORMAT( '         W2(', i6, ' ) =   W1(', i6, ' ) * ', F12.5 )
 4070 FORMAT( '     *                 - W1(', i6, ' ) ' )
 4080 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) ' )
 4090 FORMAT( '     *                 - W1(', i6, ' ) * ', F12.5 )
 4100 FORMAT( '         W2(', i6, ' ) = - W1(', i6, ' ) * ', F12.5 )
 4110 FORMAT( '         W2(', i6, ' ) = 0.0D+0 ' )
 4111 FORMAT( '         W2(', i6, ' ) = 0.0E+0 ' )
 4112 FORMAT( '         W2(', i6, ' ) =   W2(', i6, ' ) ' )

!  end of subroutine OUTRN2

      END SUBROUTINE OUTRN2

!-*-*-*-*-*-*- S I F D E C O D E   N U N A M E    F U N C T I O N -*-*-*-*-*-*-

      FUNCTION NUNAME( i1, i2, i3, i4, i5, i6, iires, ninmax, nrenam, ninnam,  &
                       nlonam, nminam, nexnam, nlmax, neltyp, yname, FIELDI,   &
                       RENAME, INNAME, LONAME, MINAME, EXNAME, ETYPES )
      CHARACTER ( len = 6 ) :: nuname
      INTEGER :: i1, i2, i3, i4, i5, i6, iires, nlmax, ninmax
      INTEGER :: nrenam, ninnam, nlonam, nminam, nexnam, neltyp
      CHARACTER ( len = 6 ) :: yname
      CHARACTER ( len = 8 ) :: FIELDI( iires )
      CHARACTER ( len = 10 ) :: RENAME( ninmax ), INNAME( ninmax )
      CHARACTER ( len = 10 ) :: LONAME( ninmax )
      CHARACTER ( len = 10 ) :: MINAME( ninmax ), EXNAME( ninmax )
      CHARACTER ( len = 10 ) :: ETYPES( nlmax  )

!  -------------------------------------------------
!  find a name that does not occur in any other list
!  -------------------------------------------------

!  local variables

      INTEGER :: i
      CHARACTER ( len = 1 ) :: CHARAC( 36 )
      DATA CHARAC / 'Z', 'Y', 'X', 'W', 'V', 'U', 'T', 'S', 'R',               &
                    'Q', 'P', 'O', 'N', 'M', 'L', 'K', 'J', 'I',               &
                    'H', 'G', 'F', 'E', 'D', 'C', 'B', 'A', '0',               &
                    '9', '8', '7', '6', '5', '4', '3', '2', '1' / 

   10 CONTINUE

!  find the next name in the list

      i1 = i1 + 1
      IF ( i1 == 27 ) THEN
         i1 = 1
         i2 = i2 + 1
         IF ( i2 == 37 ) THEN
            i2 = 1
            i3 = i3 + 1
            IF ( i3 == 37 ) THEN
               i3 = 1
               i4 = i4 + 1
               IF ( i4 == 37 ) THEN
                  i4 = 1
                  i5 = i5 + 1
                  IF ( i5 == 37 ) THEN
                     i5 = 1
                     i6 = i6 + 1
                     IF ( i6 == 37 ) THEN
                        write( 6, * ) ' no characters left '
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      nuname = CHARAC( i1 ) // CHARAC( i2 ) // CHARAC( i3 ) //                 &
               CHARAC( i4 ) // CHARAC( i5 ) // CHARAC( i6 )

!  see if the name has already been used

      DO 110 i = 1, nrenam
         IF ( RENAME( i )(1:6) == nuname ) GO TO 10
  110 CONTINUE
      DO 120 i = 1, ninnam
         IF ( INNAME( i )(1:6) == nuname ) GO TO 10
  120 CONTINUE
      DO 130 i = 1, nlonam
         IF ( LONAME( i )(1:6) == nuname ) GO TO 10
  130 CONTINUE
      DO 140 i = 1, nminam
         IF ( MINAME( i )(1:6) == nuname ) GO TO 10
  140 CONTINUE
      DO 150 i = 1, nexnam
         IF ( EXNAME( i )(1:6) == nuname ) GO TO 10
  150 CONTINUE
      DO 160 i = 1, neltyp
         IF ( ETYPES( i )(1:6) == nuname ) GO TO 10
  160 CONTINUE
      DO 170 i = 1, iires
         IF ( FIELDI( i )(1:6) == nuname ) GO TO 10
  170 CONTINUE
      IF ( nuname == yname ) GO TO 10
      RETURN

!  end of function NUNAME

      END FUNCTION NUNAME

!-*-*-*- S I F D E C O D E   F R E E _ F O R M A T   S U B R O U T I N E -*-*-*-

      SUBROUTINE FREE_format( nuline, leline, mendat, INDIC8, LENIND, NULINA,  &
                              maxnul, nlines, issdif, status, out )
      INTEGER :: leline, mendat, maxnul, nlines, status, out
      LOGICAL :: issdif
      INTEGER :: LENIND( mendat )
      CHARACTER ( len = 1 ) :: NULINE( leline )
      CHARACTER ( len = 12 ) :: INDIC8( mendat )
      CHARACTER ( len = 65 ) :: NULINA( maxnul )

!  ----------------------------------------------------
!  construct a fixed format line from a free format one
!  ----------------------------------------------------

!  local variables

      INTEGER :: i, j, k, nfield, nstfie, lfield, len, icard
      LOGICAL :: field, nextl, waitnl, spsal2
      INTEGER, DIMENSION( 6, 2 ), PARAMETER :: LENFIE =                        &
        RESHAPE( (/ 2, 10, 10, 12, 10, 12, 2, 10, 10, 41, 0, 0 /), (/ 6, 2 /) )
      INTEGER, DIMENSION( 6, 2 ), PARAMETER :: ISTFIE =                        &
        RESHAPE( (/ 2, 5, 15, 25, 40, 50, 2, 5, 15, 25, 65, 65 /), (/ 6, 2 /) )
      INTEGER, DIMENSION( 2 ), PARAMETER :: NFIE = (/ 6, 4 /)

!  if issdif is .true., the call is made from subroutine gps, where the
!  card length is 61. otherwise, the card length is 65

      IF ( issdif ) THEN
        icard = 1
      ELSE
        icard = 2
      END IF
      nfield = 0
      nlines = 0
      field = .FALSE.
      nextl = .FALSE.
      spsal2 = .FALSE.
      waitnl = .TRUE.

!  process the next character on the card

      DO 500 i = 1, leline

!  copy comments unchanged

        IF ( waitnl .AND. NULINE( i ) == '$' ) THEN
          nlines = nlines + 1
          DO j = 1, 65
            NULINA( nlines )( j : j ) = ' '
          END DO
          NULINA( nlines )( 1 : 1 ) = '*'
          DO j = 2, leline + 1 - i
            NULINA( nlines )( j : j ) = NULINE( i + j - 1 )
          END DO
          GO TO 600
        END IF

!  if we are looking for an end of line marker, check whether we have
!  found it

        IF ( nextl ) THEN
          IF ( NULINE( i ) == ';' ) THEN
            nextl = .FALSE.
            waitnl = .TRUE.

!  reset the card type to 2 when a special card has been finished

            IF ( spsal2 ) THEN
              spsal2 = .FALSE.
              icard = 2
            END IF
          END IF
          GO TO 500
        END IF

!  next check whether we have found an end of line marker anyway

        IF ( NULINE( i ) == ';' ) THEN
          waitnl = .TRUE.

!  finish off the current line

          j = ISTFIE( nfield, icard ) - 1
          DO k = 1, lfield
            NULINA( nlines )( j + K: j + k ) = NULINE( nstfie + k )
          END DO
          field = .FALSE.

!  reset the card type to 2 when a special card has been finished

          IF ( spsal2 ) THEN
            spsal2 = .FALSE.
            icard = 2
          END IF
          GO TO 500
        END IF

!  a field has been started

        IF ( field ) THEN

!  the field has now come to an end

          IF ( ( NULINE( i ) == ' ' .AND. .NOT.                                &
               ( icard == 2 .AND. nfield == 4 ) ) .OR. NULINE( i ) == '_' ) THEN
            field = .FALSE.

!  store the field in its correct position in nulina

            j = ISTFIE( nfield, icard ) - 1
            DO k = 1, lfield
              NULINA( nlines )( j + K: j + k ) = NULINE( nstfie + k )
            END DO

!  the field has now come to an end and a blank field follows

            IF ( NULINE( i ) == '_' ) THEN
              nfield = nfield + 1
              lfield = 0
              IF ( nfield > NFIE( icard ) ) THEN
                 status = 45
                 WRITE( out, 2450 )
                 RETURN
              END IF
            END IF
          ELSE

!  an extra character has been added to the field

            lfield = lfield + 1

!  check that the field has not exceeded its allowed space
!  this may happen if a) there is an error on the card, b) the
!  card is of type 2 and only blanks remain or c) the field is
!  actually an indicator card. check which

            IF ( LENFIE( nfield, icard ) < lfield ) THEN

!  if we are considering field 4 when the card is of type 2 and
!  all the space has been exhausted, finish the line

              IF ( icard == 2 .AND. nfield == 4 ) THEN
                waitnl = .TRUE.
                j = ISTFIE( nfield, icard ) - 1
                DO k = 1, lfield
                  NULINA( nlines )( j + K: j + k ) = NULINE( nstfie + k )
                END DO
                field = .FALSE.
                GO TO 500
              END IF

!  there is an error in the current field

              IF ( nfield > 1 ) THEN
                status = 44
                WRITE( out, 2440 ) nfield
                RETURN

!  the first field may be an indicator card. check

              ELSE
                DO 70 j = 2, mendat
                  len = LENIND( j )
                  DO k = 1, len
                    IF ( NULINE( nstfie + k ) /= INDIC8( j )( K: k ) ) GO TO 70
                  END DO
                  GO TO 80
   70           CONTINUE

!  the indicator card is unknown. exit with an error message

                status = 2
                WRITE( out, 2020 )
                RETURN

!  the indicator card is recognised. output this card as the next
!  line and await a further line. (the title card is an exception as
!  the title has still to arrive)

   80           CONTINUE
                IF ( j /= 4 ) THEN
                  field = .FALSE.
                  nextl = .TRUE.
                  NULINA( nlines )( 1: 12 ) = INDIC8( j )( 1: 12 )
                 END IF
              END IF
            END IF
          END IF

!  we are between fields

        ELSE

!  a new field has started

          IF ( NULINE( i ) /= ' ' ) THEN

!  it is the first field

            IF ( waitnl ) THEN
              waitnl = .FALSE.
              nlines = nlines + 1

!  initialize the new line, nulina( nlines ), as a blank line

              DO j = 1, 65
                NULINA( nlines )( J: j ) = ' '
              END DO
              nfield = 1
              IF ( NULINE( i ) == '_' ) lfield = 0

!  if a special card occurs (that is, a card on which the range
!  transformation is specified within MAKE_elfun), mark it

              IF ( NULINE( i ) == 'R' .AND. icard == 2 ) THEN
                spsal2 = .TRUE.
                icard = 1
              END IF
            ELSE

!  it is not the first field

              nfield = nfield + 1
              IF ( nfield > NFIE( icard ) ) THEN
                status = 45
                WRITE( out, 2450 )
                RETURN
              END IF

!  if the string is in fields 3 or 5 and starts with a '$', the
!  remainder of the card is considered to be a comment

              IF ( ( nfield == 3 .OR. nfield == 5 ) .AND. NULINE( i ) == '$' ) &
                THEN
                j = ISTFIE( nfield, icard ) - 1
                DO k = 1, 66 - i
                  NULINA( nlines )( j + k : j + k ) = NULINE( i + k - 1 )
                END DO
                GO TO 600
              END IF
            END IF

!  skip a field if a '_' is encountered

            IF ( NULINE( i ) == '_' ) THEN
              lfield = 0
            ELSE

!  set the current length of the field, lfield, the starting address
!  in nuline - 1, nstfie, and the field number, nfield

              field = .TRUE.
              lfield = 1
              nstfie = i - 1
            END IF
          END IF
        END IF
  500 CONTINUE
  600 CONTINUE

!  finish off the last line

      IF ( field ) THEN
        j = ISTFIE( nfield, icard ) - 1
        DO k = 1, lfield
          NULINA( nlines )( j + k : j + k ) = NULINE( nstfie + k )
        END DO
      END IF
      RETURN

!  non-executable statements

 2020 FORMAT( ' ** Exit from GPSMPS - indicator card not recognised ' )
 2440 FORMAT( ' ** Exit from GPSMPS - field ', i1, ' on free-form',            &
              ' card too long' )
 2450 FORMAT( ' ** Exit from GPSMPS - too many fields on free-form',           &
              ' card' )

!  end of subroutine FREE_format

      END SUBROUTINE FREE_format

!-*-*-*-*-*- S I F D E C O D E   T R A N S    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE TRANS( out, input, output, tempry, single, iauto,             &
                        iad0, RNAMES, lnames, ADUMMY, ldummy )
      INTEGER :: out, input, output, tempry, iauto, iad0, lnames, ldummy
      CHARACTER ( len = 10 ) :: RNAMES( lnames ), ADUMMY( ldummy )
      LOGICAL :: single

!  -------------------------------------------------------
!  translate a fortran 77 program so that it is capable of
!  accepting AD01 or AD02 reals instead of ordinary reals
!  -------------------------------------------------------

!  local variables

      INTEGER :: i, i1, i2, ii, j, jj, nreal, nre, lre, mre, ndummy
      LOGICAL :: intri, sub, fun, nofiel, ebrack
      LOGICAL :: freal, endlin, startr
      CHARACTER ( len = 1 ) :: card
      CHARACTER ( len = 4 ) :: ad0
      CHARACTER ( len = 10 ) :: blank, field
      CHARACTER ( len = 15 ) :: adtype
      CHARACTER ( len = 72 ) :: nuline, oldlin, bl72
      CHARACTER ( len = 72 ) :: RELINE( 20 )

      CHARACTER ( len = 6 ), PARAMETER :: lsp = ' real '
      CHARACTER ( len = 6 ), PARAMETER :: usp = ' REAL '
      CHARACTER ( len = 10 ), PARAMETER :: lf = ' function '
      CHARACTER ( len = 10 ), PARAMETER :: uf = ' FUNCTION '
      CHARACTER ( len = 11 ), PARAMETER :: li = ' intrinsic '
      CHARACTER ( len = 11 ), PARAMETER :: ui = ' INTRINSIC '
      CHARACTER ( len = 13 ), PARAMETER :: ls = ' subroutine '
      CHARACTER ( len = 13 ), PARAMETER :: us = ' SUBROUTINE '
      CHARACTER ( len = 18 ), PARAMETER :: ldp = ' double precision '
      CHARACTER ( len = 18 ), PARAMETER :: udp = ' DOUBLE PRECISION '
      CHARACTER ( len = 15 ), PARAMETER :: un = '=AD01_UNDEFINED'

!  create a blank name

      DO i = 1, 10
        BLANK( i : i ) = ' '
      END DO
      DO i = 1, 72
        BL72( i : i ) = ' '
      END DO

!  determine the type of automatic derivative to be used

      IF ( iauto == 1 ) THEN
        IF ( single ) THEN
          adtype = 'FORWARD_SINGLE '
        ELSE
          adtype = 'FORWARD_DOUBLE '
        END IF
      ELSE
        IF ( single ) THEN
          adtype = 'BACKWARD_SINGLE'
        ELSE
          adtype = 'BACKWARD_DOUBLE'
        END IF
      END IF

!  determine the ad routine to be used

      IF ( iad0 == 1 ) THEN
        ad0 = 'AD01'
      ELSE
        ad0 = 'AD02'
      END IF

!  initialize logicals

      intri = .FALSE.
      sub = .FALSE.
      fun = .FALSE.
      startr = .FALSE.
      REWIND( input )
      REWIND( tempry )
   10 CONTINUE

!  read a new line

      READ ( input, 1000, END = 600, ERR = 600 ) nuline
      IF ( sub .OR. fun ) THEN

!  ignore comments

        IF ( NULINE( 1: 1 ) == 'C' .OR.                                        &
             NULINE( 1: 1 ) == 'c' ) GO TO 400

!  is the line a continuation?

        IF ( NULINE( 6: 6 ) /= ' ' ) THEN
          IF ( card == 'I' ) GO TO 10
          ii = 7
          IF ( card == 'B' .OR. card == 'E' )                                 &
            CALL GETDUM( nuline, ii, ADUMMY, ldummy, ndummy, blank )

!  find what kind of line this is. find its first nonzero character
!  find the start of the name

        ELSE
          DO i = 7, 72
            IF ( NULINE( i : i ) /= ' ' ) GO TO 30
          END DO
          GO TO 10
   30     CONTINUE
          card = ' '
          IF ( NULINE( i : i + 6 ) == 'INTEGER' ) GO TO 400
          IF ( NULINE( i : i + 6 ) == 'COMPLEX' ) GO TO 400
          IF ( NULINE( i : i + 6 ) == 'LOGICAL' ) GO TO 400
          IF ( NULINE( i : i + 8 ) == 'CHARACTER' ) GO TO 400
          IF ( NULINE( i : i + 8 ) == 'DIMENSION' ) GO TO 400
          IF ( NULINE( i : i + 7 ) == 'IMPLICIT' ) GO TO 400
          IF ( NULINE( i : i + 10 ) == 'EQUIVALENCE' ) GO TO 400
          IF ( NULINE( i : i + 7 ) == 'EXTERNAL' ) THEN
            card = 'E'
            CALL GETDUM( nuline, i + 8, ADUMMY, ldummy, ndummy, blank )
            GO TO 400
          END IF
          IF ( single ) THEN
            IF ( NULINE( i : i + 15 ) == 'DOUBLE PRECISION' ) GO TO 400
            IF ( NULINE( i : i + 3 ) == 'REAL' ) THEN
              card = 'R'
              ii = i + 4
              GO TO 200
            END IF
          ELSE
            IF ( NULINE( i : i + 3 ) == 'REAL' ) GO TO 400
            IF ( NULINE( i : i + 15 ) == 'DOUBLE PRECISION' ) THEN
              card = 'R'
              ii = i + 16
              GO TO 200
            END IF
          END IF
          IF ( NULINE( i : i + 3 ) == 'SAVE' ) THEN
            WRITE( out, 2100 ) ad0
            card = 'S'
            ii = i + 4
            GO TO 200
          END IF
          IF ( NULINE( i : i + 8 ) == 'INTRINSIC' ) THEN
            card = 'I'
            GO TO 10
          END IF
          IF ( NULINE( i : i + 5 ) == 'COMMON' ) THEN
            WRITE( out, 2110 ) ad0
            card = 'C'
            ii = i + 6
            GO TO 200
          END IF
          IF ( NULINE( i : i + 8 ) == 'PARAMETER' ) THEN
            card = 'P'
            ii = i + 9
            GO TO 200
          END IF
          IF ( NULINE( i : i + 3 ) == 'DATA' ) THEN
            card = 'D'
            ii = i + 4
            GO TO 200
          END IF

!  the body of the procedure has been found. complete the
!  introduction
! 
          REWIND( tempry )
          card = 'B'
   60     CONTINUE
          READ ( tempry, 1000, END = 190, ERR = 190 ) oldlin

!  write out comments

          IF ( OLDLIN( 1: 1 ) == 'C' .OR. OLDLIN( 1: 1 ) == 'c' ) GO TO 180

!  search for cards defining appropriate real values

          IF ( OLDLIN( 6: 6 ) /= ' ' ) THEN
            IF ( card /= 'R' ) GO TO 180
            ii = 7
          ELSE
            IF ( card == 'B' ) THEN
              card = 'F'
              GO TO 180
            END IF
            IF ( card == 'F' ) WRITE( output, 2020 ) ad0, adtype

!  write out the previous set of real values
!       
            IF ( startr ) THEN
              IF ( nre > 0 ) THEN
                DO i = 1, mre - 1 
                   CALL OUTLIN( RELINE( i ), 72, output )
                END DO
                CALL OUTLIN( RELINE( mre ), lre, output )
              END IF
              startr = .FALSE.
            END IF
            DO i = 7, 72
              IF ( OLDLIN( i : i ) /= ' ' ) GO TO 80
            END DO
            GO TO 60
   80       CONTINUE
            card = ' '
            IF ( OLDLIN( i : i + 3 ) == 'SAVE' ) GO TO 180
            IF ( OLDLIN( i : i + 3 ) == 'DATA' ) GO TO 180
            IF ( OLDLIN( i : i + 5 ) == 'COMMON' ) GO TO 180
            IF ( OLDLIN( i : i + 6 ) == 'INTEGER' ) GO TO 180
            IF ( OLDLIN( i : i + 6 ) == 'COMPLEX' ) GO TO 180
            IF ( OLDLIN( i : i + 6 ) == 'LOGICAL' ) GO TO 180
            IF ( OLDLIN( i : i + 7 ) == 'EXTERNAL' ) GO TO 180
            IF ( OLDLIN( i : i + 7 ) == 'IMPLICIT' ) GO TO 180
            IF ( OLDLIN( i : i + 8 ) == 'CHARACTER' ) GO TO 180
            IF ( OLDLIN( i : i + 8 ) == 'DIMENSION' ) GO TO 180
            IF ( OLDLIN( i : i + 8 ) == 'PARAMETER' ) GO TO 180
            IF ( OLDLIN( i : i + 10 ) == 'EQUIVALENCE' ) GO TO 180
            IF ( single ) THEN
              IF ( OLDLIN( i : i + 15 ) == 'DOUBLE PRECISION' ) GO TO 180
              ii = i + 4
              RELINE( 1 ) = bl72
              RELINE( 1 )( 1 : 11 ) = '      REAL '
              lre = 11
              mre = 1
            ELSE
              IF ( OLDLIN( i : i + 3 ) == 'REAL' ) GO TO 180
              ii = i + 16
              RELINE( 1 ) = bl72
              RELINE( 1 )( 1 : 23 ) = '      DOUBLE PRECISION '
              mre = 1
              lre = 23
            END IF
            nre = 0
            card = 'R'
            startr = .TRUE.
          END IF
  110     CONTINUE
          CALL GETFLD( ii, i1, i2, endlin, oldlin, blank,                    &
                       field, nofiel, ebrack, .FALSE. )
          IF ( nofiel ) GO TO 60

!  the parameter will be of type ad01_real or ad02_real

          j = ii - i1
          DO i = 1, nreal
            IF ( field == RNAMES( i ) ) THEN
              DO j = 1, ndummy
                IF ( field == ADUMMY( j ) ) THEN
                  WRITE( output, 2060 ) ad0,                                   &
                    ( OLDLIN( jj : jj ), jj = i1, ii - 1 )
                  GO TO 130
                END IF
              END DO
              IF ( iad0 == 1 ) THEN
                WRITE( output, 2060 ) ad0,                                     &
                   ( OLDLIN( jj : jj ), jj = i1, ii - 1 ),                     &
                   ( UN( jj : jj ), jj = 1, 15 )
              ELSE
                WRITE( output, 2060 ) ad0,                                     &
                   ( OLDLIN( jj : jj ), jj = i1, ii - 1 )
              END IF
              GO TO 130
            END IF
          END DO

!  the parameter will be of type real

          IF ( nre > 0 ) THEN
            IF ( lre + 1 > 72 ) THEN
              mre = mre + 1
              RELINE( mre ) = bl72
              RELINE( mre )( 1 : 8 ) = '     * ,'
              lre = 8
            ELSE
              RELINE( mre )( lre + 1 : lre + 1 ) = ','
              lre = lre + 1
            END IF
          END IF                   
          IF ( lre + j + 1 > 72 ) THEN
            mre = mre + 1
            RELINE( mre ) = bl72
            RELINE( mre )( 1 : 7 ) = '     * '
            lre = 7
          END IF
          RELINE( mre )( lre + 1 : lre + j ) = OLDLIN( i1 : ii - 1 )
          lre = lre + j
          nre = nre + 1
  130     CONTINUE   
          IF ( endlin ) GO TO 60

!  find the next parameter

          GO TO 110

!  output the current line

  180     CONTINUE
          CALL OUTLIN( oldlin, 72, output )
          GO TO 60

!  write out any unfinished set of real values
!       
  190     CONTINUE
          IF ( startr ) THEN
            IF ( nre > 0 ) THEN
              DO i = 1, mre - 1
                CALL OUTLIN( RELINE( i ), 72, output )
              END DO
              CALL OUTLIN( RELINE( mre ), lre, output )
            END IF
            startr = .FALSE.
          END IF

!  the introduction is complete

          IF ( sub ) THEN
            sub = .FALSE.
          ELSE
            fun = .FALSE.
            IF ( freal .AND. iad0 == 1 )                                      &
              WRITE( output, 2030 ) RNAMES( 1 )( 1 : 6 )
            IF ( freal .AND. iad0 == 2 )                                      &
              WRITE( output, 2040 ) ad0, RNAMES( 1 )( 1 : 6 )
          END IF
          REWIND( tempry )
          GO TO 410
        END IF

!  find all variables mentioned on the current string

  200   CONTINUE

!  add the variable to the list

        IF ( card == 'R' ) THEN
  210     CONTINUE
            CALL GETFLD( ii, i1, i2, endlin, nuline, blank,                    &
                         field, nofiel, ebrack, .FALSE. )
            IF ( nofiel ) GO TO 400
            DO i = 1, nreal
              IF ( field == RNAMES( i ) ) GO TO 230
            END DO
            nreal = nreal + 1
            RNAMES( nreal ) = field
  230       CONTINUE   
            IF ( endlin ) GO TO 400
          GO TO 210
        END IF

!  remove the variable from the list

        IF ( card == 'C' .OR. card == 'D' .OR.                                 &
             card == 'P' .OR. card == 'S' ) THEN
  310     CONTINUE
          CALL GETFLD( ii, i1, i2, endlin, nuline, blank,                      &
                       field, nofiel, ebrack, .FALSE. )
          IF ( nofiel ) GO TO 400
          DO i = 1, nreal
            IF ( field == RNAMES( i ) ) THEN
              RNAMES( i ) = RNAMES( nreal )
              nreal = nreal - 1
              EXIT
            END IF
          END DO
          IF ( endlin ) GO TO 400

!  for parameter statements, skip the segments after the "="

          IF ( card == 'P' ) THEN
            DO i = ii, 72
              IF ( NULINE( i : i ) == ',' .OR. NULINE( i : i ) == ')' ) THEN
                 ii = i + 1
                 GO TO 310
              END IF
            END DO
          END IF
          GO TO 310
        END IF
  400   CONTINUE
        WRITE( tempry, 2000 ) nuline
        GO TO 10
      END IF
  410 CONTINUE

!  ignore comments

      IF ( .NOT. ( sub .OR. fun ) ) THEN
        IF ( NULINE( 1: 1 ) == 'C' .OR.                                        &
             NULINE( 1: 1 ) == 'c' ) GO TO 500

!  remove lines mentioning intrinsic functions

        IF ( intri ) THEN
           IF ( NULINE( 6: 6 ) /= ' ' ) GO TO 10
           intri = .FALSE.
        END IF
        DO i = 1, 62
          IF ( NULINE( i : i + 10 ) == ui .OR.                                 &
               NULINE( i : i + 10 ) == li ) THEN
            intri = .TRUE.
            GO TO 10
          END IF         
        END DO

!  hunt for the start of a subroutine

        card = ' '
        DO i = 1, 60
          IF ( NULINE( i : i + 11 ) == us .OR.                                 &
               NULINE( i : i + 11 ) == ls ) THEN
            ii = i + 12
            sub = .TRUE.
            card = 'B'
            nreal = 0
            ndummy = 0
            CALL GETDUM( nuline, ii, ADUMMY, ldummy, ndummy, blank )
            WRITE( tempry, 2000 ) nuline
            GO TO 10
          END IF         
        END DO

!  hunt for the start of a function

        DO i = 1, 63
          IF ( NULINE( i : i + 9 ) == uf .OR.                                  &
               NULINE( i : i + 9 ) == lf ) THEN
            ii = i + 10
            fun = .TRUE.
            card = 'B'

!  find what kind of function this is

            freal = .FALSE.

!  hunt for the string ' real '

            IF ( single ) THEN
              DO j = 1, i
                IF ( NULINE( j : j + 5 ) == usp .OR.                           &
                     NULINE( j : j + 5 ) == lsp ) THEN
                  freal = .TRUE.
                  NULINE( j : j + 5 ) = '      '
                  NULINE( 6 : 6 ) = '*'
                  WRITE( tempry, 2010 ) ad0
                  GO TO 433
                END IF
              END DO

!  hunt for the string ' double precision '

            ELSE
              DO j = 1, i
                IF ( NULINE( j : j + 17 ) == udp .OR.                          &
                     NULINE( j : j + 17 ) == ldp ) THEN
                  IF ( iad0 == 1 ) THEN
                    NULINE( j : j + 17 ) = ' TYPE (' // ad0 // '_REAL) '
                  ELSE
                    NULINE( j : j + 17 ) = '                           '
                    ad0 = 'AD02'
                  END IF
                  freal = .TRUE.
                  GO TO 433
                END IF
              END DO
            END IF
            WRITE( tempry, 2000 ) nuline
            ndummy = 0
            CALL GETDUM( nuline, ii, ADUMMY, ldummy, ndummy, blank )
            GO TO 10

!  the function will be of type ad01_real or ad02_real. find its name

  433       CONTINUE
            WRITE( tempry, 2000 ) nuline

!  find the start of the name

            DO j = ii, 72
              IF ( NULINE( j : j ) /= ' ' ) GO TO 435
            END DO

!  no name has been found so far. read the next card
!   
            READ ( input, 1000, END = 600, ERR = 600 ) nuline
            ii = 7
            GO TO 433

!  find the end of the name

  435       CONTINUE   
            DO jj = j + 1, MIN( 72, j + 5 ) 
              IF ( .NOT. ( CHARA( NULINE( jj : jj ) ) .OR.                     &
                           NUMBA( NULINE( jj : jj ) ) ) ) GO TO 437
            END DO
            jj = MIN( 72, j + 6 )
  437       CONTINUE
            nreal = 1
            RNAMES( nreal ) = blank
            RNAMES( nreal )( 1 : jj - j ) = NULINE( j : jj - 1 )
            ndummy = 0
            CALL GETDUM( nuline, jj, ADUMMY, ldummy, ndummy, blank )
            GO TO 10
          END IF         
        END DO

!  hunt for the start of a subroutine

  500   CONTINUE
        CALL OUTLIN( nuline, 72, output )
        GO TO 10
      END IF
  600 CONTINUE
      RETURN

!  non-executable statements

 1000 FORMAT( A72 )
 2000 FORMAT( A72 )
 2010 FORMAT( '      TYPE ( ', A4, '_REAL )' )
 2020 FORMAT( '      USE HSL_', A4, '_', A15 )
!2030 format( '      call ad01_undefine(', a6, ')' )
 2030 FORMAT( '      ', A6, ' = AD01_UNDEFINED' )
 2040 FORMAT( '      TYPE (', A4, '_REAL) :: ', A6 )
 2060 FORMAT( '      TYPE (', A4, '_REAL) :: ', 46( A1, : ), /,                &
              ( '     *', 66( A1, : ) ) )
 2100 FORMAT( ' ** Warning: a user-supplied external procedure',               &
              ' SAVEs parameters.', /,                                         &
              '    This is not allowed by the automatic',                      &
              ' differentiation package ', A4, '.' )
 2110 FORMAT( ' ** Warning: a user-supplied external procedure',               &
              ' saves parameters via common.', /,                              &
              '    This is not allowed by the automatic',                      &
              ' differentiation package ', A4, '.' )

!  end of subroutine TRANS

      END SUBROUTINE TRANS

!-*-*-*-*-*-*- S I F D E C O D E   C H A R A    F U N C T I O N -*-*-*-*-*-*-*-

      LOGICAL FUNCTION CHARA( c )
      CHARACTER ( len = 1 ) :: c

!  ----------------------------
!  is the character c a letter?
!  ----------------------------

!  local variables

      INTEGER :: i

      DO i = 1, 26
        IF ( c == LCHARS( i ) .OR. c == UCHARS( i ) ) THEN
          chara = .TRUE.
          RETURN
        END IF
      END DO
      chara = .FALSE.
      RETURN

!  end of function CHARA

      END FUNCTION CHARA

!-*-*-*-*-*-*- S I F D E C O D E   N U M B A    F U N C T I O N -*-*-*-*-*-*-*-

      LOGICAL FUNCTION NUMBA( c )
      CHARACTER ( len = 1 ) :: c

!  ----------------------------
!  is the character c a number?
!  ----------------------------

!  local variables

      INTEGER :: i

      DO i = 1, 10
        IF ( c == CHARS( i ) ) THEN
          numba = .TRUE.
          RETURN
        END IF
      END DO
      numba = .FALSE.
      RETURN

!  end of function NUMBA

      END FUNCTION NUMBA

!-*-*-*-*-*- S I F D E C O D E   U P P E R    S U B R O U T I N E -*-*-*-*-*-*-

      SUBROUTINE UPPER( c, n )
      INTEGER :: n
      CHARACTER ( len = * ) :: c

!  -------------------------------
!  convert character to upper case
!  -------------------------------

!  local variables

      INTEGER :: i, j

      DO 20 j = 1, n
        DO i = 1, 26
          IF ( C( j : j ) == LCHARS( i ) ) THEN
            C( j : j ) = UCHARS( i )
            GO TO 20
          END IF
        END DO
   20 CONTINUE   
      RETURN

!  end of subroutine UPPER

      END SUBROUTINE UPPER

!-*-*-*-*-*- S I F D E C O D E   L O W E R    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE LOWER( c, n )
      INTEGER :: n
      CHARACTER ( len = * ) :: c

!  -------------------------------
!  convert character to lower case
!  -------------------------------

!  local variables

      INTEGER :: i, j

      DO 20 j = 1, n
        DO i = 1, 26
          IF ( C( j : j ) == UCHARS( i ) ) THEN
            C( j : j ) = LCHARS( i )
            GO TO 20
          END IF
        END DO
   20 CONTINUE   
      RETURN

!  end of subroutine LOWER

      END SUBROUTINE LOWER

!-*-*-*-*-*- S I F D E C O D E   G E T F L D    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETFLD( ii, i1, i2, endlin, nuline, blank,                    &
                         field, nofiel, ebrack, ignorb )
      INTEGER :: ii, i1, i2
      LOGICAL :: endlin, nofiel, ebrack, ignorb
      CHARACTER ( len = 72 ) :: nuline
      CHARACTER ( len = 10 ) :: blank, field

!  ------------------------------------------------------------
!  find the first character string in nuline beyond position ii
!  ------------------------------------------------------------

!  local variables

      INTEGER :: i
      LOGICAL :: arrays

      ebrack = .TRUE.
      endlin = .FALSE.
      DO i = ii, 72
        IF ( CHARA( NULINE( i : i ) ) ) THEN
          nofiel = .FALSE.
          i1 = i
          GO TO 30
        END IF
      END DO
      nofiel = .TRUE.
      RETURN

!  next, find its last character

   30 CONTINUE   
      DO i = i1 + 1, MIN( 72, i1 + 6 )
        IF ( .NOT. CHARA( NULINE( i : i ) ) .AND.                              &
             .NOT. NUMBA( NULINE( i : i ) ) ) THEN
          i2 = i - 1
          ii = i
          IF ( ignorb ) GO TO 70
          GO TO 50
        END IF
      END DO
      i2 = MIN( 72, i1 + 6 )
      endlin = .TRUE.

!  last, check to see if it is an array 

   50 CONTINUE
      arrays = .FALSE.
      DO i = i2 + 1, 72
        IF ( .NOT. arrays ) THEN
          IF ( NULINE( i : i ) /= ' ' ) THEN
            IF ( NULINE( i : i ) /= '(' ) THEN
              EXIT
            ELSE
              arrays = .TRUE.
              ebrack = .FALSE.
            END IF
          END IF
        ELSE
          IF ( NULINE( i : i ) == ')' ) THEN
            ebrack = .TRUE.
            ii = i + 1
            EXIT
          END IF
        END IF
      END DO

   70 CONTINUE
!     write( 6, * ) ' line, i1, i2, ii ', line, i1, i2, ii
      field = blank
      field( 1 : i2 - i1 + 1 ) = nuline( i1 : i2 )
      CALL UPPER( field, 10 )
      RETURN

!  end of subroutine GETFLD

      END SUBROUTINE GETFLD

!-*-*-*-*-*- S I F D E C O D E   G E T D U M    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE GETDUM( nuline, ii, ADUMMY, ldummy, ndummy, blank )
      INTEGER :: ii, ndummy, ldummy
      CHARACTER ( len = 10 ) :: blank
      CHARACTER ( len = 72 ) :: nuline
      CHARACTER ( len = 10 ) :: ADUMMY( ldummy )

!  -------------------------------------------------------------------
!  determine the list of variables beyond column ii on the line nuline
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, jj, j1, j2
      CHARACTER ( len = 10 ) :: field
      LOGICAL nofiel, ebrack, endlin

      jj = ii
   10 CONTINUE
      CALL GETFLD( jj, j1, j2, endlin, nuline, blank,                          &
                   field, nofiel, ebrack, .TRUE. )
      IF ( nofiel ) RETURN
      DO i = 1, ndummy
        IF ( field == ADUMMY( i ) ) GO TO 30
      END DO
      ndummy = ndummy + 1
      ADUMMY( ndummy ) = field
   30 CONTINUE   
      IF ( endlin ) RETURN
      GO TO 10

!  end of subroutine GETDUM

      END SUBROUTINE GETDUM

!-*-*-*-*-*- S I F D E C O D E   O U T L I N    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE OUTLIN( nuline, iend, output )
      INTEGER :: iend, output
      CHARACTER ( len = 72 ) :: nuline

!  -------------------------------------------------------
!  write out the current line, cutting off trailing blanks
!  -------------------------------------------------------

!  local variables
      INTEGER :: i, i2

      DO i2 = MIN( iend, 72 ), 1, - 1
        IF ( NULINE( i2 : i2 ) /= ' ' ) GO TO 20
      END DO
      i2 = 1
   20 CONTINUE
      WRITE( output, "( 72( A1 : ) )" ) ( NULINE( i : i ), i = 1, i2 )
      RETURN

!  end of subroutine OUTLIN

      END SUBROUTINE OUTLIN

!-*-*-*-*-*- S I F D E C O D E   R E O R D A    S U B R O U T I N E -*-*-*-*-*-

      SUBROUTINE REORDA( nc, nnz, IRN, JCN, A, ip, IW )
      INTEGER :: nc, nnz
      INTEGER :: IRN( nnz ), JCN( nnz )
      INTEGER :: IW( * ), IP( * )
      REAL ( KIND = wp ) ::   A( nnz  )

!  ---------------------------------------------------------
!  sort a sparse matrix from arbitrary order to column order
!  ---------------------------------------------------------

!  local variables

      INTEGER :: i, j, k, l, ic
      INTEGER :: ncp1, itemp, jtemp,  locat
      REAL ( KIND = wp ) :: anext, atemp

!  initialize the workspace as zero

      ncp1 = nc + 1
      IW( : ncp1 ) = 0

!  pass 1. count the number of elements in each column

      DO k = 1, nnz
        j = JCN( k )
        IW( j ) = IW( j ) + 1
      END DO

!  put the positions where each column begins in a compressed collection into 
!  ip and iw

      IP( 1 ) = 1
      DO j = 2, ncp1
        IP( j ) = IW( j - 1 ) + IP( j - 1 )
        IW( j - 1 ) = IP( j - 1 )
      END DO

!  pass 2. reorder the elements into column order. Fill in each column in turn

      DO ic = 1, nc

!  consider the next unfilled position in column ic

        DO k = IW( ic ), IP( ic + 1 ) - 1

!  the entry should be placed in column j

          i = IRN( k )
          j = JCN( k )
          anext = A( k )
          DO l = 1, nnz

!  see if the entry is already in place

            IF ( j == ic ) EXIT
            locat = IW( j )
!          
!  as a new entry is placed in column j, increase the pointer iw( j ) by one
!          
            IW( j  ) = locat + 1

!  record details of the entry which currently occupies location locat

            itemp = IRN( locat ) ; jtemp = JCN( locat ) ; atemp = A( locat )

!  move the new entry to it correct place. 

            IRN( locat ) = i ; JCN( locat ) = j ; A( locat ) = anext

!  make the displaced entry the new entry

            i = itemp ; j = jtemp ; anext = atemp
          END DO

!  move the new entry to it correct place. 

          JCN( k ) = j ; IRN( k ) = i ; A( k ) = anext
        END DO
      END DO
      RETURN

!  end of subroutine REORDA

      END SUBROUTINE REORDA

!-  S I F D E C O D E   H A S H _ i n i t i a l i z e    S U B R O U T I N E  -

      SUBROUTINE HASH_initialize( length, ITABLE )
      INTEGER :: length
      INTEGER :: ITABLE( length )

!  ------------------------------------------------------------
!  set up initial scatter table (Williams, CACM 2, 21-24, 1959)

!  ITABLE(i) gives the status of table entry i. 
!  If status = - (length+1), the entry is unused
!  ------------------------------------------------------------

!  local variables

      INTEGER :: prime

      hash_empty = length + 1

!  Find an appropriate prime number for the hash function. Compute the largest 
!  prime smaller than length

      prime = 2 * ( ( length + 1 ) / 2 ) - 1
   10 CONTINUE

!  Is prime prime?

      IF ( .NOT. HASH_is_prime( prime ) ) THEN
        prime = prime - 2
        GO TO 10
      END IF
      hash_prime = prime

!  initialize each table entry as unfilled

      ITABLE( : length ) = - hash_empty
      RETURN

!  end of subroutine HASH_initialize

      END SUBROUTINE HASH_initialize

!-*-*- S I F D E C O D E   H A S H _ i n s e r t    S U B R O U T I N E -*-*-

      SUBROUTINE HASH_insert( length, nchar, field, KEY, ITABLE, ifree )
      INTEGER :: nchar, ifree, length
      INTEGER :: ITABLE( length )
      CHARACTER ( len = 1 ) :: FIELD( nchar ), KEY( nchar, length )

!  ----------------------------------------------------------------
!  Insert in chained scatter table (Williams, CACM 2, 21-24, 1959)

!  ITABLE(i) gives the status of table entry i
!  if status = - (length+1), the entry is unused
!  if status = - k, the entry was used but has been deleted. k gives
!              the index of the next entry in the chain
!  if status = 0, the entry is used and lies at the end of a chain
!  if status = k, the entry is used. k gives the index of the next
!              entry in the chain
!  IFIELD(i) gives the field key for used entries in the table
!  ----------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      CHARACTER ( len = 1 ) :: BFIELD( nbytes )
      INTEGER :: IVALUE( 2 )

!  find a starting position, ifree, for the insertion. Perform the hashing on 
!  8 characters of field at a time

      ifree = 0
      DO j = 1, nchar, nbytes
        DO i = 1, nbytes
          k = j + i - 1
          IF ( k <= nchar ) THEN
            BFIELD( i ) = FIELD( k )
          ELSE
            BFIELD( i ) = ' '
          END IF
        END DO

!  convert the character string into two integer numbers

        IVALUE( 1 ) = ICHAR( BFIELD( 1 ) ) / 2
        IVALUE( 2 ) = ICHAR( BFIELD( nbytes_by_2 + 1 ) ) / 2
        DO i = 2, nbytes_by_2
          IVALUE( 1 ) = 256 * IVALUE( 1 ) + ICHAR( BFIELD( i ) )
          IVALUE( 2 ) = 256 * IVALUE( 2 ) + ICHAR( BFIELD( nbytes_by_2 + i ) )
        END DO

!  convert the character string into a double precision number

!       READ( UNIT = FIELD8, FMT = 1000 ) value

!  hash and add the result to ifree

        ifree = ifree + HASH_value( IVALUE( 1 ), hash_prime )
      END DO

!  ensure that ifree lies within the allowed range

      ifree = MOD( ifree, IDINT( hash_prime ) ) + 1

!  is there a list?

      IF ( ITABLE( ifree ) >= 0 ) THEN

!  compare to see if the key has been found

   40   CONTINUE
        DO i = 1, nchar
          IF ( FIELD( i ) .NE. KEY( i, ifree ) ) GO TO 60
        END DO

!  the key already exists and therefore cannot be inserted

        IF ( ITABLE( ifree ) >= 0 ) THEN
          ifree = - ifree
          RETURN
        END IF

!  the key used to exist but has been deleted and must be restored

        GO TO 100

!  advance along the chain to the next entry

   60   CONTINUE
        IF ( ITABLE( ifree ) /= 0 ) THEN
          ifree = IABS( ITABLE( ifree ) )
          GO TO 40
        END IF

!  the end of the chain has been reached. Find empty entry in the table

   70   CONTINUE
        hash_empty = hash_empty - 1
        IF ( hash_empty == 0 ) THEN
          ifree = 0
          RETURN
        END IF
        IF ( ITABLE( hash_empty ) >= - length ) GO TO 70
        ITABLE( ifree ) = hash_empty
        ifree = hash_empty
      ELSE

!  the starting entry for the chain is unused

        IF ( ITABLE( ifree ) >= - length ) THEN
           ITABLE( ifree ) = - ITABLE ( ifree )
           GO TO 100
        END IF
      END IF

!  there is no link from the newly inserted field

      ITABLE( ifree ) = 0

!  insert new key

  100 CONTINUE
      KEY( : nchar, ifree ) = FIELD( : nchar )
      RETURN

!  end of subroutine HASH_insert

      END SUBROUTINE HASH_insert

!-*-*- S I F D E C O D E   H A S H _ s e a r c h    S U B R O U T I N E -*-*-

      SUBROUTINE HASH_search( length, nchar, field, KEY, ITABLE, ifree )
      INTEGER :: length, nchar, ifree
      INTEGER :: ITABLE( length )
      CHARACTER ( len = 1 ) :: FIELD( nchar ), KEY( nchar, length )

!  -------------------------------------------------------------------
!  search within chained scatter table (Williams, CACM 2, 21-24, 1959)

!  ITABLE(i) gives the status of table entry i
!  if status = - (length+1), the entry is unused
!  if status = - k, the entry was used but has been deleted. k gives
!              the index of the next entry in the chain
!  if status = 0, the entry is used and lies at the end of a chain
!  if status = k, the entry is used. k gives the index of the next
!              entry in the chain
!  IFIELD(i) gives the field key for used entries in the table
!  -------------------------------------------------------------------

!  local variables

      INTEGER :: i, j, k
      CHARACTER ( len = 1 ) :: BFIELD( nbytes )
      INTEGER :: IVALUE( 2 )

!  find a starting position, ifree, for the chain leading to the required 
!  location. Perform the hashing on nbytes characters of field at a time

      ifree = 0
      DO j = 1, nchar, nbytes
        DO i = 1, nbytes
          k = j + i - 1
          IF ( k <= nchar ) THEN
            BFIELD( i ) = FIELD( k )
          ELSE
            BFIELD( i ) = ' '
          END IF
        END DO

!  convert the character string into two integer numbers

        IVALUE( 1 ) = ICHAR( BFIELD( 1 ) ) / 2
        IVALUE( 2 ) = ICHAR( BFIELD( nbytes_by_2 + 1 ) ) / 2
        DO i = 2, nbytes_by_2
          IVALUE( 1 ) = 256 * IVALUE( 1 ) + ICHAR( BFIELD( i ) )
          IVALUE( 2 ) = 256 * IVALUE( 2 ) + ICHAR( BFIELD( nbytes_by_2 + i ) )
        END DO

!  convert the character string into a double precision number

!        READ( UNIT = FIELD8, FMT = 1000 ) value

!  hash and add the result to ifree

        ifree = ifree + HASH_value( IVALUE( 1 ), hash_prime )
      END DO

!  ensure that ifree lies within the allowed range

      ifree = MOD( ifree, IDINT( hash_prime ) ) + 1

!  is there a list?

      IF ( ITABLE( ifree ) < - length ) THEN
        ifree = 0
        RETURN
      END IF

!  compare to see if the key has been found

   40 CONTINUE
        DO i = 1, nchar
          IF ( FIELD( i ) /= KEY( i, ifree ) ) GO TO 60
        END DO

!  check that the table item has not been removed

        IF ( ITABLE( ifree ) < 0 ) THEN
          ifree = - ifree
        END IF
        RETURN

!  advance to next

   60   CONTINUE
        IF ( ITABLE( ifree ) == 0 ) THEN
          ifree = 0
          RETURN
        END IF
        ifree = IABS( ITABLE( ifree ) )
      GO TO 40

!  end of subroutine HASH_search

      END SUBROUTINE HASH_search

!-*-*-*-*- S I F D E C O D E   H A S H _ v a l u e    F U N C T I O N -*-*-*-*-

      INTEGER FUNCTION HASH_value( ivalue, hash_prime )
      INTEGER :: IVALUE( 2 )
      REAL ( KIND = wp ) :: hash_prime

!  -------------------------------------
!  a hash function proposed by John Reid
!  -------------------------------------

      HASH_value = INT( DMOD( DBLE( IVALUE( 1 ) ) + IVALUE( 2 ), hash_prime ) )
      HASH_value = ABS( HASH_value ) + 1
      RETURN

!  end of function HASH_value

      END FUNCTION HASH_value

!-*-*- S I F D E C O D E   H A S H  _ i s _ p r i m e    F U N C T I O N -*-*-

      LOGICAL FUNCTION HASH_is_prime( prime )
      INTEGER :: prime

!  -------------------------------------------
!  returns the value .TRUE. if prime is prime
!  -------------------------------------------

!  local variables

      INTEGER :: i

      HASH_is_prime  = .FALSE.
      IF ( MOD( prime, 2 ) == 0 ) RETURN
      DO i = 3, INT( DSQRT( DBLE( prime ) ) ), 2
        IF ( MOD( prime, i ) == 0 ) RETURN
      END DO
      HASH_is_prime  = .TRUE.
      RETURN

!  end of function HASH_is_prime

      END FUNCTION HASH_is_prime

    END MODULE SIFDECODE
