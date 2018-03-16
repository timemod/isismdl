module mcordr
    !
    ! module for ordering equations of a model
    !
    use kinds
    use model_type
    use mccedp
    integer(kind = SOLVE_IKIND), dimension(:), allocatable, save &
&          :: icode, nrow, ncol, lrow1, lcol1, irow, jcol, lrow, lcol
    integer, save :: ordr_alloc_stat

    contains

subroutine order_equations(ier, errpar, mdl, usinac, fixepi, &
&                          numfix, fbfix, fbtypfix)

!     Purpose

!     Mcordr determines the ordering of the equations of a model


!     Arguments

!      Out     ier      integer             error number
!                                               (0 for all ok)(see below)

!      Out     errpar   integer             error help variable (see below)

!      In      mdl      type(model)         the model

!      In      numfix (opt.)   integer      number of fixed feedback variables
!                                           0 if none
!      In      fbfix  (opt.)   integer(*)   numbers of variables desired as
!                                           feedback variables
!                                           not referenced if numfix == 0
!      In      fbtypfix (opt.) integer(*)   types of fixed feedback variables,


!     Return values for ordering are contained in
!          mdl%order(*), mdl%numfb(*) and mdl%nfb

!     mdl%order(1)       - mdl%order(mdl%loops-1)   are the prologue equations
!     mdl%order(mdl%loops)   - mdl%order(mdl%loope)     are the simultaneous equations
!     mdl%order(mdl%loope+1) - mdl%order(mdl%neq)       are the epilogue equatons

!     The last mdl%nfb equations in the simultaneous block compute
!     the feedback variables

!     Numfb(1..mdl%nfb)  records the numbers of the feedback variables
!     A model is recursive if mdl%nfb = 0
!     However, if a model has implicit equations iterating is
!              still required (Gauss-Seidel should be ok)

!     Error status

!     ier records error status
!      0  All OK

!      1  diagonal element does not match
!      2  no ordering found
!      3  diagonal element not found
!      4  equations still unordered
!      5  error ordering feedback variables
!      6  two or more equations for same left hand side variable
!         (errpar contains variable number). This error cannot
!         occur any more because now the model parser checks it.
!      7  too many arc's in model (maxarc exhausted)
!      8  error reading scratch file (errpar contains error number)
!      9  end of file on scratch file
!     10  mcopre not finished
!     11  too many feedback variables
!     12  non zero element in mcosgl
!     13  cannot choose feedback variable
!     20  not enough memory

!     All of these errors are internal with possible exception of 6


integer, intent(out) :: ier, errpar
integer, intent(in), optional :: numfix, fbfix(*), fbtypfix(*)
type(model), intent(inout)  :: mdl
logical, intent(in), optional :: usinac, fixepi

integer(kind = MC_IKIND), dimension(:), allocatable :: tmp
integer ::    nvcode, nrest
integer ::    inactv
integer ::    i, lri, last, m, mm, nresto, nsingl
integer ::    loopso, loopeo, nfbold
integer ::    fx,eqfnum, numfix_tmp, idum

integer, external ::    bysget
character(len = 1) :: eqtyp

logical ::    usinac_tmp, fixepi_tmp
integer ::    fbctyp

if (present(numfix)) then
    numfix_tmp = numfix
else
    numfix_tmp = 0
endif
if (present(usinac)) then
    usinac_tmp = usinac
else
    usinac_tmp = .true.
endif
if (present(fixepi)) then
    fixepi_tmp = fixepi
else
    fixepi_tmp = .true.
endif
!
! allocate mdl%numfb and mdl%fbtype with maximum possible size
!
deallocate(mdl%numfb, stat = idum)
deallocate(mdl%fbtype, stat = idum)
allocate(mdl%numfb(mdl%neq), stat = ordr_alloc_stat)
if (ordr_alloc_stat == 0) then
    allocate(mdl%fbtype(mdl%neq), stat = ordr_alloc_stat)
endif
if (ordr_alloc_stat == 0 .and. .not. allocated(icode)) then
    call allocate_mcordr_work(mdl)
endif
if (ordr_alloc_stat > 0) then
    ier = 20
    return
endif

ier = 0
errpar = 0

!     initialize icode; construct structural graph matrix

mdl%order(:mdl%neq) = 0

inactv = 0

if (usinac_tmp) then
   do  i=1,mdl%neq
      eqtyp = char(bysget(mdl%etype,i))
      if( ichar(eqtyp) .gt. 96 ) then
!              skip inactive equation
         icode(i) = 1
         inactv  = inactv + 1
      else
         icode(i) = 0
      endif
   enddo
else
   do  i=1,mdl%neq
      icode(i) = 0
   enddo
endif

call mcotab
if( ier .ne. 0 ) goto 900

!     find pro- and epilogue blocks

mdl%loops  = 1
mdl%loope  = mdl%neq - inactv
nrest  = mdl%neq - inactv
mdl%nfb    = 0
nvcode = 1

!     implicit equations (if done properly by model developer)
!     should have the lefthandside variable on the righthand side
!     this causes the equation NOT to be included in pro/epilogue

if (fixepi_tmp) then
    call mcopre
else
    call mcoprx
endif

if( ier .ne. 0 ) goto 900

if(nrest .eq. 0) then
!        model is recursive; can be checked by caller (mdl%nfb == 0)
!        it may be possible that some equations are in epilogue
!        ensure that model only has prologue
!        for cosmetic reasons

   if( mdl%loope .ne. mdl%neq - inactv) then
       mdl%loops = mdl%neq - inactv + 1
       mdl%loope = mdl%neq - inactv
   endif
   goto 810
endif


!     handle fixed feedback variables (if any)

do  i=1,numfix_tmp
   fx = fbfix(i)
   if( fx .ge. 1 .and. fx .le. mdl%nrv ) then
       eqfnum = equno(fx)
       if(eqfnum .ne. 0 ) then
!                active and/or endogenous
           if( icode(eqfnum) .eq. 0 ) then
!                    not in pro-/epilogue
               fbctyp = fbtypfix(i)
               call mcofbk(eqfnum,fbctyp, nrow,ncol,lrow1,lcol1,irow,jcol,lrow, &
&                          lcol)
               if( ier .ne. 0 ) goto 900
           endif
       endif
   endif
enddo

!     find feedback variables

!     1. check for diagonal elements

call mcorim
if( ier .ne. 0 ) goto 901

do  i=1,mdl%neq
   if(icode(i) .eq. 0) then
      lri = lrow1(i)
      call mcorrc(i,lrow,jcol,icode,lri,last)
      if(lri .ne. 0) then
!              self explanatory feedback variable
         call mcofbk(i, 2, nrow,ncol,lrow1,lcol1,irow,jcol,lrow,lcol)
         if( ier .ne. 0 ) goto 900
      endif
      if(nrest .eq. 0) goto 730
   endif
enddo

!     2. perform reduction rules

!  !! nvcode = 9 signals to mcozer to not adjust mdl%loops/mdl%loope

nvcode = 9
do  mm=1,mdl%neq
   nresto = nrest
   nsingl = 0

   do  m=1,mdl%neq

      if(icode(m) .gt. 0) cycle
      if(nrow(m) .eq. 0 .or. ncol(m) .eq. 0) then

!              no incoming/outgoing arc

         call mcozer(m)

      else if(nrow(m) .eq. 1) then

!              1 incoming arc

         call mcosgl(m,irow,lrow,lrow1,jcol,lcol,lcol1, nrow,ncol)
         if( ier .ne. 0 ) goto 900
         nsingl = nsingl + 1

      else if(ncol(m) .eq. 1) then

!              1 outgoing arc

         call mcosgl(m,jcol,lcol,lcol1,irow,lrow,lrow1, ncol,nrow)
         if( ier .ne. 0 ) goto 900
         nsingl = nsingl + 1

      endif

   enddo

   if(nrest .eq. 0) goto 600
   if(nrest .eq. nresto .and. nsingl .eq. 0) then

!              choose feedback variable heuristically

         call mcochs
         if( ier .ne. 0 ) goto 900
   endif
enddo

!     No ordering found (error)
goto 902

600 continue

!     rebuild graph for simultaneous part(excluding feedback variables)

do  i=1,mdl%neq
   if(icode(i) .ne. 1) icode(i) = 0
enddo

call mcotab
if( ier .ne. 0 ) goto 900

call mcorim
if( ier .ne. 0 ) goto 903


!     mdl%order the simultaneous part given feedback variables
!     nvcode = 1 signals to mcozer that mdl%loops/mdl%loope should be adjusted
!                ==> so save current values

nvcode = 1
loopso = mdl%loops
loopeo = mdl%loope
mdl%loope  = mdl%loope - mdl%nfb
nrest  = mdl%loope - mdl%loops + 1
call mcopre
if( ier .ne. 0 ) goto 900

!     nrest equations unordered
if(nrest .ne. 0) goto 904

!     reset to original values

mdl%loops = loopso
mdl%loope = loopeo

!     rebuild graph for feedback variables, and mdl%order them
!     not needed if mdl%nfb <= 1

if(mdl%nfb .gt. 1) then

   nfbold = mdl%nfb
   mdl%nfb   = 0
   mdl%loops = mdl%loope - nfbold + 1
   nrest = nfbold
   do  i=mdl%loops,mdl%loope
      icode(mdl%order(i)) = 0
   enddo

   call mcotab
   if( ier .ne. 0 ) goto 900

   call mcopre
   if( ier .ne. 0 ) goto 900

   if(nrest .ne. 0) then
      do  i=1,mdl%neq
         if(icode(i) .eq. 0) then
            mdl%order(mdl%loops) = i
            mdl%loops = mdl%loops + 1
         endif
      enddo

!           error assigning mdl%order to feedbacks
      if(mdl%loops .ne. mdl%loope+1) goto 905

   endif
   mdl%loops = loopso
   mdl%loope = loopeo
   mdl%nfb   = nfbold

endif

730 continue

!     convert equation numbers in mdl%numfb(*) to variable numbers

do  i=1,mdl%nfb
   mdl%numfb(i) = mdl%lhsnum(mdl%numfb(i))
enddo

!     ordering completed

810 continue

900 continue
!
! adjust the size of mdl%numfb and mdl%fbtype
!
allocate(tmp(mdl%nfb), stat = ordr_alloc_stat)
if (ordr_alloc_stat > 0) then
    ier = 20
    return
endif
tmp(1: mdl%nfb) = mdl%numfb(1: mdl%nfb)
call move_alloc(tmp, mdl%numfb)
allocate(tmp(mdl%nfb))
tmp(1: mdl%nfb) = mdl%fbtype(1: mdl%nfb)
call move_alloc(tmp, mdl%fbtype)

return

901 ier = 1
goto 900
902 ier = 2
goto 900
903 ier = 3
goto 900
904 ier = 4
goto 900
905 ier = 5
goto 900

return

contains
subroutine mcotab

!     creates table giving graph structural matrix

!     irow contains row (equation, receiving node) numbers per element (arc)
!     jcol contains column (explanatory variables, emitting node) numbers

!     lrow1 (lcol1) points to first element in row (column)
!     lrow  (lcol ) points to next element in same row (column)
!     a pointer value of zero means no (next) element

!     nrow (ncol) contains the row (column) sum, i.e. the number of
!     explanatory variables (equations) per equation (expl. var.)

!     initially, lrow1 and lrow are relatively trivial
!     they will however change in the process of analysing the graph

!     in second call, mcotab rebuilds graph structural matrix
!     now only for the variables having icode(i).eq.0
!     i.e. the non-feedback variables in the simultaneous block


integer ::    ielem, iequ, jj, jv, p, last

lrow1(:mdl%neq) = 0
lcol1(:mdl%neq) = 0
nrow(:mdl%neq) = 0
ncol(:mdl%neq) = 0

lrow(:maxarc) = 0
lcol(:maxarc) = 0

ielem = 0

do  iequ=1,mdl%neq

!        equations already ordered are not considered

   if(icode(iequ) .ne. 0) cycle

   do  jv=edptr(iequ),edptr(iequ+1)-1
      jj = equno(eqdep(jv))
!           skip inactive equation or variable with no equation (exogenous)
      if( jj .eq. 0 ) cycle
!           skip equation already ordered
      if(icode(jj) .ne. 0) cycle
      if(lrow1(iequ) .eq. 0) then
         lrow1(iequ) = ielem + 1
      else
!              ---------------------------------------------------------
!              if an edge from <jj> to <iequ> already exists ==> skip
!              ---------------------------------------------------------
         p = lrow1(iequ)
         call mcorrc(jj,lrow,jcol,icode,p,last)
         if( p .ne. 0 ) cycle
         lrow(ielem) = ielem + 1
      endif
      ielem = ielem + 1

!           too many arcs
      if(ielem .gt. maxarc) goto 930

      irow(ielem) = iequ
      jcol(ielem) = jj
      nrow(iequ)  = nrow(iequ) + 1
      if(ncol(jj) .eq. 0) then
         lcol1(jj) = ielem
      else
!              ---------------------------------------------------------
!              locate last entry for column <jj> and update
!              ---------------------------------------------------------
         p = lcol1(jj)
         call mcorrc(mdl%neq+1,lcol,irow,icode,p,last)
         lcol(last) = ielem
      endif
      ncol(jj) = ncol(jj) + 1
   enddo
enddo
if(ielem .lt. maxarc) irow(ielem+1) = 0

goto 1000

930 ier = 7

1000 return
end subroutine mcotab

! ---------------------------------------------------------------------

subroutine mcorim

!     -----------------------------------------------------------------
!      implicit equations have a fake diagonal entry in data structure
!      remove this entry

!      ier = 0 if all went ok
!      ier = 1 if error ocurred after call of mcorrc (shouldn't happen)
!     -----------------------------------------------------------------

integer ::  i, lri, lcj, last
character(len = 1) :: eqtyp

integer ::  bysget
external bysget

ier = 0

do  i=1,mdl%neq
   if(icode(i) .eq. 0) then
       eqtyp = char(bysget(mdl%etype,i))
       if( .not. usinac_tmp) then
           if( ichar(eqtyp) .gt. 96 ) then
              eqtyp = char(ichar(eqtyp) - 32)
           endif
       endif

       if( eqtyp .eq. 'N' .or. eqtyp .eq. 'M' ) then

          lri = lrow1(i)
          call mcorrc(i,lrow,jcol,icode,lri,last)
          if(lri .ne. 0) then
!                 ---------------------------------------------------------
!                 implicit equation
!                 delete redundant diagonal entry for lefthandside variable
!                 ---------------------------------------------------------
            if(last .eq. 0) then
                lrow1(i) = lrow(lri)
            else
                lrow(last) = lrow(lri)
            endif
            nrow(i) = nrow(i) - 1
            lcj = lcol1(i)
            call mcorrc(i,lcol,irow,icode,lcj,last)
            if(lcj .ne. lri) then
               ier = 1
               exit
            endif
            if(last .eq. 0) then
                lcol1(i) = lcol(lcj)
            else
                lcol(last) = lcol(lcj)
            endif
            ncol(i) = ncol(i) - 1
          endif

       endif
   endif
enddo

return
end subroutine mcorim

!-----------------------------------------------------------------------

subroutine mcorrc(jc,lrow,jcol,icode,lcj,last)

!     this subroutine searches for the arc in row ir at column jc
!     the ielem position is returned in lcj
!     last is the position of the previous arc in the row, coming from
!     a node that has icode.eq.0
!     on entry we must have lcj = lrow1(ir)
!     search for a nonexisting element yields lcj = 0 and
!     last equal to last arc number in the row

!     important: mcorrc is also called to search in a column
!      then the calling arguments are
!      (jc,ir,lcol,irow,icode,lri,last)
!      and on entry we must have lri = lcol1(jc)

integer ::    jc, lcj, last
integer(kind = MC_IKIND) :: jcol(*),icode(*), lrow(*)

last = 0

!     while( lcj .ne. 0 ) do
10 if(lcj .ne. 0) then
   if(jcol(lcj) .eq. jc) goto 900
   if(icode(jcol(lcj)) .eq. 0) last = lcj
   lcj = lrow(lcj)
!        endwhile
   goto 10
endif

900 return
end subroutine mcorrc

!-----------------------------------------------------------------------

subroutine mcopre

!     determines prologue and epilogue equations
!     on exit, mdl%loops and mdl%loope contain starting and ending points
!     of the simultaneous block, nrest the number of simult. variables
!     the ordering is stored in the vector mdl%order
!     mcopre considers only variables with icode.eq.0


integer ::  mm, m, nresto

!     Prologue equations (no incoming arcs)

do  mm=1,mdl%neq
   nresto = nrest
   do  m=1,mdl%neq
      if(icode(m) .eq. 0) then
         if( nrow(m) .eq. 0 ) then
               call mcozer(m)
         endif
      endif
   enddo
!        nrest == 0 implies all done
!        nrest==nresto means did nothing : done with prologue
   if(nrest .eq. 0 )goto 900
   if(nrest .eq. nresto) goto 300
enddo

300 continue

!    Epilogue equations (no outgoing arcs)

do  mm=1,mdl%neq
   nresto = nrest
   do  m=1,mdl%neq
      if(icode(m) .eq. 0) then
         if( ncol(m) .eq. 0 ) then
               call mcozer(m)
         endif
      endif
   enddo
   if(nrest .eq. 0 .or. nrest .eq. nresto) goto 900
enddo

ier = 10
900 return
end subroutine mcopre

!-----------------------------------------------------------------------

subroutine mcoprx

!     determines prologue and epilogue equations
!     same as mcopre
!     but will not put equation in pro-/epilogue if it is to be a feedback variable

integer ::  mm, m, nresto

!     Prologue equations (no incoming arcs)

do  mm=1,mdl%neq
   nresto = nrest
   do  m=1,mdl%neq
      if(icode(m) .eq. 0) then
         if( nrow(m) .eq. 0 ) then
               do  i=1,numfix_tmp
                     if ( m.eq.equno(fbfix(i)) ) then
                        goto 90
                     endif
               enddo
               call mcozer(m)
90 continue
         endif
      endif
   enddo
!        nrest == 0 implies all done
!        nrest==nresto means did nothing : done with prologue
   if(nrest .eq. 0 ) goto 900
   if(nrest .eq. nresto) goto 300
enddo

300 continue

!    Epilogue equations (no outgoing arcs)

do  mm=1,mdl%neq
   nresto = nrest
   do  m=1,mdl%neq
      if(icode(m) .eq. 0) then
         if( ncol(m) .eq. 0 ) then
               do  i=1,numfix_tmp
                     if ( m.eq.equno(fbfix(i)) ) then
                        goto 390
                     endif
               enddo
               call mcozer(m)
390 continue
         endif
      endif
   enddo
   if(nrest .eq. 0 .or. nrest .eq. nresto) goto 900
enddo

ier = 10
900 return
end subroutine mcoprx

!-----------------------------------------------------------------------

subroutine mcozer(m)
integer, intent(in) :: m

!     deletes nodes with no incoming or no outgoing arcs
!     and puts the corresponding variables in the pro- or
!     epilogue block accordingly

icode(m) = nvcode
nrest = nrest - 1
if(nrow(m) .eq. 0) then
   call mcodel(m,irow,lcol,lcol1,nrow)
   if(nvcode .ne. 9) then
      mdl%order(mdl%loops) = m
      mdl%loops = mdl%loops + 1
   endif
else
   call mcodel(m,jcol,lrow,lrow1,ncol)
   if(nvcode .ne. 9) then
      mdl%order(mdl%loope) = m
      mdl%loope = mdl%loope - 1
   endif
endif

return
end subroutine mcozer

!-----------------------------------------------------------------------

subroutine mcodel(jc,irow,lcol,lcol1,nrow)

!     Mcodel adjusts the row sums nrow for the deletion of column jc

!     it can also be used to adjust ncol for deleting a row ir, if the
!     argument list in the call reads (ir,jcol,lrow,lrow1,ncol)

integer ::    jc
integer(kind = MC_IKIND) :: irow(*),nrow(*)
integer(kind = MC_IKIND) :: lcol(*),lcol1(*)

integer ::    lnext, ir

lnext = lcol1(jc)

!     while( lnext .ne. 0 ) do
10 if(lnext .ne. 0) then
   ir = irow(lnext)
   if(icode(ir) .eq. 0) nrow(ir) = nrow(ir) - 1
   lnext = lcol(lnext)
   goto 10
endif

return
end subroutine mcodel

!-----------------------------------------------------------------------

subroutine mcofbk(m, fbtype, nrow, ncol, lrow1, lcol1, irow, jcol, lrow, lcol)

!     registers variable <m> as a feedback variable with type <fbtype>


integer ::    m, fbtype
integer(kind = MC_IKIND) :: nrow(*),ncol(*), irow(*),jcol(*)
integer(kind = MC_IKIND) :: lrow1(*),lcol1(*),lrow(*),lcol(*)

mdl%nfb = mdl%nfb + 1
if(mdl%nfb .gt. size(mdl%numfb)) then
   ier = 11
else
   mdl%order(mdl%loope-mdl%nfb+1) = m
   icode(m) = 1
   call mcodel(m,irow,lcol,lcol1,nrow)
   call mcodel(m,jcol,lrow,lrow1,ncol)
   nrest = nrest - 1
   mdl%numfb(mdl%nfb) = m
   mdl%fbtype(mdl%nfb) = fbtype
endif

return
end subroutine mcofbk

!-----------------------------------------------------------------------

subroutine mcosgl(ia,irow,lrow,lrow1,jcol,lcol,lcol1, nrow,ncol)

!     handles row ia which has only one nonzero element (node with
!     only one incoming arc, equation with one explan. var.)
!     can also handle column ia which has only one nonzero element
!     (node with only one outgoing arc, variable in only one eq.)
!     then calling arguments ia,jcol,lcol,lcol1,irow,lrow,lrow1,
!                           ncol,nrow

integer ::    ia
integer(kind = MC_IKIND) irow(*),jcol(*), nrow(*),ncol(*)
integer(kind = MC_IKIND) lrow1(*),lcol1(*),lrow(*),lcol(*)

integer ::    lnext, lzero, last, jb, lsave, iram, liram, lrib

!     search nonzero element in row ia (yields column number jb)

lzero = lrow1(ia)
call mcorrc(mdl%neq+1,lrow,jcol,icode,lzero,last)

if(last .eq. 0) then
   ier = 12
   goto 900
endif

jb = jcol(last)

!     the nonzero elements of column ia are transferred to column jb,
!     if they do not already exist there

lnext = lcol1(ia)

!     while( lnext .ne. 0 ) do
10 if(lnext .ne. 0) then

   lsave = lcol(lnext)
   iram  = irow(lnext)
   if(icode(iram) .eq. 0) then
      liram = lcol1(jb)
      call mcorrc(iram,lcol,irow,icode,liram,last)
      if(liram .gt. 0) then
         nrow(iram) = nrow(iram) - 1
      else
         jcol(lnext) = jb
         lcol(lnext) = 0
         lcol(last)  = lnext
         ncol(jb)    = ncol(jb) + 1
      endif
   endif
   lnext = lsave

   goto 10

endif

icode(ia) = nvcode
ncol(jb)  = ncol(jb) - 1
nrest     = nrest - 1

!     check whether a diagonal element has emerged in column jb

lrib = lrow1(jb)
call mcorrc(jb,lrow,jcol,icode,lrib,last)
if(lrib .ne. 0) then
!        note: feedback is itself symmetric in col/row arrays
   call mcofbk(jb, 2, nrow, ncol, lrow1, lcol1, irow, jcol, lrow, lcol)
endif

900 return
end subroutine mcosgl

!-----------------------------------------------------------------------

subroutine mcochs

!     choose a feedback variable heuristically
!     by selecting row/column with largest product of row/column entries

integer ::    i, pmax, pidx, prodrc

pmax = 0
pidx = 0

do  i=1,mdl%neq
   if(icode(i) .eq. 0) then
      prodrc = nrow(i)*ncol(i)
      if(prodrc .gt. pmax) then
         pmax = prodrc
         pidx = i
      endif
   endif
enddo

if(pidx .eq. 0) then
!        can't find a feedback variable
   ier = 13
else

   call mcofbk(pidx, 3, nrow,ncol,lrow1,lcol1,irow,jcol,lrow,lcol)

endif

return
end subroutine mcochs

end subroutine order_equations

subroutine allocate_mcordr_work(mdl)
    type(model), intent(in) :: mdl

    integer :: stat
    !
    ! allocate work arrays for ordering the equation of a model
    !
    call deallocate_mcordr_work
    allocate(icode(mdl%neq), stat = stat)
    if (stat == 0) allocate(nrow(mdl%neq), stat = stat)
    if (stat == 0) allocate(ncol(mdl%neq), stat = stat)
    if (stat == 0) allocate(lrow1(mdl%neq), stat = stat)
    if (stat == 0) allocate(lcol1(mdl%neq), stat = stat)
    if (stat == 0) allocate(irow(maxarc), stat= stat)
    if (stat == 0) allocate(jcol(maxarc), stat = stat)
    if (stat == 0) allocate(lrow(maxarc), stat = stat)
    if (stat == 0) allocate(lcol(maxarc), stat = stat)
    ordr_alloc_stat = stat

end subroutine allocate_mcordr_work

subroutine deallocate_mcordr_work
    !
    ! deallocate work arrays for ordering the equation of a model
    !
    integer :: stat
    deallocate(icode, nrow, ncol, lrow1, lcol1, irow, &
&              jcol, lrow,  lcol, stat = stat)

end subroutine deallocate_mcordr_work


end module mcordr
