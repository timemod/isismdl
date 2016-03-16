module mcxref

    private :: addxrf

    contains

subroutine write_xref(ier, mrfopt, fbcopt,prifbi, prisjc)
use mcvars
use mcpars
use kinds
use mdl_name_utils
use mcjsbl
use mcdep

!     writes cross-reference information
!     error returns in ier
!       0    all ok
!       1    insufficient memory
!       2    cannot open mrf file for write (filerr/filios variables set)
!       3    write error on mrf file        (filerr/filios variables set)
!       4    internal error 1 in mcxref
!       5    internal error 2 in mcxref
!       6    internal error 3 in mcxref
!       7    cannot allocate enough memory

!     sets global error string
!     but does not produce error message

integer ::   ier
integer ::    mrfopt(*)
integer ::    fbcopt(*)
logical ::    prifbi,prisjc

logical ::  rfshrt

integer(kind = MC_IKIND), dimension(:), allocatable :: &
& ivref,ipref,ieref,ixref, equno
integer(kind = MC_IKIND), dimension(:,:), allocatable :: fbsjac

character*(mcmxnm) name

integer ::   bysget
external bysget

character*8 string
logical ::   exist

integer ::   ios, i, maxlg, maxld, nxref, iref
integer ::   j, ldum, npro, nsim, nepi
integer ::   p,cnt,tcnt

integer ::   lp
character*256 line

integer ::   mxvlen,mxplen,mxelen,leadsp
integer ::   maxcln
integer :: xref_alloc_stat

integer ::   minfbo,maxfbo,fbostp
real(kind = SOLVE_RKIND) :: avgfbo, z

character*1 vartyp

integer  :: cntrdv, nzrcnt, idum
integer(kind = MC_IKIND) :: lhsnum
integer :: stat

ier = 0

maxref = max(1,maxref)
rfshrt = mrfopt(2) .eq. 1

allocate(equno(mdl%nrv), stat = xref_alloc_stat)
if (xref_alloc_stat > 0) then
    goto 900
endif

if( .not. rfshrt ) then

!        !!! allocate memory for LONG cross reference

   allocate(ivref(mdl%nrv), stat = stat)
   if (stat == 0) allocate(ipref(mdl%nrp), stat = stat)
   if (stat == 0) allocate(ieref(maxref), stat = stat)
   if (stat == 0) allocate(ixref(maxref), stat = stat)
   xref_alloc_stat = stat
   if (xref_alloc_stat > 0) then
       goto 900
   endif
   ivref = 0
   ipref = 0
endif

if (prisjc .and. mdl%nfb > 0) then
    ! calculate symbolic Jacobian
    allocate(fbsjac(mdl%nfb, mdl%nfb), stat = xref_alloc_stat)
    if (xref_alloc_stat > 0) then
        goto 900
    endif
    call gen_sjac(mdl, fbsjac, mdl%nfb)
endif

! calculate number of structural non zero elements in feedback jacobian
call mcjzct(mdl, nzrcnt, equno)

ier = 0

lenxrf = max(mrfopt(1),50)

inquire(file=xrfnam,exist=exist)
if(exist) then
   open(xrfunt,file=xrfnam)
   close(xrfunt,status='DELETE', iostat=ios)
   if( ios .ne. 0 ) goto 910
endif

open(xrfunt,file=xrfnam,form='formatted', &
&    status='NEW',iostat=ios, round = 'compatible')
if(ios.ne.0) goto 910

write(xrfunt,'(6a/)',err=990,iostat=ios) &
& ' ISIS model compilation : ',mdl%cname(:mdl%cnamelen),' ', &
& mdl%date, '  ', mdl%time

write(xrfunt,'(a/)',err=990,iostat=ios) ' Model reference table'

if( rfshrt ) then

  write(xrfunt,'(a/a/a/a/)',err=990,iostat=ios) &
& ' Each variable name is followed by its', &
& ' maximum lag (with minus sign) and maximum lead in the model,', &
& ' its type:  E(xogenous), B(ehavioral), I(dentity).', &
& ' Exogenous variables listed first.'

  write(xrfunt,'(a/a/)',err=990,iostat=ios) ' Parameters are listed separately.', &
& ' Each parameter name is followed by its length.'

else

  write(xrfunt,'(a/a/a/a/a/a/)',err=990,iostat=ios) &
& ' Each variable name is followed by its', &
& ' maximum lag (with minus sign) and maximum lead in the model,', &
& ' its type:  E(xogenous), B(ehavioral), I(dentity),', &
& ' and then by the names of the equations it occurs in.', &
& ' For endogenous variables own equation listed first.', &
& ' Exogenous variables listed first.'

  write(xrfunt,'(a/a/a/)',err=990,iostat=ios) &
& ' Parameters are listed separately.', &
& ' Each parameter name is followed by its length and', &
& ' the names of the equations in which it occurs.'

endif

write(xrfunt,'(a/a/)',err=990,iostat=ios) &
& ' Equations are listed in solution order,', &
& ' followed by a list of feedback variables.'

maxlg = 0
maxld = 0
do  i=1,mdl%nrv
   if(maxlag(i) .gt. maxlg) maxlg = maxlag(i)
   if(mxlead(i) .gt. maxld) maxld = mxlead(i)
enddo

if( .not. rfshrt ) then

!     !!!!   fill cross reference table
!            only needed for long cross reference

   rewind depunt
   nxref = 0

   do i = 1, mdl%neq

       read(depunt) lhsnum, nsimv, isimv(1:nsimv), &
&             nv, iv(1:nv), nparv, iparv(1:nparv)

       equno(lhsnum) = i

       call addxrf(i, nv   , iv,    nxref, maxref, &
&                  ivref, ixref, ieref, *900, *930)

       call addxrf(i, nparv, iparv, nxref, maxref, &
&                  ipref, ixref, ieref, *900, *940)

   end do
endif

!     write cross reference

! **  Statistics section

write(xrfunt,'(a/)',err=990,iostat=ios)' *** Statistics ***'

write(xrfunt,'(1x,i7,a)', err=990,iostat=ios) mdl%nrv,' variables of which'
write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) mdl%nrv-mdl%neq,' exogenous'
write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) mdl%nca,' behavioral'
write(xrfunt,'(1x,7x,i7,a/)', err=990,iostat=ios) mdl%neq-mdl%nca,' identity'

write(xrfunt,'(1x,i7,a)', err=990,iostat=ios) &
&          mdl%nd, ' total number of lags and leads with'
write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) maxlg, ' maximum lag'
write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) maxld, ' maximum lead'
write(xrfunt,'(1x,7x,i7,a/)', err=990,iostat=ios) &
&          mdl%nendex,' variables with leads'

write(xrfunt,'(1x,i7,a)', err=990,iostat=ios) mdl%nrp,' parameters '
write(xrfunt,'(1x,7x,i7,a/)', err=990,iostat=ios) &
&          mdl%nrc,' total length of parameter values'

npro = mdl%loops - 1
nsim = mdl%loope - mdl%loops + 1
nepi = nre - mdl%loope

write(xrfunt,'(1x,i7,a)', err=990,iostat=ios) nre,' equations of which'
write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) npro ,' in prologue'
write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) nsim ,' in simultaneous block'
write(xrfunt,'(1x,7x,i7,a/)', err=990,iostat=ios) nepi ,' in epilogue'


if( mdl%nfb .gt. 0 ) then
    z = dble(nzrcnt)/(dble(mdl%nfb*mdl%nfb))*100.0
    write(xrfunt,'(1x,i7,a)', err=990,iostat=ios) mdl%nfb,' feedback variables'
    write(xrfunt,'(8x,i10,a,f4.1,a//)', err=990,iostat=ios) &
&          nzrcnt,' (',z,'%) structural non zero''s in jacobian'
else
    write(xrfunt,'(1x,i7,a//)', err=990,iostat=ios) mdl%nfb,' feedback variables'
endif

if (mdl%fboflg > 0 .and. mdl%fbomem > 0) then

!        feedback ordering calculated ==> print summary statistics

   maxfbo = 0
   minfbo = mdl%loope - mdl%loops + 1
   do i = 1, mdl%nfb
      j      = mdl%fboptr(i + 1) - mdl%fboptr(i)
      maxfbo = max(maxfbo, j)
      minfbo = min(minfbo, j)
   end do
   avgfbo = dble(mdl%fboptr(mdl%nfb + 1) - 1) / dble(mdl%nfb)
!        effective number of steps for jacobian (rounded up)
   fbostp = (mdl%fboptr(mdl%nfb + 1) - 1 + mdl%nfb * mdl%nfb - 1) / nsim + 1

   write(xrfunt,'(1x,a)',err=990,iostat=ios) 'Statistics of feedback ordering'

   write(xrfunt,'(1x,5x,i10,1x,a)',err=990,iostat=ios) &
&          mdl%fboptr(mdl%nfb + 1) - 1, 'words of memory used'

   write(xrfunt,'(1x,5x,i8,3x,a)',err=990,iostat=ios) &
&          minfbo, 'minimum feedback chain (excl. fb equations)'
   write(xrfunt,'(1x,5x,i8,3x,a)',err=990,iostat=ios) &
&          maxfbo, 'maximum feedback chain (excl. fb equations)'
   write(xrfunt,'(1x,7x,f8.1,1x,a)',err=990,iostat=ios) &
&          avgfbo, 'average feedback chain (excl. fb equations)'
   write(xrfunt,'(1x,5x,i8,3x,a//)',err=990,iostat=ios) &
&          fbostp, 'average newton steps (rounded up)'

endif

!     get length of longest variable  name
!     get length of longest equation  name
!     get length of longest parameter name
!     set indent for continuation lines

mxvlen = maxnln(mdl%ivnames, 1_MC_IKIND, mdl%nrv)
mxvlen = ( (mxvlen-1)/4 + 1 ) * 4

mxelen = maxnln(mdl%ienames, 1_MC_IKIND, mdl%neq)
mxelen = ( (mxelen-1)/4 + 1 ) * 4

mxplen = 0
if( mdl%nrp .gt. 0 ) then
   mxplen = maxnln(mdl%ipnames, 1_MC_IKIND, mdl%nrp)
   mxplen = ( (mxplen-1)/4 + 1 ) * 4
endif

! **  Variable section

write(xrfunt,'(a/)',err=990,iostat=ios)' *** Variables ***'

lp = 0
line = ' '
leadsp = mxvlen + 8 + 1 + 1 + 6 + 3

! **  Exogenous

write(xrfunt,'(a/)',err=990,iostat=ios)' *** Exogenous ***'

do  i=1,mdl%nrv

   vartyp = char(bysget(vtype,int(mdl%indexv(i), ISIS_IKIND)))
   if( vartyp .ne. 'E' ) cycle
   name = gtnmex(mdl%ivnames(mdl%indexv(i)),mdl%vnames,ldum)
   call namxrf(name,0,1,mxvlen,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   write(string,'(2i4)') -maxlag(mdl%indexv(i)), mxlead(mdl%indexv(i))
   call namxrf(string,1,8,8,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   if( rfshrt ) then
     call namxrf(' '//vartyp,1,8,8,ios,lp,line,lenxrf,xrfunt)
     if( ios .gt. 0 ) goto 990
   else
      call namxrf(' '//vartyp//' in : ',1,8,8,ios, lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990
      iref = ivref(mdl%indexv(i))
      do  j=1,maxref
         if(iref.eq.0) exit
         name = gtnmex(mdl%ienames(ieref(iref)),mdl%enames,ldum)
         call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
         if( ios .gt. 0 ) goto 990
         iref = ixref(iref)
      enddo
   endif

enddo

call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
if( ios .gt. 0 ) goto 990

! **  Endogenous

write(xrfunt,'(a/)',err=990,iostat=ios)' *** Endogenous ***'

do  i=1,mdl%nrv

   vartyp = char(bysget(vtype,int(mdl%indexv(i), ISIS_IKIND)))
   if( vartyp .eq. 'E' ) cycle
   name = gtnmex(mdl%ivnames(mdl%indexv(i)),mdl%vnames,ldum)
   call namxrf(name,0,1,mxvlen,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   write(string,'(2i4)') -maxlag(mdl%indexv(i)), mxlead(mdl%indexv(i))
   call namxrf(string,1,8,8,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   if( rfshrt ) then
      call namxrf(' '//vartyp,1,8,8,ios,lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990
   else
      call namxrf(' '//vartyp//' in : ',1,8,8,ios, lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990
      name = gtnmex(mdl%ienames(equno(mdl%indexv(i))), mdl%enames,ldum)
      call namxrf(name,1,leadsp,mxelen,ios,lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990
      iref = ivref(mdl%indexv(i))
      do  j=1,maxref
         if(iref.eq.0) exit
         name = gtnmex(mdl%ienames(ieref(iref)),mdl%enames,ldum)
         call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
         if( ios .gt. 0 ) goto 990
         iref = ixref(iref)
      enddo
   endif

enddo

! **  Parameter section

call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
if( ios .gt. 0 ) goto 990

write(xrfunt,'(a/)',err=990,iostat=ios)' *** Parameters ***'

leadsp = mxplen + 4 + 1 + 8 + 2

do  i=1,mdl%nrp

   name = gtnmex(mdl%ipnames(mdl%indexp(i)),mdl%pnames,ldum)
   call namxrf(name,0,0,mxplen,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   write(string,'(i4)') mdl%nvalp(mdl%indexp(i))
   call namxrf(string,1,4,4,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   if( .not. rfshrt ) then
      call namxrf('  in :  ',1,8,8,ios,lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990
      iref = ipref(mdl%indexp(i))
      do  j=1,maxref
         if(iref.eq.0) exit
         name = gtnmex(mdl%ienames(ieref(iref)),mdl%enames,ldum)
         call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
         if( ios .gt. 0 ) goto 990
         iref = ixref(iref)
      enddo
   endif

enddo
call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
if( ios .gt. 0 ) goto 990

! **  Redundant variable section
!      redundant variables occur only once in model as
!      left hand side variable and also don't appear anywhere
!      as lag or lead

if( .not. rfshrt ) then
  cntrdv = 0
  do i=1,mdl%nrv
    iref = ivref(mdl%indexv(i))
    if( iref .eq. 0 ) then
       cntrdv = cntrdv + 1
    endif
  enddo

  if(cntrdv .gt. 0 ) then
     write(xrfunt,'(a/)',err=990,iostat=ios) &
&             ' *** Redundant endogenous variables ***'
     write(xrfunt,'(1x,i5,a)',err=990,iostat=ios) &
&       cntrdv,' redundant endogenous variables'

     call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
     if( ios .gt. 0 ) goto 990

     do i=1,mdl%nrv
       iref = ivref(mdl%indexv(i))
       if(iref.eq.0) then
          name = gtnmex(mdl%ivnames(mdl%indexv(i)),mdl%vnames, ldum)
          call namxrf(name,1,0,mxvlen,ios, lp,line,lenxrf,xrfunt)
          if( ios .gt. 0 ) goto 990
       endif
     enddo

     call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
     if( ios .gt. 0 ) goto 990

  endif

endif

! **  Equations section

write(xrfunt,'(a/)',err=990,iostat=ios) ' *** Equations (in solution order) ***'

write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) npro,' Prologue equations'

do  j=1,mdl%loops-1
   name = gtnmex(mdl%ienames(mdl%order(j)),mdl%enames,ldum)
   call namxrf(name,1,0,mxelen,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
enddo

call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
if( ios .gt. 0 ) goto 990

write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) nsim,' Simultaneous equations'

do  j=mdl%loops,mdl%loope
   name = gtnmex(mdl%ienames(mdl%order(j)),mdl%enames,ldum)
   call namxrf(name,1,0,mxelen,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
enddo

call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
if( ios .gt. 0 ) goto 990

write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) nepi,' Epilogue equations'

do  j=mdl%loope+1,nre
   name = gtnmex(mdl%ienames(mdl%order(j)),mdl%enames,ldum)
   call namxrf(name,1,0,mxelen,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
enddo

call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
if( ios .gt. 0 ) goto 990
write(xrfunt,'(a/)',err=990,iostat=ios) ' *** Feedback variables ***'
write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) mdl%nfb,' Feedback variables'

do  j=1,mdl%nfb
   name = gtnmex(mdl%ivnames(mdl%numfb(j)),mdl%vnames,ldum)
   call namxrf(name,1,0,mxvlen,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
enddo

call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
if( ios .gt. 0 ) goto 990

!     show length of feedback chain
if (mdl%fboflg .gt. 0 .and. mdl%fbomem > 0) then

   call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   write(xrfunt,'(a/a/a)',err=990,iostat=ios) &
&  ' *** Additional information for feedback variables', &
&  '     Name of feedback variable' // ' followed by how it was chosen:' // &
&  ' fixed, diagonal, heuristic,', '     and the length of the feedback cycle' // &
&  ' (excluding feedback variables)'

   if( fbcopt(1) .eq. 1 ) then
      write(xrfunt,'(a/)',err=990,iostat=ios) &
&     '     followed by the names of equations ' // 'in the feedback cycle'
   else
      write(xrfunt,'(/)',err=990,iostat=ios)
   endif

   do  j=1,mdl%nfb
      name = gtnmex(mdl%ivnames(mdl%numfb(j)),mdl%vnames,ldum)
      call namxrf(name,0,0,mxvlen,ios,lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990
      if (mdl%fbtype(j) .eq. 1 ) then
        call namxrf('fixed',1,12,12,ios,lp,line,lenxrf,xrfunt)
      elseif (mdl%fbtype(j) .eq. 2 ) then
        call namxrf('diagonal',1,12,12,ios,lp,line,lenxrf,xrfunt)
      elseif (mdl%fbtype(j) .eq. 3 ) then
        call namxrf('heuristic',1,12,12,ios,lp,line,lenxrf,xrfunt)
      else
        call namxrf('?',1,4,4,ios,lp,line,lenxrf,xrfunt)
      endif
      if( ios .gt. 0 ) goto 990

      write(string,'(i6)') mdl%fboptr(j+1)-mdl%fboptr(j)
      call namxrf(string,1,8,8,ios,lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990

      if( fbcopt(1) .eq. 1 ) then
!             !! print names of equations in feedback chain

          maxcln = fbcopt(2)
          p = mdl%fboptr(j)
          cnt = mdl%fboptr(j + 1) - mdl%fboptr(j)
          leadsp = ((mxvlen-1)/4 + 1)*4 + 12 + 8 + 2 + 1

          if( cnt .le. maxcln) then
              do  i=p,p+cnt-1
                  name = gtnmex(mdl%ienames(mdl%fbordr(i)), mdl%enames,ldum)
                  call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
                  if( ios .gt. 0 ) goto 990
              enddo
          else
              tcnt = maxcln/2
              do  i=p,p+tcnt-1
                  name = gtnmex(mdl%ienames(mdl%fbordr(i)), mdl%enames,ldum)
                  call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
                  if( ios .gt. 0 ) goto 990
              enddo

              if(tcnt .gt. 1 ) then
                  call namxrf('....',1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
              else
                  call namxrf('....',1,leadsp,4,ios, lp,line,lenxrf,xrfunt)
              endif
              if( ios .gt. 0 ) goto 990

              p = mdl%fboptr(j+1) - tcnt
              do  i=p,mdl%fboptr(j+1)-1
                  name = gtnmex(mdl%ienames(mdl%fbordr(i)), mdl%enames,ldum)
                  call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
                  if( ios .gt. 0 ) goto 990
              enddo

          endif

      endif

   enddo

   call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990

elseif( prifbi ) then

   call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   write(xrfunt,'(a/a/a/)',err=990,iostat=ios) &
&  ' *** List of feedback variables ***', &
&  '     each name followed by how it was chosen: ' // &
&  'fixed, diagonal, heuristic'

   do  j=1,mdl%nfb
      name = gtnmex(mdl%ivnames(mdl%numfb(j)),mdl%vnames,ldum)
      call namxrf(name,0,0,mxvlen,ios,lp,line,lenxrf,xrfunt)
      if( ios .gt. 0 ) goto 990

      if( mdl%fbtype(j) .eq. 1 ) then
        call namxrf('fixed',1,10,10,ios,lp,line,lenxrf,xrfunt)
      elseif( mdl%fbtype(j) .eq. 2 ) then
        call namxrf('diagonal',1,10,10,ios,lp,line,lenxrf,xrfunt)
      elseif( mdl%fbtype(j) .eq. 3 ) then
        call namxrf('heuristic',1,10,10,ios,lp,line,lenxrf,xrfunt)
      else
        call namxrf('?',1,4,4,ios,lp,line,lenxrf,xrfunt)
      endif
      if( ios .gt. 0 ) goto 990
   enddo

   call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990

endif

if( prisjc .and. mdl%nfb > 0) then
   call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
   if( ios .gt. 0 ) goto 990
   write(xrfunt,'(a/)',err=990,iostat=ios) &
&  ' *** Symbolic rhs jacobian of feedback variables ***'
    call mcosjs(xrfunt,fbsjac,mdl%nfb,mdl%nfb)
    call mcosjc(xrfunt,lenxrf,fbsjac,mdl%nfb,mdl%nfb,mdl%ivnames, &
&               mdl%vnames,mdl%numfb)
endif

!     Done
goto 1000

900 continue
errstr = 'Insufficient memory for cross-reference'
ier = 1
goto 999

910 continue
errstr = 'cannot open mrf file for write'
ier = 2
filerr = 6
filios = ios
goto 999

930 continue
errstr = 'Mcxref: internal error 2'
ier = 5
goto 999

940 continue
errstr = 'Mcxref: internal error 3'
ier = 6
goto 999

970 continue
errstr = 'Mcxref: cannot allocate memory'
ier = 7
goto 999

990 continue
errstr = 'write error on mrf file'
ier = 3
filerr = 13
filios = ios
goto 999

999 continue

1000 continue
close(xrfunt)

deallocate(ixref, stat = idum)
deallocate(ieref, stat = idum)
deallocate(ipref, stat = idum)
deallocate(ivref, stat = idum)
deallocate(equno, stat = idum)
deallocate(fbsjac, stat = idum)
return
end subroutine write_xref

!-----------------------------------------------------------------------

subroutine addxrf(iequ, nsimv, isimv, nxref, maxref, ivref, ixref, ieref, *, *)
use model_params
integer, intent(in) :: iequ, nsimv, isimv(*), maxref
integer, intent(inout) :: nxref
integer(kind = MC_IKIND), intent(inout) :: ivref(*), ixref(*), ieref(*)

integer ::  j, k, iref

do  k=1,nsimv
   nxref = nxref + 1
   if(nxref .gt. maxref) goto 910
   ixref(nxref) = 0
   ieref(nxref) = iequ
   iref = ivref(isimv(k))
   if(iref .eq. 0) then
      ivref(isimv(k)) = nxref
   else
      do  j=1,maxref
         if(ixref(iref) .eq. 0) then
            ixref(iref) = nxref
            goto 100
         else
            iref = ixref(iref)
         endif
      enddo
!           Internal error
      goto 920
   endif
100 continue
enddo

return
910 return 1
920 return 2
end subroutine addxrf

!-----------------------------------------------------------------------

subroutine namxrf(name,icode,ncsp,minwid,ios, lp,line,lenxrf,xrfunt)

!     writes name to cross reference file
!     trailing blanks are truncated so remaining length is
!     smallest possible multiple of 4
!     if icode.eq.0  a new line is started, otherwise name is appended until
!     current line is full and dumped.

!     Fortran IO error code in ios (0 implies all ok)

!     continuation lines start with ncsp spaces
!     if icode.eq.0  and name.eq.' ', then remainder is dumped and empty
!     line written

integer ::  icode, ncsp, minwid,ios

integer ::  lp, lenxrf,xrfunt
character*(*) line

character*(*) name
integer ::   n4blank
integer ::   lenstr

n4blank = ( (lenstr(name)-1)/4 + 1 )*4
if(icode .eq. 0) then
   if(lp .gt. 0) write(xrfunt,'(a)',err=990,iostat=ios) line(:lp)
   if(name .eq. ' ') then
      lp = 0
      write(xrfunt,*,err=990,iostat=ios)
   else
      line = ' '//name
      lp = 1 + max(n4blank,minwid)
   endif
else
   if(lp+1+max(n4blank,minwid) .gt. lenxrf) then
      write(xrfunt,'(a)',err=990,iostat=ios) line(:lp)
      line = ' '
      lp = ncsp
   endif
   line(lp+2:) = name
   lp = lp + 1 + max(n4blank,minwid)
endif

990 return
end subroutine namxrf

end module mcxref
