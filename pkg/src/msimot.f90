module msimot
    use msvars
    use mdl_nlen

    ! all output for model simulation

    ! solve report option. The option is initialised to opts%repopt
    ! in simot1, but can be changed by subroutine fitot0 in msfit0.f90.
    integer, save :: repopt

    ! The following routines system dependent
    !     strout  output string buffer

    ! output buffer types (determines output stream)
    integer, parameter :: O_WMSG = 1, &
                     O_ERRM = 2, O_ERRQ = 3, O_ERRF = 4, &
                     O_OUTN = 5, O_OUTB = 6, O_OUTF = 7, &
                     O_LOGQ = 8

    integer, save :: errcnt
    character(len = 12), save :: perstr  ! current period string

    ! 1 line output buffer and current position
    integer, parameter, private :: STRLEN = 255
    character(len = STRLEN), save ::  str

    integer, save :: spos

    ! for extracting variable name
    character(len = MCMXNM), save :: name
    integer, save :: nlen

    ! for printing numbers
    ! width is 12 and autoswitch between f12.5/e12.5
    ! intercolumn space is 1
    ! Warning:   if NUMWID is changed, also change numeric formats in msimot
    !                and recompile
    ! MaxIntWid: maximum number of digits printed before decimal point
    ! MinNoEfmt: minimum number of digits printed if number is printed
    !                using E format.
    integer, parameter :: NUMWID = 12, ICSPAC = 1, &
&                     MaxIntWid = 7, MinNoEfmt = 3, LWIDTH = 80

    private :: maxperlen


contains

    subroutine strout(otype)
        use output_utils
        integer, intent(in) :: otype

        ! handle output in this routine

        ! variable outerr (common) set to 1 for stop after more prompt
        !                                 2 for write errors

        ! otype     description                      destination
        ! -------   -------------------------------  ----------------------
        ! O_WMSG    informative warning message      stdout  no more prompt

        ! O_ERRM    informative error message        stdout     more prompt
        !           (1 of many)                                 stop simul
        !                                            curout  no more prompt
        ! O_ERRQ    same as O_ERRM with more to come stdout  no more prompt

        ! O_ERRF    final error message              stdout  no more prompt

        ! O_OUTN    unbuffered (immediate) output    stdout  no more prompt
        !                                            curout

        ! O_OUTB    buffered output                  curout  no more prompt
        !                                            stdout     more prompt
        !                                                       stop print

        ! O_OUTF    final unbuffered output          stdout  no more prompt
        !            also a copy is made to curout
        !            if curout <> stdout

        ! O_LOGQ    same as O_ERRQ to isis logfile

        ! carcon is used for FTN carriage control


        call isismdl_out(str)
    
        select case (otype)
        case (O_ERRM, O_ERRF, O_ERRQ)
            errcnt = errcnt + 1
        end select

        return
    end subroutine strout

    !--------------------------------------------------------------------------

    subroutine strini( istr, start )
        character(len = *), intent(in) :: istr
        integer, intent(in) ::   start
        ! initialize internal string buffer to istr and set spos to start
        ! if start == 1 initialize to blank and 1
    
        if (start == 1 ) then
            str  = ' '
            spos = 1
        else
            str  = istr
            spos = start
        endif
    
        return
    end subroutine strini

    !--------------------------------------------------------------------------


subroutine snmfmt(names, namptr, nidx, ib, ie, colwid)
    use model_params
    use mdl_name_utils
    integer(kind = MC_IKIND) names(*), namptr(*), nidx(*)
    integer, intent(in) :: ib, ie
    integer, intent(in) :: colwid

!         output a sequence of names to put above row of numbers
!         into string buffer
!         properly justified

integer ::         rpos, i

    do i = ib, ie
       call mcf7ex(name ,nlen, namptr(nidx(i)), names)
       rpos = spos + colwid - nlen
       str(rpos : rpos + nlen - 1) = name(:nlen)
       spos = spos + colwid + ICSPAC
    end do

    return
end subroutine snmfmt

!-----------------------------------------------------------------------

subroutine msnmot(cnt, vnum, ivnames, vnames,hdr)

!     output a sequence of names with a header in <hdr>

use model_params
use mdl_name_utils
integer(kind = MC_IKIND) :: cnt, vnum(*),ivnames(*),vnames(*)
character*(*) hdr

integer ::  i, j, ib, ie, cstep
integer ::  colwid

colwid = maxnli(ivnames, vnum, 1_MC_IKIND, cnt)
cstep  = max(1, (LWIDTH-6) / (colwid + 1))

call strini( ' ', 1 )
call strout(O_OUTB)

write(str,'(1x,a)') hdr
call strout(O_OUTB)

do j = 1, cnt, cstep

   ib = j
   ie = min(cnt, ib + cstep - 1)
   call strini(' ', 6)

   do i=ib,ie
      call mcf7ex(name, nlen, ivnames(vnum(i)), vnames)
      str(spos : spos + nlen - 1) = name(:nlen)
      spos = spos + colwid + 1
   enddo

   call strout(O_OUTB)

enddo

return
end subroutine msnmot

!-----------------------------------------------------------------------

subroutine sperfmt(jb, je, colwid)

! Writes a series of periods with number indices jb..je to the
! string buffer right justified.

integer, intent(in) :: jb, je, colwid
integer             :: rpos, j

do j = jb, je
   nlen = maxperlen()
   rpos = spos + colwid - nlen
   call sjttmp(str(rpos : rpos + nlen - 1), j)
   spos = spos + colwid + ICSPAC
enddo
return
end subroutine sperfmt

!-----------------------------------------------------------------------

subroutine nvlfmt( x, numstr )
use nucnst
use nuna
! basic number format routine
! fixed width and switch to E format if abs(x) too big/small

real*8      x
character*(*) numstr

character*10 EFMT, FFMT
parameter( EFMT = '(1P,E12.5)' )
parameter( FFMT = '(F12.5)'    )
real*8     TOOBIG, TOOSML
parameter (TOOBIG = 1.0D5, TOOSML = 1.0D-5)

logical ::   usefmt
real*8   z

usefmt(z) = abs(z) .ge. TOOBIG .or. (abs(z) .gt. Rzero .and. abs(z) .le. TOOSML)

if (.not. nuifna(x) ) then
   if( usefmt(x) ) then
      write(numstr, EFMT, round = 'compatible') x
   else
      write(numstr, FFMT, round = 'compatible') x
   endif
else
   numstr = 'Invalid'
endif

return
end subroutine nvlfmt

!-----------------------------------------------------------------------

subroutine nvlvfmt( x, numstr, numWidV, intWid, nodecim, noEfmt)
use nuna
! Prints x as FnumWidV.nodecim or Inumwid if nodecim = 0.
! intWid is the maximum number of printed digits before the decimal
! point.
! If the number is too big or small, then the number is printed using
! ES format with noEfmt printed numbers.

real*8, intent(in) ::  x
integer, intent(in)         :: numWidV, intWid, nodecim, noEfmt
character*(*), intent(out)  :: numstr
logical                     :: uffmt
character*9                 :: formt
character*2                 :: f
character*3                 :: d

if (nuifna(x))  then
   ! number is NA
    numstr(numWidV:numWidV) = '.'
    return
endif

uffmt = useFfmt(x, intWid, nodecim)

if (uffmt .and. nodecim .eq. 0) then
    write(f, '(A2)') 'I'
else if (uffmt) then
    write(f, '(A2)') 'F'
else
    write(f, '(A2)') 'ES'
ENDIF

if (uffmt) then
    if (nodecim .eq. 0) then
        write(d, '(A3)') '   '
    else if (nodecim .lt. 10) then
        write(d, '(A1, I1)') '.', nodecim
    else
        write(d, '(A1, I2)') '.', nodecim
    endif
else
    if (noEfmt - 1 .lt. 10) then
        write(d, '(A1, I1)') '.', max(0, noEfmt - 1)
    else
        write(d, '(A1, I2)') '.', max(0, noEfmt - 1)

    endif
endif

if (numWidV .lt. 10) then
    write(formt, '(A1, A2, I1, A3, A1)') '(',  f,  numWidV, d,  ')'
else
    write(formt, '(A1, A2, I2, A3, A1) ') '(',  f,  numWidV, d,  ')'
endif

if (f .eq. ' I') then
   write(numstr, formt) nint(x)
else
   write(numstr, formt) x
endif
return
end subroutine nvlvfmt

!-----------------------------------------------------------------------

subroutine svlfmt(val, ib, ie, colwid)

real*8        val(*)
integer ::        ib, ie
integer ::        colwid

!     companion to nvlfmt routine
!     print numbers in specified width with f format
!     if too big/small switch to e-format
!     right justified within column width

integer ::  i, rpos

do  i=ib,ie
   rpos = spos + colwid - numwid
   call nvlfmt( val(i), str(rpos : rpos + numwid - 1))
   spos = spos + colwid + icspac
enddo

return
end subroutine svlfmt

!-----------------------------------------------------------------------

subroutine svlvfmt(val, ib, ie, colwid, numWidV, intWid, nodecim, noEfmt)
!     companion to nvlvfmt name routine
!     print numbers in specified width with F format
!     if too big/small switch to E-format
!     right justified within column width
real*8, intent(in) ::  val(*)
integer, intent(in) :: ib, ie
integer, intent(in) :: colwid, numWidV, intWid, nodecim
integer, intent(in) :: noEfmt

integer ::  i, rpos

do  i=ib,ie
   rpos = spos + colwid - numWidV
   call nvlvfmt( val(i), str(rpos : rpos + numWidV - 1), &
&              numWidV, intWid, nodecim, noEfmt)
   spos = spos + colwid + ICSPAC
enddo

return
end subroutine svlvfmt

    !--------------------------------------------------------------------------

    subroutine sjttmp(datstr, jper)
        character(len = *), intent(out) :: datstr
        integer, intent(in) :: jper
        ! writes period name in datstr (including year indicator)
        ! jper is period index
        ! (1 is first in period defined by iyfd and ipfd)
        
        integer :: year, subper, nsub, freq
        character(len = 1)::  cfreq
        character(len = 1), dimension(12), save ::  frqext
        
        data frqext / 'Y', 'H', '?', 'Q', 7 * '?', 'M' /
        
        datstr = ''
            
        freq = mws%start_period%frequency
        nsub = mws%start_period%year * freq + mws%start_period%subperiod &
                   - 1 + jper - 1
        year  = nsub  / freq
        
        if (freq == 1) then
           write(datstr, '(I4)' ) year
        else
           subper = mod(nsub, freq) + 1
           cfreq  = frqext(freq)
           if (freq < 10) then
              write(datstr, '(I4,A1,I1)') year,  cfreq, subper
           else
              write(datstr, '(I4,A1,I2)') year,  cfreq, subper
           endif
        endif
        
        return
    end subroutine sjttmp

    !--------------------------------------------------------------------------


    integer function maxperlen()
        !  returns the maximum length of a period name written by sjttmp
        if (mws%start_period%frequency == 1) then
            maxperlen = 4
        else if (mws%start_period%frequency < 10) then
            maxperlen = 7
        else
            maxperlen = 8
        endif
        return
    end function maxperlen

    !--------------------------------------------------------------------------

    subroutine sjcstr(jper)
        integer, intent(in) :: jper
        ! sets period string for output system in
        ! variable perstr in common from mpsdat
        ! jper is period index
        ! (1 is first in period defined by iyfd and ipfd)
        call sjttmp(perstr, jper)
    return
    end subroutine sjcstr

    !--------------------------------------------------------------------------

    subroutine simot1
        !initialize output controlling variables
        errcnt = 0
        return
    end subroutine simot1

    !--------------------------------------------------------------------------

subroutine simot2

! a harmless warning only for unavailable initial feedback values

if (repopt == REP_NONE) return

str = 'Warning - previous period initial feedback values'
call strout(O_WMSG)
str = 'in first period of simulation not available'
call strout(O_WMSG)
str = 'Using current period values'
call strout(O_WMSG)

return
end subroutine simot2

!-----------------------------------------------------------------------

subroutine simot3(tuse, itrtot, ndiver)

!     print a summary report of status of a simulation

real    ::  tuse
integer ::  itrtot, ndiver

if (repopt == REP_NONE) return

if( itrtot .le. 9999 ) then
    write(str,'(A,I4)' ) 'Total number of iterations ',itrtot
else if( itrtot .le. 99999 ) then
    write(str,'(A,I5)' ) 'Total number of iterations ',itrtot
else
    write(str,'(A,I8)' ) 'Total number of iterations ',itrtot
endif

call strout(O_OUTF)

if(tuse .lt. 0.0) tuse = tuse - 24.0*60.0*60.0

write(str,'(a,f8.2,a)', round = 'compatible') &
&       'Solve model used ', tuse,' CPU secs'
call strout(O_OUTF)

if (ndiver > 0) then
   write(str,'(a,i4,a)') 'No convergence occurred in ',ndiver,' periods'
   call strout(O_OUTF)
endif

call strini(' ', 1)
call strout(O_OUTN)

return
end subroutine simot3

!-----------------------------------------------------------------------

subroutine simot4(iv, p)
use mdl_name_utils

! print message for invalid/missing exogenous variable
! iv is variable index

integer ::   iv, p

call sjcstr(p)
call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
write(str,'(4a)') 'First missing or invalid value for exogenous variable ', &
&  name(:nlen), ' in period ', perstr
call strout(O_ERRM)

return
end subroutine simot4

!-----------------------------------------------------------------------

subroutine simot5(iv, p)
use mdl_name_utils

!     print message for invalid/missing constant adjustment
!     vnptr is pointer to name of endogenous variable in vnames

integer ::   iv, p

call sjcstr(p)
call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
write(str,'(4a)') 'First missing or invalid value for CA of ',name(:nlen), &
&  ' in period ', perstr
call strout(O_ERRM)

return
end subroutine simot5

!-----------------------------------------------------------------------

subroutine nshfmt(name, nmlen, shift, dst, dlen)

!     append lag/lead indicator shift to variable name <name>
!     nlen is exact length of name

!     result is stored in dst with exact length dlen

character*(*) name
integer ::        nmlen,shift
character*(*) dst
integer ::        dlen

character*8   temp
integer ::        shfwid
integer ::        i

write(temp, '(SP,I8)' ) shift
!     skip leading whitespace
do  i=1,8
   if(temp(i:i) .ne. ' ' ) goto 20
enddo
20 shfwid = 8 - i + 1

dst(                   : nmlen                  ) = name(:nmlen)
dst(nmlen + 1          : nmlen + 1              ) = '('
dst(nmlen + 2          : nmlen + 2 + shfwid - 1 ) = temp(i:8)
dst(nmlen + 2 + shfwid : nmlen + 2 + shfwid + 1 ) = ')'

dlen = nmlen + 2 + shfwid

return
end subroutine nshfmt

!-----------------------------------------------------------------------

subroutine matotn(x, xdim, nxr, nxc, ridx, cidx, mathdr)
use mdl_name_utils
use nucnst

! output a matrix x(nxr,nxc) nicely with a header mathdr
! matrix x declared as x(xdim,*)
! inames name pointer array
! names  name string memory

! ridx   contains numbers of variables to print in rows
! cidx   contains numbers of variables to print in columns


real*8        x(xdim,*)
integer(kind = MC_IKIND) :: xdim, nxr, nxc, ridx(*), cidx(*)
character*(*) mathdr

integer ::    colwid, maxrhl, maxchl
integer ::    ncpb,ncpe,npcols,i,j
real*8        temp(20)

!     colwid  column width
!     maxrhl  length of longest row    header
!     maxchl  length of longest column header
!     npcols  number of numeric columns in each strip
!     ncpb    index of first column to print
!     ncpe    index of last  column to print

maxrhl = maxnli(mdl%ivnames, ridx, 1_MC_IKIND, nxr)
maxchl = maxnli(mdl%ivnames, cidx, 1_MC_IKIND, nxc)

colwid = max(maxchl, NUMWID)

npcols = max(1, (LWIDTH - maxrhl - 1) / (colwid + ICSPAC) )
npcols = min(npcols, nxc, 20)

!     write matrix header

call strini(' ', 1)
call strout(O_OUTB)

str = mathdr
call strout(O_OUTB)

call strini(' ', 1)
call strout(O_OUTB)

do  ncpb = 1, nxc, npcols

     ncpe = min(ncpb + npcols - 1, nxc)

!          write col headers (right justified)

     call strini(' ', 1 + maxrhl + ICSPAC)
     call snmfmt(mdl%vnames, mdl%ivnames, cidx, ncpb, ncpe, colwid)
     call strout(O_OUTB)

     do  i=1,nxr

!             write row header (left justified)

        call mcf7ex(name, nlen, mdl%ivnames(ridx(i)), mdl%vnames)
        call strini(name(:nlen),1 + maxrhl + ICSPAC)

!             write x(i,j=ncpb..ncpe) (right justified)

        do  j = ncpb, ncpe
           temp(j-ncpb+1) = x(i,j)
        enddo
        call svlfmt(temp, 1, ncpe-ncpb+1, colwid)
        call strout(O_OUTB)

     enddo
enddo

return
end subroutine matotn

! -----------------------------------------------------------------

integer function minNumWid(x, intWid, nodec)
use nucnst
use nuna

! Returns the smallest possible numWidV, the number of characters
! used to print a number. Function useFfmt determines whether
! the number is printed in F or E format.

implicit none
real*8, intent(in)  :: x
real*8              :: xref
integer, intent(in) :: intWid, nodec
integer             :: numWidV, noEfmt, pointWid


!     numWidV    field width for printing numbers
!     pointWid  number of characters required to represent a point
!                (zero if no decimals are printed, 1 otherwize).
!     noEfmt    number of digits printed when ES format is used.

numWidV  = intWid + nodec + 1
noEfmt = max(MinNoEfmt, nodec + 1)

if (nodec .eq. 0) then
    pointWid = 0
else
    pointWid = 1
endif

xref    = Rten**(intWid - 1)

if (nuifna(x)) then
   minNumWid = numWidV
   return
endif

if (.not. useFfmt(x, MaxIntWid, nodec)) then
   if (x .lt. Rzero) then
       numWidV = max(numWidV,noEfmt + 6 )
   else
       numWidV = max(numWidV,noEfmt + 5)
   endif
else if (x .le. Rzero) then
       if (x .le. -xref .or. (intWid .eq. 1 .and. x .gt. -Rone)) then
           numWidV = max(numWidV, intWid + nodec + pointWid + 1)
       endif
endif

minNumWid = numWidV

return
end function minNumWid

!-----------------------------------------------------------------------

logical function useFfmt(x, intWid, nodec)
use nucnst

! Determine whether a number can be printed using F format
! with intWid digits before the decimal point and nodec decimals.
! If false, then the number is too small or too big to be printed
! using this format.

implicit none
integer, intent(in) :: intWid, nodec
real*8, intent(in)  ::  x
real*8              :: toobig, toosml

toobig = Rten**(intWid)
toosml = Rten**(-nodec)

useFfmt =  (dabs(x) .lt. toobig .and. dabs(x) .ge. toosml) .or. &
&           dabs(x) .eq. Rzero
return
end function useFfmt

!-----------------------------------------------------------------------

       subroutine print_solve_options
            integer :: pos, tab_pos

            character(len = 20) :: tmp_string

            if (repopt == REP_NONE) return
    
            tab_pos = 31

            str = ""
            call strout(O_OUTB)
            str = "Model Solve Options"
            call strout(O_OUTB)

            str = "Solution period"
            call sjttmp(str(tab_pos:), jf)
            pos = len_trim(str)
            str((pos + 1):(pos + 1)) = "/"
            pos = pos + 1
            call sjttmp(str(pos + 1:), jl)
            call strout(O_OUTB)
    
            call print_option_txt("Simulation mode", get_mode_text(opts%mode), &
                                  tab_pos)
            call print_option_txt("Feedback starting values", &
                              get_start_text(opts%start), tab_pos)
            call print_option_int("Maximum iterations per period", opts%maxit, &
                                 tab_pos)
            call print_option_real("Relaxation minimum", opts%rlxmin, tab_pos)
            call print_option_real("           maximum", opts%rlxmax, tab_pos)
            call print_option_real("           shrinkage", opts%rlxspeed, &
                                  tab_pos)
            call print_option_real("Criteria stepback", opts%cstpbk, tab_pos)
            call print_option_real("          matrix", opts%cnmtrx, tab_pos)

            tab_pos = 58
            call print_option_int("Maximum updates Newton matrix per period", &
                                  opts%maxmat, tab_pos)
            call print_option_int(&
                 "Maximum number of line searches with old Jacobian", &
                                  opts%bktmax, tab_pos)
            call print_option_txt("Criterion for line search decisions etc.", &
                  get_arith_text(opts%arith), tab_pos)

            str = " "
            call strout(O_OUTB)

            contains
                subroutine print_option_txt(option_txt, option_text, tab_pos)
                    character(len = *), intent(in):: option_txt, option_text
                    integer, intent(in):: tab_pos
                    str = option_txt
                    str(tab_pos:) = option_text
                    call strout(O_OUTB)
                end subroutine print_option_txt

                subroutine print_option_int(option_txt, ival, tab_pos)
                    character(len = *), intent(in):: option_txt
                    integer, intent(in):: ival, tab_pos
                    str = option_txt
                    write(tmp_string, *) ival
                    str(tab_pos:) =  adjustl(tmp_string)
                    call strout(O_OUTB)
                end subroutine print_option_int

                subroutine print_option_real(option_txt, rval, tab_pos)
                    character(len = *), intent(in):: option_txt
                    real(kind = ISIS_RKIND), intent(in):: rval
                    integer, intent(in):: tab_pos
                    str = option_txt
                    write(tmp_string, '(G10.3)') rval
                    str(tab_pos:) =  adjustl(tmp_string)
                    call strout(O_OUTB)
                end subroutine print_option_real

        end subroutine print_solve_options


end module msimot
