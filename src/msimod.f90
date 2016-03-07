module msimod
use msvars
use msimot
use mdl_name_utils

contains

subroutine simod1(iv, jlag)

!     print missing or invalid lag message for a variable
!     iv is the variable index

integer ::   iv
integer ::   jlag

character*80  dst
integer ::        dlen

call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
call nshfmt(name, nlen, -jlag, dst, dlen)

write(str,'(2a)') 'Missing or invalid value for ', dst(:dlen)

call strout(O_ERRM)

return
end subroutine simod1

!-----------------------------------------------------------------------

subroutine simod2(iv, jlead)
! print missing or invalid lead message for a variable
!  iv is the variable index

integer ::  jlead
integer ::  iv

character*80  dst
integer ::        dlen

call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
call nshfmt(name, nlen, jlead, dst , dlen)

write(str,'(2a)') 'Missing or invalid value for ', dst(:dlen)

call strout(O_ERRM)

return
end subroutine simod2

!-----------------------------------------------------------------------

subroutine simod3(nonval)

!     print total of invalid/missing lags/leads
!     alternate return 1 for quit simulation

integer ::  nonval

write(str,'(i5,2a)') &
&    nonval,' missing or invalid lags or leads in period ', perstr
call strout(O_ERRF)

return
end subroutine simod3

!-----------------------------------------------------------------------

subroutine simod4(p,i)

!     print message for invalid/missing parameter
!     p is parameter index
!     i is index of coefficient for the parameter

integer ::  p, i

character*80  dst
integer ::        dlen

call mcf7ex(name, nlen, mdl%ipnames(p), mdl%pnames)
if (mdl%nvalp(p) .eq. 1) then
  write(str,'(2a)') 'Missing or invalid value for parameter ', name(:nlen)
else
   call nshfmt(name, nlen, -i+1, dst, dlen)
  write(str,'(2a)') 'Missing or invalid value for parameter ', dst(:dlen)
endif

call strout(O_ERRM)

return
end subroutine simod4

end module msimod
