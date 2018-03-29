module msjcot
    use msimot
    use msvars

    contains

subroutine jacot1(iv, ivc)
use mdl_name_utils

!     print message for problems with calculating newton matrix
!     iv is index of feedback variable causing problem
!     ivc is the index of the feedback variable column

integer, intent(in) :: iv
integer(kind = MC_IKIND), intent(in) :: ivc

call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)

str = 'Invalid numbers obtained while computing Newton matrix'
call strout(O_ERRQ)
write(str, '(2a)' ) 'Problem first detected for variable ', name(:nlen)
call strout(O_ERRQ)
call mcf7ex(name, nlen, mdl%ivnames(ivc), mdl%vnames)
write(str, '(2a)' ) 'While calculating derivative for ', name(:nlen)
call strout(O_ERRQ)
str = 'Further calculation is useless'
call strout(O_ERRF)

return
end subroutine jacot1

!-----------------------------------------------------------------------

subroutine jacot2(matitr,itr,rcond)

! print message for calculating newton matrix
! if output options require it

integer ::  matitr, itr
real(kind = SOLVE_RKIND) :: rcond

if (repopt >= REP_PERIOD) then
   write(str, 901, round = 'compatible') matitr, itr, rcond
   call strout(O_OUTN)
endif

901 format(i4, ' steps for Newton matrix at iteration', i4, &
&      ' (1/condscal = ', 1p,e9.2, ')' )

return
end subroutine jacot2

!-----------------------------------------------------------------------

subroutine jacot3(itr)
use msvars

!     print jacobian matrix for a feedback set

integer ::  itr

character(len = 80) :: jachdr

if (repopt == REP_NONE) return

write(jachdr,'(a,i4,2a)') 'Newton Jacobian matrix at iteration ',itr, &
&           ' in period ',perstr

call matotn(jacob, mdl%nfb, mdl%nfb, mdl%nfb, mdl%numfb, mdl%numfb, jachdr)

return
end subroutine jacot3

!-----------------------------------------------------------------------

subroutine jacot4(ifbk)
use msvars
use mdl_name_utils
integer, intent(in) :: ifbk

!     print message about locally dependent jacobian
!     caused by variable with index iv

integer ::        iv

iv = mdl%numfb(ifbk)

call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)

str = '** Serious error: equation system is locally dependent'
call strout(O_ERRQ)

write(str,'(2a)') 'Feedback variable ',name(:nlen)
call strout(O_ERRQ)

str = 'is not independent of preceding feedback variables'
call strout(O_ERRQ)

str = '===> Try another scaling method or no scaling'
call strout(O_ERRQ)

str = 'Further iterations are useless'
call strout(O_ERRF)

return
end subroutine jacot4

!-----------------------------------------------------------------------

subroutine jacot5(ier)

! print message about (probably) dependent jacobian
! not same as previous (called at a different stage)

integer ::  ier

if(ier .ne. 4) then

   str = '** Serious error: Newton matrix is singular'
   call strout(O_ERRQ)

   str = '... equations are dependent ...'
   call strout(O_ERRF)

else

   str = '** Warning: Newton matrix is nearly singular'
   call strout(O_ERRQ)
   str = '... equations are probably dependent ...'
   call strout(O_ERRF)

endif

return
end subroutine jacot5

!-----------------------------------------------------------------------

subroutine jacot6(rcond)

! print message for ill conditioned/singular broyden update

real(kind = solve_RKIND), intent(in) :: rcond

write(str, 901, round = 'compatible') rcond
call strout(O_OUTN)

901 format(' Broyden update becomes singular ', ' (1/condscal = ', 1p,e9.2, ')' )

return
end subroutine jacot6

!-----------------------------------------------------------------------

subroutine jacot7

!        print message about insufficient memory to allocate the Jacobian

   str = 'Not enough memory to allocate the Jacobian'
   call strout(O_ERRF)

end subroutine jacot7

end module msjcot
