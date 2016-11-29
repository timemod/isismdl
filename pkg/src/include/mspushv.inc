!     Push a variable on the stack
!     This piece of code in used in subroutine evaleq.
!     NOTE: this code could also be implemented as an internal
!     subprogram of subroutine evaleq. However, it turns out
!     that that implementation runs much slower on Linux.

nstack = nstack + 1
typstk(nstack) = T_REAL

if (solve_model) then

   ! running a model so load data immediately
   !assume value is ok (normal)
   if (jshift == 0) then
       stack(nstack) = rhsvars(kxar)
   else if (jshift < 0) then
       stack(nstack) = lags_leads(mdl%ibx1(kxar) - jshift - 1)
   else
      stack(nstack) = lags_leads(mdl%ibx2(kxar) + jshift - 1)
   endif

else

    ! Not running a model so load data through function safely
    ! and see if value is valid

    call get_var_value(mws, kxar, jtime + jshift, stack(nstack), error)
    pushvar_err = nuifna(stack(nstack))

endif
