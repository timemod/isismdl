!     Push a formal argument on the stack.
!     This piece of code in used in subroutine evaleq.
!     NOTE: this code could also be implemented as an internal
!     subprogram of subroutine evaleq. However, it turns out
!     that that implementation runs much slower on Linux.

iarg = nstuf + mws%mdl%equat(ip)

if (typstk(iarg) .eq. T_VAR) then ! variable

    kxar = istack(iarg)
    jshift = jshift + ostack(iarg) + jdel
    include "mspushv.inc"

else  ! parameter

    nstack = nstack + 1
    typstk(nstack) = T_REAL
    kxar = istack(iarg)
    jshift = jshift + ostack(iarg)
    stack(nstack) = mws%mdl%coef(mws%mdl%cfptr(kxar) - jshift)

endif


