module mscalc
use mws_type

contains

   subroutine modequ(retcod, result, ipequb, ipequl)
   use model_params
   use kinds
   ! Evaluates an equation when solving a model
   integer, intent(out) :: retcod
   real(kind = SOLVE_RKIND), intent(out) :: result
   integer(kind = MC_IKIND), intent(in) :: ipequb, ipequl

   call evaleq(retcod, result, ipequb, ipequl, .true.)

   end subroutine modequ

!---------------------------------------------------------------------

   subroutine onequ(retcod, result, ipequb, ipequl, jtime)

   ! Evaluates an equation outside a model.
   ! jtime the date for which the equation has to be run

   use model_params
   use kinds
   integer, intent(out) :: retcod
   integer, intent(in)  ::  jtime
   real(kind = SOLVE_RKIND), intent(out) :: result
   integer(kind = MC_IKIND), intent(in) :: ipequb, ipequl
    
   call evaleq(retcod, result, ipequb, ipequl, .false., jtime)

   end subroutine onequ

!---------------------------------------------------------------------

   subroutine evaleq(retcod, result, ipequb, ipequl, solve_model, jtime)
   use msufstack
   use kinds
   use mcedef
   use mcstak
   use msvars
   use nuv
   use nuna

!     Solves equation coded in mdl%equat starting at ipequb ending at
!     ipequl.
!     Assumes that user function and equation code are in the same array
!     with user functions at start.

!     The logical solve_model is true when solving a model or false
!     when an equation is run outside a model. If model, then
!     jtime is not used. If not model, then jtime is the date
!     for which the equation has to be run.

!     If mws%dbgeqn = .TRUE., then numerical problems are checked.

!     retcod
!      1 indicates serious error (syntax)
!      2 indicates computational error / numerical problems
!      3 indicates missing value in rhs variable

   integer, intent(out) :: retcod
   real(kind = SOLVE_RKIND), intent(out) :: result
   integer(kind = MC_IKIND), intent(in) :: ipequb, ipequl
   integer, intent(in), optional :: jtime
   logical, intent(in) :: solve_model

   !real(kind = ISIS_RKIND), external :: sdcdnm, sdivnm
   real(kind = ISIS_RKIND) :: rop1, rop2


!     symbolic constants to keep track of type of value
!     on the calculation stack
!     T_PAR and T_VAR must be > T_REAL, T_LOGIC
!     and T_PAR must be < T_VAR

   integer, parameter :: T_REAL = 1, T_LOGIC = 2, T_PAR = 3, T_VAR = 4

!     for some compilers this will be double precision

   real(kind = ISIS_RKIND), dimension(0:MAXSTK) :: stack
   integer, dimension(0:MAXSTK) :: istack, ostack, typstk

   real(kind = ISIS_RKIND), parameter :: QZERO = 0.0D0 , QONE = 1.0D0
   real(kind = ISIS_RKIND) :: tmp

   integer :: ip, ipend, iposum, ipodel
   integer :: ipcode,kxar,nstack
   integer :: jsum,mindo, maxdo,jdel,ndel
   integer :: jshift,juf,nstuf,nstuf_old,iarg
   logical :: errflg, pushvar_err
   integer :: iskerr

!     for loading real constants from equation code

   real(kind = 4) :: r4val
   real(kind = 8) :: r8val
   real(kind = SOLVE_RKIND) :: ulf_res
   integer ::   ier, narg, ldtype
   logical ::   error
   real(kind = MWS_RKIND), pointer, dimension(:) :: rhsvars

   ulf_res = NA
   jdel = 0
   maxdo = 0
   jsum = 0

   ! includes definitions for special ipcode data arrays
   ! !! all are 0-base and must be declared as such in
   ! receiving functions and subroutines
   ! weird errors will possibly result if (0:*) is omitted
   ! forcheck can't detect this
        
   if (solve_model) then
       if (mode == 'R') then
           rhsvars => yp
       else
           rhsvars => curvars
       endif
   else 
       rhsvars => null()
   endif

! evaluates an equation by running a stack machine
! for each operation code execute appropriate instructions
! see mcedef.rf9 for values and this code for interpretation

! Important variables used
!     ---------------------------
! nstack    stack pointer
! ipcode    current ipcode
! ip        instruction pointer
! ipend     last valid index for ip
! iposum    value of ip for a sum function
! ipodel    value of ip for a del function
!               Sum and Del may Not be nested !!

! nstuf     stack pointer before function was called

! jdel      current value of del function offset (0 or -ndel)
!               !! jdel is only applied to variables
!                  never to parameters
! ndel      current value of del function offset (argument of del)
! jsum      current value of sum index
! mindo     lower bound of sum index
! maxdo     upper bound of sum index

! kxar      number of variable/parameter from argument to ipcode

! jshift    shift (lead/lag) to apply to parameter/variable
!             0  implies current period value
!               < 0  implies a lag for a variable
!                            an index for a parameter (can only be < 0)
!               > 0  implies a lead for a variable

!               warning: starts as absolute value from argument of
!                        ipcode and changes depending on current context

!     Stack arrays
!     ------------
!     stack     real numbers
!     istack    holds 1 (true) and (0) for logical values
!               or address of variables or parameters for function
!               arguments passed by address
!     ostack    holds shift values for function arguments
!               passed by address
!     typstk    type indicator for the stack
!               T_REAL  real                  in stack
!               T_LOGIC logical               in istack
!               T_PAR   parameter address     in istack
!               T_VAR   variable  address     in istack

   retcod = 0
   nstack = 0
   iposum = 0
   jdel   = 0
   ipodel = 0
   jshift = 0
   iskerr = 0

   call init_ufstack

   pushvar_err  = .false.

   ipend = ipequl
   ip    = ipequb

   do while (.not. pushvar_err)

     if (mws%dbgeqn) then
         ! same topmost operands on stack (for error messages *)
         if (nstack > 0) rop1 = stack(nstack)
         if (nstack > 1) rop2 = stack(nstack -1)
     endif

     ! fetch next ipcode from byte array
     ! compress ipcode (skip unused ipcodes)
     ! increment instruction pointer

     ipcode = mdl%equat(ip)
     ip     = ip + 1

     !! ip points to byte after ipcode !!

     select case (ipcode)

     case (E_LIRL)
       ! real constant
       nstack = nstack + 1
       typstk(nstack) = T_REAL
       stack(nstack) = transfer(mdl%equat(ip), r4val)
       ip    = ip + 1

     case (E_LIRL8)
       ! real(kind = ISIS_RKIND) constant
       nstack = nstack + 1
       typstk(nstack) = T_REAL
       stack(nstack) = transfer(mdl%equat(ip:ip+1), r8val)
       ip = ip + 2

     case (E_LISUM)
       !  sum index
       nstack = nstack + 1
       typstk(nstack) = T_REAL
       stack(nstack) = jsum

     ! parameters (ipcode E_LIP0 .. E_LIP5 )
     case (E_LIP0 : E_LIP5)
       nstack = nstack + 1
       typstk(nstack) = T_REAL
       kxar = mdl%equat(ip)
       ldtype = ipcode - E_LIP0 + 1
       include "msshift.inc"
       stack(nstack) = mdl%coef(mdl%cfptr(kxar) - jshift)
       ip = ip + opskip(ipcode)

     ! variables in model (ipcode E_LIV0 .. E_LIV5 )
     case (E_LIV0 : E_LIV5)
       kxar   = mdl%equat(ip)
       ldtype = ipcode - E_LIV0 + 1
       include "msshift.inc"
       jshift = jshift + jdel
       include "mspushv.inc"
       ip     = ip + opskip(ipcode)

     ! E_LIF0: formal argument in function without lag / lead / index
     case (E_LIF0)
         iarg = nstuf + mdl%equat(ip)

         select case (typstk(iarg))

         case (T_REAL)
            nstack = nstack + 1
            typstk(nstack) = T_REAL
            stack(nstack) = stack(iarg)

         case (T_LOGIC)
            nstack = nstack + 1
            typstk(nstack) = T_LOGIC
            istack(nstack) = istack(iarg)

         case (T_PAR)
            nstack = nstack + 1
            typstk(nstack) = T_REAL
            kxar = istack(iarg)
            jshift = ostack(iarg)
            stack(nstack) = mdl%coef(mdl%cfptr(kxar) - jshift)

         case (T_VAR)
            kxar = istack(iarg)
            jshift = ostack(iarg) + jdel
            include "mspushv.inc"

       end select

       ip   = ip + opskip(ipcode);

!      E_LIF1 .. E_LIF5: function argument with lag/lead or sum index
!      the argument can be a variable or parameter
    case (E_LIF1 : E_LIF5)
       ldtype = ipcode - E_LIF0 + 1
       include "msshift.inc"
       include "mspushf.inc"
       ip = ip + opskip(ipcode)

    !          push adress of parameter (ipcode E_LAP0 .. E_LAP5)
    case (E_LAP0 : E_LAP5)
           nstack = nstack + 1
           typstk(nstack) = T_PAR
           istack(nstack) = mdl%equat(ip)
           ldtype = ipcode - E_LAP0 + 1
           include "msshift.inc"
           ostack(nstack) = jshift
           ip = ip + opskip(ipcode)

    !         push adress of variable (ipcode E_LAV0 .. E_LAV5)
    !         Do Not use jdel here (it will be applied twice)
    !         istack holds number of variable
    !         ostack holds shift (lag|lead)
    case (E_LAV0 : E_LAV5)
           nstack = nstack + 1
           typstk(nstack) = T_VAR
           istack(nstack) = mdl%equat(ip)
           ldtype = ipcode - E_LAV0 + 1
           include "msshift.inc"
           ostack(nstack) = jshift
           ip = ip + opskip(ipcode)

    !         push adress of formal argument (ipcode E_LAF0 .. E_LAF5)
    !         the actual argument can be a variable or parameter
!         istack holds number of variable
!         ostack holds shift (lag|lead)
     case (E_LAF0 : E_LAF5)
        nstack = nstack + 1
        iarg = nstuf + mdl%equat(ip)
        typstk(nstack) = typstk(iarg)
        istack(nstack) = istack(iarg)
        ldtype = ipcode - E_LAF0 + 1
        include "msshift.inc"
        ostack(nstack) = ostack(iarg) + jshift
        ip = ip + opskip(ipcode)

!         unary operators:

     case (E_NOT)
       istack(nstack) = 1 - istack(nstack)

     case (E_NEG)
       stack(nstack) = - stack(nstack)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

!         builtin 1 argument functions

     case (E_LOG)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       if( stack(nstack) .le. Qzero ) then
           iskerr = 1
           exit
       endif
       stack(nstack) = log(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_EXP)
       if( nuifna(stack(nstack)) ) then
          retcod = 3
          return
       endif
       stack(nstack) = exp(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_ABS)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = abs(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_SIN)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = sin(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_COS)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = cos(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_ATAN)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = atan(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_SQRT)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       if( stack(nstack) .lt. Qzero ) then
           iskerr = 2
           exit
       endif
       stack(nstack) = sqrt(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_NINT)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = anint(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_LOG10)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       if(stack(nstack) .le. Qzero) then
           iskerr = 1
           exit
       endif
       stack(nstack) = log10(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_TAN)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = tan(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_ASIN)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       if( abs(stack(nstack)) .gt. Qone ) then
           iskerr = 5
           exit
       endif
       stack(nstack) = asin(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_ACOS)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       if( abs(stack(nstack)) .gt. Qone ) then
           iskerr = 5
           exit
       endif
       stack(nstack) = acos(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_SINH)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = sinh(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_COSH)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = cosh(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_TANH)
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       stack(nstack) = tanh(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_TORL)
!            cast logical to number
       if( istack(nstack) .eq. 1 ) then
          stack(nstack) = Qone
       else
          stack(nstack) = Qzero
       endif
       typstk(nstack) = T_REAL

     case (E_ISVL)
!            test if top of stack is valid
       if( nuifna(stack(nstack)) ) then
          istack(nstack) = 0
       else
          istack(nstack) = 1
       endif
       typstk(nstack) = T_LOGIC

     case (E_ABSZ)
!            special abs
       if( .not. nuifna(stack(nstack)) ) then
           stack(nstack) = abs(stack(nstack))
       endif

     case (E_CUMNOR)
       call rexit("cumnor not supported yet")
       exit
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       !stack(nstack) = sdcdnm(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_INVNOR)
       call rexit("invnor not supported yet")
       exit
       if( nuifna(stack(nstack)) ) then
           retcod = 3
           return
       endif
       if( stack(nstack) .lt. Qzero .or. stack(nstack) .gt. Qone )  then
            iskerr = 5
            exit
       endif
       !stack(nstack) = sdivnm(stack(nstack))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

!            binary operators
!            no checking for valid numbers (except for E_DIV and E_POW)
!            for speed.
!            hopefully max and min don't give up ungracefully

     case (E_ADD)
       nstack = nstack - 1
       stack(nstack) = stack(nstack) + stack(nstack+1)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_SUB)
       nstack = nstack - 1
       stack(nstack) = stack(nstack) - stack(nstack+1)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_MUL)
       nstack = nstack - 1
       stack(nstack) = stack(nstack) * stack(nstack+1)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_DIV)
       nstack = nstack - 1
       if( nuifna(stack(nstack+1)) ) then
           retcod = 3
           return
       endif
       if( stack(nstack+1) .eq. Qzero ) then
           iskerr = 3
           exit
       endif
       stack(nstack) = stack(nstack) / stack(nstack+1)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_POW)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
!----          if( (stack(nstack) .eq. Qzero .and. stack(nstack+1) .le. Qzero)
!----         *                           .or.
!----         *    (stack(nstack) .lt. Qzero .and. stack(nstack+1) .ne. Qzero)
!----         *  )    goto 7040

!         No need to check. Current ftn compiler does it correctly
!         including negative number ** (integral number)
       stack(nstack) = stack(nstack) ** stack(nstack+1)
!            result is invalid: exponentiation error
       if( nuifna(stack(nstack)) ) then
           iskerr = 4
           exit
       endif

     case (E_MAX)
       nstack = nstack - 1
       stack(nstack) = max(stack(nstack), stack(nstack+1))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_MIN)
       nstack = nstack - 1
       stack(nstack) = min(stack(nstack), stack(nstack+1))
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack) ) ) iskerr = 6
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_ADDZ)
!          special binary add/sub

       nstack = nstack - 1
       if (nuifna(stack(nstack)).and. nuifna(stack(nstack+1))) then
         stack(nstack) = stack(nstack) + stack(nstack+1)
       else
         if( nuifna(stack(nstack  )) ) stack(nstack  ) = 0
         if( nuifna(stack(nstack+1)) ) stack(nstack+1) = 0
         stack(nstack) = stack(nstack) + stack(nstack+1)
       endif
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_SUBZ)
       nstack = nstack - 1
       if (nuifna(stack(nstack)).and. nuifna(stack(nstack+1))) then
          stack(nstack) = stack(nstack) - stack(nstack+1)
       else
          if( nuifna(stack(nstack  )) ) stack(nstack  ) = 0
          if( nuifna(stack(nstack+1)) ) stack(nstack+1) = 0
          stack(nstack) = stack(nstack) - stack(nstack+1)
       endif
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

!          special binary mul/div (NOT USED)

     case (E_MULZ)
       nstack = nstack - 1
!            if( nuifna(stack(nstack  )) ) stack(nstack  ) = 1
!            if( nuifna(stack(nstack+1)) ) stack(nstack+1) = 1
       stack(nstack) = stack(nstack) * stack(nstack+1)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

     case (E_DIVZ)
       nstack = nstack - 1
!            if( nuifna(stack(nstack  )) ) stack(nstack  ) = 0
!            if( nuifna(stack(nstack+1)) ) stack(nstack+1) = 1
       stack(nstack) = stack(nstack) / stack(nstack+1)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 6
           exit
       endif

!            relational operators
!            must check for valid numbers

     case (E_LT)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       if( stack(nstack) .lt. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_GT)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       if( stack(nstack) .gt. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_EQ)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       if( stack(nstack) .eq. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_LE)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       if( stack(nstack) .le. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_GE)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       if( stack(nstack) .ge. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_NE)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       if( stack(nstack) .ne. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_OR)
       nstack = nstack - 1
       if( istack(nstack) + istack(nstack+1) .ne. 0 ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif

     case (E_AND)
       nstack = nstack - 1
       if( istack(nstack) * istack(nstack+1) .eq. 1 ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif

!            special relational
!            one of args is invalid ==> false

     case (E_LTZ)
       nstack = nstack - 1
       if( nuifna(stack(nstack))   ) then
           istack(nstack) = 0
       elseif( nuifna(stack(nstack+1)) ) then
           istack(nstack) = 0
       elseif( stack(nstack) .lt. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_GTZ)
       nstack = nstack - 1
       if( nuifna(stack(nstack))   ) then
           istack(nstack) = 0
       elseif( nuifna(stack(nstack+1)) ) then
           istack(nstack) = 0
       elseif( stack(nstack) .gt. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_EQZ)
       nstack = nstack - 1
       if( nuifna(stack(nstack))   ) then
           istack(nstack) = 0
       elseif( nuifna(stack(nstack+1)) ) then
           istack(nstack) = 0
       elseif( stack(nstack) .eq. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_LEZ)
       nstack = nstack - 1
       if( nuifna(stack(nstack))   ) then
           istack(nstack) = 0
       elseif( nuifna(stack(nstack+1)) ) then
           istack(nstack) = 0
       elseif( stack(nstack) .le. stack(nstack+1) ) then
           istack(nstack) = 1
       else
        istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_GEZ)
       nstack = nstack - 1
       if( nuifna(stack(nstack))   ) then
           istack(nstack) = 0
       elseif( nuifna(stack(nstack+1)) ) then
           istack(nstack) = 0
       elseif( stack(nstack) .ge. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

     case (E_NEZ)
       nstack = nstack - 1
       if( nuifna(stack(nstack))   ) then
           istack(nstack) = 0
       elseif( nuifna(stack(nstack+1)) ) then
           istack(nstack) = 0
       elseif( stack(nstack) .ne. stack(nstack+1) ) then
           istack(nstack) = 1
       else
           istack(nstack) = 0
       endif
       typstk(nstack) = T_LOGIC

!        Two-argument functions ....

     case (E_HYPOT)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       stack(nstack) = dnrm2(2, stack(nstack), 1)
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_FIBUR)
       nstack = nstack - 1
       if( nuifna(stack(nstack)) .or. nuifna(stack(nstack+1))) then
           retcod = 3
           return
       endif
       tmp = stack(nstack) + stack(nstack+1)
       stack(nstack) = dnrm2(2, stack(nstack), 1) - tmp
       if( .not. mws%dbgeqn ) cycle
       if(nuifna( stack(nstack))) then
           iskerr = 5
           exit
       endif

     case (E_IF)
       nstack = nstack - 1
       if( istack(nstack+1) .eq. 0 ) then

!               if condition is false ==> skip to the else part
!               ip will be changed to point to byte after the else ipcode

          call skelse(ip, ipend, errflg)
          if( errflg ) then
              iskerr = 7
              exit
          endif
       endif
!            execute then part and skip to endif later on (next label)

     case (E_ELSE)
!            have executed then part of an if expression
!            skip to the endif
!            ip wil be changed to point to byte after the endif ipcode

       call skendif(ip, ipend, errflg)
       if( errflg ) then
           iskerr = 7
           exit
       endif

     case (E_ENDIF)
       continue

     case (E_SUM)
!            store lower and upper bound of sum
!            remember instruction pointer (after the arg fetch above)

       mindo  = mdl%equat(ip)
       maxdo  = mdl%equat(ip+1)
       ip     = ip + 2
       jsum   = mindo
       iposum = ip
       nstack = nstack + 1
       typstk(nstack) = T_REAL
       stack(nstack)  = Qzero

     case (E_ENDSUM)
       jsum = jsum + 1
       if( jsum .gt. maxdo ) then
!               loop expression exhausted
          iposum = 0
          jsum   = 0
       else
          ip = iposum
       endif
!            pop top element
!            add top 2 elements on stack
       nstack = nstack - 1
       stack(nstack) = stack(nstack) + stack(nstack+1)

     case (E_DEL)
!            remember Del argument in ndel and
!            calculate expression for first time
!            set ipodel to point to first byte after this ipcode + arg
       ndel   = mdl%equat(ip)
       ip     = ip + 1
       jdel   = 0
       ipodel = ip

     case (E_ENDDEL)
       if( jdel .eq. 0 ) then
!               first time
!               set jdel and calculate expression again
          jdel = -ndel
          ip   = ipodel
       else
!               have executed del expression for the second time
!               reset and subtract 2 top elements on stack
          jdel   = 0
          ipodel = 0
!               subtract top from next below on stack and pop top element
          nstack = nstack - 1
          stack(nstack) = stack(nstack) - stack(nstack+1)
       endif

     case (E_CALL)
!            User statement function

       juf   = mdl%equat(ip)

!            make nstuf point to place in stack before first argument
       nstuf_old = nstuf
       nstuf = nstack - mdl%narguf(juf)

!            nstack points to last argument on stack
!            nstuf  index in stack arrays of (first) argument - 1

!            push ip adress and nstuf on user function stack
       call push_userf(ip + opskip(ipcode), ipend, nstuf, nstuf_old, ier)

!            set new context
       ip    = mdl%nrecuf(juf) + 1
       ipend = mdl%nrecuf(juf + 1)

     case (E_ULCALL)

       juf   = mdl%equat(ip)
       narg = mdl%equat(ip + 1)

       !ulf_res =  do_ulcall(juf, narg, stack(nstack - (narg - 1)))
       call rexit("Calling R function not supported yet")

       nstack = nstack - (narg - 1)
       stack(nstack) = ulf_res
       typstk(nstack) = T_REAL


       ip = ip + opskip(ipcode)

     case (E_RET)
         ! E_RET : return from user statement function
         ! pop all arguments for function from stack
         ! and put function result on stack
         !restore ip, ipend and nstuf for the current user function
       call pop_userf(ip, ipend, nstuf, nstuf_old)

       if(typstk(nstack) .eq. T_REAL) then
          stack(nstuf+1)  = stack(nstack)
       else if(typstk(nstack) .eq. T_LOGIC) then
          istack(nstuf+1) = istack(nstack)
       endif

       typstk(nstuf+1) = typstk(nstack)

       nstack = nstuf + 1
       nstuf = nstuf_old

     case (E_STOP)
       if (nstack /= 1) then
           iskerr = 8
           exit
       endif
       if (typstk(1) /= T_REAL) then
           result = istack(1)
       else
           ! invalid result ==> return missing value
           ! if working with real(kind = ISIS_RKIND) should check for overflow
           if (.not. nuifna(stack(1))) then
                result = stack(1)
           else
                retcod = 3
           endif
       endif
       return

     case default
        iskerr = 9
        exit

     end select

   end do

!     Error handling
!     --------------

!     right-hand-side variable not valid:

   if (pushvar_err) then
       retcod = 3
       return
   endif

!     --------------

!    numerical problems / syntax errors:

   select case (iskerr)
   case(1:6)
       ! numerical problem
       if (mws%dbgeqn)  then
           call mdlerr(iskerr, ipcode, rop1, rop2)
           retcod = 2
       else
           retcod = 3
         endif
   case(7:9)
       ! internal error
       call mdlerr(iskerr, ipcode, rop1, rop2)
       retcod = 1

    end select

    return

    contains

!   -----------------------------------------------------------------------

!           skelse and skendif search for E_ELSE or E_ENDIF in mdl%equat.
!           This is not the most efficient approach to implement the if
!           expression. Better would be to determine the amount of bytes
!           to skip at compile time, and put it in mdl%equat.
!           However, numerical tests indicate that skelse and skendif
!           take only about 2 % of processor time for a typical CPB model
!           (SAFFIER).

       subroutine skelse( ip, ipend, errflg)
       use mcedef

!            skip to the else part of an if expression
!            errflg == .true. for syntax error
!            walk through code looking for matching else
!            return instruction pointer ip pointing to first byte
!            after else

       integer, intent(inout) :: ip
       integer, intent(in) :: ipend
       logical, intent(out) :: errflg

       integer :: ih, nf, nskip, ipcode


       errflg = .false.

       nskip = 0
       nf    = 1
       do  ih = ip, ipend
          if( nskip .gt. 0 ) then
              nskip = nskip - 1
              cycle
          endif
          ipcode = mdl%equat(ih)
          if( ipcode .eq. E_IF ) then
              nf = nf + 1
          else if( ipcode .eq. E_ELSE ) then
              nf = nf - 1
              if( nf .eq. 0 ) goto 60
          else
!                   skip required number of bytes for this ipcode
!                   as given in opskip array (see file mpeini)
              nskip = opskip(ipcode)
          endif
       enddo

!            syntax error
       errflg = .true.

60 continue
       ip = ih + 1

       return
       end subroutine skelse

!-----------------------------------------------------------------------

       subroutine skendif(ip, ipend, errflg)
       use mcedef

!            skip to the endif of an if expression
!            errflg == .true. for syntax error
!            walk through code looking for matching endif
!            return instruction pointer ip pointing to first byte
!            after endif
!            works in an analogous way as skelse

       integer, intent(inout) :: ip
       integer, intent(in) :: ipend
       logical, intent(out) :: errflg

       integer :: ih, ne, nskip, ipcode


       errflg = .false.

       nskip = 0
       ne    = 1
       do  ih = ip, ipend
          if( nskip .gt. 0 ) then
              nskip = nskip - 1
              cycle
          endif
          ipcode = mdl%equat(ih)
          if( ipcode .eq. E_ELSE ) then
              ne = ne + 1
          else if( ipcode .eq. E_ENDIF ) then
              ne = ne - 1
              if( ne .eq. 0 ) goto 60
          else
              nskip = opskip(ipcode)
          endif
       enddo

!            syntax error
       errflg = .true.

60 continue
      ip = ih + 1

      return
      end subroutine skendif

! ----------------------------------------------------------------------

!      real(kind = 8) function  do_ulcall(juf, narg, args)
!         call R function directly from the model code
!          use interfaces
!          use mcnmut
!          integer, intent(in)  :: juf, narg
!          real(kind = 8), intent(in) :: args(narg)

!          integer ::  nmlen,  min
!          integer, dimension(in_name_i4 + 1) :: nm
!          real(kind = 8) :: run_ulfunc, x

!          call mcbyin(tmp, nm(BYLNGT), mdl%iulfnames(juf), mdl%ulfnames)
!          if (nm(BYLNGT) > in_name_noch) then
!              call erfset("DO_ULCALL", errec, 650303, 3, &
!&                         nm(BYLNGT), tmp(1), 1, nm(BYLNGT), 1, in_name_noch)
!              return
!          endif
!          call bysfmv(tmp, nm(BYLNGT), nm(BYDATA))
!          do_ulcall =  run_ulfunc(nm, narg, args)

!      end function do_ulcall

! ----------------------------------------------------------------------

      subroutine mdlerr(errcod, ipcode, rop1, rop2)
      use msimot
      use mcedef

!     prints error messages for errors detected by modequ and onequ
!     during calculation of 1 equation
!     called by modequ

      integer, intent(in) :: errcod, ipcode
      real(kind = SOLVE_RKIND), intent(in) :: rop1, rop2

      select case (errcod)

      case (1)
         write(str,'(2a)') '** Non-positive argument for ', opname(ipcode)

      case (2)
         write(str,'(2a)') '** Negative argument for ', opname(E_SQRT)

      case (3)
         str =  '** Division by zero'

      case (4)
         str =  '** Illegal arguments for exponentiation'

      case (5, 6)
         write(str,'(2a)') '** Numerical problems in evaluation of ', &
&               opname(ipcode)

      case (7)
         str =  '** (Internal) missing Endif opcode'

      case (8)
         str =  '** (Internal) Unbalanced stack'

      case (9)
         str =  '** (Internal) Invalid opcode'

      case default
         write(str,'(a,i4)') '** Unknown error code in mdlerr ', errcod

      end select

      call strout(O_ERRF)

      if (errcod == 4) then
          !  exponentation error. (negative_number)**(pow), if pow is a
          !  non-integer
          write(str,'(a, a20)') '** ---> Base:           ', rop1
          call strout(O_ERRF)
          write(str,'(a, a20)') '** ---> Exponent:       ', rop2
          call strout(O_ERRF)
      elseif (errcod <= 5 .and. errcod /= 3) then
          ! unitary operators
          write(str,'(a, a20)') '** ---> Operand:        ', rop1
          call strout(O_ERRF)
      elseif (errcod == 6) then
!         binary operators
          write(str,'(a, a20)') '** ---> Left operand:   ', rop2
          call strout(O_ERRF)
          write(str,'(a, a20)') '** ---> Right operand:  ', rop1
          call strout(O_ERRF)
      endif

      str = ''

      return
      end subroutine mdlerr

   end subroutine evaleq

end module mscalc
