module mcodep

contains

subroutine mcvdep_eq(mdl, eqnum, mode, useleads, ier)
    use model_type
    type(model), intent(in) :: mdl
    integer, intent(in) :: eqnum, mode
    integer, intent(out) :: ier
    logical, intent(in) ::  useleads

    call mcvdep(mdl, mdl%nrecp(eqnum) + 1, mode, useleads, ier)
return
end subroutine mcvdep_eq

!-----------------------------------------------------------------------

subroutine mcvdep(mdl, ipequb, mode, useleads, ier)
use model_type
use msufstack
use mcdep
use mcedef
use mcstak

!     Check dependencies in one equation. This subroutines determines
!     which variables and parameters occur on the rhs of the equation,
!     and stores the result in module mcdep (see src/libf90/mcdep.rf9)

!     in   ipeqsub  integer        start of equation code in mdl%equat
!     in   mdl%equat   integer(*)     equation and user function code
!     in   Narguf   integer(nuf)   number of arguments of user functions
!     in   mdl%nrecuf   integer(nuf+1) user function code pointers
!     in   mode     integer        mode (see below)
!     in   useleads logical        treat leads as simultaneous (see
!                                  below)
!     out  ier      integer        error flag (see below)

!     Possible modes:
!         CMPMDL (used by Isis procedure compile_mdl):
!                 check all variables, parameters and simultaneous
!                 variables, and determine the minimum and maximum
!                 shift (lag or lead)
!         ORMDL  (used by Isis procedure order_mdl):
!                 check only simultaneous variables
!         LINEQN (used by Isis procedure lineqn):
!                 check all variables and determine mininum and maximum
!                 shift (lag or lead)

!     If useleads is .true., all variables with positive shifts
!     (leads) are considered as simultaneous with the current period. This
!     option is required for the ratex procedure.

!     error status in ier
!       0 all ok
!       1 too many rhs variables
!       2 indicates problems in equation code
!       3 invalid opcode (code corrupted/internal error)
!       4 ran out of stack
!       5 too many nested user functions

type(model), intent(in) :: mdl
integer(kind = MC_IKIND) :: ipequb
integer ::      ier
integer ::      mode
logical ::      useleads

type :: stack_value
    logical :: is_ref   ! .true. if element is a reference
                        ! of a variable or parameter
    integer :: shift    ! shift (shift)
    logical :: lv       ! true if variable has a loop index
    integer(kind = MC_IKIND) :: address  ! address of variable or parameter
end type stack_value

type(stack_value) :: stack(0:MAXSTK)

integer ::  ip
integer ::  ipcode,kxar,nstack, mindo, maxdo,ndel,juf
integer ::  nstuf, nstuf_old
integer ::  argp, shift, ldtype, min_shift, max_shift, idum, narg
logical ::  lv

!     Important variables used
!     ---------------------------
!     nstack    stack pointer
!     ipcode    current opcode
!     ip        instruction pointer

!     ndel      - current value of del function shift (argument of del)
!     mindo     lower bound of sum index
!     maxdo     upper bound of sum index

!     kxar      number of variable/parameter from argument to ipcode

!     Stack array
!     ------------
!     stack     holds information about stack value


nsimv  = 0
ier    = 0
nstack = 0
ndel   = 0
mindo  = 0
maxdo  = 0
nparv  = 0

!     for lineqn nv and iv are initialised in msnleq.rf7.
!     this is necessary for a special treatment of implicit equations
if (mode /= LINEQN) nv = 0

call init_ufstack

ip    = ipequb


!     fetch next opcode from byte array
!     increment instruction pointer

do while (ier .eq. 0)

  ipcode = mdl%equat(ip)

  ip = ip + 1

  select case (ipcode)

  case (E_LIRL, E_LIRL8, E_LISUM)
     call push_value

!          model parameters
  case (E_LIP0:E_LIP5, E_LAP0:E_LAP5)
     call push_value
     if (mode == CMPMDL) then
         kxar   = mdl%equat(ip)
         call setsmv(kxar,nparv,iparv,maxrhs,ier)
     endif

!          arithmetic binary
!          relational operators, .or., .and.
!          2-argument functions e.g. HYPOT or FIBUR
  case (E_ADD:E_MIN, E_LT:E_NE, E_AND, E_OR, E_ADDZ:E_NEZ, E_HYPOT, E_FIBUR)

     if (nstack < 2) then
        ier = 2
     else
         nstack = nstack - 1
         stack(nstack)%is_ref = .false.
     endif

!          negate, .not., builtin 1 argument functions
  case (E_NOT, E_NEG:E_INVNOR)
      if (nstack > 0) then
          stack(nstack)%is_ref = .false.
      else
          ier = 2
      endif

!          function arguments (opcode E_LIF0 .. E_LIF5)
  case (E_LIF0:E_LIF5)
     call push_value
     argp  = nstuf + mdl%equat(ip)
     if (stack(argp)%is_ref) then
        ldtype = ipcode - E_LIF0 + 1
        shift = get_shift() + stack(argp)%shift
        lv = has_loopvar() .or. stack(argp)%lv
!             If has_loopvar() is true, then the formal argument
!             is used with a loop index. Example:

!             function f(x) = sum(j = -2 , 1 : x(j));

!             If stack(argp)%lv is .true., then the actual
!             argument in the function call has a loop index.
!             Example:

!             ident eq1 x = sum(j = -2, 1 : f(x(j)));.

!             Another example with a user function calling another user
!             function:
!             function f2(x) = sum(j = -2, 1 : f(x(j)));

!             Note that it is not possible that both has_loopvar()
!             and stack(argp)%lv are true,  because the sum function
!             may not be nested, also not indirectly via user functions.

        kxar  = stack(argp)%address
        call check_variable
     endif

!          push adress of formal argument (opcode E_LAF0 .. E_LAF5)
!          the actual argument can be a variable or parameter
  case (E_LAF0:E_LAF5)
     argp = nstuf + mdl%equat(ip)
     ldtype = ipcode - E_LAF0 + 1
     shift = stack(argp)%shift + get_shift()
     lv = stack(argp)%lv .or. has_loopvar()
     call push_ref(shift, lv, stack(argp)%address)

!          variables in model (opcode E_LIV0 .. E_LIV5)
  case (E_LIV0:E_LIV5)
     call push_value
     kxar   = mdl%equat(ip)
     ldtype = ipcode - E_LIV0 + 1
     shift = get_shift()
     lv = has_loopvar()
     call check_variable

!          push adress of variable (opcode E_LAV0 .. E_LAV5)
!          save variable number in istack
!          call getshf to set ostack to hold shift
  case (E_LAV0:E_LAV5)
     ldtype = ipcode - E_LAV0 + 1
     call push_ref(get_shift(), has_loopvar(), mdl%equat(ip))

  case (E_IF, E_ENDIF)
     if (nstack < 1) then
         ier = 2
     else
         nstack = nstack - 1
     endif

  case (E_ELSE)
!          have executed then part of an if expression
!          keep it on the stack
     continue

  case (E_SUM)
!          fetch lower and upper bound of sum as integer
!          set type on stack to .false. (for start 0.0)
!          NOTE: sum functions may not be nested, so there is no need to
!          store mindo and maxdo on a stack.
     mindo  = mdl%equat(ip)
     maxdo  = mdl%equat(ip+1)

  case (E_ENDSUM)
     mindo  = 0
     maxdo  = 0

  case (E_DEL)
!          remember Del argument in ndel
!          NOTE: del functions may not be nested, so there is no need to
!          store ndel on a stack.
     ndel   = - mdl%equat(ip)

  case (E_ENDDEL)
     ndel   = 0

!          User function
  case (E_CALL)

     juf = mdl%equat(ip)

!           determine nstuf: place on stack before the arguments
!           of the user function
     nstuf_old = nstuf
     nstuf = nstack - mdl%narguf(juf)

!          nstack points to last argument on stack
!          nstuf  index in stack arrays of (first) argument - 1

!          push ip address and nstuf on user function stack
     call push_userf(ip + opskip(ipcode), idum,  nstuf, nstuf_old, ier)
     if (ier /= 0) ier = 5

!          set new context
     ip    = mdl%nrecuf(juf) + 1

!          User language function
  case (E_ULCALL)

     narg = mdl%equat(ip + 1)

     if (nstack < narg) then
        ier = 2
     else
         nstack = nstack - (narg - 1)
         stack(nstack)%is_ref = .false.
     endif

!       return from user statement function
  case (E_RET)
!          no automatic skip here
!          reset equation context

     call pop_userf(ip, idum, nstuf, nstuf_old)

!          pop arguments and reset nstuf
     nstack = nstuf + 1
     nstuf = nstuf_old

!          top of stack now contains function result (this is a value)
     if (nstack > 0) then
         stack(nstack)%is_ref = .false.
     else
         ier = 2
     endif

!          ip now points to "byte" after <call> opcode + args

  case (E_STOP)
     if( nstack .ne. 1 ) ier = 2
     exit

  case default
     ier = 3

  end select

  if (ier /= 0) exit

!       move to the next ip code by skipping arguments, except
!       if ipcode == E_CALL. In that case ip already points
!       to start of the code for the user function
  if (ipcode /= E_CALL) ip = ip + opskip(ipcode)


end do


return

contains

! ------------------------------------------------------------------------

subroutine push_value
!         pushes a value on the stack
    nstack = nstack + 1
    if (nstack > MAXSTK) then
         ier = 4
    else
         stack(nstack) = stack_value(.false., 0, .false., 0)
    endif
end subroutine push_value

! ------------------------------------------------------------------------

subroutine push_ref(shift, lv,  address)
!         pushes a reference on the stack
integer ::  shift
    integer(kind = MC_IKIND) :: address
logical ::  lv

    nstack = nstack + 1
    if (nstack > MAXSTK) then
         ier = 4
    else
         stack(nstack) = stack_value(.true., shift, lv, address)
    endif
end subroutine push_ref


! ------------------------------------------------------------------------

subroutine check_variable

  if (lv) then
!           variable with loop index
      min_shift = mindo + shift
      max_shift = maxdo + shift
  else
!           variable without loop index
      min_shift = shift
      max_shift = shift
  endif
  min_shift = min_shift + ndel

  if (mode /= LINEQN) then
      if (is_simultaneous()) then
!               register simultaneous variable
          call setsmv(kxar,nsimv,isimv,maxrhs,ier)
      endif
  endif
  if (mode /= ORDMDL) then
!           register variable and update min / max leads
      call  setvlg(kxar, min_shift, max_shift, &
&                  iminld, imaxld, nv, iv, maxrhs, ier)
  endif

end subroutine check_variable

! ------------------------------------------------------------------------

logical function is_simultaneous ()

! returns true if the variable is simultaneous

  if (useleads) then
!          all variables with lead >= 0
     is_simultaneous = max_shift >= 0
  else
!          only variables occuring without lag or lead
     is_simultaneous = (min_shift *  max_shift <= 0)
  endif

end function is_simultaneous

!-------------------------------------------------

logical function has_loopvar()

! based on the load type ld, this function determines if the
! variable has a loop variable index

  has_loopvar = ldtype == 2 .or. ldtype == 4 .or. ldtype == 6

end function has_loopvar

!-------------------------------------------------

integer function get_shift()

!     get shift implied by load type ldtype

  select case (ldtype)
  case (1:2)
      get_shift = 0
  case (3:4)
      get_shift = - mdl%equat(ip+1)
  case (5:6)
      get_shift =   mdl%equat(ip+1)
  end select
end function get_shift


!-----------------------------------------------------------------------

end subroutine mcvdep

!-----------------------------------------------------------------------

subroutine setsmv(kxar, nrhs, irhs, maxrhs, ier)

!     see if variable or parameter with number kxar has already
!     been registered in irhs
!     if not add its number to irhs array

integer ::  irhs(*)
integer ::  kxar, nrhs, maxrhs, ier
integer ::  j

do j=1,nrhs
   if(irhs(j) .eq. kxar) return
end do

nrhs = nrhs + 1
if(nrhs .gt. maxrhs) then
    ier = 1
else
    irhs(nrhs)   = kxar
endif

return
end

!-----------------------------------------------------------------------

subroutine setvlg(kxar, minld, maxld, iminld, imaxld, nrhs, irhs, maxrhs, ier)

!     see if variable with number kxar has already been registered
!     if so modify it's min/max shift and continue with main loop
!     else register variable

integer ::  iminld(*), imaxld(*), irhs(*)
integer ::  kxar, minld, maxld, nrhs
integer ::  ier, maxrhs, j

ier = 0
do  j=1,nrhs
   if(irhs(j) .eq. kxar) then
      iminld(j) = min(minld,iminld(j))
      imaxld(j) = max(maxld,imaxld(j))
      return
   endif
enddo
nrhs = nrhs + 1
if(nrhs .gt. maxrhs) then
  ier = 1
  return
endif
irhs(nrhs)   = kxar
iminld(nrhs) = minld
imaxld(nrhs) = maxld

return
end

end module mcodep
