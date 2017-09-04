module mcheck

use model_type

private :: chkequ

contains
subroutine chkmdl(mdl, nerr)
    type(model), intent(in) :: mdl
    integer, intent(out) :: nerr

    integer :: iequ, ier

    nerr = 0

    do iequ = 1, mdl%neq
       call chkequ(mdl, iequ, mdl%nrecp(iequ) + 1, mdl%nrecp(iequ + 1), ier)
       if (ier /= 0) nerr = nerr + 1
    end do

    return
end subroutine chkmdl

!-----------------------------------------------------------------------

subroutine chkequ(mdl, iequ, ipequb, ipequl, ier)
use msufstack
use mcedef
use mcstak

!     checks equation coded in ebytes starting at ipequb ending at
!     ipequl for semantic errors and such which cannot be checked
!     during compilation. It also checks if the value stack and the
!     user function stack are large enough. This is NOT checked
!     in mscalc. Therefore, elements should be pushed and popped
!     from the stack exactly as in mscalc!

!     The new xpc compiler can detect more errors during compilation
!     than the original model compiler of Isis. For example,
!     it can detect nested SUM or DEL. However, since this subroutine
!     is used in procedure read_mdl, it is possible that the current
!     model has been compiled with the old model compiler. It is
!     therefore still necessary that chkequ checks for these errors.

!     Assumes that user function and equation code are in the same
!     array with user functions at start

type(model), intent(in) :: mdl
integer, intent(in) :: iequ
integer(kind = MC_IKIND), intent(in) :: ipequb, ipequl
integer, intent(out) :: ier

!     symbolic constants to keep track of type of value
!     on the calculation stack

integer ::  t_real, t_logic, t_par, t_var, t_err
parameter (T_REAL = 1, T_LOGIC = 2, T_PAR = 3, T_VAR = 4, T_ERR  = 5)

integer ::   typstk(0:maxstk)

integer ::   ip
logical ::   insum, indel
integer ::   ipcode, nstack, juf,nstuf, nstuf_old, iarg, idum
integer ::   iskerr
integer ::   rtyp, ltyp, narg, i

!     includes definitions for special opcode data arrays
!     !! all are 0-base and must be declared as such in
!        receiving functions and subroutines
!        weird errors will possibly result if (0:*) is omitted
!        forcheck can't detect this


!     checks an equation by simulating a stack machine
!     for each operation code execute appropriate instructions

!     in this machine arithmetic operators expect arithmetic operands
!                     logical    operators expect logical    operands
!                     relational operators expect arithmetic operands

!     a function may return a logical or an arithmetic expression
!     !! nice for testing expressions through a function

!     arguments to a function may be logical expressions
!     !! so you can convert (cast) logical to arithmetic
!        with chosen values

!     the type of a then/else expresion must be the same
!     and may be logical or arithmetic
!     !! to enable if expressions in the condition part
!        of another if expression

!     Important variables used
!     ---------------------------
!     nstack    stack pointer
!     ipcode    current opcode
!     ip        instruction pointer
!     insum     .true.. if in sum, .false. if not
!     indel     .true.. if in del, .false. if not
!               Sum and Del may Not be nested !!

!     juf       number of function currently executing
!               0 if not in function
!     nstuf     stack pointer before function was called

!     Stack arrays
!     ------------
!     typstk    type indicator for the stack
!               T_REAL  real
!               T_LOGIC logical
!               T_PAR   parameter address
!               T_VAR   variable  address

nstack = 0
insum  = .false.
indel  = .false.
juf    = 0
iskerr = 0

call init_ufstack

if( mdl%equat(ipequl) .ne. E_STOP ) iskerr = 10

ip    = ipequb

opcode_loop: do while (iskerr .eq. 0)

!       fetch next opcode from byte array
!       compress ipcode
!       increment instruction pointer

  ipcode = mdl%equat(ip)
  ip = ip + 1

  select case (ipcode)

!          constants, sum variables, parameters and variables
  case(E_LIRL, E_LIRL8, E_LISUM, E_LIP0 : E_LIP5, E_LIV0 : E_LIV5)
      call push_type(T_REAL)

!           formal arguments in function (opcode E_LIF0 .. E_LIF5 )
  case(E_LIF0:E_LIF5)
      iarg = nstuf + mdl%equat(ip)
      select case (typstk(iarg))
      case (T_REAL, T_PAR, T_VAR)
          call push_type(T_REAL)
      case (T_LOGIC)
          call push_type(T_LOGIC)
      case default
          iskerr = 4
      end select

!           push adress of parameter (opcode E_LAP0 .. E_LAP5)
  case(E_LAP0:E_LAP5)
      call push_type(T_PAR)

!           push adress of variable (opcode E_LAV0 .. E_LAV5)
  case(E_LAV0:E_LAV5)
      call push_type(T_VAR)

!           push adress of formal argument (opcode E_LAF0 .. E_LAF5)
  case(E_LAF0:E_LAF5)
      iarg = nstuf + mdl%equat(ip)
      call push_type(typstk(iarg))

!           not operator:
  case(E_NOT)
      if (get_type() .ne. T_LOGIC ) iskerr = 3

!           negate (unary minus)
  case(E_NEG)
      if (get_type() .ne. T_REAL ) iskerr = 2

!           builtin 1 argument functions:
  case(E_LOG:E_TANH, E_ABSZ:E_INVNOR)
      if (get_type() .ne. T_REAL ) iskerr = 2

!           cast logical to number
  case(E_TORL)
      if (get_type() .ne. T_LOGIC ) then
         iskerr = 3
      else
         typstk(nstack) = T_REAL
      endif

!           test if number is valid (not used any more)
  case(E_ISVL)
      if (get_type() .ne. T_REAL ) then
          iskerr = 2
      else
          typstk(nstack) = T_LOGIC
      endif

!           binary operators (arithmetic) or 2 argument functions

  case(E_ADD:E_MIN, E_ADDZ:E_DIVZ, E_HYPOT, E_FIBUR)
      call pop_binop
      if ((ltyp .ne. T_REAL) .or. (rtyp .ne. T_REAL )) iskerr = 2

!           relational operators

  case(E_LT:E_NE, E_LTZ:E_NEZ)
      call pop_binop
      if ((ltyp .ne. T_REAL) .or. (rtyp .ne. T_REAL)) then
          iskerr = 2
      else
          typstk(nstack) = T_LOGIC
      endif

  case(E_OR, E_AND)
      call pop_binop
      if ((ltyp .ne. T_LOGIC) .or. (rtyp .ne. T_LOGIC)) iskerr = 3

  case(E_IF)
      if (pop_type() .ne. T_LOGIC ) iskerr = 3

  case(E_ELSE)
!           have executed then part of if expression
!           keep it on the stack and check at endif
      continue

  case(E_ENDIF)
!           check then/else expression types (must be the same)
!           pop 1 element
      call pop_binop
      if (ltyp .ne. rtyp) iskerr = 1

  case(E_SUM)
      if (insum) then
          iskerr = 9
      else
          insum = .true.
!               push real to stack: same is done in mscalc.
          call push_type(T_REAL)
      endif

  case(E_ENDSUM)
!           top of stack must be T_REAL
      if( .not. insum) then
          iskerr = 7
      else
          insum = .false.
!               pop top element of stack: same is done in mscalc
          if (pop_type() .ne. T_REAL ) iskerr = 2
      endif

  case(E_DEL)
      if (indel) then
          iskerr = 9
      else
          indel = .true.
!               push T_REAL to stack. In mscalc, the top of the
!               stack is not popped at the first occurence of E_ENDDEL
!               (the top contains the del expression). To simulate this
!               behaviour we push a T_REAL to the stack.
          call push_type(T_REAL)
      endif

  case(E_ENDDEL)
!           top of stack must be T_REAL
      if (.not. indel ) then
          iskerr = 8
      else
          indel = .false.
!               pop top of stack (see comment in E_DEL).
          if (pop_type() .ne. T_REAL ) iskerr = 2
      endif

!           User statement function
  case(E_CALL)
      juf   = mdl%equat(ip)

!           make nstuf point to place in stack before first argument
      nstuf_old = nstuf
      nstuf = nstack - mdl%narguf(juf)

!          nstack points to last argument on stack
!          nstuf  index in stack arrays of (first) argument - 1

!          push ip adress and nstuf on user function stack
     call push_userf(ip + opskip(ipcode), idum, nstuf, nstuf_old, ier)

     if (ier /= 0) then
!              too deeply nestued user function calls
         iskerr = 103
         exit
     endif

!           set new context

      ip    = mdl%nrecuf(juf) + 1
      if( mdl%equat(mdl%nrecuf(juf+1)) .ne. E_RET ) then
          iskerr = 11
          exit
      endif

  ! User statement function
  ! Check if all actual arguments are real numbers
  case(E_ULCALL)
      narg = mdl%equat(ip + 1)
      nstack = nstack - (narg - 1)
      iskerr = 0
      arg_loop : do i = 1, narg
          if (typstk(nstack + i - 1) /= T_REAL) then
              iskerr = 12
              exit opcode_loop
           endif
      end do arg_loop
      typstk(nstack) = T_REAL

  case(E_RET)
      ! return from user function (T_REAL or T_LOGIC allowed)
      ! pop all arguments for function from stack
      ! and put function result on stack

      ! pop user function info
      call pop_userf(ip, idum, nstuf, nstuf_old)

      if( typstk(nstack) .ge. T_PAR ) iskerr = 4

      typstk(nstuf+1) = typstk(nstack)
      juf    = 0

      nstack = nstuf + 1
      nstuf = nstuf_old

  case(E_STOP)
      if( nstack .ne. 1 ) iskerr = 102
      exit

  case default
      iskerr = 200

  end select

  ! move to the next ip code by skipping arguments, except
  ! if ipcode == E_CALL. In that case ip already points
  ! to start of the code for the user function
  if (ipcode /= E_CALL) ip = ip + opskip(ipcode)

end do opcode_loop

if (iskerr .ne. 0) then
    call chkerr(iskerr)
    ier = 1
else
    ier = 0
endif
return


contains

!-----------------------------------------------------------------------

    subroutine push_type(typ)
      ! pushes a type on the stack
      integer ::  typ
      nstack = nstack + 1
      if (nstack > MAXSTK) then
          iskerr = 101
      else
          typstk(nstack) = typ
      endif
    end subroutine push_type

!-----------------------------------------------------------------------

    integer function pop_type()
      ! pops the last element from stack and returns its type
      if (nstack < 1) then
          iskerr = 100
          pop_type = T_ERR
      else
          pop_type = typstk(nstack)
          nstack = nstack - 1
      endif
      return
    end function pop_type

!-----------------------------------------------------------------------

    integer function get_type()
      ! gets the type of the element at the top of the stack
      if (nstack < 1 ) then
          iskerr = 100
          get_type = T_ERR
      else
          get_type = typstk(nstack)
      endif
      return
    end function get_type


!-----------------------------------------------------------------------

    subroutine pop_binop()
      ! stores the types of the two top elements of the stack
      ! (which should correspond to the two operands of a binary
      !  operator) in variables ltyp and rtyp,
      ! and pops one element of the stack
      if (nstack < 2) then
          iskerr = 100
          ltyp = T_ERR
          rtyp = T_ERR
      else
          ltyp = typstk(nstack - 1)
          rtyp = typstk(nstack)
          nstack = nstack - 1
      endif
      return
   end subroutine pop_binop

!-----------------------------------------------------------------------

   subroutine chkerr(errcod)
    use mdl_name_utils
    use msufstack
    use output_utils

    ! prints error messages for errors detected by chkequ
    ! during semantic check of 1 equation
    ! called by chkequ

    integer, intent(in) :: errcod
    character(len = 255) :: str
    character(len = MCMXNM) :: varname
    integer :: nlen

    select case (errcod)
    case (1)
        str = 'Type mismatch between Then and Else expression'
    case (2)
        str = 'Arithmetic operand expected'
    case (3)
        str = 'Logical operand expected'
    case (4)
        str = 'Argument type mismatch in user function'
    case (5)
        str = 'Nested user function calls not allowed'
    case (6)
        str = 'Arithmetic operand instead of logical operand'
    case (7)
        str = 'Missing SUM'
    case (8)
        str = 'Missing DEL'
    case (9)
        str = 'Nested DEL or SUM not allowed'
    case (10)
        str = 'Missing E_STOP opcode (internal error)'
    case (11)
        str = 'Missing E_RET opcode in function (internal error)'
    case (12)
        str = 'Argument of user language function is a logical'
    case(100)
        write(str,'(a,i4)') 'Not enough values in stack at opcode ', ipcode
    case(101)
        write(str,'(a,i4)') 'Out of stack at opcode ', ipcode
    case(102)
        str = 'Unfinished stack at end of equation'
    case (103)
          write(str, '(a, i3, a)') 'Too many nested user functions (max =', &
&               MAX_USERF_STACK, ')'

    case(200)
        write(str,'(a,i4)') 'Invalid opcode ', ipcode
    case default
        write(str,'(a,i4)') 'Unknown error code in chkerr ', errcod
    end select

    call mcf7ex(varname, nlen, mdl%ienames(iequ), mdl%enames)
    str = trim(str) // ' in equation ' // varname(:nlen)
    call isismdl_out(str)

    end subroutine chkerr

end subroutine chkequ

end module mcheck
