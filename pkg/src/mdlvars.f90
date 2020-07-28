module mdlvars
!
! module contains code for storing special model variables
! such as fit targets or mdl targets
!
use model_params
use kinds
use nuna

type mdl_variable
    integer(kind = SOLVE_IKIND) :: var_index, startp, endp, pcnt
    real(kind = SOLVE_RKIND), dimension(:), allocatable :: values
    type(mdl_variable), pointer :: next => null()
end type mdl_variable

type mdl_variables
    integer(kind = SOLVE_IKIND) :: var_count = 0
    type(mdl_variable), pointer :: first => null()
end type mdl_variables

contains

function get_mdl_var_value(mdl_var, jt)
     real(kind = SOLVE_RKIND) :: get_mdl_var_value
     type(mdl_variable), intent(in) :: mdl_var
     integer, intent(in) :: jt

     !
     ! Returns the value of mdl variable mdl_var at time jt.
     !

     if (jt >= mdl_var%startp .and. jt <= mdl_var%endp) then
         get_mdl_var_value = mdl_var%values(jt - mdl_var%startp + 1)
     else
         get_mdl_var_value = NA
     endif

end function get_mdl_var_value

function find_mdl_var(mdl_vars, var_index)
    type(mdl_variable) :: find_mdl_var
    type(mdl_variables), intent(in) :: mdl_vars
    integer, intent(in) :: var_index

    ! This function returns the model variable with index
    ! var_index. If there is no model variables with that index,
    ! then the function returns a mdl_var with
    ! mdl_var%var_index = -1.

    type(mdl_variable), pointer :: mdl_var

    find_mdl_var%var_index = -1

    if (mdl_vars%var_count == 0) return

    mdl_var => mdl_vars%first
    do
        if (mdl_var%var_index == var_index)  then
            find_mdl_var = mdl_var
            return
        endif
        mdl_var => mdl_var%next
        if (.not. associated(mdl_var)) return
    end do
    return
end function find_mdl_var

logical function has_mdl_var(mdl_vars, jt)
    type(mdl_variables), intent(in) :: mdl_vars
    integer, intent(in) :: jt

    ! returns .true. if period jt has mdl variables

    type(mdl_variable), pointer :: mdl_var

    has_mdl_var = .false.

    if (mdl_vars%var_count == 0) return

    mdl_var => mdl_vars%first
    do
        if (.not. nuifna(get_mdl_var_value(mdl_var, jt))) then
            has_mdl_var = .true.
            exit
        endif
        mdl_var => mdl_var%next
        if (.not. associated(mdl_var)) return
    end do
    return
end function has_mdl_var

subroutine clear_mdl_variables(mdl_vars)
    type(mdl_variables), intent(inout) :: mdl_vars
    !
    ! clear mdl variables
    !

    type(mdl_variable), pointer :: mdl_var, prev

    if (mdl_vars%var_count == 0) return
    mdl_var => mdl_vars%first
    do
        if (.not. associated(mdl_var)) exit
        prev => mdl_var
        mdl_var => prev%next
        if (allocated(prev%values)) deallocate(prev%values)
        deallocate(prev)
    end do
    nullify(mdl_var)
    nullify(mdl_vars%first)
    mdl_vars%var_count = 0
end subroutine clear_mdl_variables

subroutine add_mdl_variable(mdl_vars, varnum, vcnt, jstart, vardat, upd_mode, &
                            ierr)

    use kinds
    use mws_params, only : UPD, UPD_VAL
    type(mdl_variables), intent(inout) :: mdl_vars
    integer, intent(in) :: varnum, vcnt, jstart, upd_mode
    real(kind = ISIS_RKIND), intent(in), dimension(*) :: vardat
    integer, intent(out) :: ierr

    !
    ! Set mdl variable.
    ! Output parameter ierr:
    !   ierr = 0 ok
    !   ierr = 1 not enough memory
    !

    type(mdl_variable), pointer :: cur, prev
    logical :: all_na
    integer :: jend, startp_new, endp_new, pcnt_new,  i, it
    real(kind = SOLVE_RKIND), allocatable :: temp(:)
    logical :: found
    integer :: stat

    ierr = 0

    all_na = nuvana(vardat, vcnt)

    found = .false.
    prev => null()
    cur => mdl_vars%first

    do
        if (.not. associated(cur)) exit
        if (cur%var_index == varnum) then
            found = .true.
            exit
        endif
        prev => cur
        cur => cur%next
    end do

    if (found) then

        ! update existing mdl value

        if (upd_mode == UPD_VAL .and. all_na) return

        ! increase the size of cur%values if necessary
        !
        jend = jstart + vcnt - 1
        if (jstart < cur%startp .or. jend > cur%endp) then
             startp_new = min(cur%startp, jstart)
             endp_new = max(cur%endp, jend)
             pcnt_new = endp_new - startp_new + 1
             allocate(temp(pcnt_new), stat = stat)
             if (stat /= 0) then
                 ierr = 1
                 return
             endif
             temp = NA
             i = cur%startp - startp_new + 1
             temp(i : i + cur%pcnt - 1) = cur%values(1 : cur%pcnt)
             call move_alloc(temp, cur%values)
             cur%startp = startp_new
             cur%endp = endp_new
             cur%pcnt = pcnt_new
        endif

        !
        ! set new mdl value values
        !
        i = jstart - cur%startp + 1
        if (all_na) then
            cur%values(i : i + vcnt - 1) = NA
        else if (upd_mode == UPD) then
            cur%values(i : i + vcnt - 1) = vardat(1 : vcnt)
        else 
            do it = 1, vcnt
                if (.not. nuifna(vardat(it))) &
                       cur%values(i + it - 1) = vardat(it)
            end do
        endif 

    else if (.not. all_na) then

       ! add a new mdl value to the list
       ! if all_na then nothing to set and to do ==> done
       allocate(cur, stat = stat)
       if (stat /= 0) then
           ierr = 1
           return
       endif
       if (associated(prev)) then
           prev%next => cur
       else
           mdl_vars%first => cur
       endif
       cur%var_index = varnum
       allocate(cur%values(vcnt), stat = stat)
       if (stat /= 0) then
           ierr = 1
           return
       endif
       cur%values = vardat(:vcnt)
       cur%startp = jstart
       cur%endp = jstart + vcnt - 1
       cur%pcnt = vcnt
       cur%next => null()
       mdl_vars%var_count = mdl_vars%var_count + 1

    else

       ! mdl value not found and new mdl value values only NA's
       return

    endif

    ! if all mdl value values are NA, then remove mdl value
    ! from the list
    if (nuvana(cur%values, size(cur%values))) then
        if (associated(prev)) then
            prev%next => cur%next
        else
            mdl_vars%first => cur%next
        endif
        deallocate(cur%values)
        deallocate(cur)
        mdl_vars%var_count = mdl_vars%var_count - 1
    endif

end subroutine add_mdl_variable

subroutine clone_mdl_variables(mdl_vars, mdl_vars_clone)
    type(mdl_variables), intent(in)  :: mdl_vars
    type(mdl_variables), intent(out) :: mdl_vars_clone

    integer :: stat
    type(mdl_variable), pointer :: cur, cur_clone, prev_clone
    
    mdl_vars_clone%var_count = mdl_vars%var_count
    if (mdl_vars%var_count == 0) return

    cur => mdl_vars%first
    prev_clone => null()
    do
       allocate(cur_clone, stat = stat)
       cur_clone = cur
       cur_clone%next => null()
       if (associated(prev_clone)) then
           prev_clone%next => cur_clone
       else 
           mdl_vars_clone%first => cur_clone
       endif
       prev_clone => cur_clone
       cur => cur%next
       if (.not. associated(cur)) exit
    end do

    cur_clone => mdl_vars_clone%first

end subroutine clone_mdl_variables

subroutine dump_mdl_variables(mdl_vars)
    type(mdl_variables), intent(in) :: mdl_vars

    !
    ! Dump mdl values (for testing purpose only
    !

    type(mdl_variable), pointer :: cur

    if (mdl_vars%var_count == 0) then
        return
    endif

    cur => mdl_vars%first
    do
       print *,'Variable number ', cur%var_index
       print *,'Startp ', cur%startp
       print *,'Value ', cur%values
       cur => cur%next
       if (.not. associated(cur)) return
    end do
end subroutine dump_mdl_variables

logical function mdl_var_valid(mdl_vars, var_index, jf, jl)
    type(mdl_variables), intent(in) :: mdl_vars
    integer, intent(in) :: var_index, jf, jl

    ! Returns .true. if the model variable with index var_index has
    ! valid values for all periods between jf and jl.

    type(mdl_variable) :: mdl_var
    integer :: j
    logical :: retval

    mdl_var = find_mdl_var(mdl_vars, var_index)

    if (mdl_var%var_index == -1) then
        mdl_var_valid = .false.
        return
    endif

    retval = .true.  
    do j = jf, jl
        if (nuifna(get_mdl_var_value(mdl_var, j))) then
           retval = .false.
           exit
        endif
    enddo

    mdl_var_valid = retval

    return

end function mdl_var_valid
     
end module mdlvars
