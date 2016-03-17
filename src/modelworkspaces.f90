module modelworkspaces
    use mws_type
    integer, parameter, private :: MAX_SIZE = 100
    type(modelworkspace), dimension(MAX_SIZE), target, save :: mws_array
    logical, dimension(MAX_SIZE), private, save :: mws_used = .false.

    !
    ! This module takes care of the administration of the modelworkspaces
    ! for different instances of models.
    ! This is only a prototype with limitations (these should be
    ! corrected in a true production environment):
    ! There is a maximum of MAX_SIZE models that can be handled
    ! simultaneously.
    !

    contains
        integer function create_mws()
            use output_utils, only : macromod_error
    
            integer :: i

            ! find first free mws
            do i = 1, MAX_SIZE
                if (.not. mws_used(i)) then
                    mws_used(i) = .true.
                    create_mws = i
                    return
                endif
            end do

            ! no free mws found: error
            call macromod_error("Maximum number of models in memory reached")
            create_mws = -1
            return
        end function create_mws

        subroutine remove_mws(mws_index)
            integer, intent(in) :: mws_index
            mws_used(mws_index) = .false.
        end subroutine remove_mws

end module modelworkspaces
