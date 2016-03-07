module modelworkspaces
    use mws_type
    use init
    integer, parameter, private :: MAX_SIZE = 100
    integer, save, private :: count = 0
    type(modelworkspace), dimension(MAX_SIZE), target, save :: mws_array

    !
    ! This module takes care of the administration of the modelworkspaces
    ! for different instances of models.
    ! This is only a prototype with limitations (these should be
    ! corrected in a true production environment):
    ! 1) There is a maximum of MAX_SIZE models that can be handled.
    ! 2) The implementation does not reuse elements in mdl_data_array
    !   that are no longer connected to an actual IsisModel (the IsisModel
    !   that has been removed by the garbage collector of R).
    !

    contains
        integer function create_mws()
            call init_modules()
            count = count + 1
            if (count > MAX_SIZE) then
                call rexit("Maximum number of models reached")
            endif
            create_mws = count
        end function create_mws
end module modelworkspaces
