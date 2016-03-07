module msufstack

    !
    ! stack of user function calls in the model
    !

    integer, parameter :: MAX_USERF_STACK  = 10

    type :: userf_ipinfo
        integer :: ipsave
        integer :: ipendsave
        integer :: nstuf
        integer :: nstuf_old
    end type userf_ipinfo

    integer, save :: iuserf
    type(userf_ipinfo), save :: userf_stack(MAX_USERF_STACK)

    contains

        subroutine init_ufstack
            iuserf = 0
        end subroutine init_ufstack

        subroutine push_userf(ipsave, ipendsave, nstuf, nstuf_old, error)
            integer, intent(in) :: ipsave, ipendsave, nstuf, nstuf_old
            integer, intent(out) :: error

            if (iuserf == MAX_USERF_STACK) then
                error = 1
                return
            else
                error = 0
            endif

            iuserf = iuserf + 1
            userf_stack(iuserf) = userf_ipinfo(ipsave, ipendsave, &
&                                              nstuf, nstuf_old)

        end subroutine push_userf

        subroutine pop_userf(ipsave, ipendsave, nstuf, nstuf_old)
            integer, intent(out) :: ipsave, ipendsave, nstuf, nstuf_old

            type(userf_ipinfo) :: info

            info = userf_stack(iuserf)
            ipsave = info%ipsave
            ipendsave = info%ipendsave
            nstuf = info%nstuf
            nstuf_old = info%nstuf_old
            iuserf = iuserf - 1

        end subroutine pop_userf

end module msufstack
