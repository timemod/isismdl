module output_utils

    ! buffer for string conversion
    ! byte string for output
    ! TODO: use c_char type of iso-c-binding, also update byf7as.
    ! NOTE: one extra byte for the terminating 0.
    ! todo: is parameter STRLEN long enough?

    integer, parameter, private :: STRLEN = 255
    integer, parameter, private ::  MDNYI4 = 4
    integer, dimension((STRLEN + 1 - 1) / MDNYI4 + 1), private :: istr

    contains

        subroutine macromod_report(string)
            character(len = *), intent(in) :: string

            integer :: nb

            call byf7as(string, istr, 1, nb)
            call report_str(istr)

        end subroutine macromod_report

        subroutine macromod_out(string)
            character(len = *), intent(in) :: string
            call intpr(string, -1, 1, 0)
        end subroutine macromod_out

        subroutine macromod_warn(string)
            character(len = *), intent(in) :: string
            call rwarn(string)
        end subroutine macromod_warn

        subroutine macromod_error(string)
            character(len = *), intent(in) :: string
            call rexit(string)
        end subroutine macromod_error

end module output_utils
