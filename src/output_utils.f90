module output_utils

    interface   
        ! interface to C function report_str
        subroutine report_str(string) bind(C, name = "report_str")
            use iso_c_binding, only : c_char
            character(kind = c_char), intent(in) :: string
        end subroutine report_str
    end interface

    contains

        subroutine macromod_out(string)
            character(len = *), intent(in) :: string
            integer :: n
            n = len_trim(string)
            if (n > 0) then
                call intpr(string, n, 1, 0)
            else 
                call intpr(" ", -1, 1, 0)
            endif
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
