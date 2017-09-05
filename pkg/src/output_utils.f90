module output_utils

    contains

        subroutine isismdl_out(string)
            character(len = *), intent(in) :: string
            integer :: n
            n = len_trim(string)
            if (n > 0) then
                call intpr(string, n, 1, 0)
            else 
                call intpr(" ", -1, 1, 0)
            endif
        end subroutine isismdl_out

        subroutine isismdl_warn(string)
            character(len = *), intent(in) :: string
            call rwarn(trim(string))
        end subroutine isismdl_warn

        subroutine isismdl_error(string)
            character(len = *), intent(in) :: string
            call rexit(trim(string))
        end subroutine isismdl_error

end module output_utils
