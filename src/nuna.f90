module nuna
    use kinds

    ! constant for holding missing values.
    ! the constant should be intitialised once.
    real(kind = ISIS_RKIND), save :: NA = -1.0_ISIS_RKIND

    contains

        ! initialise constant NA by calling the C function 
        ! get_na_real (see nuna.c). 
        subroutine init_na
            real(kind = ISIS_RKIND), external :: get_na_real
            NA = get_na_real()
        end subroutine init_na

        logical function nuifna(x)
            use iso_c_binding
            real(kind = ISIS_RKIND), intent(in) :: x
    
            integer(c_int) :: i
            integer(c_int), external :: r_finite

            i  = r_finite(x)
            nuifna = i == 0
            return
        end function nuifna

        ! Numerical test for vector having all NA
        function nuvana(x, n)
            logical :: nuvana
            real(kind = ISIS_RKIND), dimension(*), intent(in) :: x
            integer, intent(in) :: n
             
            !    Logical function nuvana(x,n)
             
            !    Arguments
             
            !        In         x    real(*)   vector of reals
            !        In         n    integer   elements in x
             
            !    Nuvana returns true if all elements x[1..n] are NA
            !    If n == 0 then nuvana will return false
            
            integer ::  i
            
            nuvana = .false.
            do  i = 1, n
                if (.not. nuifna(x(i))) return
            enddo

            ! all elements of x are NA
            nuvana = .true.
        end function nuvana

        subroutine skipna(x, n, first, last) 
            ! This subroutine calculates the indices of the 
            ! first and the last valid value in an array.
            ! If all values are invalid, then first and last are both
            ! set to -1.
            real(kind = ISIS_RKIND), dimension(*), intent(in) :: x
            integer, intent(in) :: n
            integer, intent(out) :: first, last

            integer ::  i

            first = -1
            last  = -1
            
            do  i = 1, n
                if (.not. nuifna(x(i))) then
                    first = i
                    exit
                endif
            enddo
            if (first == -1) return
            do  i = n, 1, -1
                if (.not. nuifna(x(i))) then
                    last = i
                    exit
                endif
            enddo
        end subroutine skipna

end module nuna
