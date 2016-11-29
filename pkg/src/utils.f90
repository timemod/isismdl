module utils

contains

    integer function idxlzuk(a, n, kxar)
        ! linear lookup in array a(1..n) for element .eq. kxar
        ! return 0 if not found
        ! return index of element

        use model_params
        integer(kind = MC_IKIND), intent(in) :: a(*)
        integer(kind = MC_IKIND), intent(in) :: n
        integer, intent(in) :: kxar

        integer ::  j

        idxlzuk = 0
        do j = 1, n
            if (a(j) == kxar) then
                idxlzuk = j
                exit
            endif
        enddo
        return
     end function idxlzuk

end module utils
