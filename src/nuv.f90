module nuv

    ! module for numerical vector routines
    ! most subroutines are just interfaces to blas and lapack,
    ! some routines have been implemented

    ! interface of blas and lapack routines
    interface

        function nuvana(x, n)
            use kinds
            logical :: nuvana
            real(kind = ISIS_RKIND), intent(in) :: x(*)
            integer, intent(in) :: n
        end function nuvana

        subroutine nuvaxy(n, alpha, x, incx, y, incy)
            use kinds
            integer, intent(in) :: n, incx, incy
            real(kind = ISIS_RKIND), intent(in) :: alpha, x(*)
            real(kind = ISIS_RKIND), intent(inout) :: y(*)
        end subroutine nuvaxy

        subroutine nuvcop(n, x, incx, y, incy)
            use kinds
            integer, intent(in) :: n, incx, incy
            real(kind = ISIS_RKIND), intent(in) :: x(*)
            real(kind = ISIS_RKIND), intent(inout) :: y(*)
        end subroutine nuvcop

        subroutine nuvcpz(n, x, incx, y, incy)
            use kinds
            integer, intent(in) :: n, incx, incy
            real(kind = ISIS_RKIND), intent(in) :: x(*)
            real(kind = ISIS_RKIND), intent(inout) :: y(*)
        end subroutine nuvcpz

        function ddot(n, x, incx, y, incy)
            use kinds
            real(kind = ISIS_RKIND) :: ddot
            integer, intent(in) :: n, incx, incy
            real(kind = ISIS_RKIND), intent(in) :: x(*), y(*)
        end function ddot

        function dnrm2(n, x, incx)
            use kinds
            real(kind = ISIS_RKIND) :: dnrm2
            integer, intent(in) :: n, incx
            real(kind = ISIS_RKIND), intent(in) :: x(*)
        end function dnrm2

        function dasum(n, x, incx)
            use kinds
            real(kind = ISIS_RKIND) :: dasum
            integer, intent(in) :: n, incx
            real(kind = ISIS_RKIND), intent(in) :: x(*)
        end function dasum

        subroutine nuvsca(n, alpha, x, incx)
            use kinds
            integer, intent(in) :: n, incx
            real(kind = ISIS_RKIND), intent(in) :: alpha
            real(kind = ISIS_RKIND), intent(inout) :: x(*)
        end subroutine nuvsca

        integer function idamax(n, dx, inc)
            use kinds
            integer, intent(in) :: n, inc
            real(kind = ISIS_RKIND), intent(in) :: dx(*)
        end function idamax

        subroutine nuvswa(n, x, incx, y, incy)
            use kinds
            integer, intent(in) :: n, incx, incy
            real(kind = ISIS_RKIND), intent(inout) :: x(*), y(*)
        end subroutine nuvswa

    end interface

    contains

    ! Numerical calculate givens rotator
    subroutine nuvgiv(x, y, c, s)
        use kinds
        use nucnst
        real(kind = ISIS_RKIND), intent(inout) :: x, y
        real(kind = ISIS_RKIND), intent(in) :: c, s
    
        !     Format
         
        !        Call Nuvgiv( x, y, c, s)
         
        !     Parameters
         
        !     Inout   x     Real*8       x input / c*x+s*y on output
        !     Inout   y     Real*8       y input / 0       on output
        !     Out     c     Real*8       c of tranformation (cosine)
        !     Out     s     Real*8       s of tranformation (  sine)
         
         
        !     Description
         
        !     Nuvgrot calculates the givens rotator
         
         
        !             |  c   s |
        !         G = |        |
        !             | -s   c |
         
        !     with  c*c+s*s=1
         
        !     for which G * | x | = | z |
        !                   | y |   | 0 |
         
        !     then we have
         
        !            c * x + s * y = z
        !           -s * x + c * y = 0   ==>  s/c = y/x or c/s = x/y
         
        !    Use Lapack dlartg routine and return c and s and modified
        !    x and y.
         
        real(kind = ISIS_RKIND) :: t
        
        call dlartg(x, y, c, s, t)
    
        x = t
        y = Rzero
        
        return
    end subroutine nuvgiv

end module nuv
