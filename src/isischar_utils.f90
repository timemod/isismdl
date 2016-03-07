!
! Isis uses its own character set (called internal characters).
! They are of course not used in the new module macromod,
! but old mif files created with Isis still contain them.
! Therefore convert them to ASCII characters.
!
module isischar_utils
use model_params

private :: byinas, byaslw, convert_name

contains

!------------------------------------------------------------------------------

subroutine convert_name(nptr, names)

!   convert a name with pointer nptr from names(*)
!   from Isis internal characters to ASCII characters.
!   This is needed for reading old mif files 
!   generated with Isis
    integer(kind = MC_IKIND), intent(in) :: nptr
    integer(kind = MC_IKIND), intent(inout) :: names(*)
    integer ::  end, n

    integer :: dummy(100)

    end = nptr / 64
    n   = mod(nptr,64)

    call bysmov(names, end - n, n + 1, dummy, 1)
    call byinas(dummy, n + 1, dummy)
    call byaslw(dummy, n + 1, dummy)
    call bysmov(dummy, 1, n + 1, names, end - n)

    return
end subroutine convert_name

!------------------------------------------------------------------------------

subroutine convert_names(n, inames, names, indexn, sort)
    use mdl_name_utils
    integer, intent(in) :: n, inames(*)
    integer, intent(inout) :: names(*)
    integer, intent(out) :: indexn(*)
    logical, intent(in) :: sort

    !
    ! convert model names from Isis internal characters to ASCII characters.
    ! This is temporarily required for the model conversion
    !
    
    integer :: i

    do i = 1, n 
        call convert_name(inames(i), names)
    end do

    if (.not. sort) return

    ! sort the names. For ASCII characters the ordering of
    ! the names nmay be different
    do i = 1, n
        indexn(i) = i
    end do
    call hsortmvn(indexn, n, inames, names)

end subroutine convert_names

! Byte convert ISIS TEXT -> ASCII FORTRAN routine
! Only needed to convert Isis characters to ASCII characters,
! this routine will not be needed in the final version.
subroutine byinas(fstr, nb, tstr)
use c_iso_tables

integer ::  fstr(*), nb, tstr(*)
!----------------------------------------------------------------------!
!                                                                      !
!  Programmed: april 1986 (Jan Bunzel)                                 !
!                                                                      !
!      Subroutine BYINAS move NB bytes translated from ISIS TEXT       !
!      to ASCII characters from FSTR to TSTR.                          !
!                                                                      !
!  Modified october 2012 (Rob van Harrevelt)                           !
!      byinas and byinex are now identical, sy byinex is removed       !
!      It seems that in old versions of Isis byinas                    !
!      did something with special fonts (bold, italic), but special    !
!      fonts are no longer supported in modern versions of Isis        !
!      (it is not clear whether special fonts have ever been           !
!       functional in Isis).                                           !
!----------------------------------------------------------------------!

!     **** local variables
integer ::  bvs, bvg, i

!     **** functions
integer ::  bysgtn

!     **** initialise next-routines
call bysgti(fstr, 1)
call byssti(tstr, 1)

!     **** convert characters
do i = 1, nb
  bvg = bysgtn()
  bvs = TBLCVt(bvg)
  call bysstn(bvs)
end do

!     **** reinitialise next-routines
call bysgtr
call bysstr

return
end subroutine byinas

subroutine byaslw(fstr,nb,tstr)
use c_iso_tables
integer fstr(*),nb,tstr(*)
!----------------------------------------------------------------------!
!                                                                      !
!      Subroutine BYASLW move NB bytes translated to lowercase         !
!      from FSTR to TSTR.                                              !
!                                                                      !
!----------------------------------------------------------------------!
!     **** local variables
integer bvs,bvg,i
!     **** functions
integer bysgtn

!     **** initialise next-routines
call bysgti(fstr,1)
call byssti(tstr,1)

!     **** convert characters
do  i=1,nb
  bvg = bysgtn()
  bvs = LOWTBl(bvg)
  call bysstn(bvs)
enddo

!     **** reinitialise next-routines
call bysgtr
call bysstr

return
end subroutine byaslw

end module isischar_utils
