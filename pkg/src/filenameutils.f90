module filenameutils

contains

subroutine get_basename_sans_ext(filename, basename_sans_ext)

    ! Returns the basename (file without path) without extension.
    !
    ! Arguments:
    !   filename          - input filename (with or without path)
    !   basename_sans_ext - output; caller supplies a string long enough
    !                       to hold the result (not checked)
    !
    character(len=*), intent(in)  :: filename
    character(len=*), intent(out) :: basename_sans_ext

    character(len = 1), parameter :: EXTSEP = '.'

    integer :: dirloc, extloc, length

    extloc = index(filename, EXTSEP, back = .true.)
    dirloc  = max(index(filename, "/", back = .true.), &
                  index(filename, "\", back = .true.))
    if (extloc > dirloc) then
        length = extloc - 1
    else
        length = len_trim(filename)
    endif
    basename_sans_ext = filename(dirloc + 1 : length)

end subroutine get_basename_sans_ext

end module filenameutils
