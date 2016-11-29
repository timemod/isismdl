module filenameutils

private :: get_dir_loc

character(len = 1), parameter, private :: EXTSEP = '.'

contains

subroutine remove_extension(filename, filename_no_ext)
    character(len = *), intent(in)  :: filename
    character(len = *), intent(out) :: filename_no_ext

    !
    ! Removes the extension from a filename and returns the result
    ! in character string filename_no_ext.
    !

integer ::  extloc, dirloc, length

    extloc = index(filename, EXTSEP, back = .true.)
    dirloc = get_dir_loc(filename)
    if (extloc > dirloc) then
        length = extloc - 1
    else
        length = len_trim(filename)
    endif
    filename_no_ext = filename(:length)
    return
end subroutine remove_extension

integer function get_dir_loc(filename)
    character(len = *), intent(in)  :: filename

    ! Returns the location of the last directory
    ! separator in the filename.

    integer :: dir_loc_slash, dir_loc_backslash

    dir_loc_slash = index(filename, "/", back = .true.)
    dir_loc_backslash = index(filename, "\", back = .true.)
    get_dir_loc = max(dir_loc_slash, dir_loc_backslash)
end function get_dir_loc

subroutine split_path(filename, dirname, basename, dirsep)
    character(len = *), intent(in)  :: filename
    character(len = *), intent(out) :: dirname, basename
    character(len = 1), intent(out) :: dirsep

    integer :: dirloc

    dirloc = get_dir_loc(filename)
    if (dirloc > 0) then
        dirname = filename(: dirloc - 1)
        basename = filename(dirloc + 1 :)
        dirsep = filename(dirloc : dirloc)
    else
        dirname = ''
        basename = filename
        dirsep = ''
    endif

end subroutine split_path

end module filenameutils
