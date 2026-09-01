module file_utils

  use :: iso_c_binding, only : c_ptr, c_int, c_char, c_null_char, c_loc
  use :: mdl_flen
 
  private

  interface
      function c_unlink(path) bind(c, name='unlink')
          import :: c_ptr, c_int
          type(c_ptr), value :: path
          integer(c_int) :: c_unlink
      end function c_unlink
  end interface

  public :: delete_file

  contains

  ! delete file filenm. Returns 0 on success and nonzero on failure.
  ! The function also returns 0 if the file does not exist.
  integer function delete_file(filenm) result(res)
    character(len = *), intent(in) :: filenm

    character(len = MAXFLEN + 1, kind = c_char), target :: c_path
    logical :: exist

    inquire(file = filenm, exist = exist)
    if (.not. exist) then
      res = 0
      return
    end if

    c_path = trim(filenm) // c_null_char
    res = c_unlink(c_loc(c_path))
  end function delete_file

end module file_utils
