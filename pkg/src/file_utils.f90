 
! file utilities in fortran
 

! ------------------------------------------------------------
integer function delete_file(filenm)
  character(len = *) :: filenm
 
! delete file filenm. Returns 0 on success and nonzero on failure.
! The function also returns 0 if the file does not exist.
 
 logical :: exist

inquire(file = filenm, exist = exist)
if (.not. exist) then
   delete_file = 0
   return
endif

delete_file = unlink(filenm)

end function delete_file
