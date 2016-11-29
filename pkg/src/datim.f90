subroutine datim(c,i)
 
! Writes the current time or date to string c.
! String c should contain at least 10 characters.
! Input parameter i determines if the time (i = 1)
! or date (i = 2) is written to c.

character*(*) c
integer ::  i

integer ::  values(8)

call date_and_time(VALUES = values)

if (i.eq.1) then
     write(c,'(I2.2,A1,I2.2,A1,I2.2)') values(5),':', values(6),':', values(7)
else
     write(c,'(I4.4,A1,I2.2,A1,I2.2)') values(1),'-', values(2),'-', values(3)
endif

return
end
