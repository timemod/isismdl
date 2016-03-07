! BYte string from a Fortran 77 AScii charstring.
subroutine byf7as(ch,str,fb,nb)
!     BYte string from a Fortran 77 AScii charstring,
!     including a terminating 0.
!     convert CH to STR starting in FB and returns length
!     of CH in NB.
!     Trailing blanks are ignored, except if the string contains
!     only blanks.

logical*1 cstr,str(*)
integer ::  fb, nb, i, cstri, ichar
integer ::  lenstr
character ch*(*)
equivalence(cstr,cstri)

!     get length with trailing blanks removed
nb = lenstr(ch)
if (nb.eq.0) then
!         string contains nothing but blanks. set size equal to original
!         size
    nb=len(ch)
endif

do i=1,nb
  cstri = ichar(ch(i:i))
  str(fb+i-1) = cstr
end do
! add terminating 0
cstri = 0
str(fb+nb) = cstr
return
end
