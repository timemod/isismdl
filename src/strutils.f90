 
! Function lenstr returns the length of string c without
! trailing blank or zero characters

integer function lenstr( c )
 
 
 
character*(*) c
integer ::  cnt, ic,l
 
!  Skip trailing blanks/zeroes.
 
cnt = len(c)
5 ic = ichar(c(cnt:cnt))
if ((ic.eq.32).or.(ic.eq.0)) then
   cnt=cnt-1
   if (cnt.gt.1) then
      goto 5
   else
      cnt = 1
      goto 20
   endif
endif
20 lenstr = cnt
return
end

!----------------------------------------------------------------------
subroutine str_rplc( text, length, c1, c2 )
 
!     subroutine replaces all fortran character c1 in txt by character c2
 
character*(*) text, c1*1, c2*1
integer ::  length, i, ix, k
 
i = 1
do
   ix = index(text(i:length), c1)
   if (ix == 0) exit
!        replace character c1 then search for other ones
   k = ix+i-1
   text(k:k) = c2
   i = ix+1
end do
 
return
end

!----------------------------------------------------------------------
subroutine str_delc( text, length, c1 )
 
!     subroutine deletes all fortran character c1 in txt
 
character*(*) text, hlp*(length), c1*1
integer ::  length, i, k
 
hlp = text
k = 0
do i = 1,length
   if (hlp(i:i) .ne. c1) then
      k = k+1
      text(k:k) = hlp(i:i)
   endif
end do
length = k
 
return
end

!----------------------------------------------------------------------
subroutine str_trunc( text, length, c1 )
 
!     subroutine truncates text at first character c1
 
character*(*) text, c1*1
integer ::  length, i, ix
 
ix = index(text, c1)
if (ix == 0) return

!     truncate name at first appearance of character c1
length = ix-1
text = text(:length)
 
return
end

