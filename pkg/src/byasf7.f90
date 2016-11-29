      subroutine byasf7(str,fb,nb,ch)
!     BYte string to a Fortran 77 AScii charstring.
!     convert STR of length NB to CH starting in FB
!
      logical*1 cstr,str(*)
      integer*4 fb,nb,i,cstri
      character ch*(*),char
      equivalence(cstr,cstri)

! initialise ch
      ch = ' '

! copy nb bytes from str to ch
      do 10 i=1,min(nb,len(ch))
        cstr=str(fb+i-1)
        ch(i:i)=char(cstri)
10    continue
      return
      end
