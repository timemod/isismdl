      subroutine byasf7(str,fb,nb,ch)
!     BYte string to a Fortran 77 AScii charstring.
!     convert STR of length NB to CH starting in FB
!
        integer str(*)
        character ch*(*)
        integer   fb,nb,i
        integer,external :: bysget
        ch = ' '
        do i=1,min(nb,len(ch))
          ch(i:i) = char(bysget(str,fb+i-1))
        enddo
        return
        end
