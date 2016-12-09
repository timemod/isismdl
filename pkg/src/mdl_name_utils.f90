module mdl_name_utils
use model_params
use mdl_nlen
private :: putnam, mcswap, heapifymvn

contains

!
! model name utilities
!


!-----------------------------------------------------------------------

function putnam(word,wlen,names,kmax,plast)

!     adds <word> to <names> string memory
!     both <word> and <names> are in Isis chars

!     Arguments

!        In     word       Integer(*)      name
!        In     wlen       Integer         length of word
!        Inout  names      Integer(*)      string memory
!        In     kmax       Integer         sizeof names in integers
!        Inout  plast      Integer         pointer for last names into names
!                                          on first call must be 0

!     if word fits then
!       putnam will contain pointer <k,n>: k * 64 + n - 1
!          k points to last byte in names array
!          n is length of name minus 1 (range 0..63)
!     else
!       putnam is set to -1

use mdl_nlen

integer(kind = MC_IKIND) :: putnam
integer(kind = MC_IKIND), intent(in) ::  wlen, word(*)
integer, intent(in) ::  kmax
integer(kind = MC_IKIND), intent(inout) :: plast, names(*)

integer ::  lc, end, knew, newend

!     end     = byte index of end of last name inserted
!     newend  = byte index of end of newly to insert name
!     knew    = index into <names> of end of newly to insert name

lc     = wlen
end    = plast / 64
newend = end + lc
knew   = (newend - 1) / MCNYI4 + 1

if(knew .gt. kmax) then
!        string memory full ==> fatal
   putnam = -1
else
   plast  = newend * 64 + lc - 1
   putnam = plast
   call bysmov(word,1,lc,names,end+1)
endif

return
end function putnam

!-----------------------------------------------------------------------

function gtnmex(ptr, names, nlen)

!         returns name with pointer <ptr> from string memory <names>
!         nlen holds exact length
!         the name is returned in F77 characters

    use mdl_nlen
!         MCMXI4  maximum length of name in integer words
integer ::  mcmxi4
    parameter(MCMXI4 = MCMXNM/MCNYI4)

    character*(mcmxnm) gtnmex

    integer(kind = MC_IKIND), intent(in) :: ptr
    integer(kind = MC_IKIND), dimension(*), intent(in) :: names
    integer, intent(out) :: nlen

integer ::  itname(mcmxi4)
    character*(mcmxnm) tempn
integer ::  end,n

    tempn = ' '

    end = ptr / 64
    n   = mod(ptr,64)
    call bysmov(names,end-n,n+1,itname,1)
    call byasf7(itname,1, n+1, tempn)
    nlen = n+1
    gtnmex = tempn

    return
end function gtnmex

!-----------------------------------------------------------------------

subroutine mcf7ex( name, nlen, vnptr, vnames)

!         extract a name in f77 chars with pointer vnptr from vnames(*)
!         into name and put exact length in nlen


    character*(mcmxnm), intent(out) :: name
    integer, intent(out) ::  nlen
    integer(kind = MC_IKIND), intent(in) :: vnptr, vnames(*)

    name = gtnmex(vnptr, vnames, nlen)

    return
end subroutine mcf7ex

!-----------------------------------------------------------------------

subroutine mcgetnam( name, nlen, vnptr, vnames)

!   extract a name with pointer vnptr from vnames(*)
!   into name as byte string and in ASCII charset
!   and put exact length in nlen

    integer, intent(out) :: name(*), nlen
    integer(kind = MC_IKIND), intent(in) :: vnptr, vnames(*)

    integer ::  end, n

    end = vnptr / 64
    n   = mod(vnptr,64)
    call bysmov(vnames,end-n,n+1,name,1)
    nlen = n+1

    return
end subroutine mcgetnam

!-----------------------------------------------------------------------

integer function mcncmp(name,nb, snames, sptr)

!         compare <name> with length <nb> with
!         the name with pointer <sptr> from string memory <snames>
!         returns
!            0      if equal
!           -1      if name < snames[~sptr]
!            1      if name > snames[~sptr]

    integer, intent(in) ::  name(*),nb
    integer(kind = MC_IKIND), intent(in) :: snames(*), sptr

integer ::  sb,start, k
integer ::  byscmp

    sb    = mod(sptr,64)
    start = sptr / 64 - sb
    sb    = sb + 1

    k = byscmp(name,1,min(sb,nb),snames,start)
    if( k .eq. 0 .and. sb .ne. nb ) then
       if( nb .lt. sb ) then
           k = -1
       else
           k =  1
       endif
    endif

    mcncmp = k
    return
end function mcncmp

!-----------------------------------------------------------------------

integer function add_name(word, word_len, indx, names, nptr, cnt, kmax, ilast)

!     adds <word> in string memory <names> at position indx.
!     <nptr> contains pointers into <names>
!     <word> is a ASCII byte string

!     <cnt > is number of elements in <names>
!     <indx> is the index in the list

!     returns from word == names[k]

!     <kmax> is maximum dimension of <names>
!     <ilast> is last pointer into <names>

!     if out of string memory condition occurred when adding a name
!     return value is -1


integer(kind = MC_IKIND), intent(in) :: word_len
integer(kind = MC_IKIND), intent(in) :: word(*), indx
integer(kind = MC_IKIND), intent(inout) :: cnt, ilast
integer, intent(in) ::  kmax
integer(kind = MC_IKIND), intent(inout) ::  names(*), nptr(*)

integer(kind = MC_IKIND) :: k
integer :: nb

add_name = 0

!     convert <word> to Isis string in Isis chars
nb = word_len

!     add new name and insert index key

k = putnam(word, word_len, names, kmax, ilast)
if( k == -1 ) then
    add_name = -1
else
    cnt = cnt + 1
    nptr(indx) = k
    add_name = indx
endif

return
end function add_name

!-----------------------------------------------------------------------

integer function maxnln(inames,ib,ie)

!     return maximum length of names referenced by inames pointer array
!     for entries ib .. ie

   integer(kind = MC_IKIND), dimension(*), intent(in) :: inames
   integer(kind = MC_IKIND), intent(in) :: ib, ie

   integer(kind = MC_IKIND) ::  nlen,i

   nlen = 0
   do i = ib, ie
      nlen = max( nlen , 1 + mod(inames(i),64) )
   end do

   maxnln = nlen

   return
end function maxnln

!-----------------------------------------------------------------------

integer function maxnli(inames, nidx, ib,ie)

!        return maximum length of names referenced by inames pointer array
!        for entries nidx(ib) .. nidx(ie)

   integer(kind = MC_IKIND) :: inames(*), nidx(*)
   integer(kind = MC_IKIND) :: ib, ie

   integer(kind = MC_IKIND) :: nlen, i

   nlen = 0
   do i = ib, ie
      nlen = max( nlen , 1 + mod(inames(nidx(i)),64) )
   end do

   maxnli = nlen

   return
end function maxnli

!-----------------------------------------------------------------------

integer function maxnlz(inames, nidx, ib,ie)

!        same as maxnli but skip entries <= 0 in nidx

   integer(kind = MC_IKIND) :: inames(*), nidx(*)
   integer(kind = MC_IKIND) :: ib, ie

   integer(kind = MC_IKIND) :: nlen, i

   nlen = 0
   do i = ib, ie
       if (nidx(i) .gt. 0 ) then
           nlen = max( nlen , 1 + mod(inames(nidx(i)),64) )
       endif
   end do

   maxnlz = nlen

   return
end function maxnlz

!----------------------------------------------------------

function mvncmp(a, b, nameptrs, names)

! compare names of model variables a and b lexicographically

implicit none
integer(kind = MC_IKIND) :: mvncmp
integer(kind = MC_IKIND), intent(in) :: a, b, nameptrs(*), names(*)
integer, external   :: byscmp
integer             :: sa, sb, starta, startb, k

sa     = mod(nameptrs(a), 64)
sb     = mod(nameptrs(b), 64)
starta = nameptrs(a) / 64 - sa
startb = nameptrs(b) / 64 - sb
sa     = sa + 1
sb     = sb + 1

k  = byscmp(names, starta, min(sa, sb), names, startb)
if( k .eq. 0 .and. sa .ne. sb ) then
    if( sa .lt. sb ) then
        k = -1
    else
        k =  1
    endif
endif

mvncmp = k

return
end function mvncmp

!-------------------------------------------------------------------

subroutine hsortmvn(ivar, nvar, nameptrs, names)

    ! Sort model variable names lexicographically using heapsort.
    ! See A. Aho, B.W. Kernighan, and P.J.Weinberger, The AWK programming
    ! language.
    ! ivar(i) (i=1, nvar) is a list of indices for the variable names
    ! to be ordered.
    ! nameptrs(ivar(i)) points to the position in names where the name
    ! of the corresponding variable name is stored.
    implicit none
    integer(kind = MC_IKIND), intent(out) :: ivar(nvar)
    integer(kind = MC_IKIND), intent(in)  :: nvar, nameptrs(*), names(*)

    integer(kind = MC_IKIND)  :: i

    do i = 1, nvar
        ivar(i) = i
    end do

    do i = nvar / 2, 1, -1
        call heapifymvn(ivar, i, nvar, nameptrs, names)
    end do

    do i = nvar, 2, -1
        call mcswap(ivar,1_MC_IKIND, i) 
        call heapifymvn(ivar, 1_MC_IKIND, i - 1_MC_IKIND, nameptrs, names)
    end do

    return
 end subroutine hsortmvn

!----------------------------------------------------------------------

 subroutine heapifymvn(ivar, l, r, nameptrs, names)

     ! routine for heapsort algorithm for model variables names
     ! see hsortmvn

     implicit none
     integer(kind = MC_IKIND), intent(inout) :: ivar(*)
     integer(kind = MC_IKIND), intent(in) :: l, r, nameptrs(*), names(*)
     integer(kind = MC_IKIND) :: p, c

     p = l
     c = 2*p
     do while (c <= r)
        if (c < r .and. mvncmp(ivar(c+1), ivar(c), nameptrs, names) .eq. 1) then
            c = c + 1
        endif
        if (mvncmp(ivar(p), ivar(c), nameptrs, names) .eq. -1)  then
            call mcswap(ivar, c, p)
        endif
        p = c
        c = 2*p
     end do
     return
 end subroutine heapifymvn

! -----------------------------------------------------------------------------

 subroutine mcswap(x, i, j)
     integer(kind = MC_IKIND), intent(inout) ::  x(*)
     integer(kind = MC_IKIND), intent(in) ::  i, j

     integer(kind = MC_IKIND) ::  tmp
 
     tmp = x(i)
     x(i) = x(j)
     x(j) = tmp
     return
 end subroutine mcswap

! -----------------------------------------------------------------------------

  integer function find_name(name, namelen, nptr, sortidx, names, cnt)
      integer, intent(in) :: name(*), namelen
      integer(kind = MC_IKIND), intent(in) ::  nptr(*), sortidx(*), names(*)
      integer(kind = MC_IKIND), intent(in) :: cnt
  
  
      integer :: sb, se, j, k, index
  
      find_name = 0
  
      sb = 1
      se = cnt
      do
           if (sb > se) exit
           j = (sb + se) / 2
           k = mcncmp(name, namelen, names, nptr(sortidx(j)))
           if (k == 0) then
               find_name = sortidx(j);
               return
           elseif (k > 0) then
               sb = j + 1
           else
               se = j - 1
           endif
      end do
  
  end function find_name

end module mdl_name_utils


