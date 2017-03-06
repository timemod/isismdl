subroutine mcosjc(mftunt,mftwid,x,xdim,nxrc,inames,names,rcidx)

!     output structural jacobian in readable format
!     output a matrix x(nxrc,nxrc) nicely with a header mathdr
!     matrix x declared as x(xdim,*)
!     inames name pointer array
!     names  name string memory

!     rcidx   contains numbers of variables to print in rows/columns

use model_params
use mdl_name_utils
integer ::        mftunt,mftwid
integer(kind = MC_IKIND) :: xdim, nxrc, inames(*), names(*), rcidx(*), x(xdim, *)


integer ::        colwid, maxrhl, maxchl
integer ::        ncpb,ncpe,npcols,i,j

!     for extracting variable name

character*(mcmxnm)  name
integer ::        nlen

!     output buffer (1 line)

character(len = 256) :: str
integer ::        spos

integer ::        rpos, k

integer ::   icspac,xwid
parameter(ICSPAC = 1, XWID = 1)

!     colwid  column width
!     maxrhl  length of longest row    header
!     maxchl  length of longest column header
!     npcols  number of numeric columns in each strip
!     ncpb    index of first column to print
!     ncpe    index of last  column to print

maxrhl = maxnli(inames, rcidx, 1_MC_IKIND, nxrc)
maxchl = maxrhl

colwid = max(maxchl, 1)

npcols = max(1, (mftwid - maxrhl - 1) / (colwid + ICSPAC) )
npcols = min(npcols, nxrc, 20)

do  ncpb = 1, nxrc, npcols

     ncpe = min(ncpb + npcols - 1, nxrc)

     if( ncpb .gt. 1 ) then
        write(mftunt, '()')
     endif

!          write col headers (right justified)

     str  = ''
     spos = 1 + maxrhl + ICSPAC

     do  k = ncpb, ncpe
        call mcf7ex(name ,nlen, inames(rcidx(k)), names)
        rpos = spos + colwid - nlen
        str(rpos : rpos + nlen - 1) = name(:nlen)
        spos = spos + colwid + ICSPAC
     enddo

     write(mftunt,'(1x,a)') trim(str)

     do  i=1,nxrc

!             write row header (left justified)

        call mcf7ex(name, nlen, inames(rcidx(i)), names)

        str  = name(:nlen)
        spos = 1 + maxrhl + ICSPAC

!             write x(i,j=ncpb..ncpe) (right justified)

        do  j = ncpb, ncpe
           rpos = spos + colwid - XWID
           if( x(i,j) .gt. 0 ) then
              str(rpos : rpos + XWID - 1) = 'x'
           else
              str(rpos : rpos + XWID - 1) = '-'
           endif
           spos = spos + colwid + ICSPAC
        enddo

        write(mftunt,'(1x,a)') trim(str)

     enddo
enddo

return
end

!-----------------------------------------------------------------------

subroutine mcosjs(mftunt,x,xdim,nxrc)

!     output statistics of symbolic jacobian
!     matrix x declared as x(xdim,*)
!      !!!! matrix elements should contain 1 or 0

use model_params
integer ::    mftunt
integer(kind = MC_IKIND) :: xdim, nxrc, x(xdim, *)

integer ::    nzcnt,mxzcol,mxzrow,i,j,k

!     non zero entries

nzcnt = 0
do  j=1,nxrc
   do  i=1,nxrc
      nzcnt = nzcnt + x(i,j)
   enddo
enddo

!     max non zero column

mxzcol = 0
do  j=1,nxrc
   k = 0
   do  i=1,nxrc
      k = k + x(i,j)
   enddo
   mxzcol = max(mxzcol,k)
enddo

!     max non zero row

mxzrow = 0
do  i=1,nxrc
   k = 0
   do  j=1,nxrc
      k = k + x(i,j)
   enddo
   mxzrow = max(mxzrow,k)
enddo

write(mftunt,900)
write(mftunt,901) nzcnt
write(mftunt,902) mxzrow
write(mftunt,903) mxzcol
write(mftunt,'()')

900 format( 5x, 'Statistics of matrix')
901 format( 8x, 'Number of non zeros  ',i8)
902 format( 8x, 'Longest non zero row ',i8)
903 format( 8x, 'Longest non zero col ',i8)

return
end
