subroutine mcwmif(mdl, mifnam, ier)
use model_type
use invpol
use mcpars

!     produces the Mif file for compiled model

!     Error return in argument <ier>

!        0        all ok
!        5        cannot open MIF file for write
!        6        write error MIF file
!        7        read error scratch file (only possible when mcwmif is called
!                 from mcisis)


!     Sets global error message string
!     but no error message is written (caller must do)

!     file errors also set global filerr/filios variables

type(model), intent(in)         :: mdl
character(len = *), intent(in)  :: mifnam
integer, intent(out)            :: ier

integer(kind = MC_IKIND), dimension(maxpol) :: polish

integer ::   ios, i
integer ::   smax
integer(kind = MC_IKIND) :: fbosiz
logical ::   exist, code_in_memory
logical(kind = MC_LKIND) :: ldum
logical(kind = MC_IKIND) :: idum

ier = 0

code_in_memory = allocated(mdl%equat)

!     fbosiz is the total size of the arrays mdl%fboptr (mdl%nfb + 1)
!     and mdl%fbordr (mdl%fbomem)
fbosiz = (mdl%nfb + 1) + mdl%fbomem

! --- MIF file

inquire(file=mifnam,exist=exist)
if(exist) then
   open(mciunt,file=mifnam)
   close(mciunt,status='DELETE', iostat=ios)
   if( ios .ne. 0 ) goto 950
endif

open(mciunt,file=mifnam,form='unformatted', &
&    status='new',access='stream',iostat=ios)

if(ios.ne.0) goto 950

!     write MIF header

write(mciunt,err=960,iostat=ios) mifver
write(mciunt,err=960,iostat=ios) mdl%cnamelen
write(mciunt,err=960,iostat=ios) mdl%cname(:mdl%cnamelen), mdl%date, mdl%time

write(mciunt,err=960,iostat=ios) mdl%neq,mdl%loops,mdl%loope,ldum,idum, &
&                 mdl%nuf,mdl%ielast,mdl%ufblen, mdl%eqblen, &
&                 mdl%nrp,mdl%nrc,mdl%nfb,mdl%nrv,mdl%nca,mdl%nd, &
&                 mdl%nd1,mdl%nendex,mdl%ivlast,mdl%iplast, &
&                 mdl%iflast,mdl%nulf,mdl%iulflast

!     write fbo flag to signal possible additional info on file

!     fboflg
!       0       No feedback ordering for this model
!       1       Feedback ordering on file
!       2       Generate feedback ordering after reading mif

!     mdl%fbomem > 0 size of feedback ordering
write(mciunt,err=960,iostat=ios) mdl%fboflg
if (mdl%fboflg > 0) then
    write(mciunt,err=960,iostat=ios) fbosiz
endif

!     string memory equation names

smax = (mdl%ielast/64 - 1) / MCNYI4 + 1
write(mciunt,err=960,iostat=ios) mdl%enames(1:smax)


!     equation datapointers

write(mciunt,err=960,iostat=ios) mdl%ienames(1:mdl%neq)
write(mciunt,err=960,iostat=ios) mdl%etype(1:(mdl%neq/MCNYI4+1))
write(mciunt,err=960,iostat=ios) mdl%lhsnum(1:mdl%neq)
write(mciunt,err=960,iostat=ios) mdl%order(1:mdl%neq)
write(mciunt,err=960,iostat=ios) mdl%indexe(1:mdl%neq)

!     string memory var/par names

smax = (mdl%ivlast/64 - 1) / MCNYI4 + 1
write(mciunt,err=960,iostat=ios) mdl%vnames(1:smax)

!     variables data/pointers etc

write(mciunt,err=960,iostat=ios) mdl%numfb(1:mdl%nfb)

write(mciunt,err=960,iostat=ios) mdl%iendex(1:mdl%nendex)
write(mciunt,err=960,iostat=ios) mdl%ica(1:mdl%nca)

write(mciunt,err=960,iostat=ios) mdl%aci(1:mdl%nrv)

write(mciunt,err=960,iostat=ios) mdl%ivnames(1:mdl%nrv)
write(mciunt,err=960,iostat=ios) mdl%ibx1(1:mdl%nrv)
write(mciunt,err=960,iostat=ios) mdl%ibx2(1:mdl%nrv)
write(mciunt,err=960,iostat=ios) mdl%indexv(1:mdl%nrv)
write(mciunt,err=960,iostat=ios) mdl%lik(1:mdl%nrv)

if( mdl%nrp .gt. 0 ) then
   smax = (mdl%iplast/64 - 1) / MCNYI4 + 1
   write(mciunt,err=960,iostat=ios) mdl%pnames(1:smax)
   write(mciunt,err=960,iostat=ios) mdl%ipnames(1:mdl%nrp)
   write(mciunt,err=960,iostat=ios) mdl%indexp(1:mdl%nrp)
   write(mciunt,err=960,iostat=ios) mdl%nvalp(1:mdl%nrp)
   write(mciunt,err=960,iostat=ios) mdl%cfptr(1:mdl%nrp)
   write(mciunt,err=960,iostat=ios) mdl%coef(1:mdl%nrc)
endif

if (mdl%nulf > 0) then
   ! the names of user language functions
   smax = (mdl%iulflast/64 - 1) / MCNYI4 + 1
   write(mciunt,err=960,iostat=ios) mdl%ulfnames(1:smax)
   write(mciunt,err=960,iostat=ios) mdl%iulfnames(1:mdl%nulf)
endif

if (mdl%nuf > 0) then
   ! the names of model functions
   smax = (mdl%iflast/64 - 1) / MCNYI4 + 1
   write(mciunt,err=960,iostat=ios) mdl%fnames(1:smax)
   write(mciunt,err=960,iostat=ios) mdl%ifnames(1:mdl%nuf)
   write(mciunt,err=960,iostat=ios) mdl%narguf(1:mdl%nuf)

   if (code_in_memory) then
       do i = 1, mdl%nuf
           npol = mdl%nrecuf(i + 1) - mdl%nrecuf(i)
           write(mciunt,err=960,iostat=ios) &
&            npol, mdl%equat(mdl%nrecuf(i) + 1: mdl%nrecuf(i + 1))
       end do
   else
       ! read polish code from ufnunt
       rewind ufnunt
       do i = 1, mdl%nuf
           read(ufnunt, err=970, iostat = ios) npol, polish(:npol)
           write(mciunt,err=960,iostat=ios) npol, polish(:npol)
       end do
   endif
endif

if (code_in_memory) then
    do i = 1, mdl%neq
        npol = mdl%nrecp(i + 1) - mdl%nrecp(i)
        write(mciunt,err=960,iostat=ios) &
&         npol, mdl%equat(mdl%nrecp(i) + 1 : mdl%nrecp(i + 1))
    end do
else
    ! read polish code from polunt
    rewind polunt
    do i=1,mdl%neq
        read(polunt, err=970, iostat=ios) npol, polish(:npol)
        write(mciunt, err=960, iostat=ios) npol, polish(:npol)
    end do
endif

if (mdl%fboflg .eq. 1 .and. mdl%fbomem > 0) then
   write(mciunt,err=960,iostat=ios) mdl%fboptr(1 : mdl%nfb + 1), &
&                                   mdl%fbordr(1 : mdl%fbomem)
endif

goto 1000

!     Error handling

950 continue
ier = 5
goto 990

960 continue
close(mciunt, status = 'delete' )
ier = 6
goto 1010

970 continue
ier = 7
goto 990

990 continue
1000 continue

! Cleanup and return

close(mciunt)

1010 continue

return
end subroutine mcwmif
