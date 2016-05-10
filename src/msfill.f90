module msfill

contains
subroutine fill_mdl_data(jt1, jt2, repopt)
    use msvars
    use msimot
    integer, intent(in) :: jt1, jt2, repopt

    ! fills missing values by running identities for period jt1 .. jt2
    ! skips inactive and behavioural equations

    ! walks through model in solution order
    ! and runs identities if the left hand side variable is missing

    integer ::  jtime, ngain, tgain, itnum
    logical ::  didfb

    ! initialize simulation output system
    call simot1

    tgain = 0

    do jtime = jt1, jt2
       itnum = 0
       do
          itnum = itnum + 1
          ngain = 0
          call msfilp(jtime, ngain, didfb)
          if (repopt == 2) call filot1(ngain, jtime, itnum)
          tgain = tgain + ngain
          if (ngain == 0 .or. mdl%nfb == 0 .or. .not. didfb) exit
        end do

     end do
     call filot2(tgain)

     return
     end subroutine fill_mdl_data

     subroutine msfilp(jtime, ngain, didfb)
        use mssneq
        integer, intent(in) :: jtime
        integer, intent(inout) :: ngain
        logical, intent(out) :: didfb

        ! fills missing values by running identities for period jtime
        ! skips inactive and behavioural equations

        ! walks through model in solution order
        ! and runs identities if the left hand side variable is missing

        integer, external ::  bysget
        character(len = 1) :: eqtype
        integer ::      tgain
        logical ::     error

        real*8 x, xresult
        integer ::  i, iequ, lhsvar, ier

        didfb = .false.
        
        do i = 1, mdl%neq
           iequ   = mdl%order(i)
           if (iequ == 0) cycle
           lhsvar = mdl%lhsnum(iequ)
           eqtype = char(bysget(mdl%etype, iequ))

           ! skip all equation types except (implicit) identities
           if (eqtype /= 'I' .and. eqtype /= 'N') cycle

           ! possibly something to do
           ! calculate index in mws%mdl_data array
           ! if available entry is not missing do nothing
           call get_var_value(mws, lhsvar, jtime, x, error)

           if (.not. nuifna(x) .or. error) cycle

           if (eqtype == 'I') then
               call msisng(xresult, iequ, jtime, ier)
           else
               ! implicit identity (solve with newton)
               call msinwt(xresult, lhsvar, 0, iequ, jtime, ier)
           endif

           if (ier /= 0 .and. ier /= 3 .and. ier /= 7) then
              ! no message for any kind of missing values
              call msverr(ier, jtime, iequ)
           endif

           ! store result; if not missing then
           ! a missing value has been replaced and therefore
           ! progress has been made

           if (.not. nuifna(xresult) ) then
               ngain = ngain + 1
               call set_var_value(mws, lhsvar, jtime, xresult, error)
               ! check for fill of feedback variable
               if ((i .ge. mdl%loope - mdl%nfb + 1) .and. &
                   (i .le. mdl%loope) ) then
                   didfb = .true.
               endif
           endif
        end do

        return
    end subroutine msfilp

    subroutine filot1(ngain, jtime, itnum)
        use msimot
        integer, intent(in) :: ngain, jtime, itnum

        call sjcstr(jtime)

        write(str,'(I10,2A,A,I4,A)') &
              ngain , ' Missing/invalid values replaced in period ', &
              perstr, '(iteration',itnum,')'

        call strout(O_OUTB)

        return
    end subroutine filot1

    subroutine filot2(tgain)
        use msimot
        integer, intent(in) :: tgain

        integer, parameter :: MAX_INT_STR_LEN = 10
        character(len = MAX_INT_STR_LEN) :: istr

        if (tgain /= 0) then
            write(istr, '(I10)') tgain
            istr = adjustl(istr)
            write(str, '(3A)') 'Replaced a total of ', trim(istr), &
                               ' missing/invalid values in identities'
        else
            str = 'No missing/invalid values replaced in identities'
        endif

        call strout(O_OUTB)

        return
    end subroutine filot2

end module msfill
