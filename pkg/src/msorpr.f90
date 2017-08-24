module msorpr

    integer, parameter, private :: xrfunt = 4

    contains

    ! print ordering information to a file
    subroutine print_orf(mdl, orfnmlen, orfnm)
        use model_type
        use mcjsbl
        use mcxref
        use mdl_flen
        use mdl_name_utils
        use kinds
        use output_utils
            
        type(model), intent(inout) :: mdl
        integer, intent(in) :: orfnmlen, orfnm(*)
        
        integer, parameter, dimension(2) :: mrfopt = (/128, 1 /)
        integer, parameter, dimension(2) :: fbcopt = (/0, 200/)
        logical ::    prifbi,prisjc,usinac
        integer ::    inactv
        integer ::    i, j, ios, ldum, npro, nepi, nsim
        integer ::    p, leadsp, cnt, tcnt, ier
        
        integer ::    lp, lenxrf
        character(len = 256) :: line
        
        integer ::    mxvlen,mxelen
        integer ::    maxcln
        
        integer ::    minfbo,maxfbo,fbostp
        real(kind = SOLVE_RKIND) :: avgfbo, z
        
        integer ::    bysget
        character(len = 1) :: eqtyp
        
        character(len = 8) :: string
        logical ::   exist
        
        character(len = MCMXNM) name
        character(len = MAXFLEN) :: orfnam
        integer(kind = MC_IKIND), dimension(:,:), allocatable :: sjac
        integer(kind = MC_IKIND), dimension(:), allocatable :: work
        integer ::  nzrcnt
        
        orfnam = ' '
        call byasf7(orfnm, 1, orfnmlen, orfnam)

        if (mdl%nfb > 0) then
            allocate(work(mdl%nrv))
            call mcjzct(mdl, nzrcnt, work)
            deallocate(work)
        endif

        if (prisjc .and. mdl%nfb > 0) then
           ! calculate symbolic rhs Jacobian
           allocate(sjac(mdl%nfb, mdl%nfb))
           call gen_sjac(mdl, sjac, mdl%nfb)
        endif
        
        lenxrf = max(mrfopt(1),50)
        
        ier = 0

        inquire(file=orfnam,exist=exist)
        if(exist) then
           open(xrfunt,file=orfnam)
           close(xrfunt,status='DELETE', iostat=ios)
           if( ios .ne. 0 ) goto 910
        endif

        
        open(xrfunt,file=orfnam,form='formatted', &
        &    status='NEW',iostat=ios, round = 'compatible')
        if(ios.ne.0) goto 910
        
        write(xrfunt,'(6a/)',err=990,iostat=ios) ' ISIS model ordering information: ', &
        & mdl%cname(:mdl%cnamelen)
        
        !     count number of inactive equations
        inactv = 0
        
        if( usinac ) then
            do i = 1, mdl%neq
               eqtyp = char(bysget(mdl%etype, i))
               if( ichar(eqtyp) .gt. 96 ) then
        !               skip inactive equation
                  inactv  = inactv + 1
               endif
            enddo
        endif
        
        npro = mdl%loops - 1
        nsim = mdl%loope - mdl%loops + 1
        nepi = (mdl%neq - inactv) - mdl%loope
        
        write(xrfunt,'(1x,i7,a)', err=990,iostat=ios) mdl%neq,' equations of which'
        write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) npro ,' in prologue'
        write(xrfunt,'(1x,7x,i7,a)', err=990,iostat=ios) nsim ,' in simultaneous block'
        write(xrfunt,'(1x,7x,i7,a/)', err=990,iostat=ios) nepi ,' in epilogue'
        
        if( usinac .and. inactv .gt. 0 ) then
           write(xrfunt,'(1x,7x,i7,a/)', err=990,iostat=ios) inactv,' inactive'
        endif
        
        
        if (mdl%nfb .gt. 0) then
            z = dble(nzrcnt)/(dble(mdl%nfb * mdl%nfb))*100.0
            write(xrfunt,'(1x,i7,a)', err=990,iostat=ios) mdl%nfb,' feedback variables'
            write(xrfunt,'(8x,i10,a,f4.1,a//)', err=990,iostat=ios) &
        &          nzrcnt,' (',z,'%) structural non zero''s in jacobian'
        else
            write(xrfunt,'(1x,i7,a//)', err=990,iostat=ios) mdl%nfb,' feedback variables'
        endif
        
        if (mdl%fboflg > 0 .and. allocated(mdl%fboptr)) then
        
        !        feedback ordering calculated ==> print summary statistics
        
           maxfbo = 0
           minfbo = mdl%loope - mdl%loops + 1
           do i = 1, mdl%nfb
              j      = mdl%fboptr(i + 1) - mdl%fboptr(i)
              maxfbo = max(maxfbo, j)
              minfbo = min(minfbo, j)
           end do
           avgfbo = dble(mdl%fboptr(mdl%nfb + 1) - 1) / dble(mdl%nfb)
        !        effective number of steps for jacobian (rounded up)
           fbostp = (mdl%fboptr(mdl%nfb + 1) - 1 + mdl%nfb * mdl%nfb - 1) / nsim + 1
        
           write(xrfunt,'(1x,a)',err=990,iostat=ios) 'Statistics of feedback ordering'
        
           write(xrfunt,'(1x,5x,i10,1x,a)',err=990,iostat=ios) &
        &          mdl%fboptr(mdl%nfb + 1) - 1, 'words of memory used'
        
           write(xrfunt,'(1x,5x,i8,3x,a)',err=990,iostat=ios) &
        &          minfbo, 'minimum feedback chain (excl. fb equations)'
           write(xrfunt,'(1x,5x,i8,3x,a)',err=990,iostat=ios) &
        &          maxfbo, 'maximum feedback chain (excl. fb equations)'
           write(xrfunt,'(1x,7x,f8.1,1x,a)',err=990,iostat=ios) &
        &          avgfbo, 'average feedback chain (excl. fb equations)'
           write(xrfunt,'(1x,5x,i8,3x,a//)',err=990,iostat=ios) &
        &          fbostp, 'average newton steps (rounded up)'
        
        endif
        
        !     get length of longest variable  name
        !     get length of longest equation  name
        !     set indent for continuation lines
        
        mxvlen = maxnln(mdl%ivnames, 1_MC_IKIND, mdl%nrv)
        mxvlen = ( (mxvlen-1)/4 + 1 ) * 4
        
        mxelen = maxnln(mdl%ienames, 1_MC_IKIND, mdl%neq)
        mxelen = ( (mxelen-1)/4 + 1 ) * 4
        
        lp = 0
        line = ' '
        leadsp = mxvlen + 8 + 1 + 1 + 6 + 3
        
        ! **  Equations section
        
        write(xrfunt,'(a/)',err=990,iostat=ios) ' *** Equations (in solution order) ***'
        
        write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) npro,' Prologue equations'
        
        do j = 1, mdl%loops - 1
           name = gtnmex(mdl%ienames(mdl%order(j)), mdl%enames, ldum)
           call namxrf(name,1,0,mxelen,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
        end do
        
        call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
        if( ios .gt. 0 ) goto 990
        
        write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) nsim,' Simultaneous equations'
        
        do  j=mdl%loops,mdl%loope
           name = gtnmex(mdl%ienames(mdl%order(j)),mdl%enames,ldum)
           call namxrf(name,1,0,mxelen,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
        enddo
        
        call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
        if( ios .gt. 0 ) goto 990
        
        write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) nepi,' Epilogue equations'
        
        do  j=mdl%loope+1,mdl%neq-inactv
           name = gtnmex(mdl%ienames(mdl%order(j)),mdl%enames,ldum)
           call namxrf(name,1,0,mxelen,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
        enddo
        
        call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
        if( ios .gt. 0 ) goto 990
        write(xrfunt,'(a/)',err=990,iostat=ios) ' *** Feedback variables ***'
        write(xrfunt,'(1x,i5,a/)',err=990,iostat=ios) mdl%nfb,' Feedback variables'
        
        do  j=1,mdl%nfb
           name = gtnmex(mdl%ivnames(mdl%numfb(j)),mdl%vnames,ldum)
           call namxrf(name,1,0,mxvlen,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
        enddo
        
        call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
        if( ios .gt. 0 ) goto 990
        
        !     show length of feedback chain
        if (mdl%fboflg .gt. 0 .and. allocated(mdl%fboptr)) then
        
           call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
           write(xrfunt,'(a/a/a)',err=990,iostat=ios) &
        &  ' *** Additional information for feedback variables', &
        &  '     Name of feedback variable' // ' followed by how it was chosen:' // &
        &  ' fixed, diagonal, heuristic,', '     and the length of the feedback cycle' // &
        &  ' (excluding feedback variables)'
        
           if( fbcopt(1) .eq. 1 ) then
              write(xrfunt,'(a/)',err=990,iostat=ios) &
        &     '     followed by the names of equations ' // 'in the feedback cycle'
           else
              write(xrfunt,'(/)',err=990,iostat=ios)
           endif
        
           do  j=1, mdl%nfb
              name = gtnmex(mdl%ivnames(mdl%numfb(j)), mdl%vnames, ldum)
              call namxrf(name,0,0,mxvlen,ios,lp,line,lenxrf,xrfunt)
              if( ios .gt. 0 ) goto 990
        
              if (mdl%fbtype(j) .eq. 1 ) then
                call namxrf('fixed',1,12,12,ios,lp,line,lenxrf,xrfunt)
              elseif (mdl%fbtype(j) .eq. 2 ) then
                call namxrf('diagonal',1,12,12,ios,lp,line,lenxrf,xrfunt)
              elseif (mdl%fbtype(j) .eq. 3 ) then
                call namxrf('heuristic',1,12,12,ios,lp,line,lenxrf,xrfunt)
              else
                call namxrf('?',1,4,4,ios,lp,line,lenxrf,xrfunt)
              endif
              if( ios .gt. 0 ) goto 990
        
              write(string,'(i6)') mdl%fboptr(j + 1) - mdl%fboptr(j)
              call namxrf(string,1,8,8,ios,lp,line,lenxrf,xrfunt)
              if( ios .gt. 0 ) goto 990
        
              if( fbcopt(1) .eq. 1 ) then
        !             !! print names of equations in feedback chain
        
                  maxcln = fbcopt(2)
                  p = mdl%fboptr(j)
                  cnt = mdl%fboptr(j + 1) - mdl%fboptr(j)
                  leadsp = ((mxvlen-1)/4 + 1)*4 + 12 + 8 + 2 + 1
        
                  if( cnt .le. maxcln) then
                      do  i=p,p+cnt-1
                          name = gtnmex(mdl%ienames(mdl%fbordr(i)), mdl%enames,ldum)
                          call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
                          if( ios .gt. 0 ) goto 990
                      enddo
                  else
                      tcnt = maxcln/2
                      do  i=p,p+tcnt-1
                          name = gtnmex(mdl%ienames(mdl%fbordr(i)), mdl%enames,ldum)
                          call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
                          if( ios .gt. 0 ) goto 990
                      enddo
        
                      if(tcnt .gt. 1 ) then
                          call namxrf('....',1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
                      else
                          call namxrf('....',1,leadsp,4,ios, lp,line,lenxrf,xrfunt)
                      endif
                      if( ios .gt. 0 ) goto 990
        
                      p = mdl%fboptr(j + 1) - tcnt
                      do  i = p, mdl%fboptr(j + 1) - 1
                          name = gtnmex(mdl%ienames(mdl%fbordr(i)), mdl%enames,ldum)
                          call namxrf(name,1,leadsp,mxelen,ios, lp,line,lenxrf,xrfunt)
                          if( ios .gt. 0 ) goto 990
                      enddo
        
                  endif
        
              endif
        
           enddo
        
           call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
        
        elseif( prifbi ) then
        
           call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
           write(xrfunt,'(a/a/a/)',err=990,iostat=ios) &
        &  ' *** List of feedback variables ***', &
        &  '     each name followed by how it was chosen: ' // &
        &  'fixed, diagonal, heuristic'
        
           do  j=1,mdl%nfb
              name = gtnmex(mdl%ivnames(mdl%numfb(j)), mdl%vnames,ldum)
              call namxrf(name,0,0,mxvlen,ios,lp,line,lenxrf,xrfunt)
              if( ios .gt. 0 ) goto 990
        
              if (mdl%fbtype(j) .eq. 1 ) then
                call namxrf('fixed',1,10,10,ios,lp,line,lenxrf,xrfunt)
              elseif (mdl%fbtype(j) .eq. 2 ) then
                call namxrf('diagonal',1,10,10,ios,lp,line,lenxrf,xrfunt)
              elseif (mdl%fbtype(j) .eq. 3 ) then
                call namxrf('heuristic',1,10,10,ios,lp,line,lenxrf,xrfunt)
              else
                call namxrf('?',1,4,4,ios,lp,line,lenxrf,xrfunt)
              endif
              if( ios .gt. 0 ) goto 990
           enddo
        
           call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
        
        endif
        
        if( prisjc .and. mdl%nfb > 0) then
           call namxrf(' ',0,0,0,ios,lp,line,lenxrf,xrfunt)
           if( ios .gt. 0 ) goto 990
           write(xrfunt,'(a/)',err=990,iostat=ios) &
        &  ' *** Symbolic rhs jacobian of feedback variables ***'
            call mcosjs(xrfunt,sjac, mdl%nfb, mdl%nfb)
            call mcosjc(xrfunt,lenxrf,sjac, mdl%nfb, mdl%nfb, &
        &               mdl%ivnames,mdl%vnames,mdl%numfb)
        endif
        
        goto 1000
        
  910 continue
        ier = 1
        goto 1000
        
  990 continue
        ier = 2
        
 1000 continue
        close(xrfunt)
       
        if (allocated(sjac)) deallocate(sjac)
    
        if (ier /= 0) then
            if (ier == 1) then
                write(line, "(a, a, a)") "Cannot open file ", trim(orfnam), &
                          " for writing ordering information"
            else 
                write(line, "(a, a)") &
                    "Error writing ordering information to file ", orfnam
            endif
            call isismdl_error(line)
        endif

        return
    end subroutine print_orf
    
end module msorpr
