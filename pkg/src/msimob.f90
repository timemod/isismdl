module msimob
use msimot

!
! print messages about invalid lags and leads
!

contains

    subroutine simob1(iv, jlag)
        use msvars
        use mdl_name_utils
        integer, intent(in) :: iv, jlag
        
        ! print missing or invalid lag message for variable with lag jlag
        ! (jlag > 0). iv is variable index
        
        character(len = 80) :: dst
        integer :: dlen
        
        call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
        call nshfmt(name, nlen, -jlag, dst, dlen)
        
        write(str,'(2a)') 'Missing or invalid value for ', dst(:dlen)
        call strout(O_ERRM)
        
        return
    end subroutine simob1
    
    subroutine simob2(nonval)
        integer, intent(in) ::  nonval
        
        ! print total of missing or invalid lags
        
        write(str,'(i5,2a)') nonval,' missing or invalid lags in period ',perstr
        call strout(O_ERRF)
        return
    end subroutine simob2
    
    subroutine simob3(iv, jlead)
        use msvars
        use mdl_name_utils
        integer, intent(in) :: iv, jlead
        
        ! print missing or invalid lead message for variable with lead jlead
        ! iv is variable index
        
        character(len = 80) :: dst
        integer ::        dlen
        
        call mcf7ex(name, nlen, mdl%ivnames(iv), mdl%vnames)
        call nshfmt(name, nlen, jlead, dst, dlen)
        
        write(str,'(2a)') 'Missing or invalid value for ',dst(:dlen)
        call strout(O_ERRM)
        return
    end subroutine simob3
    
    subroutine simob4(nonval)
        integer, intent(in) ::  nonval

        ! print total of missing or invalid leads
        
        write(str,'(i5,2a)') nonval,' missing or invalid leads in period ',perstr
        call strout(O_ERRF)
        return
        end subroutine simob4
    
end module msimob
