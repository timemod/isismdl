 
!     Get shift implied by load type ldtype.
!     This piece of code in used in subroutine evaleq.
!     NOTE: this code could also be implemented as an internal
!     subprogram of subroutine evaleq. However, it turns out
!     that that implementation runs much slower on Linux.
 
select case (ldtype)
case (1:2)
    jshift = 0
case (3:4)
    jshift = - mdl%equat(ip+1)
case (5:6)
    jshift =   mdl%equat(ip+1)
end select

if (ldtype == 2 .or. ldtype == 4 .or. ldtype == 6) jshift = jshift + jsum

