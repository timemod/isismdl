! fortran routine for X86,X86_64 version.
      SUBROUTINE BYSSET(STR,FB,BV)
!     BYteS SET set byte FB in STR to byte value BV.
!     byte value 0.
      INTEGER FB,IBV,BV
      LOGICAL*1 LBV,STR(*)
      EQUIVALENCE(LBV,IBV)
      IBV=BV
      STR(FB)=LBV
      RETURN
      END
