      SUBROUTINE BYSZER(STR,FB,NB)
!     BYteS ZERo initialize NB bytes in STR starting in byte FB to
!     byte value 0.
      INTEGER FB,NB,I,IBV
      LOGICAL*1 LBV,STR(*)
      EQUIVALENCE(LBV,IBV)
      IBV=0
      IF (FB.GT.0) THEN
        DO 10 I=1,NB
          STR(FB+I-1)=LBV
10      CONTINUE
      ENDIF
      RETURN
      END
