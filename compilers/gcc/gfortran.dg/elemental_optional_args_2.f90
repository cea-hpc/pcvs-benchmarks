! { dg-do run }
!
! PR fortran/50981
! The program used to dereference a NULL pointer when trying to access
! an optional dummy argument to be passed to an elemental subprocedure.
!
! Original testcase from Andriy Kostyuk <kostyuk@fias.uni-frankfurt.de>

PROGRAM test
  IMPLICIT NONE
  REAL(KIND=8), DIMENSION(2) :: aa, rr

  aa(1)=10.
  aa(2)=11.


  ! WRITE(*,*) 'Both f1 and ff work if the optional parameter is present:'

  rr=f1(aa,1)
  ! WRITE(*,*) ' rr(1)=', rr(1), '  rr(2)=', rr(2)
  IF (ANY(rr /= (/ 110, 132 /))) CALL ABORT

  rr=0
  rr=ff(aa,1)
  ! WRITE(*,*) ' rr(1)=', rr(1), '  rr(2)=', rr(2)
  IF (ANY(rr /= (/ 110, 132 /))) CALL ABORT


  ! WRITE(*,*) 'But only f1 works if the optional parameter is absent:'

  rr=0
  rr=f1(aa)
  ! WRITE(*,*) ' rr(1)=', rr(1), '  rr(2)=', rr(2)
  IF (ANY(rr /= (/ 110, 132 /))) CALL ABORT

  rr = 0
  rr=ff(aa)
  ! WRITE(*,*) ' rr(1)=', rr(1), '  rr(2)=', rr(2)
  IF (ANY(rr /= (/ 110, 132 /))) CALL ABORT


CONTAINS 

    ELEMENTAL REAL(KIND=8) FUNCTION ff(a,b)
      IMPLICIT NONE
      REAL(KIND=8), INTENT(IN) :: a
      INTEGER, INTENT(IN), OPTIONAL :: b
      REAL(KIND=8), DIMENSION(2) :: ac
      ac(1)=a
      ac(2)=a**2
      ff=SUM(gg(ac,b))
    END FUNCTION ff

    ELEMENTAL REAL(KIND=8) FUNCTION f1(a,b)
      IMPLICIT NONE
      REAL(KIND=8), INTENT(IN) :: a
      INTEGER, INTENT(IN), OPTIONAL :: b
      REAL(KIND=8), DIMENSION(2) :: ac
      ac(1)=a
      ac(2)=a**2
      f1=gg(ac(1),b)+gg(ac(2),b) ! This is the same as in ff, but without using the elemental feature of gg
    END FUNCTION f1

    ELEMENTAL REAL(KIND=8) FUNCTION gg(a,b)
      IMPLICIT NONE
      REAL(KIND=8), INTENT(IN) :: a
      INTEGER, INTENT(IN), OPTIONAL :: b
      INTEGER ::b1
      IF(PRESENT(b)) THEN
        b1=b
      ELSE
        b1=1
      ENDIF
      gg=a**b1
    END FUNCTION gg


END PROGRAM test

