! Recursive Fortran 95 quicksort routine
! sorts real numbers into ascending numerical order
! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! Based on algorithm from Thomas H. Cormen, Charles E. Leiserson, Ronald L.
! Rivest, and Clifford Stein, Introduction to Algorithms, The MIT Press, 1997 edition.

! Made F conformant by Walt Brainerd

! Modified by Giulio Fella (02/2013) to sort the array index accordingly 

! mod_gf_qsort declares kind of real array and
! contains the component subroutines: Partition, QsortC

MODULE mod_gf_qsort
  IMPLICIT NONE
  PUBLIC :: QsortC
  PRIVATE :: Partition

  ! Set kind of real arrays
  INTEGER, PARAMETER :: ki=SELECTED_REAL_KIND(p = 15, r = 30)

CONTAINS

  RECURSIVE SUBROUTINE QsortC(A, index_A)
    ! Input/output variables
    ! A    	: real(precision) array to sort
    ! index_A   : integer array indexing elements of A
    REAL(ki), INTENT(in out), DIMENSION(:) :: A
    INTEGER, INTENT(in out), DIMENSION(SIZE(A)) :: index_A
    INTEGER :: iq

    IF(SIZE(A) > 1) THEN
      CALL Partition(A,index_A, iq)
      CALL QsortC(A(:iq-1),index_A(:iq-1))
      CALL QsortC(A(iq:),index_A(iq:))
    ENDIF
  END SUBROUTINE QsortC

  SUBROUTINE Partition(A, index_A, marker)
    REAL(ki), INTENT(in out), DIMENSION(:) :: A
    INTEGER, INTENT(in out), DIMENSION(SIZE(A)) :: index_A
    INTEGER, INTENT(out) :: marker
    INTEGER :: i, j
    INTEGER :: index_temp
    REAL(ki) :: temp
    REAL(ki) :: x      ! pivot point
    x = A(1)
    i= 0
    j= SIZE(A) + 1

    DO
      j = j-1
      DO
        IF (A(j) <= x) EXIT
        j = j-1
      END DO
      i = i+1
      DO
        IF (A(i) >= x) EXIT
        i = i+1
      END DO
      IF (i < j) THEN
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp

        index_temp = index_A(i)
        index_A(i) = index_A(j)
        index_A(j) = index_temp

      ELSEIF (i == j) THEN
        marker = i+1
        RETURN
      ELSE
        marker = i
        RETURN
      ENDIF
    END DO

  END SUBROUTINE Partition

END MODULE mod_gf_qsort


