PROGRAM sortdriver
  ! Test program for gf_qsort.f90
  USE mod_gf_qsort
  IMPLICIT NONE
  INTEGER, PARAMETER :: ki=SELECTED_REAL_KIND(p = 15, r = 30)
  INTEGER, PARAMETER :: r = 10
  INTEGER :: i
  REAL(ki), DIMENSION(1:r) :: myarray = &        
      (/0, 50, 20, 25, 90, 10, 5, 99, 99, 99/)
  INTEGER, DIMENSION(1:r) :: myindex = (/ (i,i=1,r) /)
  PRINT *, "Original array is ", myarray
  CALL QsortC(myarray,myindex)
  PRINT *, "Sorted array is ", myarray
  PRINT *, "Sorted index is ", myindex
END PROGRAM sortdriver
