      MODULE kokkos_loops_mod
!  ABI for the generated, bounds-aware Kokkos row kernels.
      USE, INTRINSIC :: iso_c_binding
      USE kokkos_step3d_t_mod, ONLY :                                  &
     &    kk_row_contiguous => kk_is_contiguous
      implicit none
      INTERFACE kk_row_contiguous
        MODULE PROCEDURE kk_contiguous1
      END INTERFACE
      TYPE, BIND(C) :: kk_loop_args
        TYPE(C_PTR) :: p(128)
        integer(C_INT) :: b(1280)
        real(C_DOUBLE) :: s(256)
      END TYPE
      CONTAINS
!  SW GCC 7 has no IS_CONTIGUOUS. Reuse the existing rank 2--5 stride
!  checks without changing that module; extend the renamed generic for
!  rank-one coefficient tables and advection flags.
      logical FUNCTION kk_contiguous1(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:)
      kk_contiguous1=.TRUE.
      IF (SIZE(a).gt.1) THEN
        kk_contiguous1=                                                &
     &    TRANSFER(C_LOC(a(2)),0_C_INTPTR_T)-                            &
     &    TRANSFER(C_LOC(a(1)),0_C_INTPTR_T).eq.8_C_INTPTR_T
      END IF
      END FUNCTION kk_contiguous1
!
      SUBROUTINE kk_loop_warn(name)
      character(len=*), intent(in) :: name
      character(len=64), save :: names(32)=''
      integer, save :: n=0
      integer :: i
      logical :: found
!$OMP CRITICAL (KK_ROW_FALLBACK)
      found=.FALSE.
      DO i=1,n
        IF (names(i).eq.name) found=.TRUE.
      END DO
      IF (.not.found) THEN
        WRITE (*,'(a)') 'Kokkos row fallback: '//TRIM(name)
        IF (n.lt.SIZE(names)) THEN
          n=n+1
          names(n)=name
        END IF
      END IF
!$OMP END CRITICAL (KK_ROW_FALLBACK)
      END SUBROUTINE kk_loop_warn
      END MODULE kokkos_loops_mod
