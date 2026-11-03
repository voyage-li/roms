      MODULE kokkos_step3d_t_mod
!
!  Zero-copy interface for the diagnostic U3/C4 and HSIMT tracer path.
!
      USE, INTRINSIC :: iso_c_binding, ONLY : C_INT, C_DOUBLE, C_PTR, &
     &                                      C_INTPTR_T, C_LOC
      implicit none
      PRIVATE
      PUBLIC :: kokkos_tracer_config, k_step3d_t, k_step3d_t_rows, &
     &          k_step3d_t_chunks, &
     &          kk_is_contiguous
!
      TYPE, BIND(C) :: kokkos_tracer_config
        integer(C_INT) :: LBi, UBi, LBj, UBj
        integer(C_INT) :: Istr, Iend, Jstr, Jend
        integer(C_INT) :: N, NT, NAT, NDT, nnew
        integer(C_INT) :: xadv, yadv, hadv, vadv, vdif
        integer(C_INT) :: west, east, south, north
        integer(C_INT) :: periodic_ew, periodic_ns
        integer(C_INT) :: composite_w, composite_e
        integer(C_INT) :: composite_s, composite_n
        real(C_DOUBLE) :: dt, cc1, cc2, cc3
      END TYPE kokkos_tracer_config
!
!  The SW GCC 7 frontend lacks the IS_CONTIGUOUS intrinsic. Verify
!  every nontrivial array stride instead, using the actual descriptor.
      INTERFACE kk_is_contiguous
        MODULE PROCEDURE kk_contiguous2, kk_contiguous3
        MODULE PROCEDURE kk_contiguous4, kk_contiguous5
      END INTERFACE
!
      INTERFACE
        SUBROUTINE k_step3d_t (config, schemes, pm, pn, Hz, Huon, &
     &                        Hvom, W, zr, Akt, tracer, diagnostics) &
     &                        BIND(C, NAME='k_step3d_t')
          IMPORT :: kokkos_tracer_config, C_INT, C_PTR
          TYPE(kokkos_tracer_config), INTENT(IN) :: config
          integer(C_INT), INTENT(IN) :: schemes(*)
          TYPE(C_PTR), VALUE :: pm, pn, Hz, Huon, Hvom, W, zr, Akt
          TYPE(C_PTR), VALUE :: tracer, diagnostics
        END SUBROUTINE k_step3d_t
!
        SUBROUTINE k_step3d_t_rows (config, schemes, pm, pn, Hz, Huon, &
     &                        Hvom, W, zr, Akt, tracer, diagnostics) &
     &                        BIND(C, NAME='k_step3d_t_rows')
          IMPORT :: kokkos_tracer_config, C_INT, C_PTR
          TYPE(kokkos_tracer_config), INTENT(IN) :: config
          integer(C_INT), INTENT(IN) :: schemes(*)
          TYPE(C_PTR), VALUE :: pm, pn, Hz, Huon, Hvom, W, zr, Akt
          TYPE(C_PTR), VALUE :: tracer, diagnostics
        END SUBROUTINE k_step3d_t_rows
!
        SUBROUTINE k_step3d_t_chunks (config, schemes, pm, pn, Hz,      &
     &                        Huon, Hvom, W, zr, Akt, tracer,           &
     &                        diagnostics)                              &
     &                        BIND(C, NAME='k_step3d_t_chunks')
          IMPORT :: kokkos_tracer_config, C_INT, C_PTR
          TYPE(kokkos_tracer_config), INTENT(IN) :: config
          integer(C_INT), INTENT(IN) :: schemes(*)
          TYPE(C_PTR), VALUE :: pm, pn, Hz, Huon, Hvom, W, zr, Akt
          TYPE(C_PTR), VALUE :: tracer, diagnostics
        END SUBROUTINE k_step3d_t_chunks
      END INTERFACE
      CONTAINS
      logical FUNCTION kk_contiguous2(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous2=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous2=kk_contiguous2.and. &
     &    (TRANSFER(C_LOC(a(2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous2=kk_contiguous2.and. &
     &    (TRANSFER(C_LOC(a(1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,2),C_INTPTR_T)
      END FUNCTION kk_contiguous2
!
      logical FUNCTION kk_contiguous3(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous3=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous3=kk_contiguous3.and. &
     &    (TRANSFER(C_LOC(a(2,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous3=kk_contiguous3.and. &
     &    (TRANSFER(C_LOC(a(1,2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,2),C_INTPTR_T)
      IF (SIZE(a,3).gt.1) THEN
        kk_contiguous3=kk_contiguous3.and. &
     &    (TRANSFER(C_LOC(a(1,1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,3),C_INTPTR_T)
      END FUNCTION kk_contiguous3
!
      logical FUNCTION kk_contiguous4(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:,:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1,1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous4=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(2,1,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(1,2,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,2),C_INTPTR_T)
      IF (SIZE(a,3).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(1,1,2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,3),C_INTPTR_T)
      IF (SIZE(a,4).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(1,1,1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,4),C_INTPTR_T)
      END FUNCTION kk_contiguous4
!
      logical FUNCTION kk_contiguous5(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:,:,:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1,1,1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous5=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(2,1,1,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,2,1,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,2),C_INTPTR_T)
      IF (SIZE(a,3).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,1,2,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,3),C_INTPTR_T)
      IF (SIZE(a,4).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,1,1,2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,4),C_INTPTR_T)
      IF (SIZE(a,5).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,1,1,1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,5),C_INTPTR_T)
      END FUNCTION kk_contiguous5
!
      END MODULE kokkos_step3d_t_mod
