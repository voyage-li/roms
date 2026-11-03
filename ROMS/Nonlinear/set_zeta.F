#include "cppdefs.h"
      MODULE set_zeta_mod

#ifdef SOLVE3D
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine sets free-surface to its fast-time averaged value.     !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: set_zeta
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE set_zeta (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_coupling
      USE mod_ocean
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 12, __LINE__, MyFile)
# endif
      CALL set_zeta_tile (ng, tile,                                     &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    COUPLING(ng) % Zt_avg1,                       &
     &                    OCEAN(ng) % zeta)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 12, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE set_zeta
!
!***********************************************************************
      SUBROUTINE set_zeta_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          Zt_avg1, zeta)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
#if defined ROMS_KOKKOS && defined KK_STAGE_SETZETA
      USE, INTRINSIC :: iso_c_binding, ONLY : C_LOC
      USE kokkos_rhs3d_mod, ONLY : kokkos_tile_bounds, k_set_zeta,      &
     &                             kk_fill_bounds
#endif
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
# ifdef ASSUMED_SHAPE
      real(r8), TARGET, intent(in) :: Zt_avg1(LBi:,LBj:)

      real(r8), TARGET, intent(out) :: zeta(LBi:,LBj:,:)
# else
      real(r8), TARGET, intent(in) :: Zt_avg1(LBi:UBi,LBj:UBj)

      real(r8), TARGET, intent(out) :: zeta(LBi:UBi,LBj:UBj,3)
# endif
!
!  Local variable declarations.
!
      integer :: i, j
#if defined ROMS_KOKKOS && defined KK_STAGE_SETZETA
      TYPE(kokkos_tile_bounds) :: KK_TILE
#endif

# include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Prepare to time-step 2D equations:  set initial free-surface
!  to its fast-time averaged values (which corresponds to the time
!  step "n").
!-----------------------------------------------------------------------
!
#if defined ROMS_KOKKOS && defined KK_STAGE_SETZETA
!
!  Kokkos path: the two copies live in ROMS/Cpp/kokkos_rhs3d.cpp
!  (k_set_zeta).  The halo exchange below stays here.
!
      CALL kk_fill_bounds (KK_TILE, ng, tile)
      CALL k_set_zeta (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 C_LOC(Zt_avg1), C_LOC(zeta), KK_TILE)
#else
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          zeta(i,j,1)=Zt_avg1(i,j)
          zeta(i,j,2)=Zt_avg1(i,j)
        END DO
      END DO
#endif /* ROMS_KOKKOS && KK_STAGE_SETZETA */

      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          zeta(:,:,1))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          zeta(:,:,2))
      END IF

# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    zeta(:,:,1),                                  &
     &                    zeta(:,:,2))
# endif
!
      RETURN
      END SUBROUTINE set_zeta_tile
#endif
      END MODULE set_zeta_mod
