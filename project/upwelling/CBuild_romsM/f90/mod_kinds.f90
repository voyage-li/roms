      MODULE mod_kinds
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!
        implicit none
!
        integer, parameter :: i1b= SELECTED_INT_KIND(1)        !  8-bit
        integer, parameter :: i2b= SELECTED_INT_KIND(2)        !  8-bit
        integer, parameter :: i4b= SELECTED_INT_KIND(4)        ! 16-bit
        integer, parameter :: i8b= SELECTED_INT_KIND(8)        ! 32-bit
        integer, parameter :: c8 = SELECTED_REAL_KIND(6,30)    ! 32-bit
        integer, parameter :: dp = SELECTED_REAL_KIND(12,300)  ! 64-bit
        integer, parameter :: r4 = SELECTED_REAL_KIND(6,30)    ! 32-bit
        integer, parameter :: r8 = SELECTED_REAL_KIND(12,300)  ! 64-bit
        integer, parameter :: r16 = SELECTED_REAL_KIND(15,300) !128-bit
      END MODULE mod_kinds
