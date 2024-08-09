      MODULE def_dim_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module defines the requested NetCDF dimension.                 !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_scalars
!
      USE strings_mod, ONLY : FoundError
!
      implicit none
!
      INTERFACE def_dim
        MODULE PROCEDURE def_dim_nf90
      END INTERFACE def_dim
!
      CONTAINS
!
!***********************************************************************
      FUNCTION def_dim_nf90 (ng, model, ncid, ncname,                   &
     &                       DimName, DimSize, DimID) RESULT (status)
!***********************************************************************
!                                                                      !
!  This function defines the requested NetCDF dimension when using the !
!  standard NetCDF-3 or NetCDF-4 library.                              !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng       Nested grid number (integer)                            !
!     model    Calling model identifier (integer)                      !
!     ncid     NetCDF file ID (integer)                                !
!     ncname   NetCDF filename (string)                                !
!     DimName  Dimension name (string)                                 !
!     DimSize  Dimension size (integer)                                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     status   NetCDF error flag (integer)                             !
!     DimId    NetCDF dimension ID (integer)                           !
!                                                                      !
!***********************************************************************
!
      USE mod_netcdf
!
      USE distribute_mod, ONLY : mp_bcasti
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent (in)  :: ng, model, ncid
      integer, intent (in)  :: DimSize
      integer, intent (out) :: DimId
!
      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: DimName
!
!  Local variable declarations.
!
      integer :: status
      integer, dimension(3) :: ibuffer
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/def_dim.F"//", def_dim_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Define requested NetCDF dimension.
!-----------------------------------------------------------------------
!
      status=nf90_noerr
!
      IF (OutThread) THEN
        status=nf90_def_dim(ncid, TRIM(DimName), DimSize, DimId)
        IF (FoundError(status, nf90_noerr, 96, MyFile)) THEN
          IF (Master) WRITE (stdout,10) TRIM(DimName), TRIM(ncname)
          exit_flag=3
          ioerror=status
        END IF
      END IF
!
!  Broadcast information to all threads in the group.
!
      ibuffer(1)=DimID
      ibuffer(2)=status
      ibuffer(3)=exit_flag
      CALL mp_bcasti (ng, model, ibuffer)
      DimID=ibuffer(1)
      status=ibuffer(2)
      exit_flag=ibuffer(3)
!
 10   FORMAT (/,' DEF_DIM_NF90 - error while defining dimension: ',a,   &
     &        /,16x,'in file: ',a)
!
      RETURN
      END FUNCTION def_dim_nf90
      END MODULE def_dim_mod
