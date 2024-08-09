      MODULE nf_fwrite4d_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This function writes out a generic floating point 4D array into an  !
!  output NetCDF file.                                                 !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number                                  !
!     model        Calling model identifier                            !
!     ncid         NetCDF file ID                                      !
!     ifield       Field metadata index (integer)                      !
!     ncvarid      NetCDF variable ID                                  !
!     tindex       NetCDF time record index to write                   !
!     gtype        Grid type. If negative, only write water points     !
!     LBi          I-dimension Lower bound                             !
!     UBi          I-dimension Upper bound                             !
!     LBj          J-dimension Lower bound                             !
!     UBj          J-dimension Upper bound                             !
!     LBk          K-dimension Lower bound                             !
!     UBk          K-dimension Upper bound                             !
!     LBt          Time-dimension Lower bound                          !
!     UBt          Time-dimension Upper bound                          !
!     Amask        land/Sea mask, if any (real)                        !
!     Ascl         Factor to scale field before writing (real)         !
!     Adat         Field to write out (real)                           !
!     SetFillVal   Logical switch to set fill value in land areas      !
!                    (OPTIONAL)                                        !
!     ExtractField Field extraction flag (integer, OPTIONAL)           !
!                    ExtractField = 0   no extraction                  !
!                    ExtractField = 1   extraction by intrpolation     !
!                    ExtractField > 1   extraction by decimation       !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     status       Error flag (integer)                                !
!     MinValue     Minimum value (real, OPTIONAL)                      !
!     MaxValue     Maximum value (real, OPTIONAL)                      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_ncparam
      USE mod_scalars
!
      USE pack_field_mod,  ONLY : pack_field4d
!
      implicit none
!
      INTERFACE nf_fwrite4d
        MODULE PROCEDURE nf90_fwrite4d
      END INTERFACE nf_fwrite4d
!
      CONTAINS
!
!***********************************************************************
      FUNCTION nf90_fwrite4d (ng, model, ncid, ifield,                  &
     &                        ncvarid, tindex, gtype,                   &
     &                        LBi, UBi, LBj, UBj, LBk, UBk, LBt, UBt,   &
     &                        Ascl,                                     &
     &                        Adat,                                     &
     &                        SetFillVal,                               &
     &                        ExtractField,                             &
     &                        MinValue, MaxValue) RESULT (status)
!***********************************************************************
!
      USE mod_netcdf
!
      USE distribute_mod, ONLY : mp_bcasti
      USE distribute_mod, ONLY : mp_gather3d
!
!  Imported variable declarations.
!
      logical, intent(in), optional :: SetFillVal
!
      integer, intent(in) :: ng, model, ncid, ncvarid, tindex, gtype
      integer, intent(in) :: ifield
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk, LBt, UBt
!
      integer, intent(in), optional :: ExtractField
!
      real(dp), intent(in) :: Ascl
!
      real(r8), intent(in) :: Adat(LBi:,LBj:,LBk:,LBt:)
      real(r8), intent(out), optional :: MinValue
      real(r8), intent(out), optional :: MaxValue
!
!  Local variable declarations.
!
      logical :: LandFill
!
      integer :: Extract_Flag
      integer :: i, fourth, Npts, tile
      integer :: status
      integer, dimension(5) :: start, total
!
      real(r8), dimension((Lm(ng)+2)*(Mm(ng)+2)*(UBk-LBk+1)) :: Awrk
!
!-----------------------------------------------------------------------
!  Set starting and ending indices to process.
!-----------------------------------------------------------------------
!
      status=nf90_noerr
!
!  Set parallel tile.
!
      tile=MyRank
!
!  Set switch to replace land areas with fill value, spval.
!
      LandFill=.FALSE.
!
!  If appropriate, set the field extraction flag to the provided grid
!  geometry through interpolation or decimation.
!
      IF (PRESENT(ExtractField)) THEN
        Extract_Flag=ExtractField
      ELSE
        Extract_Flag=0
      END IF
!
!  If appropriate, initialize minimum and maximum processed values.
!
      IF (PRESENT(MinValue)) THEN
        MinValue=spval
        MaxValue=-spval
      END IF
!
!  Initialize local array to avoid denormalized numbers. This
!  facilitates processing and debugging.
!
      Awrk=0.0_r8
!
!-----------------------------------------------------------------------
!  Pack 4D field data into 1D array. Proccess fourth dimension record
!  by record.
!-----------------------------------------------------------------------
!
      DIM4_LOOP : DO fourth=LBt,UBt
        CALL pack_field4d (ng, model, tile,                             &
     &                     gtype, ifield, tindex,                       &
     &                     LandFill, Extract_Flag,                      &
     &                     LBi, UBi, LBj, UBj, LBk, UBk, LBt, UBt,      &
     &                     fourth,                                      &
     &                     Ascl, Adat,                                  &
     &                     start, total, Npts, Awrk)
!
!-----------------------------------------------------------------------
!  If applicable, compute output field minimum and maximum values.
!-----------------------------------------------------------------------
!
        IF (PRESENT(MinValue)) THEN
          IF (OutThread) THEN
            DO i=1,Npts
              IF (ABS(Awrk(i)).lt.spval) THEN
                MinValue=MIN(MinValue,Awrk(i))
                MaxValue=MAX(MaxValue,Awrk(i))
              END IF
            END DO
          END IF
        END IF
!
!-----------------------------------------------------------------------
!  Write output buffer into NetCDF file.
!-----------------------------------------------------------------------
!
        IF (OutThread) THEN
          status=nf90_put_var(ncid, ncvarid, Awrk, start, total)
        END IF
      END DO DIM4_LOOP
!
!-----------------------------------------------------------------------
!  Broadcast IO error flag to all nodes.
!-----------------------------------------------------------------------
!
      CALL mp_bcasti (ng, model, status)
!
      RETURN
      END FUNCTION nf90_fwrite4d
!
      END MODULE nf_fwrite4d_mod
