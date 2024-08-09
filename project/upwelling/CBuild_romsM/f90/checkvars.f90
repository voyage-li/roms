      MODULE checkvars_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine checks if needed state variables are available in      !
!  requested file using either the standard NetCDF library or the      !
!  Parallel-IO (PIO) library.                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number                                    !
!     model      Calling model identifier                              !
!     ncname     NetCDF file name                                      !
!     ncid       NetCDF file ID                                        !
!     string     Identification string                                 !
!     Nvar       Size of logical switches arrays                       !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Nrec       Number of time records available                      !
!     tvarnam    Name of time record variable                          !
!     get_var    Logical switches (T/F), in terms of variable ID,      !
!                  indicating state variables needed by the model      !
!     have_var   Logical switches (T/F), in terms of variable ID,      !
!                  indicating state variables available in NetCDF      !
!                  file                                                !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
!
      USE strings_mod, ONLY : FoundError
!
      implicit none
!
!  Interfaces for same name routine overloading..
!
      INTERFACE checkvars
        MODULE PROCEDURE checkvars_nf90
      END INTERFACE checkvars
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE checkvars_nf90 (ng, model, ncname, ncid, string,       &
     &                           Nrec, Nvar, tvarnam, get_var, have_var)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in)    :: ng, model, ncid, Nvar
      integer, intent(inout) :: Nrec
!
      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: string
      character (len=*), intent(inout) :: tvarnam
!
      logical, dimension(Nvar), intent(out) :: get_var
      logical, dimension(Nvar), intent(out) :: have_var
!
!  Local variable declarations.
!
      integer :: IDmod, i, itrc
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/checkvars.F"//", checkvars_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Determine state variables needed and check if they are available in
!  requested NetCDF file.
!-----------------------------------------------------------------------
!
!  Limit model identifier. The profiling is limited to iNLM, iTLM, iRPM,
!  and iADM.
!
      IF ((model.lt.1).or.(model.gt.4)) THEN
        IDmod=iNLM
      ELSE
        IDmod=model
      END IF
!
!  Inquire about the dimensions and check for consistency.
!
      CALL netcdf_check_dim (ng, IDmod, ncname,                         &
     &                       ncid = ncid)
      IF (FoundError(exit_flag, NoError, 119, MyFile)) RETURN
      Nrec=rec_size
!
!  Inquire about the variables.
!
      CALL netcdf_check_var (ng, IDmod, ncname,                         &
     &                       ncid = ncid)
      IF (FoundError(exit_flag, NoError, 126, MyFile)) RETURN
!
!  Initialize logical switches.
!
      DO i=1,Nvar
        get_var(i)=.FALSE.
        have_var(i)=.FALSE.
      END DO
!
!  Determine state variables to read from input NetCDF file.  Notice
!  that these state variable are only assigned if input flag model < 12,
!  model = 14, or model = 15. That is, all the state variables associated
!  with ROMS prognostic equations.  The remaining values are used to
!  process open boundary condition or surface forcing variable in the
!  4D-Var control vector.
!
      IF ((model.le.11).or.(model.eq.14).or.(model.eq.15)) THEN
        IF (nrrec(ng).ne.0) THEN
          get_var(idFsur)=.TRUE.
          get_var(idUbar)=.TRUE.
          get_var(idVbar)=.TRUE.
          get_var(idUvel)=.TRUE.
          get_var(idVvel)=.TRUE.
          DO itrc=1,NAT
            get_var(idTvar(itrc))=.TRUE.
          END DO
        END IF
        IF (nrrec(ng).ne.0) THEN
          DO itrc=1,NBT
            get_var(idTvar(idbio(itrc)))=.TRUE.
          END DO
        END IF
      END IF
!
!  Scan variable list from input NetCDF and activate switches for
!  model state variables.
!
      DO i=1,n_var
        IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
          tvarnam=TRIM(var_name(i))
          have_var(idtime)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idKver))) THEN
          have_var(idKver)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUvel))) THEN
          have_var(idUvel)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idRu3d))) THEN
          have_var(idRu3d)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVvel))) THEN
          have_var(idVvel)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idRv3d))) THEN
          have_var(idRv3d)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idFsur))) THEN
          have_var(idFsur)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idRzet))) THEN
          have_var(idRzet)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUbar))) THEN
          have_var(idUbar)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idRu2d))) THEN
          have_var(idRu2d)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVbar))) THEN
          have_var(idVbar)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idRv2d))) THEN
          have_var(idRv2d)=.TRUE.
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idKhor))) THEN
          have_var(idKhor)=.TRUE.
        END IF
        DO itrc=1,NT(ng)
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTvar(itrc)))) THEN
            have_var(idTvar(itrc))=.TRUE.
          END IF
        END DO
      END DO
!
!  Check if model state variables are available in input NetCDF file.
!
      IF (.not.have_var(idtime)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idtime)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idFsur).and.get_var(idFsur)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idFsur)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idRzet).and.get_var(idRzet)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idRzet)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idUbar).and.get_var(idUbar)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idUbar)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idRu2d).and.get_var(idRu2d)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idRu2d)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idVbar).and.get_var(idVbar)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idVbar)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idRv2d).and.get_var(idRv2d)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idRv2d)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idUvel).and.get_var(idUvel)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idUvel)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idRu3d).and.get_var(idRu3d)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idRu3d)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idVvel).and.get_var(idVvel)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idVvel)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.have_var(idRv3d).and.get_var(idRv3d)) THEN
        IF (Master) WRITE (stdout,10) string, TRIM(Vname(1,idRv3d)),    &
     &                                TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      DO itrc=1,NT(ng)
        IF (.not.have_var(idTvar(itrc)).and.                            &
     &      get_var(idTvar(itrc))) THEN
          IF (Master) WRITE (stdout,10) string,                         &
     &                                  TRIM(Vname(1,idTvar(itrc))),    &
     &                                  TRIM(ncname)
          exit_flag=2
          RETURN
        END IF
      END DO
!
  10  FORMAT (/,a,'CHECKVARS_NF90 - unable to find model variable: ',a, &
     &        /,23x,'in file: ',a)
!
      RETURN
      END SUBROUTINE checkvars_nf90
      END MODULE checkvars_mod
