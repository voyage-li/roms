      SUBROUTINE edit_multifile (task)
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  Edits and updates derived type structure TYPE(T_IO) for the I/O     !
!  manipulation in some algorithms.                                    !
!                                                                      !
!  For example, the forward trajectory files can be split into several !
!  multifiles to avoid creating large files in 4D-Var.                 !
!                                                                      !
!  Notice the base filename is not modified to preserve the root value !
!  specified by the user.                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
!
      USE close_io_mod, ONLY : close_file
!
!  Imported variable declarations.
!
      character (len=*) :: task
!
!  Local variable declarations.
!
      integer :: Iunder, ifile, lstr, ng
      integer :: Nfiles
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/edit_multifile.F"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Edit and update multi-file structure acconding to requested task.
!-----------------------------------------------------------------------
!
!  If multiple history files, close the last one created by the
!  nonlinear model to avoid exceeding the number of allowed opened
!  files in UNIX. Recall that the 4D-Var algorithms will open and
!  close the forward multi-file many times in the TLM and ADM in
!  the inner loops.
!
      DO ng=1,Ngrids
!
        SELECT CASE (TRIM(ADJUSTL(task)))
!
!  Load HIS information into the FWD structure so it can be used to
!  process the NLM background trajectory by the ADM and TLM kernels.
!
          CASE ('HIS2FWD')
            FWD(ng)%IOtype=HIS(ng)%IOtype
            IF (ndefHIS(ng).gt.0) THEN
              CALL close_file (ng, iNLM, HIS(ng), HIS(ng)%name)
              Nfiles=ntimes(ng)/ndefHIS(ng)
              IF (nHIS(ng).eq.ndefHIS(ng)) Nfiles=Nfiles+1
              CALL edit_file_struct (ng, Nfiles, FWD)
              DO ifile=1,Nfiles
                FWD(ng)%files(ifile)=TRIM(HIS(ng)%files(ifile))
              END DO
              FWD(ng)%name=TRIM(FWD(ng)%files(1))
            ELSE
              IF (FWD(ng)%IOtype.eq.io_nf90) THEN
                FWD(ng)%ncid=HIS(ng)%ncid
              END IF
              FWD(ng)%name=TRIM(HIS(ng)%name)
              FWD(ng)%files(1)=TRIM(HIS(ng)%name)
            END IF
!
!  Load HIS information into the BLK structure so it can be used to
!  process the NLM background surface forcing to be read and processd
!  by the ADM and TLM kernels.
!
          CASE ('HIS2BLK')
            BLK(ng)%IOtype=HIS(ng)%IOtype
            IF (ndefHIS(ng).gt.0) THEN
              CALL close_file (ng, iNLM, HIS(ng), HIS(ng)%name)
              Nfiles=ntimes(ng)/ndefHIS(ng)
              IF (nHIS(ng).eq.ndefHIS(ng)) Nfiles=Nfiles+1
              CALL edit_file_struct (ng, Nfiles, BLK)
              DO ifile=1,Nfiles
                BLK(ng)%files(ifile)=TRIM(HIS(ng)%files(ifile))
              END DO
              BLK(ng)%name=TRIM(BLK(ng)%files(1))
            ELSE
              IF (BLK(ng)%IOtype.eq.io_nf90) THEN
                BLK(ng)%ncid=-1
              END IF
              BLK(ng)%name=TRIM(HIS(ng)%name)
              BLK(ng)%files(1)=TRIM(HIS(ng)%name)
            END IF
!
!  Load QCK information into the BLK structure so it can be used to
!  process the NLM background surface forcing to be read and processed
!  by the TLM, RPM, and ADM kernels.
!
          CASE ('QCK2BLK')
            BLK(ng)%IOtype=QCK(ng)%IOtype
            IF (ndefQCK(ng).gt.0) THEN
              CALL close_file (ng, iNLM, QCK(ng), QCK(ng)%name)
              Nfiles=ntimes(ng)/ndefQCK(ng)
              IF (nQCK(ng).eq.ndefQCK(ng)) Nfiles=Nfiles+1
              CALL edit_file_struct (ng, Nfiles, BLK)
              DO ifile=1,Nfiles
                BLK(ng)%files(ifile)=TRIM(QCK(ng)%files(ifile))
              END DO
              BLK(ng)%name=TRIM(BLK(ng)%files(1))
            ELSE
              IF (BLK(ng)%IOtype.eq.io_nf90) THEN
                BLK(ng)%ncid=-1
              END IF
              BLK(ng)%name=TRIM(QCK(ng)%name)
              BLK(ng)%files(1)=TRIM(QCK(ng)%name)
            END IF
!
!  Load FWD information into the BLK structure so it can be used to
!  process the NLM background surface forcing to be read and processd
!  by the ADM and TLM kernels.
!
          CASE ('FWD2BLK')
            BLK(ng)%IOtype=FWD(ng)%IOtype
            Nfiles=FWD(ng)%Nfiles
            IF (Nfiles.gt.1) THEN
              CALL close_file (ng, iNLM, BLK(ng), BLK(ng)%name)
              CALL edit_file_struct (ng, Nfiles, BLK)
              DO ifile=1,Nfiles
                BLK(ng)%files(ifile)=TRIM(FWD(ng)%files(ifile))
              END DO
              BLK(ng)%name=TRIM(BLK(ng)%files(1))
            ELSE
              IF (BLK(ng)%IOtype.eq.io_nf90) THEN
                BLK(ng)%ncid=-1
              END IF
              BLK(ng)%name=TRIM(FWD(ng)%name)
              BLK(ng)%files(1)=TRIM(FWD(ng)%name)
            END IF
!
!  Load TLM information into the FWD structure so it can be used to
!  process the RPM background trajectory by the RPM, ADM and TLM
!  kernels.  Used in R4D-Var.
!
          CASE ('TLM2FWD')
            FWD(ng)%IOtype=TLM(ng)%IOtype
            IF (ndefTLM(ng).gt.0) THEN
              CALL close_file (ng, iNLM, TLM(ng), TLM(ng)%name)
              Nfiles=ntimes(ng)/ndefTLM(ng)
              IF (nTLM(ng).eq.ndefTLM(ng)) Nfiles=Nfiles+1
              CALL edit_file_struct (ng, Nfiles, FWD)
              DO ifile=1,Nfiles
                FWD(ng)%files(ifile)=TRIM(TLM(ng)%files(ifile))
              END DO
              FWD(ng)%name=TRIM(FWD(ng)%files(1))
            ELSE
              IF (FWD(ng)%IOtype.eq.io_nf90) THEN
                FWD(ng)%ncid=TLM(ng)%ncid
              END IF
              FWD(ng)%name=TRIM(TLM(ng)%name)
              FWD(ng)%files(1)=TRIM(TLM(ng)%name)
            END IF
!
        END SELECT
      END DO
      RETURN
      END SUBROUTINE edit_multifile
!
      SUBROUTINE edit_file_struct (ng, Nfiles, S)
!
!***********************************************************************
!                                                                      !
!  This function loads input values into requested 1D structure        !
!  containing information about I/O files.                             !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number (integer)                          !
!     Nfiles     Number of desired files (integer)                     !
!     S          Derived type 1D structure, TYPE(T_IO)                 !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     S          Updated erived type 1D structure, TYPE(T_IO)          !
!                                                                      !
!***********************************************************************
!
      USE mod_param
      USE mod_iounits
      USE mod_ncparam, ONLY : NV
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, Nfiles
      TYPE(T_IO), intent(inout) :: S(Ngrids)
!
!  Local variable declarations.
!
      integer :: i, j, lstr
      character (len=  1), parameter :: blank = ' '
!
!-----------------------------------------------------------------------
!  If the number of multifiles in the structure is different, allocate
!  to the desired number of files.
!-----------------------------------------------------------------------
!
!  If the number of muti-files in structure is different than requested
!  values, deallocate and reallocate to the desired number of files.
!
      IF (S(ng)%Nfiles.ne.Nfiles) THEN
        IF (associated(S(ng)%Nrec))     deallocate (S(ng)%Nrec)
        IF (associated(S(ng)%time_min)) deallocate (S(ng)%time_min)
        IF (associated(S(ng)%time_max)) deallocate (S(ng)%time_max)
        IF (associated(S(ng)%files))    deallocate (S(ng)%files)
!
        allocate ( S(ng)%Nrec(Nfiles) )
        allocate ( S(ng)%time_min(Nfiles) )
        allocate ( S(ng)%time_max(Nfiles) )
        allocate ( S(ng)%files(Nfiles) )
      END IF
!
!  Intialize strings to blank to facilitate processing.
!
      lstr=LEN(S(ng)%name)
      DO i=1,lstr
        S(ng)%name(i:i)=blank
      END DO
      DO j=1,Nfiles
        DO i=1,lstr
          S(ng)%files(j)(i:i)=blank
        END DO
      END DO
!
!  Initialize and load fields into structure.  The base filename value
!  was already updated somewhere else.
!
      S(ng)%Nfiles=Nfiles                      ! number of multi-files
      S(ng)%Fcount=1                           ! multi-file counter
      S(ng)%Rindex=0                           ! time index
      S(ng)%ncid=-1                            ! closed NetCDF state
      S(ng)%Vid=-1                             ! NetCDF variables IDs
      S(ng)%Tid=-1                             ! NetCDF tracers IDs
      S(ng)%Nrec=0                             ! record counter
      S(ng)%time_min=0.0_dp                    ! starting time
      S(ng)%time_max=0.0_dp                    ! ending time
!
  10  FORMAT (a,'_',i4.4,'.nc')
!
      RETURN
      END SUBROUTINE edit_file_struct
