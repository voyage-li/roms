      MODULE mod_iounits
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  The input and output files information is stored in compact derived !
!  type structure, TYPE(T_IO):                                         !
!                                                                      !
!    ADM       Output adjoint model history data                       !
!    ADS       Input adjoint sensitivity functional                    !
!    AVG       Output time-averaged data                               !
!    BLK       Input bulk fluxes data in adjoint-based applications    !
!    BRY       Input open boundary conditions data                     !
!    CLM       Input climatology data                                  !
!    DAI       Output data assimilation next cycle initial conditions  !
!                (4D-Var analysis) or restart (Ensemble Kalman Filter) !
!    DAV       Output 4D-Var data assimilation variables               !
!    DIA       Output diagnostics fields                               !
!    ERR       Output 4DVar posterior error estimate                   !
!    FLT       Output Lagrangian trajectories data                     !
!    FRC       Input forcing data                                      !
!    FWD       Input basic state forward solution                      !
!    GRD       Input grid data                                         !
!    GRX       Input grid for extracted history data                   !
!    GST       Input/output GST analysis check pointing data           !
!    HAR       Output detiding least-squares harmonics coefficients    !
!    HIS       Output nonlinear model history data                     !
!    HSS       Input/output Hessian eigenvectors                       !
!    IAD       Input adjoint model initial conditions                  !
!    INI       Input nonlinear model initial conditions                !
!    IPR       Input representer model initial conditions              !
!    ITL       Input tangent linear model initial conditions           !
!    LCZ       Input/output Lanczos vectors                            !
!    LZE       Input/output time-evolved Lanczos vectors               !
!    NRM       Input/output error covariance normalization data        !
!                NRM(1,ng)  initial conditions                         !
!                NRM(2,ng)  model error                                !
!                NRM(3,ng)  lateral open boundary conditions           !
!                NRM(4,ng)  surface forcing                            !
!    NUD       Input climatology nudging coefficients                  !
!    OBS       Input/output 4D-Var observations                        !
!    QCK       Output nonlinear model brief snpashots data             !
!    RST       Output restart data                                     !
!    TIDE      Input tide forcing                                      !
!    TLF       Input/output tangent linear model impulse forcing       !
!    TLM       Output tangent linear model history                     !
!    SSF       Input Sources/Sinks forcing (river runoff)              !
!    STA       Output station data                                     !
!    STD       Error covariance standard deviations                    !
!                STD(1,ng)  initial conditions                         !
!                STD(2,ng)  model error                                !
!                STD(3,ng)  lateral open boundary conditions           !
!                STD(4,ng)  surface forcing                            !
!                STD(5,ng)  computed from background (output)          !
!    XTR       Output extracted history fields                         !
!                                                                      !
!  Input/output information files:                                     !
!                                                                      !
!  Iname       Physical parameters standard input script filename.     !
!  NGCname     Nested grids contact point information filename.        !
!  USRname     USER input/output generic filename.                     !
!  Wname       Wave model stadard input filename.                      !
!  aparnam     Input assimilation parameters filename.                 !
!  bparnam     Input biology parameters filename.                      !
!  fbionam     Input floats biological behavior parameters filename.   !
!  fposnam     Input initial floats positions filename.                !
!  iparnam     Input ice parameters file name.                         !
!  sparnam     Input sediment transport parameters filename.           !
!  sposnam     Input station positions file name.                      !
!  varname     Input IO variables information file name.               !
!                                                                      !
!  stdinp      Unit number for standard input (often 5).               !
!  stdout      Unit number for standard output (often 6).              !
!  usrout      Unit number for generic USER output.                    !
!                                                                      !
!  Standard NetCDF library input files ID per each field:              !
!                                                                      !
!  BRYids      File ID associated with each boundary field.            !
!  CLMids      File ID associated with each climatology field.         !
!  FRCids      File ID associated with each forcing field.             !
!                                                                      !
!  Miscellaneous variables:                                            !
!                                                                      !
!  CalledFrom  Calling routine in IO operations.                       !
!  Rerror      Running error messages.                                 !
!  SourceFile  Current executed file name. It is used for IO error     !
!                purposes.                                             !
!  ioerror     IO error flag.                                          !
!  ncfile      Current NetCDF file name being processed.               !
!  nFfiles     Number of forcing files.                                !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
!
      implicit none
!
      PUBLIC :: allocate_iounits
      PUBLIC :: deallocate_iounits
!
!-----------------------------------------------------------------------
!  Define derived type structure, T_IO, used to store information about
!  the input and output files.
!-----------------------------------------------------------------------
!
!  This structure is used to declare the variables associated with
!  input and output files, like TYPE(IO) :: HIS(Ngrids). It is a
!  compact way to store a lot of information.  The "Fcount" variable
!  is used during data processing in "check_multifile" and "inquiry"
!  whereas "load" is used storing information in the structure.
!
      TYPE T_IO
        integer :: IOtype                        ! file IO type
        integer :: Nfiles                        ! number of multi-files
        integer :: Fcount                        ! multi-file counter
        integer :: load                          ! filename load counter
        integer :: Rindex                        ! NetCDF record index
        integer :: ncid                          ! NetCDF file ID
        integer,  pointer :: Nrec(:)             ! NetCDF record size
        integer,  pointer :: Vid(:)              ! NetCDF variables IDs
        integer,  pointer :: Tid(:)              ! NetCDF tracers IDs
        real(dp), pointer :: time_min(:)         ! starting time
        real(dp), pointer :: time_max(:)         ! ending time
        character (len=50 ) :: label             ! structure label
        character (len=256) :: head              ! head filename
        character (len=256) :: base              ! base filename
        character (len=256) :: name              ! current name
        character (len=256), pointer :: files(:) ! multi-file names
      END TYPE T_IO
!
!-----------------------------------------------------------------------
!  Define variables in module.
!-----------------------------------------------------------------------
!
!  I/O units.
!
      integer :: stdinp = 5                 ! standard input
      integer :: stdout                     ! standard output, usually 6
      integer :: usrout = 10                ! generic user unit
!
!  I/O files management, derived type structures.
!
      TYPE(T_IO), allocatable :: ADM(:)     ! ADM history fields
      TYPE(T_IO), allocatable :: ADS(:)     ! sensitivity functional
      TYPE(T_IO), allocatable :: AVG(:)     ! time-averaged fields
      TYPE(T_IO), allocatable :: BLK(:)     ! bulk fluxes fields
      TYPE(T_IO), allocatable :: DAI(:)     ! Data assilation IC/restart
      TYPE(T_IO), allocatable :: DAV(:)     ! 4D-Var variables
      TYPE(T_IO), allocatable :: DIA(:)     ! diagnostics fields
      TYPE(T_IO), allocatable :: ERR(:)     ! 4D-Var posterior error
      TYPE(T_IO), allocatable :: FLT(:)     ! Lagrangian trajectories
      TYPE(T_IO), allocatable :: FWD(:)     ! forward solution
      TYPE(T_IO), allocatable :: GRD(:)     ! grid geometry data
      TYPE(T_IO), allocatable :: GRX(:)     ! grid for extrated fields
      TYPE(T_IO), allocatable :: GST(:)     ! generalized stability
      TYPE(T_IO), allocatable :: HAR(:)     ! detiding harmonics
      TYPE(T_IO), allocatable :: HIS(:)     ! NLM history fields
      TYPE(T_IO), allocatable :: HSS(:)     ! Hessian eigenvectors
      TYPE(T_IO), allocatable :: IAD(:)     ! ADM initial conditions
      TYPE(T_IO), allocatable :: INI(:)     ! NLM initial conditions
      TYPE(T_IO), allocatable :: IRP(:)     ! RPM initial conditions
      TYPE(T_IO), allocatable :: ITL(:)     ! TLM initial conditions
      TYPE(T_IO), allocatable :: LCZ(:)     ! Lanczos vectors
      TYPE(T_IO), allocatable :: LZE(:)     ! evolved Lanczos vectors
      TYPE(T_IO), allocatable :: NRM(:,:)   ! normalization
      TYPE(T_IO), allocatable :: NUD(:)     ! nudging coefficients
      TYPE(T_IO), allocatable :: OBS(:)     ! observations
      TYPE(T_IO), allocatable :: QCK(:)     ! quicksave fields
      TYPE(T_IO), allocatable :: RST(:)     ! restart fields
      TYPE(T_IO), allocatable :: SSF(:)     ! Sources/Sinks forcing
      TYPE(T_IO), allocatable :: STA(:)     ! stations data
      TYPE(T_IO), allocatable :: STD(:,:)   ! standard deviation
      TYPE(T_IO), allocatable :: TIDE(:)    ! tidal forcing
      TYPE(T_IO), allocatable :: TLF(:)     ! TLM impulse fields
      TYPE(T_IO), allocatable :: TLM(:)     ! TLM history fields
      TYPE(T_IO), allocatable :: XTR(:)     ! extracted history fields
!
!  Input boundary condition data.
!
      integer, allocatable :: nBCfiles(:)
      integer, allocatable :: BRYids(:,:)
!
      TYPE(T_IO), allocatable :: BRY(:,:)
!
!  Input climatology data.
!
      integer, allocatable :: nCLMfiles(:)
      integer, allocatable :: CLMids(:,:)
!
      TYPE(T_IO), allocatable :: CLM(:,:)
!
!  Input forcing data.
!
      integer, allocatable :: nFfiles(:)
      integer, allocatable :: FRCids(:,:)
!
      TYPE(T_IO), allocatable :: FRC(:,:)
!
!  Error messages.
!
      character (len=50), dimension(9) :: Rerror =                      &
     &       (/ ' ROMS/TOMS - Blows up ................ exit_flag: ',   &
     &          ' ROMS/TOMS - Input error ............. exit_flag: ',   &
     &          ' ROMS/TOMS - Output error ............ exit_flag: ',   &
     &          ' ROMS/TOMS - I/O error ............... exit_flag: ',   &
     &          ' ROMS/TOMS - Configuration error ..... exit_flag: ',   &
     &          ' ROMS/TOMS - Partition error ......... exit_flag: ',   &
     &          ' ROMS/TOMS - Illegal input parameter . exit_flag: ',   &
     &          ' ROMS/TOMS - Fatal algorithm result .. exit_flag: ',   &
     &          ' ROMS/TOMS - Coupling error .......... exit_flag: ' /)
!
!  Standard input scripts file names.
!
      character (len=256) :: Iname          ! ROMS physical parameters
      character (len=256) :: Wname          ! wave model standard input
      character (len=256) :: NGCname        ! contact points filename
      character (len=256) :: USRname        ! use generic filename
      character (len=256) :: aparnam        ! assimilation parameters
      character (len=256) :: bparnam        ! biology model parameters
      character (len=256) :: fbionam        ! floats behavior parameters
      character (len=256) :: fposnam        ! floats positions
      character (len=256) :: iparnam        ! ice model parameters
      character (len=256) :: sparnam        ! sediment model parameters
      character (len=256) :: sposnam        ! station positions
      character (len=256) :: varname        ! I/O metadata
!
!  Miscelaneous variables.
!
      integer :: ioerror = 0                ! I/O error flag
      character (len=256) :: CalledFrom     ! routine calling IO
      character (len=256) :: MyAppCPP       ! application CPP flag
      character (len=256) :: SourceFile     ! current executed ROMS file
      character (len=256) :: ncfile         ! current NetCDF file
!
      CONTAINS
!
      SUBROUTINE allocate_iounits (Ngrids)
!
!=======================================================================
!                                                                      !
!  This routine allocates several variables in the module that depend  !
!  on the number of nested grids.                                      !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer, intent(in) :: Ngrids
!
!  Local variable declarations.
!
      integer :: i, lstr, ng
      character (len=1), parameter :: blank = ' '
!
!-----------------------------------------------------------------------
!  Allocate I/O files management, derived type structures.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(ADM)) THEN
        allocate ( ADM(Ngrids) )
      END IF
      IF (.not.allocated(ADS)) THEN
        allocate ( ADS(Ngrids) )
      END IF
      IF (.not.allocated(AVG)) THEN
        allocate ( AVG(Ngrids) )
      END IF
      IF (.not.allocated(BLK)) THEN
        allocate ( BLK(Ngrids) )
      END IF
      IF (.not.allocated(DAI)) THEN
        allocate ( DAI(Ngrids) )
      END IF
      IF (.not.allocated(DAV)) THEN
        allocate ( DAV(Ngrids) )
      END IF
      IF (.not.allocated(DIA)) THEN
        allocate ( DIA(Ngrids) )
      END IF
      IF (.not.allocated(ERR)) THEN
        allocate ( ERR(Ngrids) )
      END IF
      IF (.not.allocated(FLT)) THEN
        allocate ( FLT(Ngrids) )
      END IF
      IF (.not.allocated(FWD)) THEN
        allocate ( FWD(Ngrids) )
      END IF
      IF (.not.allocated(GRD)) THEN
        allocate ( GRD(Ngrids) )
      END IF
      IF (.not.allocated(GRX)) THEN
        allocate ( GRX(Ngrids) )
      END IF
      IF (.not.allocated(GST)) THEN
        allocate ( GST(Ngrids) )
      END IF
      IF (.not.allocated(HAR)) THEN
        allocate ( HAR(Ngrids) )
      END IF
      IF (.not.allocated(HIS)) THEN
        allocate ( HIS(Ngrids) )
      END IF
      IF (.not.allocated(HSS)) THEN
        allocate ( HSS(Ngrids) )
      END IF
      IF (.not.allocated(IAD)) THEN
        allocate ( IAD(Ngrids) )
      END IF
      IF (.not.allocated(INI)) THEN
        allocate ( INI(Ngrids) )
      END IF
      IF (.not.allocated(IRP)) THEN
        allocate ( IRP(Ngrids) )
      END IF
      IF (.not.allocated(ITL)) THEN
        allocate ( ITL(Ngrids) )
      END IF
      IF (.not.allocated(LCZ)) THEN
        allocate ( LCZ(Ngrids) )
      END IF
      IF (.not.allocated(LZE)) THEN
        allocate ( LZE(Ngrids) )
      END IF
      IF (.not.allocated(NUD)) THEN
        allocate ( NUD(Ngrids) )
      END IF
      IF (.not.allocated(OBS)) THEN
        allocate ( OBS(Ngrids) )
      END IF
      IF (.not.allocated(QCK)) THEN
        allocate ( QCK(Ngrids) )
      END IF
      IF (.not.allocated(RST)) THEN
        allocate ( RST(Ngrids) )
      END IF
      IF (.not.allocated(SSF)) THEN
        allocate ( SSF(Ngrids) )
      END IF
      IF (.not.allocated(STA)) THEN
        allocate ( STA(Ngrids) )
      END IF
      IF (.not.allocated(TIDE)) THEN
        allocate ( TIDE(Ngrids) )
      END IF
      IF (.not.allocated(TLF)) THEN
        allocate ( TLF(Ngrids) )
      END IF
      IF (.not.allocated(TLM)) THEN
        allocate ( TLM(Ngrids) )
      END IF
      IF (.not.allocated(XTR)) THEN
        allocate ( XTR(Ngrids) )
      END IF
      IF (.not.allocated(NRM)) THEN
        allocate ( NRM(4,Ngrids) )
      END IF
      IF (.not.allocated(STD)) THEN
        allocate ( STD(5,Ngrids) )
      END IF
!
!-----------------------------------------------------------------------
!  Allocate variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(nBCfiles)) THEN
        allocate ( nBCfiles(Ngrids) )
      END IF
      IF (.not.allocated(nCLMfiles)) THEN
        allocate ( nCLMfiles(Ngrids) )
      END IF
      IF (.not.allocated(nFfiles)) THEN
        allocate ( nFfiles(Ngrids) )
      END IF
!
!-----------------------------------------------------------------------
!  Initialize I/O NetCDF files ID to close state.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        ADM(ng)%ncid=-1
        ADS(ng)%ncid=-1
        AVG(ng)%ncid=-1
        BLK(ng)%ncid=-1
        DAI(ng)%ncid=-1
        DAV(ng)%ncid=-1
        DIA(ng)%ncid=-1
        ERR(ng)%ncid=-1
        FLT(ng)%ncid=-1
        FWD(ng)%ncid=-1
        GRD(ng)%ncid=-1
        GRX(ng)%ncid=-1
        GST(ng)%ncid=-1
        HAR(ng)%ncid=-1
        HIS(ng)%ncid=-1
        HSS(ng)%ncid=-1
        IAD(ng)%ncid=-1
        INI(ng)%ncid=-1
        IRP(ng)%ncid=-1
        ITL(ng)%ncid=-1
        LCZ(ng)%ncid=-1
        LZE(ng)%ncid=-1
        NUD(ng)%ncid=-1
        OBS(ng)%ncid=-1
        QCK(ng)%ncid=-1
        RST(ng)%ncid=-1
        SSF(ng)%ncid=-1
        STA(ng)%ncid=-1
        TLF(ng)%ncid=-1
        TLM(ng)%ncid=-1
        TIDE(ng)%ncid=-1
        XTR(ng)%ncid=-1
        NRM(1:4,ng)%ncid=-1
        STD(1:5,ng)%ncid=-1
        nBCfiles(ng)=-1
        nCLMfiles(ng)=-1
        nFfiles(ng)=-1
      END DO
!
!-----------------------------------------------------------------------
!  Initialize file names to blanks.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        lstr=LEN(HIS(ng)%name)
        DO i=1,lstr
          ADM(ng)%head(i:i)=blank
          ADM(ng)%base(i:i)=blank
          ADM(ng)%name(i:i)=blank
          ADS(ng)%head(i:i)=blank
          ADS(ng)%base(i:i)=blank
          ADS(ng)%name(i:i)=blank
          AVG(ng)%head(i:i)=blank
          AVG(ng)%base(i:i)=blank
          AVG(ng)%name(i:i)=blank
          BLK(ng)%head(i:i)=blank
          BLK(ng)%base(i:i)=blank
          BLK(ng)%name(i:i)=blank
          DAI(ng)%head(i:i)=blank
          DAI(ng)%base(i:i)=blank
          DAI(ng)%name(i:i)=blank
          DAV(ng)%head(i:i)=blank
          DAV(ng)%base(i:i)=blank
          DAV(ng)%name(i:i)=blank
          DIA(ng)%head(i:i)=blank
          DIA(ng)%base(i:i)=blank
          DIA(ng)%name(i:i)=blank
          ERR(ng)%head(i:i)=blank
          ERR(ng)%base(i:i)=blank
          ERR(ng)%name(i:i)=blank
          FLT(ng)%head(i:i)=blank
          FLT(ng)%base(i:i)=blank
          FLT(ng)%name(i:i)=blank
          FWD(ng)%head(i:i)=blank
          FWD(ng)%base(i:i)=blank
          FWD(ng)%name(i:i)=blank
          GRD(ng)%head(i:i)=blank
          GRD(ng)%base(i:i)=blank
          GRD(ng)%name(i:i)=blank
          GRX(ng)%head(i:i)=blank
          GRX(ng)%base(i:i)=blank
          GRX(ng)%name(i:i)=blank
          GST(ng)%head(i:i)=blank
          GST(ng)%base(i:i)=blank
          GST(ng)%name(i:i)=blank
          HAR(ng)%head(i:i)=blank
          HAR(ng)%base(i:i)=blank
          HAR(ng)%name(i:i)=blank
          HIS(ng)%head(i:i)=blank
          HIS(ng)%base(i:i)=blank
          HIS(ng)%name(i:i)=blank
          HSS(ng)%head(i:i)=blank
          HSS(ng)%base(i:i)=blank
          HSS(ng)%name(i:i)=blank
          IAD(ng)%head(i:i)=blank
          IAD(ng)%base(i:i)=blank
          IAD(ng)%name(i:i)=blank
          INI(ng)%head(i:i)=blank
          INI(ng)%base(i:i)=blank
          INI(ng)%name(i:i)=blank
          IRP(ng)%head(i:i)=blank
          IRP(ng)%base(i:i)=blank
          IRP(ng)%name(i:i)=blank
          ITL(ng)%head(i:i)=blank
          ITL(ng)%base(i:i)=blank
          ITL(ng)%name(i:i)=blank
          LCZ(ng)%head(i:i)=blank
          LCZ(ng)%base(i:i)=blank
          LCZ(ng)%name(i:i)=blank
          LZE(ng)%head(i:i)=blank
          LZE(ng)%base(i:i)=blank
          LZE(ng)%name(i:i)=blank
          NUD(ng)%head(i:i)=blank
          NUD(ng)%base(i:i)=blank
          NUD(ng)%name(i:i)=blank
          OBS(ng)%head(i:i)=blank
          OBS(ng)%base(i:i)=blank
          OBS(ng)%name(i:i)=blank
          QCK(ng)%head(i:i)=blank
          QCK(ng)%base(i:i)=blank
          QCK(ng)%name(i:i)=blank
          RST(ng)%head(i:i)=blank
          RST(ng)%base(i:i)=blank
          RST(ng)%name(i:i)=blank
          SSF(ng)%head(i:i)=blank
          SSF(ng)%base(i:i)=blank
          SSF(ng)%name(i:i)=blank
          STA(ng)%head(i:i)=blank
          STA(ng)%base(i:i)=blank
          STA(ng)%name(i:i)=blank
          TLF(ng)%head(i:i)=blank
          TLF(ng)%base(i:i)=blank
          TLF(ng)%name(i:i)=blank
          TLM(ng)%head(i:i)=blank
          TLM(ng)%base(i:i)=blank
          TLM(ng)%name(i:i)=blank
          TIDE(ng)%head(i:i)=blank
          TIDE(ng)%base(i:i)=blank
          TIDE(ng)%name(i:i)=blank
          XTR(ng)%head(i:i)=blank
          XTR(ng)%base(i:i)=blank
          XTR(ng)%name(i:i)=blank
          NRM(1:4,ng)%head(i:i)=blank
          NRM(1:4,ng)%base(i:i)=blank
          NRM(1:4,ng)%name(i:i)=blank
          STD(1:5,ng)%head(i:i)=blank
          STD(1:5,ng)%base(i:i)=blank
          STD(1:5,ng)%name(i:i)=blank
        END DO
      END DO
!
      DO i=1,LEN(Iname)
        Wname(i:i)=blank
        NGCname(i:i)=blank
        USRname(i:i)=blank
        aparnam(i:i)=blank
        bparnam(i:i)=blank
        fbionam(i:i)=blank
        fposnam(i:i)=blank
        sparnam(i:i)=blank
        sposnam(i:i)=blank
      END DO
!
      RETURN
      END SUBROUTINE allocate_iounits
!
      SUBROUTINE deallocate_iounits
!
!=======================================================================
!                                                                      !
!  This routine deallocates variables in module.                       !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Deallocate I/O derived type structures.
!-----------------------------------------------------------------------
!
      IF (allocated(ADM))       deallocate ( ADM )
      IF (allocated(ADS))       deallocate ( ADS )
      IF (allocated(AVG))       deallocate ( AVG )
      IF (allocated(BLK))       deallocate ( BLK )
      IF (allocated(BRY))       deallocate ( BRY )
      IF (allocated(CLM))       deallocate ( CLM )
      IF (allocated(DAI))       deallocate ( DAI )
      IF (allocated(DAV))       deallocate ( DAV )
      IF (allocated(DIA))       deallocate ( DIA )
      IF (allocated(ERR))       deallocate ( ERR )
      IF (allocated(FLT))       deallocate ( FLT )
      IF (allocated(FRC))       deallocate ( FRC )
      IF (allocated(FWD))       deallocate ( FWD )
      IF (allocated(GRD))       deallocate ( GRD )
      IF (allocated(GRX))       deallocate ( GRX )
      IF (allocated(GST))       deallocate ( GST )
      IF (allocated(HAR))       deallocate ( HAR )
      IF (allocated(HIS))       deallocate ( HIS )
      IF (allocated(HSS))       deallocate ( HSS )
      IF (allocated(IAD))       deallocate ( IAD )
      IF (allocated(INI))       deallocate ( INI )
      IF (allocated(IRP))       deallocate ( IRP )
      IF (allocated(ITL))       deallocate ( ITL )
      IF (allocated(LCZ))       deallocate ( LCZ )
      IF (allocated(LZE))       deallocate ( LZE )
      IF (allocated(NUD))       deallocate ( NUD )
      IF (allocated(OBS))       deallocate ( OBS )
      IF (allocated(QCK))       deallocate ( QCK )
      IF (allocated(RST))       deallocate ( RST )
      IF (allocated(SSF))       deallocate ( SSF )
      IF (allocated(STA))       deallocate ( STA )
      IF (allocated(TIDE))      deallocate ( TIDE )
      IF (allocated(TLF))       deallocate ( TLF )
      IF (allocated(TLM))       deallocate ( TLM )
      IF (allocated(XTR))       deallocate ( XTR )
      IF (allocated(NRM))       deallocate ( NRM )
      IF (allocated(STD))       deallocate ( STD )
!
!-----------------------------------------------------------------------
!  Deallocate other variables in module.
!-----------------------------------------------------------------------
!
      IF (allocated(BRYids))    deallocate ( BRYids )
      IF (allocated(CLMids))    deallocate ( CLMids )
      IF (allocated(FRCids))    deallocate ( FRCids )
      IF (allocated(nBCfiles))  deallocate ( nBCfiles )
      IF (allocated(nCLMfiles)) deallocate ( nCLMfiles )
      IF (allocated(nFfiles))   deallocate ( nFfiles )
!
      RETURN
      END SUBROUTINE deallocate_iounits
      END MODULE mod_iounits
