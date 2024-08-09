      MODULE mod_scalars
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!
        USE mod_kinds
!
        PUBLIC :: allocate_scalars
        PUBLIC :: deallocate_scalars
        PUBLIC :: initialize_scalars
!
!-----------------------------------------------------------------------
!  Multiple grid structure.
!-----------------------------------------------------------------------
!
!    Fstate        Logical switches to control computations of the
!                    Forcing Singular Vectors or Stochastic Optimals.
!    Lstate        Logical switches to control computations of the
!                    model state.
!    Sflag         Station extraction special flag:
!                    Sflag = 0  => locations in terms of (I,J) pairs.
!                    Sflag = 1  => locations in terms of (lon,lat) pairs.
!    SposX         Longitude or frational I-coordinate station location.
!    SposY         Latitude  or frational J-coordinate station location.
!    Cs_r          Set of S-curves used to stretch the vertical grid
!                    that follows the bathymetry at vertical RHO-points.
!    Cs_w          Set of S-curves used to stretch the vertical grid
!                    that follows the bathymetry at vertical W-points.
!    sc_r          S-coordinate independent variable, [-1 < sc < 0] at
!                    vertical RHO-points.
!    sc_w          S-coordinate independent variable, [-1 < sc < 0] at
!                    vertical W-points.
!
        TYPE T_SCALARS
          logical, pointer :: Fstate(:)
          logical, pointer :: Lstate(:)
          integer,  pointer :: Sflag(:)
          real(r8), pointer :: SposX(:)
          real(r8), pointer :: SposY(:)
          real(dp), pointer :: Cs_r(:)
          real(dp), pointer :: Cs_w(:)
          real(dp), pointer :: sc_r(:)
          real(dp), pointer :: sc_w(:)
        END TYPE T_SCALARS
!
        TYPE (T_SCALARS), allocatable :: SCALARS(:)
!
!-----------------------------------------------------------------------
!  Time clock structure.
!-----------------------------------------------------------------------
!
!  Reference time (yyyymmdd.f) used to compute relative time. The
!  application date clock is measured ad elapsed time interval since
!  reference-time. This parameter also provides information about the
!  calendar used:
!
!    If TIME_REF = -2, the model time and DSTART are in modified Julian
!                  days units.  The time "units" attribute is:
!
!                  'time-units since 1968-05-23 00:00:00 GMT'
!
!    If TIME_REF = -1, the model time and DSTART are in a calendar
!                  with 360 days in every year (30 days each month).
!                  The time "units" attribute is:
!
!                  'time-units since 0001-01-01 00:00:00'
!
!    If TIME_REF = 0, the model time and DSTART are in a common year
!                  calendar with 365.2524 days.  The "units" attribute
!                  is:
!
!                  'time-units since 0001-01-01 00:00:00'
!
!    If TIME_REF > 0, the model time and DSTART are the elapsed time
!                  units since specified reference time.  For example,
!                  TIME_REF=20020115.5 will yield the following
!                  time "units" attribute:
!
!                  'time-units since 2002-01-15 12:00:00'
!
        real(dp) :: time_ref = 0.0_dp                    ! YYYYMMDD.dd
!
      TYPE T_CLOCK
        integer :: yday                ! day of the year
        integer :: year                ! year including century (YYYY)
        integer :: month               ! month of the year (1,...,12)
        integer :: day                 ! day of the month
        integer :: hour                ! hour of the day (1,...,23)
        integer :: minutes             ! minutes of the hour
        real(dp) :: seconds            ! frational seconds of the minute
        real(dp) :: base               ! reference date (YYYYMMDD.dd)
        real(dp) :: DateNumber(2)      ! date number, [1]: days
                                       !              [2]: seconds
        real(dp) :: tide_DateNumber(2) ! tide reference date number,
                                       !   [1]: days  [2]: seconds
        character (len=22) :: string   ! YYYY-MM-DD hh:mm:ss.ss
        character (len=25) :: calendar ! date calendar
      END TYPE T_CLOCK
!
        TYPE (T_CLOCK) :: Rclock       ! reference/base date
!
!-----------------------------------------------------------------------
!  Tracer identification indices.
!-----------------------------------------------------------------------
!
        integer :: itemp              ! Potential temperature
        integer :: isalt              ! Salinity
!
!-----------------------------------------------------------------------
!  Diagnostic fields identification indices.
!-----------------------------------------------------------------------
!
        integer :: iTrate          ! Tracer, time rate of change
        integer :: iTvadv          ! Tracer, vertical advection
        integer :: iThadv          ! Tracer, horizontal advection
        integer :: iTxadv          ! Tracer, horizontal X-advection
        integer :: iTyadv          ! Tracer, horizontal Y-advection
        integer :: iTvdif          ! Tracer, vertical diffusion
        integer :: iThdif          ! Tracer, horizontal diffusion
        integer :: iTxdif          ! Tracer, horizontal X-diffusion
        integer :: iTydif          ! Tracer, horizontal Y-diffusion
        integer :: iTsdif          ! Tracer, horizontal S-diffusion
        integer :: M2fcor          ! 2D momentum, Coriolis
        integer :: M2hadv          ! 2D momentum, horizontal advection
        integer :: M2xadv          ! 2D momentum, horizontal X-advection
        integer :: M2yadv          ! 2D momentum, horizontal Y-advection
        integer :: M2pgrd          ! 2D momentum, pressure gradient
        integer :: M2hvis          ! 2D momentum, horizontal viscosity
        integer :: M2xvis          ! 2D momentum, horizontal X-viscosity
        integer :: M2yvis          ! 2D momentum, horizontal Y-viscosity
        integer :: M2sstr          ! 2D momentum, surface stress
        integer :: M2bstr          ! 2D momentum, bottom stress
        integer :: M2rate          ! 2D momentum, time rate of change
        integer :: M3fcor          ! 3D momentum, Coriolis
        integer :: M3vadv          ! 3D momentum, vertical advection
        integer :: M3hadv          ! 3D momentum, horizontal advection
        integer :: M3xadv          ! 3D momentum, horizontal X-advection
        integer :: M3yadv          ! 3D momentum, horizontal Y-advection
        integer :: M3pgrd          ! 3D momentum, pressure gradient
        integer :: M3vvis          ! 3D momentum, vertical viscosity
        integer :: M3hvis          ! 3D momentum, horizontal viscosity
        integer :: M3xvis          ! 3D momentum, horizontal X-viscosity
        integer :: M3yvis          ! 3D momentum, horizontal Y-viscosity
        integer :: M3rate          ! 3D momentum, time rate of change
!
!-----------------------------------------------------------------------
!  Time stepping indices, variables, and clocks.
!-----------------------------------------------------------------------
!
!    indx1         2D timestep rolling counter.
!    iic           Timestep counter for 3D primitive equations.
!    iif           Timestep counter for 2D primitive equations.
!    ndtfast       Number of barotropic timesteps between each
!                    baroclinic timestep.
!    nfast         Number of barotropic timesteps needed to compute
!                    time-averaged barotropic variables centered at
!                    time level n+1.
!    dt            Size baroclinic timestep (s).
!    dtfast        Size barotropic timestep (s).
!    run_time      Total run time for all nested grids (s), it is
!                    set in Masters/ocean.h
!    MyRunInterval Total run time for all nested grids (s), it is
!                    set in Drivers/nl_ocean.h (coupling window)
!    io_time       Current I/O time (s) processed in "get_state".
!    tdays         Model time clock (days).
!    time          Model time clock (s).
!    time_code     Model time clock (string, YYYY-MM-DD hh:mm:ss.ss)
!    AVGtime       Model time clock for averages output (s).
!    DIAtime       Model time clock for diagnostics output (s).
!    F_code        Final time string for simulation
!    I_code        Initial time string for simulation
!    INItime       Nonlinear model initial conditions time (s).
!    INItimeS      Saved nonlinear model initial conditions time (s).
!    IMPtime       Impulse forcing time (s) to process.
!    ObsTime       Observation time (s) to process.
!    FrcTime       Adjoint or tangent linear Impulse forcing time (s).
!    dstart        Time stamp assigned to model initialization (usually
!                    a Calendar day, like modified Julian Day).
!    tide_start    Reference time for tidal forcing (days).
!
        logical, allocatable :: PerfectRST(:)
        logical, allocatable :: PREDICTOR_2D_STEP(:)
!$OMP THREADPRIVATE (PREDICTOR_2D_STEP)
        integer, allocatable :: indx1(:)
        integer, allocatable :: iic(:)
        integer, allocatable :: iif(:)
        integer, allocatable :: next_kstp(:)
!$OMP THREADPRIVATE (indx1, iic, iif, next_kstp)
        integer, allocatable :: ndtfast(:)
        integer, allocatable :: nfast(:)
        real(dp), allocatable :: tdays(:)                ! days
        real(dp), allocatable :: time(:)                 ! seconds
!$OMP THREADPRIVATE (tdays, time)
        real(dp), allocatable :: dt(:)                   ! seconds
        real(dp), allocatable :: dtfast(:)               ! seconds
        real(dp), allocatable :: TimeEnd(:)              ! seconds
        real(dp), allocatable :: AVGtime(:)              ! seconds
        real(dp), allocatable :: DIAtime(:)              ! seconds
        real(dp), allocatable :: IMPtime(:)              ! seconds
        real(dp), allocatable :: INItime(:)              ! seconds
        real(dp), allocatable :: INItimeS(:)             ! seconds
        real(dp), allocatable :: ObsTime(:)              ! seconds
        real(dp), allocatable :: FrcTime(:)              ! seconds
        real(dp) :: dstart = 0.0_dp                      ! days
        real(dp) :: io_time = 0.0_dp                     ! seconds
        real(dp) :: run_time = 0.0_dp                    ! seconds
        real(dp) :: MyRunInterval = 0.0_dp               ! seconds
        real(dp) :: tide_start = 0.0_dp                  ! days
        character (len=22) :: F_code, I_code
        character (len=22), allocatable :: time_code(:)  ! date string
!$OMP THREADPRIVATE (time_code)
!
!  Power-law shape filter parameters for time-averaging of barotropic
!  Fields.  The power-law shape filters are given by:
!
!     F(xi)=xi^Falpha*(1-xi^Fbeta)-Fgamma*xi
!
!  Possible settings of parameters to yield the second-order accuracy:
!
!     Falpha  Fbeta      Fgamma
!     ------------------------------
!      2.0     1.0    0.1181  0.169     The problem here is setting
!      2.0     2.0    0.1576  0.234     Fgamma. Its value here is
!      2.0     3.0    0.1772  0.266     understood as the MAXIMUM
!      2.0     4.0    0.1892  0.284     allowed. It is computed using
!      2.0     5.0    0.1976  0.296     a Newton iteration scheme.
!      2.0     6.0    0.2039  0.304
!      2.0     8.0    0.2129  0.314
!
!  NOTE: Theoretical values of Fgamma presented in the table above are
!  derived assuming "exact" barotropic mode stepping. Consequently, it
!  does not account for effects caused by Forward-Euler (FE) startup
!  of the barotropic mode at every 3D time step.  As the result, the
!  code may become unstable if the theoretical value of Fgamma is used
!  when mode splitting ratio "ndtfast" is small, thus yielding non-
!  negligible start up effects.  To compensate this, the accepted
!  value of Fgamma is reduced relatively to theoretical one, depending
!  on splitting ratio "ndtfast".  This measure is empirical. It is
!  shown to work with setting of "ndtfast" as low as 15, which is
!  more robust that the Hamming Window the squared cosine weights
!  options in "set_weights".
!
        real(dp) :: Falpha = 2.0_dp
        real(dp) :: Fbeta  = 4.0_dp
        real(dp) :: Fgamma = 0.284_dp
!
!  Total number timesteps in current run. In 3D configurations, "ntimes"
!  is the total of baroclinic timesteps. In 2D configuration, "ntimes"
!  is the total of barotropic timesteps.
!
        integer, allocatable :: ntimes(:)
!
!  Time-step counter for current execution time-window.
!
        integer, allocatable :: step_counter(:)
!$OMP THREADPRIVATE (step_counter)
!
!  Number of time interval divisions for Stochastic Optimals.  It must
!  a multiple of "ntimes".
!
        integer :: Nintervals = 1
!
!  Starting, current, and ending ensemble run parameters.
!
        integer :: ERstr = 1                    ! Starting value
        integer :: ERend = 1                    ! Ending value
        integer :: Ninner = 1                   ! number of inner loops
        integer :: Nouter = 1                   ! number of outer loops
        integer :: Nrun = 1                     ! Current counter
        integer :: OuterLoop = 0                ! split outer loop
        integer :: inner = 0                    ! inner loop counter
        integer :: outer = 0                    ! outer loop counter
!
! Set checksum algorithm: "adler32" or "crc32"
!
        character (len=*), parameter :: HashMethod = "bitsum"
!
! Split 4D-Var phase.
!
        character (len=20) :: Phase4DVAR
!
! Number of sadde point 4D-Var intervals.
!
        integer :: Nsaddle = 1
!
!  First, starting, and ending timestepping parameters
!
        integer, allocatable :: ntfirst(:)      ! Forward-Euler step
        integer, allocatable :: ntstart(:)      ! Start step
        integer, allocatable :: ntend(:)        ! End step
!
!  Adjoint model or tangent linear model impulse forcing time record
!  counter and number of records available.
!
        integer, allocatable :: FrcRec(:)
!$OMP THREADPRIVATE (FrcRec)
        integer, allocatable :: NrecFrc(:)
!
!  HSIMT tracer advection coefficients for the TVD limiter (Wu and Zhu,
!  2010).
!
        real(r8) :: cc1 = 0.25_r8
        real(r8) :: cc2 = 0.5_r8
        real(r8) :: cc3 = 1.0_r8/12.0_r8
!
!-----------------------------------------------------------------------
!  Control switches.
!-----------------------------------------------------------------------
!
!  Switch to use three ghost-points in the halo region.
!
        logical :: ThreeGhostPoints = .FALSE.
!
!  Switch to set-up application grid, metrics, and associated variables
!  and parameters.
!
        logical, allocatable :: SetGridConfig(:)
!
!  Switch to proccess nudging coefficients for radiation open boundary
!  conditions.
!
        logical, allocatable :: NudgingCoeff(:)
!
!  Switch to proccess input boundary data.
!
        logical, allocatable :: ObcData(:)
!
!  These switches are designed to control computational options within
!  nested and/or multiple connected grids.  They are .TRUE. by default.
!  They can turned off for a particular grind in input scripts.
!
        logical, allocatable :: Lbiology(:)
        logical, allocatable :: Lfloats(:)
        logical, allocatable :: Lsediment(:)
        logical, allocatable :: Lstations(:)
!
!  If equilibrium tides, switch to apply the 18.6-year lunar nodal
!  cycle correction.
!
        logical :: Lnodal = .TRUE.
!
!-----------------------------------------------------------------------
!  Physical constants.
!-----------------------------------------------------------------------
!
!    Cp            Specific heat for seawater (Joules/Kg/degC).
!    Csolar        Solar irradiantion constant (W/m2).
!    Eradius       Earth equatorial radius (m).
!    Infinity      Value resulting when dividing by zero.
!    StefBo        Stefan-Boltzmann constant (W/m2/K4).
!    emmiss        Infrared emissivity.
!    g             Acceleration due to gravity (m/s2).
!    gorho0        gravity divided by mean density anomaly.
!    rhow          fresh water density (kg/m3).
!    vonKar        von Karman constant.
!
        real(dp) :: Cp = 3985.0_dp              ! Joules/kg/degC
        real(dp) :: Csolar = 1353.0_dp          ! 1360-1380 W/m2
        real(dp) :: Infinity                    ! Infinity = 1.0/0.0
        real(dp) :: Eradius = 6371315.0_dp      ! m
        real(dp) :: StefBo = 5.67E-8_dp         ! Watts/m2/K4
        real(dp) :: emmiss = 0.97_dp            ! non_dimensional
        real(dp) :: rhow = 1000.0_dp            ! kg/m3
        real(dp) :: g = 9.81_dp                 ! m/s2
        real(dp) :: gorho0                      ! m4/s2/kg
        real(dp) :: vonKar = 0.41_dp            ! non-dimensional
!
!-----------------------------------------------------------------------
!  Various model parameters.  Some of these parameters are overwritten
!  with the values provided from model standard input script.
!-----------------------------------------------------------------------
!
!  Switch for spherical grid (lon,lat) configurations.
!
        logical :: spherical = .FALSE.
!
!  Switch to indicate if ROMS kernel arrays have been allocated.  It
!  used to skip memory reports during early termination.
!
        logical :: LallocatedMemory = .FALSE.
!$OMP THREADPRIVATE (LallocatedMemory)
!
!  Switch to compute the grid stiffness.
!
        logical :: Lstiffness = .TRUE.
!$OMP THREADPRIVATE (Lstiffness)
!
!  Composite grid a refined grids switches. They are .FALSE. by default.
!
        logical, allocatable :: CompositeGrid(:,:)
        logical, allocatable :: RefinedGrid(:)
!
!  Refinement grid scale factor from donor grid.
!
        integer, allocatable :: RefineScale(:)
!
!  Switch to extract donor grid (coarse) data at the refinement grid
!  contact point locations. The coarse data is extracted at the first
!  sub-refined time step.  Recall that the finer grid time-step is
!  smaller than the coarser grid by a factor of RefineScale(:). This
!  switch is only relevant during refinement nesting.
!
        logical, allocatable :: GetDonorData(:)
!
!  Periodic boundary swiches for distributed-memory exchanges.
!
        logical, allocatable :: EWperiodic(:)
        logical, allocatable :: NSperiodic(:)
!
!  Lateral open boundary edges volume conservation switches.
!
        logical, allocatable :: VolCons(:,:)
!
!  Switches to read and process climatology fields.
!
        logical, allocatable :: CLM_FILE(:)          ! Process NetCDF
        logical, allocatable :: Lclimatology(:)      ! any field
        logical, allocatable :: LsshCLM(:)           ! free-surface
        logical, allocatable :: Lm2CLM(:)            ! 2D momentum
        logical, allocatable :: Lm3CLM(:)            ! 3D momentum
        logical, allocatable :: LtracerCLM(:,:)      ! tracers
!
!  Switched to nudge to climatology fields.
!
        logical, allocatable :: Lnudging(:)          ! any field
        logical, allocatable :: LnudgeM2CLM(:)       ! 2D momentum
        logical, allocatable :: LnudgeM3CLM(:)       ! 3D momentum
        logical, allocatable :: LnudgeTCLM(:,:)      ! tracers
!
!  Switches to activate point Source/Sinks in an application:
!    * Horizontal momentum transport (u or v)
!    * Vertical mass transport (w)
!    * Tracer transport
!
        logical, allocatable :: LuvSrc(:)            ! momentum
        logical, allocatable :: LwSrc(:)             ! mass
        logical, allocatable :: LtracerSrc(:,:)      ! tracers
!
!  Execution termination flag.
!
!    exit_flag = 0   No error
!    exit_flag = 1   Blows up
!    exit_flag = 2   Input error
!    exit_flag = 3   Output error
!    exit_flag = 4   IO error
!    exit_flag = 5   Configuration error
!    exit_flag = 6   Partition error
!    exit_flag = 7   Illegal input parameter
!    exit_flag = 8   Fatal algorithm result
!    exit_flag = 9   coupling error
!
        integer :: exit_flag = 0
        integer :: blowup = 0
        integer :: NoError = 0
!
!  Blow-up string.
!
        character (len=80) :: blowup_string
!
!  Set threshold maximum speed (m/s) and density anomaly (kg/m3) to
!  test if the model is blowing-up.
!
        real(dp), allocatable :: maxspeed(:)
        real(dp), allocatable :: maxrho(:)
!
        real(dp) :: max_speed = 20.0_dp         ! m/s
        real(dp) :: max_rho = 200.0_dp          ! kg/m3
!
!  Interpolation scheme.
!
        integer, parameter :: linear = 0        ! linear interpolation
        integer, parameter :: cubic  = 1        ! cubic  interpolation
!
        integer :: InterpFlag = linear          ! interpolation flag
!
!  Shallowest and Deepest levels to apply bottom momentum stresses as
!  a bodyforce
!
        integer, allocatable :: levsfrc(:)
        integer, allocatable :: levbfrc(:)
!
!  Vertical coordinates transform.  Currently, there are two vertical
!  transformation equations (see set_scoord.F for details):
!
!    Original transform (Vtransform=1):
!
!         z_r(x,y,s,t) = Zo_r + zeta(x,y,t) * [1.0 + Zo_r / h(x,y)]
!
!                 Zo_r = hc * [s(k) - C(k)] + C(k) * h(x,y)
!
!    New transform (Vtransform=2):
!
!         z_r(x,y,s,t) = zeta(x,y,t) + [zeta(x,y,t)+ h(x,y)] * Zo_r
!
!                 Zo_r = [hc * s(k) + C(k) * h(x,y)] / [hc + h(x,y)]
!
        integer, allocatable :: Vtransform(:)
!
!  Vertical grid stretching function flag:
!
!    Vstretcing = 1   Original function (Song and Haidvogel, 1994)
!               = 2   A. Shchepetkin (ROMS-UCLA) function
!               = 3   R. Geyer BBL function
!
        integer, allocatable :: Vstretching(:)
!
!  Vertical grid stretching parameters.
!
!    Tcline        Width (m) of surface or bottom boundary layer in
!                    which higher vertical resolution is required
!                    during stretching.
!    hc            S-coordinate critical depth, hc=MIN(hmin,Tcline).
!    theta_s       S-coordinate surface control parameter.
!    theta_b       S-coordinate bottom control parameter.
!
        real(dp), allocatable :: Tcline(:)      ! m, positive
        real(dp), allocatable :: hc(:)          ! m, positive
        real(dp), allocatable :: theta_s(:)     ! 0 < theta_s < 20
        real(dp), allocatable :: theta_b(:)     ! 0 < theta_b < 1
!
!  Bathymetry range values.
!
        real(dp), allocatable :: hmin(:)        ! m, positive
        real(dp), allocatable :: hmax(:)        ! m, positive
!
!  Length (m) of domain box in the XI- and ETA-directions.
!
        real(r8), allocatable :: xl(:)          ! m
        real(r8), allocatable :: el(:)          ! m
!
!  Minimum and Maximum longitude and latitude at RHO-points
!
        real(r8), allocatable :: LonMin(:)      ! degrees east
        real(r8), allocatable :: LonMax(:)      ! degrees east
        real(r8), allocatable :: LatMin(:)      ! degrees north
        real(r8), allocatable :: LatMax(:)      ! degrees north
!
!  Constant used in the Shchepetkin boundary conditions for 2D momentum,
!  Co = 1.0_r8/(2.0_r8+SQRT(2.0_r8)).
!
        real(r8) :: Co
!
!  Number of digits in grid size for format statements.
!
        integer, allocatable :: Idigits(:)
        integer, allocatable :: Jdigits(:)
        integer, allocatable :: Kdigits(:)
!
!  Diagnostic volume averaged variables.
!
        integer, allocatable :: first_time(:)
        real(dp) :: avgke = 0.0_dp              ! Kinetic energy
        real(dp) :: avgpe = 0.0_dp              ! Potential energy
        real(dp) :: avgkp = 0.0_dp              ! Total energy
        real(dp) :: volume = 0.0_dp             ! diagnostics volume
        real(dp) :: ad_volume = 0.0_dp          ! adjoint volume
        real(dp), allocatable :: TotVolume(:)   ! Total volume
        real(dp), allocatable :: MinVolume(:)   ! Minimum cell volume
        real(dp), allocatable :: MaxVolume(:)   ! Maximum cell volume
!
!  Minimun and maximum grid spacing
!
        real(dp), allocatable :: DXmin(:)       ! all grid points
        real(dp), allocatable :: DXmax(:)
        real(dp), allocatable :: DYmin(:)
        real(dp), allocatable :: DYmax(:)
        real(dp), allocatable :: DZmin(:)       ! all grid points
        real(dp), allocatable :: DZmax(:)
!
!  Maximum size of a grid node (m) over the whole curvilinear grid
!  application. Used for scaling horizontal mixing by the grid size.
!
        real(dp), allocatable :: grdmax(:)
!
!  Courant Numbers due to gravity wave speed limits.
!
        real(dp), allocatable :: Cg_min(:)      ! Minimun barotropic
        real(dp), allocatable :: Cg_max(:)      ! Maximun barotropic
        real(dp), allocatable :: Cg_Cor(:)      ! Maximun Coriolis
!
!  Time dependent Counrant Numbers due to velocity components and
!  indices location of maximum value.
!
        integer :: max_Ci = 0                   ! maximum I-location
        integer :: max_Cj = 0                   ! maximum J-location
        integer :: max_Ck = 0                   ! maximum K-location
        real(r8) :: max_C = 0.0_r8              ! maximum total
        real(r8) :: max_Cu = 0.0_r8             ! maximum I-component
        real(r8) :: max_Cv = 0.0_r8             ! maximum J-component
        real(r8) :: max_Cw = 0.0_r8             ! maximum K-component
!
!  Linear equation of state parameters.
!
!    R0            Background constant density anomaly (kg/m3).
!    Tcoef         Thermal expansion coefficient (1/Celsius).
!    Scoef         Saline contraction coefficient (1/PSU).
!
        real(r8), allocatable :: R0(:)
        real(r8), allocatable :: Tcoef(:)
        real(r8), allocatable :: Scoef(:)
!
!  Background potential temperature (Celsius) and salinity (PSU) values
!  used in analytical initializations.
!
        real(r8), allocatable :: T0(:)
        real(r8), allocatable :: S0(:)
!
!  Slipperiness variable, either 1.0 (free slip) or -1.0 (no slip).
!
        real(r8), allocatable :: gamma2(:)
!
!  Weighting coefficient for the newest (implicit) time step derivatives
!  using either a Crack-Nicolson implicit scheme (lambda=0.5) or a
!  backward implicit scheme (lambda=1.0).
!
        real(r8) :: lambda = 1.0_r8
!
!  Jerlov water type to assign everywhere, range values: 1 - 5.
!
        integer, allocatable :: lmd_Jwt(:)
!
!  Grid r-factor (non-dimensional).
!
        real(dp), allocatable :: rx0(:)         ! Beckmann and Haidvogel
        real(dp), allocatable :: rx1(:)         ! Haney
!
!  Linear (m/s) and quadratic (nondimensional) bottom drag coefficients.
!
        real(r8), allocatable :: rdrg(:)
        real(r8), allocatable :: rdrg2(:)
!
!  Minimum and maximum threshold for transfer coefficient of momentum.
!
        real(dp) :: Cdb_min = 0.000001_dp
        real(dp) :: Cdb_max = 0.5_dp
!
!  Surface and bottom roughness (m)
!
        real(r8), allocatable :: Zos(:)
        real(r8), allocatable :: Zob(:)
!
!  Minimum depth for wetting and drying (m).
!
        real(r8), allocatable :: Dcrit(:)
!
!  Mean density (Kg/m3) used when the Boussinesq approximation is
!  inferred.
!
        real(dp) :: rho0 = 1025.0_dp
!
!  Background Brunt-Vaisala frequency (1/s2).
!
        real(dp) :: bvf_bak = 0.00001_dp
!
!  Vector containing USER generic parameters.
!
        integer :: Nuser
        real(r8), allocatable :: user(:)
!
!  Weights for the time average of 2D fields.
!
        real(dp), allocatable :: weight(:,:,:)
!
!  Constants.
!
        real(dp), parameter :: pi = 3.14159265358979323846_dp
        real(dp), parameter :: deg2rad = pi / 180.0_dp
        real(dp), parameter :: rad2deg = 180.0_dp / pi
        real(dp), parameter :: day2sec = 86400.0_dp
        real(dp), parameter :: sec2day = 1.0_dp / 86400.0_dp
        real(dp), parameter :: spval = 1.0E+37_dp
        real(dp), parameter :: Large = 1.0E+20_dp
        real(dp), parameter :: jul_off = 2440000.0_dp
!
!  Set special check value.  Notice that a smaller value is assigned
!  to account for both NetCDF fill value and roundoff. There are
!  many Matlab scripts out there that do not inquire correctly
!  the spval from the _FillValue attribute in single/double
!  precision.
!
        real(dp), parameter :: spval_check = 1.0E+35_dp
!
!-----------------------------------------------------------------------
!  Horizontal and vertical constant mixing coefficients.
!-----------------------------------------------------------------------
!
!    Akk_bak       Background vertical mixing coefficient (m2/s) for
!                    turbulent energy.
!    Akp_bak       Background vertical mixing coefficient (m2/s) for
!                    generic statistical field "psi".
!    Akt_bak       Background vertical mixing coefficient (m2/s) for
!                    tracers.
!    Akv_bak       Background vertical mixing coefficient (m2/s) for
!                    momentum.
!    Akt_limit     Upper threshold vertical mixing coefficient (m2/s)
!                    for tracers.
!    Akv_limit     Upper threshold vertical mixing coefficient (m2/s)
!                    for momentum.
!    Kdiff         Isopycnal mixing thickness diffusivity (m2/s) for
!                    tracers.
!    ad_visc2      ADM lateral harmonic constant mixing coefficient
!                    (m2/s) for momentum.
!    nl_visc2      NLM lateral harmonic constant mixing coefficient
!                    (m2/s) for momentum.
!    tl_visc2      TLM lateral harmonic constant mixing coefficient
!                    (m2/s) for momentum.
!    visc2         Current lateral harmonic constant mixing coefficient
!                    (m2/s) for momentum.
!    ad_visc4      ADM lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for momentum.
!    nl_visc4      NLM lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for momentum.
!    tl_visc4      TLM lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for momentum.
!    visc4         Current lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for momentum.
!    ad_tnu2       ADM lateral harmonic constant mixing coefficient
!                    (m2/s) for tracer type variables.
!    nl_tnu2       NLM lateral harmonic constant mixing coefficient
!                    (m2/s) for tracer type variables.
!    tl_tnu2       TLM lateral harmonic constant mixing coefficient
!                    (m2/s) for tracer type variables.
!    tnu2          Current lateral harmonic constant mixing coefficient
!                    (m2/s) for tracer type variables.
!    ad_tnu4       ADM lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for tracers.
!    nl_tnu4       NLM lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for tracers.
!    tl_tnu4       TLM lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for tracers.
!    tnu4          Current lateral biharmonic (squared root) constant
!                     mixing coefficient (m2 s^-1/2) for tracers.
!    tkenu2        Lateral harmonic constant mixing coefficient
!                    (m2/s) for turbulent energy.
!    tkenu4        Lateral biharmonic (squared root) constant mixing
!                    coefficient (m2 s^-1/2) for turbulent energy.
!
        real(r8), allocatable :: Akk_bak(:)          ! m2/s
        real(r8), allocatable :: Akp_bak(:)          ! m2/s
        real(r8), allocatable :: Akv_bak(:)          ! m2/s
        real(r8), allocatable :: Akv_limit(:)        ! m2/s
        real(r8), allocatable :: ad_visc2(:)         ! m2/s
        real(r8), allocatable :: nl_visc2(:)         ! m2/s
        real(r8), allocatable :: tl_visc2(:)         ! m2/s
        real(r8), allocatable :: visc2(:)            ! m2/s
        real(r8), allocatable :: ad_visc4(:)         ! m2 s-1/2
        real(r8), allocatable :: nl_visc4(:)         ! m2 s-1/2
        real(r8), allocatable :: tl_visc4(:)         ! m2 s-1/2
        real(r8), allocatable :: visc4(:)            ! m2 s-1/2
        real(r8), allocatable :: tkenu2(:)           ! m2/s
        real(r8), allocatable :: tkenu4(:)           ! m2 s-1/2
        real(r8), allocatable :: Akt_bak(:,:)        ! m2/s
        real(r8), allocatable :: Akt_limit(:,:)      ! m2/s
        real(r8), allocatable :: Kdiff(:,:)          ! m2/s
        real(r8), allocatable :: ad_tnu2(:,:)        ! m2/s
        real(r8), allocatable :: nl_tnu2(:,:)        ! m2/s
        real(r8), allocatable :: tl_tnu2(:,:)        ! m2/s
        real(r8), allocatable :: tnu2(:,:)           ! m2/s
        real(r8), allocatable :: ad_tnu4(:,:)        ! m2 s-1/2
        real(r8), allocatable :: nl_tnu4(:,:)        ! m2 s-1/2
        real(r8), allocatable :: tl_tnu4(:,:)        ! m2 s-1/2
        real(r8), allocatable :: tnu4(:,:)           ! m2 s-1/2
!
!  Horizontal diffusive relaxation coefficients (m2/s) used to smooth
!  representer tangent linear solution during Picard iterations to
!  improve stability and convergence.
!
        real(r8), allocatable :: tl_M2diff(:)        ! 2D momentum
        real(r8), allocatable :: tl_M3diff(:)        ! 3D momentum
        real(r8), allocatable :: tl_Tdiff(:,:)       ! tracers
!
!  Basic state vertical mixing coefficient scale factors for adjoint
!  based algorithms. In some applications, a smaller/larger values of
!  vertical mixing are necessary for stability.
!
        real(r8), allocatable :: ad_Akv_fac(:)       ! ADM momentum
        real(r8), allocatable :: tl_Akv_fac(:)       ! TLM momentum
        real(r8), allocatable :: ad_Akt_fac(:,:)     ! ADM tracers
        real(r8), allocatable :: tl_Akt_fac(:,:)     ! TLM tracers
!
!  Switches to increase/decrease horizontal viscosity and/or diffusion
!  in specific areas of the application domain (like sponge areas).
!
        logical, allocatable :: Lsponge(:)
        logical, allocatable :: LuvSponge(:)         ! viscosity
        logical, allocatable :: LtracerSponge(:,:)   ! diffusion
!
!-----------------------------------------------------------------------
!  IO parameters.
!-----------------------------------------------------------------------
!
!  Switches to activate creation and writing of output NetCDF files.
!
        logical, allocatable :: LdefADJ(:)       ! Adjoint file
        logical, allocatable :: LdefAVG(:)       ! Average file
        logical, allocatable :: LdefDAI(:)       ! DA initial/restart
        logical, allocatable :: LdefDIA(:)       ! Diagnostics file
        logical, allocatable :: LdefERR(:)       ! 4DVar error file
        logical, allocatable :: LdefFLT(:)       ! Floats file
        logical, allocatable :: LdefHIS(:)       ! History file
        logical, allocatable :: LdefHSS(:)       ! Hessian file
        logical, allocatable :: LdefINI(:)       ! Initial file
        logical, allocatable :: LdefIRP(:)       ! Initial RPM file
        logical, allocatable :: LdefITL(:)       ! Initial TLM file
        logical, allocatable :: LdefLCZ(:)       ! Lanczos Vectors file
        logical, allocatable :: LdefLZE(:)       ! Evolved Lanczos file
        logical, allocatable :: LdefMOD(:)       ! 4DVAR file
        logical, allocatable :: LdefQCK(:)       ! Quicksave file
        logical, allocatable :: LdefRST(:)       ! Restart file
        logical, allocatable :: LdefSTA(:)       ! Stations file
        logical, allocatable :: LdefTIDE(:)      ! tide forcing file
        logical, allocatable :: LdefTLM(:)       ! Tangent linear file
        logical, allocatable :: LdefTLF(:)       ! TLM/RPM impulse file
        logical, allocatable :: LdefXTR(:)       ! TLM/RPM impulse file
        logical, allocatable :: LreadADM(:)      ! Read ADM multi-files
        logical, allocatable :: LreadBLK(:)      ! Read NLM bulk fluxes
        logical, allocatable :: LreadFRC(:)      ! Read FRC files
        logical, allocatable :: LreadFWD(:)      ! Read FWD trajectory
        logical, allocatable :: LreadQCK(:)      ! Read QCK trajectory
        logical, allocatable :: LreadSTD(:)      ! Read STD file
        logical, allocatable :: LreadTLM(:)      ! Read TLM multi-files
        logical, allocatable :: LwrtADJ(:)       ! Write adjoint file
        logical, allocatable :: LwrtAVG(:)       ! Write average file
        logical, allocatable :: LwrtDIA(:)       ! Write diagnostic file
        logical, allocatable :: LwrtHIS(:)       ! Write history file
        logical, allocatable :: LwrtPER(:)       ! Write during ensemble
        logical, allocatable :: LwrtQCK(:)       ! write quicksave file
        logical, allocatable :: LwrtRST(:)       ! Write restart file
        logical, allocatable :: LwrtTLF(:)       ! Write impulse file
        logical, allocatable :: LwrtTLM(:)       ! Write tangent file
        logical, allocatable :: LwrtXTR(:)       ! Write extraction file
        logical, allocatable :: LdefNRM(:,:)     ! Norm file
        logical, allocatable :: LwrtNRM(:,:)     ! Write norm file
!
!  Switch to write out adjoint 2D state arrays instead of IO solution
!  arrays and adjoint ocean time. This is used in 4DVAR for IO
!  maniputations.
!
        logical, allocatable :: LwrtState2d(:)
        logical, allocatable :: LwrtTime(:)
!
!  Switch to write 4D-Var cost functions to DAV NetCDF.
!
        logical, allocatable :: LwrtCost(:)
!
!  Switch to write out adjoint surface forcing fields adjusted by the
!  4DVAR algorithms.
!
        logical, allocatable :: Ladjusted(:)
!
!  Switch to append information to an existing ROMS standard output
!  log file.
!
        logical :: Lappend = .FALSE.
!
!  Switch to read input open boundary conditions data.
!
        logical, allocatable :: LprocessOBC(:)
!
!  Switch to read input tidal forcing data.
!
        logical, allocatable :: LprocessTides(:)
!
!  Switch to write application set-up information to standard output.
!
        logical, allocatable :: LwrtInfo(:)
!
!  Switch used to create new output NetCDF files. If TRUE, new output
!  files are created. If FALSE, data is appended to an existing output
!  files.  Used only for history, average and station files.
!
        logical, allocatable :: ldefout(:)       ! New output files
!
!  Number of timesteps between creation of new output files.
!
        integer, allocatable :: ndefADJ(:)       ! Adjoint file
        integer, allocatable :: ndefAVG(:)       ! Average file
        integer, allocatable :: ndefDIA(:)       ! Diagnostics file
        integer, allocatable :: ndefHIS(:)       ! History file
        integer, allocatable :: ndefQCK(:)       ! Quicksave file
        integer, allocatable :: ndefTLM(:)       ! Tangent linear file
        integer, allocatable :: ndefXTR(:)       ! extraction file
!
!  Starting timestep for accumulation of output.
!
        integer, allocatable :: ntsAVG(:)        ! Average file
        integer, allocatable :: ntsDIA(:)        ! Diagnostics file
!
!  Number of timesteps between writing of output data.
!
        integer, allocatable :: nADJ(:)          ! Adjoint file
        integer, allocatable :: nAVG(:)          ! Average file
        integer, allocatable :: nDIA(:)          ! Diagnostics file
        integer, allocatable :: nFLT(:)          ! Floats file
        integer, allocatable :: nHIS(:)          ! History file
        integer, allocatable :: nQCK(:)          ! Quicksave file
        integer, allocatable :: nRST(:)          ! Restart file
        integer, allocatable :: nSTA(:)          ! Stations file
        integer, allocatable :: nTLM(:)          ! Tangent linear file
        integer, allocatable :: nXTR(:)          ! extraction file
!
!  Field extraction flag to interpolate or decimate solution to the
!  provided grid geometry.
!  (For 4D-Var coarser inner loops use ExtractFlag = 2)
!
!    ExtractFlag = 0    no extraction
!    ExtractFlag = 1    extraction by interpolation
!    ExtractFlag > 1    extraction by decimation
!
        integer, allocatable :: ExtractFlag(:)
!
!  Number of timesteps between print of single line information to
!  standard output.
!
        integer, allocatable :: ninfo(:)
!
!  Number of timesteps between 4DVAR adjustment of open boundaries.
!  In strong constraint 4DVAR, it is possible to open bounadies at
!  other intervals in addition to initial time. These parameters are
!  used to store the appropriate number of open boundary records in
!  output history NetCDF files.
!
!    Nbrec(:) = 1 + ntimes(:) / nOBC(:)
!
!  Here, it is assumed that nOBC is a multiple of NTIMES or greater
!  than NTIMES. If nOBC > NTIMES, only one record is stored in the
!  output history NetCDF files and the adjustment is for constant
!  open boundaries with constant correction.
!
        integer, allocatable :: nOBC(:)          ! number of timesteps
        integer, allocatable :: Nbrec(:)         ! number of records
        integer, allocatable :: OBCcount(:)      ! record counter
!
!  Number of timesteps between adjustment of 4DVAR surface forcing
!  fields. In strong constraint 4DVAR, it is possible to adjust surface
!  forcing fields at other intervals in addition to initial time.
!  These parameters are used to store the appropriate number of
!  surface forcing records in output history NetCDF files.
!
!    Nfrec(:) = 1 + ntimes(:) / nSFF(:)
!
!  Here, it is assumed that nSFF is a multiple of NTIMES or greater
!  than NTIMES. If nSFF > NTIMES, only one record is stored in the
!  output history NetCDF files and the adjustment is for constant
!  forcing with constant correction.
!
        integer, allocatable :: nSFF(:)          ! number of timesteps
        integer, allocatable :: Nfrec(:)         ! number of records
        integer, allocatable :: SFcount(:)       ! record counter
!
!  Restart time record to read from disk and use as the initial
!  conditions. Use nrrec=0 for new solutions. If nrrec is negative
!  (say, nrrec=-1), the model will restart from the most recent
!  time record. That is, the initialization record is assigned
!  internally.
!
        integer, allocatable :: nrrec(:)
!
!  Switch to activate processing of input data.  This switch becomes
!  very useful when reading input data serially in parallel
!  applications.
!
        logical, allocatable :: synchro_flag(:)
!$OMP THREADPRIVATE (synchro_flag)
!
!  Switch to inialize model with latest time record from initial
!  (restart/history) NetCDF file.
!
        logical, allocatable :: LastRec(:)
!
!  Generalized Statbility Theory (GST) parameters.
!
        logical :: LmultiGST          ! multiple eigenvector file switch
        logical :: LrstGST            ! restart switch
        integer :: MaxIterGST         ! Number of iterations
        integer :: nGST               ! check pointing interval
!
!  Switches used to recycle time records in some output file. If TRUE,
!  only the latest two time records are maintained.  If FALSE, all
!  field records are saved.
!
        logical, allocatable :: LcycleADJ(:)
        logical, allocatable :: LcycleRST(:)
        logical, allocatable :: LcycleTLM(:)
!
!-----------------------------------------------------------------------
!  Adjoint sensitivity parameters.
!-----------------------------------------------------------------------
!
!  Starting and ending vertical levels of the 3D adjoint state whose
!  sensitivity is required.
!
        integer, allocatable :: KstrS(:)           ! starting level
        integer, allocatable :: KendS(:)           ! ending level
!
!  Starting and ending day for adjoint sensitivity forcing.
!
        real(r8), allocatable :: DstrS(:)          ! starting day
        real(r8), allocatable :: DendS(:)          ! ending day
!
!-----------------------------------------------------------------------
!  Stochastic optimals parameters.
!-----------------------------------------------------------------------
!
!  Stochastic optimals forcing records counter.
!
        integer, allocatable :: SOrec(:)
!$OMP THREADPRIVATE (SOrec)
!
!  Trace of stochastic optimals matrix.
!
        real(r8), allocatable :: TRnorm(:)
!
!  Stochastic optimals time decorrelation scale (days) assumed for
!  red noise processes.
!
        real(r8), allocatable :: SO_decay(:)
!
!  Stochastic optimals surface forcing standard deviation for
!  dimensionalization.
!
        real(r8), allocatable :: SO_sdev(:,:)
!
!-----------------------------------------------------------------------
!  Nudging variables for passive (outflow) and active (inflow) oepn
!  boundary conditions.
!-----------------------------------------------------------------------
!
!    iwest         West  identification index in boundary arrays.
!    isouth        South identification index in boundary arrays.
!    ieast         East  identification index in boundary arrays.
!    inorth        North identification index in boundary arrays.
!    obcfac        Factor between passive and active open boundary
!                    conditions (nondimensional and greater than one).
!                    The nudging time scales for the active conditions
!                    are obtained by multiplying the passive values by
!                    factor.
!    FSobc_in      Active and strong time-scale (1/sec) coefficients
!                    for nudging towards free-surface data at  inflow.
!    FSobc_out     Passive and weak  time-scale (1/sec) coefficients
!                    for nudging towards free-surface data at outflow.
!    M2obc_in      Active and strong time-scale (1/sec) coefficients
!                    for nudging towards 2D momentum data at  inflow.
!    M2obc_out     Passive and weak  time-scale (1/sec) coefficients
!                    for nudging towards 2D momentum data at outflow.
!    M3obc_in      Active and strong time-scale (1/sec) coefficients
!                    for nudging towards 3D momentum data at  inflow.
!    M3obc_out     Passive and weak  time-scale (1/sec) coefficients
!                    for nudging towards 3D momentum data at outflow.
!    Tobc_in       Active and strong time-scale (1/sec) coefficients
!                    for nudging towards tracer data at  inflow.
!    Tobc_out      Passive and weak  time-scale (1/sec) coefficients
!                    for nudging towards tracer data at outflow.
!
        integer, parameter :: iwest = 1
        integer, parameter :: isouth = 2
        integer, parameter :: ieast = 3
        integer, parameter :: inorth = 4
        real(dp), allocatable :: obcfac(:)
        real(dp), allocatable :: FSobc_in(:,:)
        real(dp), allocatable :: FSobc_out(:,:)
        real(dp), allocatable :: M2obc_in(:,:)
        real(dp), allocatable :: M2obc_out(:,:)
        real(dp), allocatable :: M3obc_in(:,:)
        real(dp), allocatable :: M3obc_out(:,:)
        real(dp), allocatable :: Tobc_in(:,:,:)
        real(dp), allocatable :: Tobc_out(:,:,:)
!
!  Inverse time-scales (1/s) for nudging at open boundaries and sponge
!  areas.
!
        real(dp), allocatable :: Znudg(:)          ! Free-surface
        real(dp), allocatable :: M2nudg(:)         ! 2D momentum
        real(dp), allocatable :: M3nudg(:)         ! 3D momentum
        real(dp), allocatable :: Tnudg(:,:)        ! Tracers
!
!  Variables used to impose mass flux conservation in open boundary
!  configurations.
!
        real(dp) :: bc_area = 0.0_dp
        real(dp) :: bc_flux = 0.0_dp
        real(dp) :: ubar_xs = 0.0_dp
!
!-----------------------------------------------------------------------
!  Generic Length Scale parameters.
!-----------------------------------------------------------------------
!
!    gls_Gh0
!    gls_Ghcri
!    gls_Ghmin
!    gls_Kmin      Minimum value of specific turbulent kinetic energy.
!    gls_Pmin      Minimum Value of dissipation.
!    gls_cmu0      Stability coefficient (non-dimensional).
!    gls_c1        Shear production coefficient (non-dimensional).
!    gls_c2        Dissipation coefficient (non-dimensional).
!    gls_c3m       Buoyancy production coefficient (minus).
!    gls_c3p       Buoyancy production coefficient (plus).
!    gls_E2
!    gls_m         Turbulent kinetic energy exponent (non-dimensional).
!    gls_n         Turbulent length scale exponent (non-dimensional).
!    gls_p         Stability exponent (non-dimensional).
!    gls_sigk      Constant Schmidt number (non-dimensional) for
!                    turbulent kinetic energy diffusivity.
!    gls_sigp      Constant Schmidt number (non-dimensional) for
!                    turbulent generic statistical field, "psi".
!
        real(r8), allocatable :: gls_m(:)
        real(r8), allocatable :: gls_n(:)
        real(r8), allocatable :: gls_p(:)
        real(r8), allocatable :: gls_sigk(:)
        real(r8), allocatable :: gls_sigp(:)
        real(r8), allocatable :: gls_cmu0(:)
        real(r8), allocatable :: gls_cmupr(:)
        real(r8), allocatable :: gls_c1(:)
        real(r8), allocatable :: gls_c2(:)
        real(r8), allocatable :: gls_c3m(:)
        real(r8), allocatable :: gls_c3p(:)
        real(r8), allocatable :: gls_Kmin(:)
        real(r8), allocatable :: gls_Pmin(:)
!
! Constants used in the various formulation of surface flux boundary
! conditions for the GLS vertical turbulence closure in terms of
! Charnock surface roughness (CHARNOK_ALPHA), roughness from wave
! amplitude (zos_hsig_alpha), wave dissipation (SZ_ALPHA), and
! Craig and Banner wave breaking (CRGBAN_CW).
!
        real(r8), allocatable :: charnok_alpha(:)
        real(r8), allocatable :: zos_hsig_alpha(:)
        real(r8), allocatable :: sz_alpha(:)
        real(r8), allocatable :: crgban_cw(:)
!
! Waves Effect on Currents dissipation energy partion scale:
!
!   [0.0] All wave dissipation goes to breaking and none to roller
!   [1.0] All wave dissipation goes to roller and none to breaking
!
        real(r8), allocatable :: wec_alpha(:)
!
!-----------------------------------------------------------------------
!  Tangent linear and adjoint model parameters.
!-----------------------------------------------------------------------
!
!  Tangent linear and adjoint model control switches.
!
        logical :: TLmodel = .FALSE.
        logical :: ADmodel = .FALSE.
!
      CONTAINS
!
      SUBROUTINE allocate_scalars
!
!=======================================================================
!                                                                      !
!  This routine allocates structure and  several variables in module   !
!  that depend on the number of nested grids.                          !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Local variable declarations.
!
      integer :: ng
      real(r8), parameter :: IniVal = 0.0_r8
!
!-----------------------------------------------------------------------
!  Allocate and initialize variables in module structure.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(SCALARS)) THEN
        allocate ( SCALARS(Ngrids) )
        DO ng=1,Ngrids
          allocate ( SCALARS(ng) % Fstate(9+2*MT) )
          Dmem(ng)=Dmem(ng)+REAL(9+2*MT,r8)
          SCALARS(ng) % Fstate = .FALSE.
          allocate ( SCALARS(ng) % Lstate(8+2*MT) )
          Dmem(ng)=Dmem(ng)+REAL(9+2*MT,r8)
          SCALARS(ng) % Lstate = .FALSE.
          allocate ( SCALARS(ng) % Cs_r(N(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)
          SCALARS(ng) % Cs_r = IniVal
          allocate ( SCALARS(ng) % Cs_w(0:N(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(N(ng)+1,r8)
          SCALARS(ng) % Cs_w = IniVal
          allocate ( SCALARS(ng) % sc_r(N(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(N(ng),r8)
          SCALARS(ng) % sc_r = IniVal
          allocate ( SCALARS(ng) % sc_w(0:N(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(N(ng)+1,r8)
          SCALARS(ng) % sc_w = IniVal
        END DO
      END IF
!
!  Allocate variables that require special treatment in shared-memory.
!  These variables are private for each thread to avoid collisions.
!
!$OMP PARALLEL
      IF (.not.allocated(PREDICTOR_2D_STEP)) THEN
        allocate ( PREDICTOR_2D_STEP(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(indx1)) THEN
        allocate ( indx1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iic)) THEN
        allocate ( iic(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iif)) THEN
        allocate ( iif(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(next_kstp)) THEN
        allocate ( next_kstp(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(FrcRec)) THEN
        allocate ( FrcRec(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SOrec)) THEN
        allocate ( SOrec(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
!$OMP END PARALLEL
!
!-----------------------------------------------------------------------
!  Allocate variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(PerfectRST)) THEN
        allocate ( PerfectRST(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndtfast)) THEN
        allocate ( ndtfast(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nfast)) THEN
        allocate ( nfast(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(dt)) THEN
        allocate ( dt(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(dtfast)) THEN
        allocate ( dtfast(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TimeEnd)) THEN
        allocate ( TimeEnd(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(AVGtime)) THEN
        allocate ( AVGtime(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DIAtime)) THEN
        allocate ( DIAtime(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(IMPtime)) THEN
        allocate ( IMPtime(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(INItime)) THEN
        allocate ( INItime(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(INItimeS)) THEN
        allocate ( INItimeS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ObsTime)) THEN
        allocate ( ObsTime(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(FrcTime)) THEN
        allocate ( FrcTime(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ntimes)) THEN
        allocate ( ntimes(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(first_time)) THEN
        allocate ( first_time(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ntfirst)) THEN
        allocate ( ntfirst(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ntstart)) THEN
        allocate ( ntstart(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ntend)) THEN
        allocate ( ntend(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
!$OMP PARALLEL
      IF (.not.allocated(synchro_flag)) THEN
        allocate ( synchro_flag(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(step_counter)) THEN
        allocate ( step_counter(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tdays)) THEN
        allocate ( tdays(Ngrids) )
        tdays=0.0_dp
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(time)) THEN
        allocate ( time(Ngrids) )
        time=0.0_dp
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(time_code)) THEN
        allocate ( time_code(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
!$OMP END PARALLEL
      IF (.not.allocated(NrecFrc)) THEN
        allocate ( NrecFrc(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SetGridConfig)) THEN
        allocate ( SetGridConfig(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(NudgingCoeff)) THEN
        allocate ( NudgingCoeff(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ObcData)) THEN
        allocate ( ObcData(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lbiology)) THEN
        allocate ( Lbiology(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lfloats)) THEN
        allocate ( Lfloats(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lsediment)) THEN
        allocate ( Lsediment(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lstations)) THEN
        allocate ( Lstations(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(CompositeGrid)) THEN
        allocate ( CompositeGrid(4,Ngrids) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(RefinedGrid)) THEN
        allocate ( RefinedGrid(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(RefineScale)) THEN
        allocate ( RefineScale(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(GetDonorData)) THEN
        allocate ( GetDonorData(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(EWperiodic)) THEN
        allocate ( EWperiodic(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(NSperiodic)) THEN
        allocate ( NSperiodic(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(VolCons)) THEN
        allocate ( VolCons(4,Ngrids) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lsponge)) THEN
        allocate ( Lsponge(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LuvSponge)) THEN
        allocate ( LuvSponge(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LtracerSponge)) THEN
        allocate ( LtracerSponge(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(CLM_FILE)) THEN
        allocate ( CLM_FILE(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lclimatology)) THEN
        allocate ( Lclimatology(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LsshCLM)) THEN
        allocate ( LsshCLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lm2CLM)) THEN
        allocate ( Lm2CLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Lm3CLM)) THEN
        allocate ( Lm3CLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LtracerCLM)) THEN
        allocate ( LtracerCLM(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(Lnudging)) THEN
        allocate ( Lnudging(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LnudgeM2CLM)) THEN
        allocate ( LnudgeM2CLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LnudgeM3CLM)) THEN
        allocate ( LnudgeM3CLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LnudgeTCLM)) THEN
        allocate ( LnudgeTCLM(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(LuvSrc)) THEN
        allocate ( LuvSrc(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwSrc)) THEN
        allocate ( LwSrc(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LtracerSrc)) THEN
        allocate ( LtracerSrc(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(maxspeed)) THEN
        allocate ( maxspeed(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(maxrho)) THEN
        allocate ( maxrho(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(levsfrc)) THEN
        allocate ( levsfrc(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(levbfrc)) THEN
        allocate ( levbfrc(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Vtransform)) THEN
        allocate ( Vtransform(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Vstretching)) THEN
        allocate ( Vstretching(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Tcline)) THEN
        allocate ( Tcline(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(hc)) THEN
        allocate ( hc(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(theta_s)) THEN
        allocate ( theta_s(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(theta_b)) THEN
        allocate ( theta_b(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(hmin)) THEN
        allocate ( hmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(hmax)) THEN
        allocate ( hmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(xl)) THEN
        allocate ( xl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(el)) THEN
        allocate ( el(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LonMin)) THEN
        allocate ( LonMin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LonMax)) THEN
        allocate ( LonMax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LatMin)) THEN
        allocate ( LatMin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LatMax)) THEN
        allocate ( LatMax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Idigits)) THEN
        allocate ( Idigits(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Jdigits)) THEN
        allocate ( Jdigits(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Kdigits)) THEN
        allocate ( Kdigits(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TotVolume)) THEN
        allocate ( TotVolume(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(MinVolume)) THEN
        allocate ( MinVolume(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(MaxVolume)) THEN
        allocate ( MaxVolume(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DXmin)) THEN
        allocate ( DXmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DXmax)) THEN
        allocate ( DXmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DYmin)) THEN
        allocate ( DYmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DYmax)) THEN
        allocate ( DYmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DZmin)) THEN
        allocate ( DZmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DZmax)) THEN
        allocate ( DZmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(grdmax)) THEN
        allocate ( grdmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Cg_min)) THEN
        allocate ( Cg_min(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Cg_max)) THEN
        allocate ( Cg_max(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Cg_Cor)) THEN
        allocate ( Cg_Cor(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(R0)) THEN
        allocate ( R0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Tcoef)) THEN
        allocate ( Tcoef(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Scoef)) THEN
        allocate ( Scoef(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(T0)) THEN
        allocate ( T0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(S0)) THEN
        allocate ( S0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gamma2)) THEN
        allocate ( gamma2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(lmd_Jwt)) THEN
        allocate ( lmd_Jwt(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rx0)) THEN
        allocate ( rx0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rx1)) THEN
        allocate ( rx1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rdrg)) THEN
        allocate ( rdrg(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rdrg2)) THEN
        allocate ( rdrg2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Zos)) THEN
        allocate ( Zos(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Zob)) THEN
        allocate ( Zob(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Dcrit)) THEN
        allocate ( Dcrit(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(weight)) THEN
        allocate ( weight(2,0:256,Ngrids) )
        Dmem(1)=Dmem(1)+514.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Akk_bak)) THEN
        allocate ( Akk_bak(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Akp_bak)) THEN
        allocate ( Akp_bak(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Akv_bak)) THEN
        allocate ( Akv_bak(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Akv_limit)) THEN
        allocate ( Akv_limit(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ad_visc2)) THEN
        allocate ( ad_visc2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nl_visc2)) THEN
        allocate ( nl_visc2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tl_visc2)) THEN
        allocate ( tl_visc2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(visc2)) THEN
        allocate ( visc2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ad_visc4)) THEN
        allocate ( ad_visc4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nl_visc4)) THEN
        allocate ( nl_visc4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tl_visc4)) THEN
        allocate ( tl_visc4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(visc4)) THEN
        allocate ( visc4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tkenu2)) THEN
        allocate ( tkenu2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tkenu4)) THEN
        allocate ( tkenu4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Akt_bak)) THEN
        allocate ( Akt_bak(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(Akt_limit)) THEN
        allocate ( Akt_limit(NAT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NAT*Ngrids,r8)
      END IF
      IF (.not.allocated(Kdiff)) THEN
        allocate ( Kdiff(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(ad_tnu2)) THEN
        allocate ( ad_tnu2(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(nl_tnu2)) THEN
        allocate ( nl_tnu2(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(tl_tnu2)) THEN
        allocate ( tl_tnu2(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(tnu2)) THEN
        allocate ( tnu2(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(ad_tnu4)) THEN
        allocate ( ad_tnu4(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(nl_tnu4)) THEN
        allocate ( nl_tnu4(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(tl_tnu4)) THEN
        allocate ( tl_tnu4(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(tnu4)) THEN
        allocate ( tnu4(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(tl_M2diff)) THEN
        allocate ( tl_M2diff(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tl_M3diff)) THEN
        allocate ( tl_M3diff(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tl_Tdiff)) THEN
        allocate ( tl_Tdiff(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(ad_Akv_fac)) THEN
        allocate ( ad_Akv_fac(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(tl_Akv_fac)) THEN
        allocate ( tl_Akv_fac(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ad_Akt_fac)) THEN
        allocate ( ad_Akt_fac(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(tl_Akt_fac)) THEN
        allocate ( tl_Akt_fac(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(LdefADJ)) THEN
        allocate ( LdefADJ(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefAVG)) THEN
        allocate ( LdefAVG(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefDAI)) THEN
        allocate ( LdefDAI(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefDIA)) THEN
        allocate ( LdefDIA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefERR)) THEN
        allocate ( LdefERR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefFLT)) THEN
        allocate ( LdefFLT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefHIS)) THEN
        allocate ( LdefHIS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefHSS)) THEN
        allocate ( LdefHSS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefINI)) THEN
        allocate ( LdefINI(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefIRP)) THEN
        allocate ( LdefIRP(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefITL)) THEN
        allocate ( LdefITL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefLCZ)) THEN
        allocate ( LdefLCZ(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefLZE)) THEN
        allocate ( LdefLZE(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefMOD)) THEN
        allocate ( LdefMOD(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefQCK)) THEN
        allocate ( LdefQCK(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefRST)) THEN
        allocate ( LdefRST(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefXTR)) THEN
        allocate ( LdefXTR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefSTA)) THEN
        allocate ( LdefSTA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefTIDE)) THEN
        allocate ( LdefTIDE(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefTLM)) THEN
        allocate ( LdefTLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefTLF)) THEN
        allocate ( LdefTLF(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LreadADM)) THEN
        allocate ( LreadADM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LreadBLK)) THEN
        allocate ( LreadBLK(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LreadFRC)) THEN
        allocate ( LreadFRC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LreadFWD)) THEN
        allocate ( LreadFWD(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LreadQCK)) THEN
        allocate ( LreadQCK(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LreadSTD)) THEN
        allocate ( LreadSTD(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LreadTLM)) THEN
        allocate ( LreadTLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtADJ)) THEN
        allocate ( LwrtADJ(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtAVG)) THEN
        allocate ( LwrtAVG(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtDIA)) THEN
        allocate ( LwrtDIA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtHIS)) THEN
        allocate ( LwrtHIS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtPER)) THEN
        allocate ( LwrtPER(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtQCK)) THEN
        allocate ( LwrtQCK(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtRST)) THEN
        allocate ( LwrtRST(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtTLF)) THEN
        allocate ( LwrtTLF(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtTLM)) THEN
        allocate ( LwrtTLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtXTR)) THEN
        allocate ( LwrtXTR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LdefNRM)) THEN
        allocate ( LdefNRM(4,Ngrids) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtNRM)) THEN
        allocate ( LwrtNRM(4,Ngrids) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtState2d)) THEN
        allocate ( LwrtState2d(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtTime)) THEN
        allocate ( LwrtTime(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtCost)) THEN
        allocate ( LwrtCost(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Ladjusted)) THEN
        allocate ( Ladjusted(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LprocessOBC)) THEN
        allocate ( LprocessOBC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LprocessTides)) THEN
        allocate ( LprocessTides(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LwrtInfo)) THEN
        allocate ( LwrtInfo(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ldefout)) THEN
        allocate ( ldefout(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndefADJ)) THEN
        allocate ( ndefADJ(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndefAVG)) THEN
        allocate ( ndefAVG(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndefDIA)) THEN
        allocate ( ndefDIA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndefHIS)) THEN
        allocate ( ndefHIS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndefQCK)) THEN
        allocate ( ndefQCK(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndefTLM)) THEN
        allocate ( ndefTLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ndefXTR)) THEN
        allocate ( ndefXTR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ntsAVG)) THEN
        allocate ( ntsAVG(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ntsDIA)) THEN
        allocate ( ntsDIA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nADJ)) THEN
        allocate ( nADJ(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nAVG)) THEN
        allocate ( nAVG(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nDIA)) THEN
        allocate ( nDIA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nFLT)) THEN
        allocate ( nFLT(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nHIS)) THEN
        allocate ( nHIS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nQCK)) THEN
        allocate ( nQCK(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nRST)) THEN
        allocate ( nRST(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nSTA)) THEN
        allocate ( nSTA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nTLM)) THEN
        allocate ( nTLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nXTR)) THEN
        allocate ( nXTR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ExtractFlag)) THEN
        allocate ( ExtractFlag(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(ninfo)) THEN
        allocate ( ninfo(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nOBC)) THEN
        allocate ( nOBC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Nbrec)) THEN
        allocate ( Nbrec(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(OBCcount)) THEN
        allocate ( OBCcount(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nSFF)) THEN
        allocate ( nSFF(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Nfrec)) THEN
        allocate ( Nfrec(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SFcount)) THEN
        allocate ( SFcount(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(nrrec)) THEN
        allocate ( nrrec(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LastRec)) THEN
        allocate ( LastRec(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LcycleADJ)) THEN
        allocate ( LcycleADJ(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LcycleRST)) THEN
        allocate ( LcycleRST(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(LcycleTLM)) THEN
        allocate ( LcycleTLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KstrS)) THEN
        allocate ( KstrS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(KendS)) THEN
        allocate ( KendS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DstrS)) THEN
        allocate ( DstrS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(DendS)) THEN
        allocate ( DendS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(TRnorm)) THEN
        allocate ( TRnorm(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SO_decay)) THEN
        allocate ( SO_decay(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SO_sdev)) THEN
        allocate ( SO_sdev(7+2*MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL((7+2*MT)*Ngrids,r8)
      END IF
      IF (.not.allocated(obcfac)) THEN
        allocate ( obcfac(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(FSobc_in)) THEN
        allocate ( FSobc_in(Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(FSobc_out)) THEN
        allocate ( FSobc_out(Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(M2obc_in)) THEN
        allocate ( M2obc_in(Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(M2obc_out)) THEN
        allocate ( M2obc_out(Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(M3obc_in)) THEN
        allocate ( M3obc_in(Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(M3obc_out)) THEN
        allocate ( M3obc_out(Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Tobc_in)) THEN
        allocate ( Tobc_in(MT,Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(Tobc_out)) THEN
        allocate ( Tobc_out(MT,Ngrids,4) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(MT*Ngrids,r8)
      END IF
      IF (.not.allocated(Znudg)) THEN
        allocate ( Znudg(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(M2nudg)) THEN
        allocate ( M2nudg(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(M3nudg)) THEN
        allocate ( M3nudg(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Tnudg)) THEN
        allocate ( Tnudg(MT,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_m)) THEN
        allocate ( gls_m(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_n)) THEN
        allocate ( gls_n(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_p)) THEN
        allocate ( gls_p(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_sigk)) THEN
        allocate ( gls_sigk(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_sigp)) THEN
        allocate ( gls_sigp(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_cmu0)) THEN
        allocate ( gls_cmu0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_cmupr)) THEN
        allocate ( gls_cmupr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_c1)) THEN
        allocate ( gls_c1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_c2)) THEN
        allocate ( gls_c2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_c3m)) THEN
        allocate ( gls_c3m(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_c3p)) THEN
        allocate ( gls_c3p(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_Kmin)) THEN
        allocate ( gls_Kmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(gls_Pmin)) THEN
        allocate ( gls_Pmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(charnok_alpha)) THEN
        allocate ( charnok_alpha(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(zos_hsig_alpha)) THEN
        allocate ( zos_hsig_alpha(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(sz_alpha)) THEN
        allocate ( sz_alpha(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(crgban_cw)) THEN
        allocate ( crgban_cw(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wec_alpha)) THEN
        allocate ( wec_alpha(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
!
      RETURN
      END SUBROUTINE allocate_scalars
!
      SUBROUTINE deallocate_scalars (ng)
!
!=======================================================================
!                                                                      !
!  This routine deallocates structures variables and module variables. !
!  Notice that "destroy" cannot be use to deallocate pointer variables !
!  because of cyclic dependencies.                                     !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer :: ng
!
!-----------------------------------------------------------------------
!  Deallocate derived-type SCALARS structure.
!-----------------------------------------------------------------------
!
      IF (ng.eq.Ngrids) THEN
        IF (allocated(SCALARS)) deallocate ( SCALARS )
      END IF
!
!-----------------------------------------------------------------------
!  Deallocate variables in modules.
!-----------------------------------------------------------------------
!
!  Deallocate variables that require special treatment in shared-memory.
!  These variables are private for each thread to avoid collisions.
!
!$OMP PARALLEL
      IF (allocated(PREDICTOR_2D_STEP)) deallocate ( PREDICTOR_2D_STEP )
      IF (allocated(indx1))             deallocate ( indx1 )
      IF (allocated(iic))               deallocate ( iic )
      IF (allocated(iif))               deallocate ( iif )
      IF (allocated(next_kstp))         deallocate ( next_kstp )
      IF (allocated(FrcRec))            deallocate ( FrcRec )
      IF (allocated(SOrec))             deallocate ( SOrec )
      IF (allocated(synchro_flag))      deallocate ( synchro_flag )
      IF (allocated(step_counter))      deallocate ( step_counter )
      IF (allocated(tdays))             deallocate ( tdays )
      IF (allocated(time))              deallocate ( time )
      IF (allocated(time_code))         deallocate ( time_code )
!$OMP END PARALLEL
!
!  Deallocate regular variables
!
      IF (allocated(PerfectRST))        deallocate ( PerfectRST )
      IF (allocated(ndtfast))           deallocate ( ndtfast )
      IF (allocated(nfast))             deallocate ( nfast )
      IF (allocated(dt))                deallocate ( dt )
      IF (allocated(dtfast))            deallocate ( dtfast )
      IF (allocated(TimeEnd))           deallocate ( TimeEnd )
      IF (allocated(AVGtime))           deallocate ( AVGtime )
      IF (allocated(DIAtime))           deallocate ( DIAtime )
      IF (allocated(IMPtime))           deallocate ( IMPtime )
      IF (allocated(INItime))           deallocate ( INItime )
      IF (allocated(INItimeS))          deallocate ( INItimeS )
      IF (allocated(ObsTime))           deallocate ( ObsTime )
      IF (allocated(FrcTime))           deallocate ( FrcTime )
      IF (allocated(ntimes))            deallocate ( ntimes )
      IF (allocated(first_time))        deallocate ( first_time )
      IF (allocated(ntfirst))           deallocate ( ntfirst )
      IF (allocated(ntstart))           deallocate ( ntstart )
      IF (allocated(ntend))             deallocate ( ntend )
      IF (allocated(NrecFrc))           deallocate ( NrecFrc )
      IF (allocated(SetGridConfig))     deallocate ( SetGridConfig )
      IF (allocated(NudgingCoeff))      deallocate ( NudgingCoeff )
      IF (allocated(ObcData))           deallocate ( ObcData )
      IF (allocated(Lbiology))          deallocate ( Lbiology )
      IF (allocated(Lfloats))           deallocate ( Lfloats )
      IF (allocated(Lsediment))         deallocate ( Lsediment )
      IF (allocated(Lstations))         deallocate ( Lstations )
      IF (allocated(CompositeGrid))     deallocate ( CompositeGrid )
      IF (allocated(RefinedGrid))       deallocate ( RefinedGrid )
      IF (allocated(RefineScale))       deallocate ( RefineScale )
      IF (allocated(GetDonorData))      deallocate ( GetDonorData )
      IF (allocated(EWperiodic))        deallocate ( EWperiodic )
      IF (allocated(NSperiodic))        deallocate ( NSperiodic )
      IF (allocated(VolCons))           deallocate ( VolCons )
      IF (allocated(Lsponge))           deallocate ( Lsponge )
      IF (allocated(LuvSponge))         deallocate ( LuvSponge )
      IF (allocated(LtracerSponge))     deallocate ( LtracerSponge )
      IF (allocated(CLM_FILE))          deallocate ( CLM_FILE )
      IF (allocated(Lclimatology))      deallocate ( Lclimatology )
      IF (allocated(LsshCLM))           deallocate ( LsshCLM )
      IF (allocated(Lm2CLM))            deallocate ( Lm2CLM )
      IF (allocated(Lm3CLM))            deallocate ( Lm3CLM )
      IF (allocated(LtracerCLM))        deallocate ( LtracerCLM )
      IF (allocated(Lnudging))          deallocate ( Lnudging )
      IF (allocated(LnudgeM2CLM))       deallocate ( LnudgeM2CLM )
      IF (allocated(LnudgeM3CLM))       deallocate ( LnudgeM3CLM )
      IF (allocated(LnudgeTCLM))        deallocate ( LnudgeTCLM )
      IF (allocated(LuvSrc))            deallocate ( LuvSrc )
      IF (allocated(LwSrc))             deallocate ( LwSrc )
      IF (allocated(LtracerSrc))        deallocate ( LtracerSrc )
      IF (allocated(maxspeed))          deallocate ( maxspeed )
      IF (allocated(maxrho))            deallocate ( maxrho )
      IF (allocated(levsfrc))           deallocate ( levsfrc )
      IF (allocated(levbfrc))           deallocate ( levbfrc )
      IF (allocated(Vtransform))        deallocate ( Vtransform )
      IF (allocated(Vstretching))       deallocate ( Vstretching )
      IF (allocated(Tcline))            deallocate ( Tcline )
      IF (allocated(hc))                deallocate ( hc )
      IF (allocated(theta_s))           deallocate ( theta_s )
      IF (allocated(theta_b))           deallocate ( theta_b )
      IF (allocated(hmin))              deallocate ( hmin )
      IF (allocated(hmax))              deallocate ( hmax )
      IF (allocated(xl))                deallocate ( xl )
      IF (allocated(el))                deallocate ( el )
      IF (allocated(LonMin))            deallocate ( LonMin )
      IF (allocated(LonMax))            deallocate ( LonMax )
      IF (allocated(LatMin))            deallocate ( LatMin )
      IF (allocated(LatMax))            deallocate ( LatMax )
      IF (allocated(Idigits))           deallocate ( Idigits )
      IF (allocated(Jdigits))           deallocate ( Jdigits )
      IF (allocated(Kdigits))           deallocate ( Kdigits )
      IF (allocated(TotVolume))         deallocate ( TotVolume )
      IF (allocated(MinVolume))         deallocate ( MinVolume )
      IF (allocated(MaxVolume))         deallocate ( MaxVolume )
      IF (allocated(DXmin))             deallocate ( DXmin )
      IF (allocated(DXmax))             deallocate ( DXmax )
      IF (allocated(DYmin))             deallocate ( DYmin )
      IF (allocated(DYmax))             deallocate ( DYmax )
      IF (allocated(DZmin))             deallocate ( DZmin )
      IF (allocated(DZmax))             deallocate ( DZmax )
      IF (allocated(grdmax))            deallocate ( grdmax )
      IF (allocated(Cg_min))            deallocate ( Cg_min )
      IF (allocated(Cg_max))            deallocate ( Cg_max )
      IF (allocated(Cg_Cor))            deallocate ( Cg_Cor )
      IF (allocated(R0))                deallocate ( R0 )
      IF (allocated(Tcoef))             deallocate ( Tcoef )
      IF (allocated(Scoef))             deallocate ( Scoef )
      IF (allocated(T0))                deallocate ( T0 )
      IF (allocated(S0))                deallocate ( S0 )
      IF (allocated(gamma2))            deallocate ( gamma2 )
      IF (allocated(lmd_Jwt))           deallocate ( lmd_Jwt )
      IF (allocated(rx0))               deallocate ( rx0 )
      IF (allocated(rx1))               deallocate ( rx1 )
      IF (allocated(rdrg))              deallocate ( rdrg )
      IF (allocated(rdrg2))             deallocate ( rdrg2 )
      IF (allocated(Zos))               deallocate ( Zos )
      IF (allocated(Zob))               deallocate ( Zob )
      IF (allocated(Dcrit))             deallocate ( Dcrit )
      IF (allocated(weight))            deallocate ( weight )
      IF (allocated(Akk_bak))           deallocate ( Akk_bak )
      IF (allocated(Akp_bak))           deallocate ( Akp_bak )
      IF (allocated(Akv_bak))           deallocate ( Akv_bak )
      IF (allocated(Akv_limit))         deallocate ( Akv_limit )
      IF (allocated(ad_visc2))          deallocate ( ad_visc2 )
      IF (allocated(nl_visc2))          deallocate ( nl_visc2 )
      IF (allocated(tl_visc2))          deallocate ( tl_visc2 )
      IF (allocated(visc2))             deallocate ( visc2 )
      IF (allocated(ad_visc4))          deallocate ( ad_visc4 )
      IF (allocated(nl_visc4))          deallocate ( nl_visc4 )
      IF (allocated(tl_visc4))          deallocate ( tl_visc4 )
      IF (allocated(visc4))             deallocate ( visc4 )
      IF (allocated(tkenu2))            deallocate ( tkenu2 )
      IF (allocated(tkenu4))            deallocate ( tkenu4 )
      IF (allocated(Akt_bak))           deallocate ( Akt_bak )
      IF (allocated(Akt_limit))         deallocate ( Akt_limit )
      IF (allocated(Kdiff))             deallocate ( Kdiff )
      IF (allocated(ad_tnu2))           deallocate ( ad_tnu2 )
      IF (allocated(nl_tnu2))           deallocate ( nl_tnu2 )
      IF (allocated(tl_tnu2))           deallocate ( tl_tnu2 )
      IF (allocated(tnu2))              deallocate ( tnu2 )
      IF (allocated(ad_tnu4))           deallocate ( ad_tnu4 )
      IF (allocated(nl_tnu4))           deallocate ( nl_tnu4 )
      IF (allocated(tl_tnu4))           deallocate ( tl_tnu4 )
      IF (allocated(tnu4))              deallocate ( tnu4 )
      IF (allocated(tl_M2diff))         deallocate ( tl_M2diff )
      IF (allocated(tl_M3diff))         deallocate ( tl_M3diff )
      IF (allocated(tl_Tdiff))          deallocate ( tl_Tdiff )
      IF (allocated(ad_Akv_fac))        deallocate ( ad_Akv_fac )
      IF (allocated(tl_Akv_fac))        deallocate ( tl_Akv_fac )
      IF (allocated(ad_Akt_fac))        deallocate ( ad_Akt_fac )
      IF (allocated(tl_Akt_fac))        deallocate ( tl_Akt_fac )
      IF (allocated(LdefADJ))           deallocate ( LdefADJ )
      IF (allocated(LdefAVG))           deallocate ( LdefAVG )
      IF (allocated(LdefDAI))           deallocate ( LdefDAI )
      IF (allocated(LdefDIA))           deallocate ( LdefDIA )
      IF (allocated(LdefERR))           deallocate ( LdefERR )
      IF (allocated(LdefFLT))           deallocate ( LdefFLT )
      IF (allocated(LdefHIS))           deallocate ( LdefHIS )
      IF (allocated(LdefHSS))           deallocate ( LdefHSS )
      IF (allocated(LdefINI))           deallocate ( LdefINI )
      IF (allocated(LdefIRP))           deallocate ( LdefIRP )
      IF (allocated(LdefITL))           deallocate ( LdefITL )
      IF (allocated(LdefLCZ))           deallocate ( LdefLCZ )
      IF (allocated(LdefLZE))           deallocate ( LdefLZE )
      IF (allocated(LdefMOD))           deallocate ( LdefMOD )
      IF (allocated(LdefQCK))           deallocate ( LdefQCK )
      IF (allocated(LdefRST))           deallocate ( LdefRST )
      IF (allocated(LdefSTA))           deallocate ( LdefSTA )
      IF (allocated(LdefTIDE))          deallocate ( LdefTIDE )
      IF (allocated(LdefTLF))           deallocate ( LdefTLF )
      IF (allocated(LdefTLM))           deallocate ( LdefTLM )
      IF (allocated(LdefXTR))           deallocate ( LdefXTR )
      IF (allocated(LreadADM))          deallocate ( LreadADM )
      IF (allocated(LreadBLK))          deallocate ( LreadBLK )
      IF (allocated(LreadFRC))          deallocate ( LreadFRC )
      IF (allocated(LreadFWD))          deallocate ( LreadFWD )
      IF (allocated(LreadQCK))          deallocate ( LreadQCK )
      IF (allocated(LreadSTD))          deallocate ( LreadSTD )
      IF (allocated(LreadTLM))          deallocate ( LreadTLM )
      IF (allocated(LwrtADJ))           deallocate ( LwrtADJ )
      IF (allocated(LwrtAVG))           deallocate ( LwrtAVG )
      IF (allocated(LwrtDIA))           deallocate ( LwrtDIA )
      IF (allocated(LwrtHIS))           deallocate ( LwrtHIS )
      IF (allocated(LwrtPER))           deallocate ( LwrtPER )
      IF (allocated(LwrtQCK))           deallocate ( LwrtQCK )
      IF (allocated(LwrtRST))           deallocate ( LwrtRST )
      IF (allocated(LwrtTLF))           deallocate ( LwrtTLF )
      IF (allocated(LwrtTLM))           deallocate ( LwrtTLM )
      IF (allocated(LwrtXTR))           deallocate ( LwrtXTR )
      IF (allocated(LdefNRM))           deallocate ( LdefNRM )
      IF (allocated(LwrtNRM))           deallocate ( LwrtNRM )
      IF (allocated(LwrtState2d))       deallocate ( LwrtState2d )
      IF (allocated(LwrtTime))          deallocate ( LwrtTime )
      IF (allocated(LwrtCost))          deallocate ( LwrtCost )
      IF (allocated(Ladjusted))         deallocate ( Ladjusted )
      IF (allocated(LprocessOBC))       deallocate ( LprocessOBC )
      IF (allocated(LprocessTides))     deallocate ( LprocessTides )
      IF (allocated(LwrtInfo))          deallocate ( LwrtInfo )
      IF (allocated(ldefout))           deallocate ( ldefout )
      IF (allocated(ndefADJ))           deallocate ( ndefADJ )
      IF (allocated(ndefAVG))           deallocate ( ndefAVG )
      IF (allocated(ndefDIA))           deallocate ( ndefDIA )
      IF (allocated(ndefHIS))           deallocate ( ndefHIS )
      IF (allocated(ndefQCK))           deallocate ( ndefQCK )
      IF (allocated(ndefTLM))           deallocate ( ndefTLM )
      IF (allocated(ndefTLM))           deallocate ( ndefXTR )
      IF (allocated(ntsAVG))            deallocate ( ntsAVG )
      IF (allocated(ntsDIA))            deallocate ( ntsDIA )
      IF (allocated(nADJ))              deallocate ( nADJ )
      IF (allocated(nAVG))              deallocate ( nAVG )
      IF (allocated(nDIA))              deallocate ( nDIA )
      IF (allocated(nFLT))              deallocate ( nFLT )
      IF (allocated(nHIS))              deallocate ( nHIS )
      IF (allocated(nQCK))              deallocate ( nQCK )
      IF (allocated(nRST))              deallocate ( nRST )
      IF (allocated(nSTA))              deallocate ( nSTA )
      IF (allocated(nTLM))              deallocate ( nTLM )
      IF (allocated(ExtractFlag))       deallocate ( ExtractFlag )
      IF (allocated(ninfo))             deallocate ( ninfo )
      IF (allocated(nOBC))              deallocate ( nOBC )
      IF (allocated(Nbrec))             deallocate ( Nbrec )
      IF (allocated(OBCcount))          deallocate ( OBCcount )
      IF (allocated(nSFF))              deallocate ( nSFF )
      IF (allocated(Nfrec))             deallocate ( Nfrec )
      IF (allocated(SFcount))           deallocate ( SFcount )
      IF (allocated(nrrec))             deallocate ( nrrec )
      IF (allocated(LastRec))           deallocate ( LastRec )
      IF (allocated(LcycleADJ))         deallocate ( LcycleADJ )
      IF (allocated(LcycleRST))         deallocate ( LcycleRST )
      IF (allocated(LcycleTLM))         deallocate ( LcycleTLM )
      IF (allocated(KstrS))             deallocate ( KstrS )
      IF (allocated(KendS))             deallocate ( KendS )
      IF (allocated(DstrS))             deallocate ( DstrS )
      IF (allocated(DendS))             deallocate ( DendS )
      IF (allocated(TRnorm))            deallocate ( TRnorm )
      IF (allocated(SO_decay))          deallocate ( SO_decay )
      IF (allocated(SO_sdev))           deallocate ( SO_sdev )
      IF (allocated(obcfac))            deallocate ( obcfac )
      IF (allocated(FSobc_in))          deallocate ( FSobc_in )
      IF (allocated(FSobc_out))         deallocate ( FSobc_out )
      IF (allocated(M2obc_in))          deallocate ( M2obc_in )
      IF (allocated(M2obc_out))         deallocate ( M2obc_out )
      IF (allocated(M3obc_in))          deallocate ( M3obc_in )
      IF (allocated(M3obc_out))         deallocate ( M3obc_out )
      IF (allocated(Tobc_in))           deallocate ( Tobc_in )
      IF (allocated(Tobc_out))          deallocate ( Tobc_out )
      IF (allocated(Znudg))             deallocate ( Znudg )
      IF (allocated(M2nudg))            deallocate ( M2nudg )
      IF (allocated(M3nudg))            deallocate ( M3nudg )
      IF (allocated(Tnudg))             deallocate ( Tnudg )
      IF (allocated(gls_m))             deallocate ( gls_m )
      IF (allocated(gls_n))             deallocate ( gls_n )
      IF (allocated(gls_p))             deallocate ( gls_p )
      IF (allocated(gls_sigk))          deallocate ( gls_sigk )
      IF (allocated(gls_sigp))          deallocate ( gls_sigp )
      IF (allocated(gls_cmu0))          deallocate ( gls_cmu0 )
      IF (allocated(gls_cmupr))         deallocate ( gls_cmupr )
      IF (allocated(gls_c1))            deallocate ( gls_c1 )
      IF (allocated(gls_c2))            deallocate ( gls_c2 )
      IF (allocated(gls_c3m))           deallocate ( gls_c3m )
      IF (allocated(gls_c3p))           deallocate ( gls_c3p )
      IF (allocated(gls_Kmin))          deallocate ( gls_Kmin )
      IF (allocated(gls_Pmin))          deallocate ( gls_Pmin )
      IF (allocated(charnok_alpha))     deallocate ( charnok_alpha )
      IF (allocated(zos_hsig_alpha))    deallocate ( zos_hsig_alpha )
      IF (allocated(sz_alpha))          deallocate ( sz_alpha )
      IF (allocated(crgban_cw))         deallocate ( crgban_cw )
!
      RETURN
      END SUBROUTINE deallocate_scalars
!
      SUBROUTINE initialize_scalars
!
!=======================================================================
!                                                                      !
!  This routine initializes several variables in module for all nested !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Local variable declarations.
!
      integer :: i, ic, j, ng, itrc
      real(r8) :: one, zero
      real(r8), parameter :: IniVal = 0.0_r8
!
!---------------------------------------------------------------------
!  Set tracer identification indices.
!---------------------------------------------------------------------
!
      itemp=1
      isalt=2
      ic=NAT
!
!---------------------------------------------------------------------
!  Set diagnostic fields identification indices.
!---------------------------------------------------------------------
!
!  Indices for tracer diagnostic variables.
!
      iThadv=1
      iTxadv=2
      iTyadv=3
      iTvadv=4
      ic=4
      iTvdif=ic+1
      iTrate=ic+2
!
!  Indices for 2D momentum diagnostic variables.  In some places in
!  the code a compact DO-loop (idiag=1:M2pgrd) is used to improve
!  flexibility. Therefore, the order of indices is very important.
!  Only those fields that require special treatment are set below
!  the M2pgrd index.
!
      ic=0
      M2fcor=ic+1
      ic=ic+1
      M2hadv=ic+1
      M2xadv=ic+2
      M2yadv=ic+3
      ic=ic+3
      M2hvis=ic+1
      M2xvis=ic+2
      M2yvis=ic+3
      ic=ic+3
      M2pgrd=ic+1
      M2sstr=ic+2             ! These indices need to be
      M2bstr=ic+3             ! specified last to allow a
      M2rate=NDM2d            ! compact DO-loop structure
!
!  Indices for 3D momentum diagnostic variables.  In some places in
!  the code a compact DO-loop (idiag=1:M3pgrd) is used to improve
!  flexibility. Therefore, the order of indices is very important.
!  Only those fields that require special treatment are set below
!  the M3pgrd index.
!
      ic=0
      M3fcor=ic+1
      ic=ic+1
      M3vadv=ic+1
      M3hadv=ic+2
      M3xadv=ic+3
      M3yadv=ic+4
      ic=ic+4
      M3pgrd=ic+1             ! needs to be here, indices below
      M3vvis=ic+2             ! require special treatment
      M3hvis=ic+3
      M3xvis=ic+4
      M3yvis=ic+5
      M3rate=NDM3d
!
!-----------------------------------------------------------------------
!  Activate all computation control switches.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        LastRec(ng)=.FALSE.
        CompositeGrid(1:4,ng)=.FALSE.
        RefinedGrid(ng)=.FALSE.
        GetDonorData(ng)=.FALSE.
        Lbiology(ng)=.TRUE.
        LcycleADJ(ng)=.FALSE.
        LcycleRST(ng)=.FALSE.
        LcycleTLM(ng)=.FALSE.
        Lfloats(ng)=.TRUE.
        Lsediment(ng)=.TRUE.
        Lstations(ng)=.TRUE.
      END DO
!
!-----------------------------------------------------------------------
!  Initialize several scalar variables.
!-----------------------------------------------------------------------
!
      one=1.0_r8
      zero=0.0_r8
      Co=1.0_r8/(2.0_r8+SQRT(2.0_r8))
      gorho0=g/rho0
      DO ng=1,Ngrids
        EWperiodic(ng)=.FALSE.
        NSperiodic(ng)=.FALSE.
        NudgingCoeff(ng)=.FALSE.
        ObcData(ng)=.FALSE.
        SetGridConfig(ng)=.TRUE.
        ExtractFlag(ng)=0
        RefineScale(ng)=0
        gamma2(ng)=-1.0_r8
        Vtransform(ng)=1
        Vstretching(ng)=1
        first_time(ng)=0
        Idigits(ng)=INT(LOG10(REAL(Lm(ng),r8)))+1
        Jdigits(ng)=INT(LOG10(REAL(Mm(ng),r8)))+1
        Kdigits(ng)=INT(LOG10(REAL(N (ng),r8)))+1
        maxspeed(ng)=-Large
        maxrho(ng)=-Large
        TotVolume(ng)=0.0_dp
        MinVolume(ng)= Large
        MaxVolume(ng)=-Large
        DXmin(ng)= Large
        DXmax(ng)=-Large
        DYmin(ng)= Large
        DYmax(ng)=-Large
        DZmin(ng)= Large
        DZmax(ng)=-Large
        grdmax(ng)=-Large
        Cg_min(ng)= Large
        Cg_max(ng)=-Large
        Cg_Cor(ng)=-Large
        rx0(ng)=-Large
        rx1(ng)=-Large
        CLM_FILE(ng)=.FALSE.
        Lnudging(ng)=.FALSE.
        LnudgeM2CLM(ng)=.FALSE.
        LnudgeM3CLM(ng)=.FALSE.
        Lclimatology(ng)=.FALSE.
        Lm2CLM(ng)=.FALSE.
        Lm3CLM(ng)=.FALSE.
        LsshCLM(ng)=.FALSE.
        Lsponge(ng)=.FALSE.
        LuvSponge(ng)=.FALSE.
        LuvSrc(ng)=.FALSE.
        LwSrc(ng)=.FALSE.
        DO itrc=1,MT
          LnudgeTCLM(itrc,ng)=.FALSE.
          LtracerCLM(itrc,ng)=.FALSE.
          LtracerSrc(itrc,ng)=.FALSE.
          LtracerSponge(itrc,ng)=.FALSE.
          ad_Akt_fac(itrc,ng)=1.0_r8
          tl_Akt_fac(itrc,ng)=1.0_r8
          ad_tnu2(itrc,ng)=IniVal
          nl_tnu2(itrc,ng)=IniVal
          tl_tnu2(itrc,ng)=IniVal
          tnu2(itrc,ng)=IniVal
          ad_tnu4(itrc,ng)=IniVal
          nl_tnu4(itrc,ng)=IniVal
          tl_tnu4(itrc,ng)=IniVal
          tnu4(itrc,ng)=IniVal
        END DO
        DO itrc=1,NAT
          Akt_limit(itrc,ng)=1.0E-3_r8
        END DO
        Akv_limit(ng)=1.0E-3_r8
        ad_Akv_fac(ng)=1.0_r8
        tl_Akv_fac(ng)=1.0_r8
        ad_visc2(ng)=IniVal
        nl_visc2(ng)=IniVal
        tl_visc2(ng)=IniVal
        visc2(ng)=IniVal
        ad_visc4(ng)=IniVal
        nl_visc4(ng)=IniVal
        tl_visc4(ng)=IniVal
        visc4(ng)=IniVal
        DO i=1,4
          VolCons(i,ng)=.FALSE.
          FSobc_in (ng,i)=0.0_dp
          FSobc_out(ng,i)=0.0_dp
          M2obc_in (ng,i)=0.0_dp
          M2obc_out(ng,i)=0.0_dp
          M3obc_in (ng,i)=0.0_dp
          M3obc_out(ng,i)=0.0_dp
        END DO
      END DO
      Tobc_in = 0.0_dp
      Tobc_out = 0.0_dp
!
!  Initialize blowup string.
!
      DO i=1,LEN(blowup_string)
        blowup_string(i:i)=' '
      END DO
!
!  Initialize thread private variables.
!
!$OMP PARALLEL
      synchro_flag=.FALSE.
      ntfirst=1
      ntstart=1
      ntend=0
      step_counter=0
!$OMP END PARALLEL
!
!  Initialize several IO flags.
!
      LmultiGST=.FALSE.
      LrstGST=.FALSE.
      DO ng=1,Ngrids
        PerfectRST(ng)=.FALSE.
        Ladjusted(ng)=.FALSE.
        LprocessOBC(ng)=.FALSE.
        LprocessTides(ng)=.FALSE.
        LdefADJ(ng)=.FALSE.
        LdefAVG(ng)=.TRUE.
        LdefDAI(ng)=.FALSE.
        LdefDIA(ng)=.TRUE.
        LdefERR(ng)=.FALSE.
        LdefFLT(ng)=.TRUE.
        LdefHIS(ng)=.TRUE.
        LdefINI(ng)=.FALSE.
        LdefIRP(ng)=.FALSE.
        LdefITL(ng)=.FALSE.
        LdefMOD(ng)=.FALSE.
        LdefQCK(ng)=.FALSE.
        LdefRST(ng)=.TRUE.
        LdefSTA(ng)=.TRUE.
        LdefTLM(ng)=.FALSE.
        LdefTIDE(ng)=.FALSE.
        LreadADM(ng)=.FALSE.
        LreadBLK(ng)=.FALSE.
        LreadFRC(ng)=.FALSE.
        LreadFWD(ng)=.FALSE.
        LreadQCK(ng)=.FALSE.
        LreadSTD(ng)=.FALSE.
        LreadTLM(ng)=.FALSE.
        LwrtADJ(ng)=.FALSE.
        LwrtAVG(ng)=.FALSE.
        LwrtDIA(ng)=.FALSE.
        LwrtHIS(ng)=.FALSE.
        LwrtPER(ng)=.FALSE.
        LwrtQCK(ng)=.FALSE.
        LwrtRST(ng)=.FALSE.
        LwrtTLM(ng)=.FALSE.
        LwrtXTR(ng)=.FALSE.
        LwrtInfo(ng)=.TRUE.
        LwrtState2d(ng)=.FALSE.
        LwrtTime(ng)=.TRUE.
        LwrtCost(ng)=.FALSE.
        ldefout(ng)=.FALSE.
      END DO
!
!  Initialize the NLM initial conditions time to a negative number to
!  check if its value was assigned elsewhere.  It can be used during
!  the initialization of the adjoint model when DSTART is not the
!  same as the start of the simulation.
!
      DO ng=1,Ngrids
        INItime(ng)=-1.0_dp
        INItimeS(ng)=-1.0_dp
      END DO
      RETURN
      END SUBROUTINE initialize_scalars
!
      END MODULE mod_scalars
