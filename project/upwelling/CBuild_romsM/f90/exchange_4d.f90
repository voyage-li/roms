      MODULE exchange_4d_mod
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                            Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  These routines apply periodic boundary conditions to generic        !
!  3D fields.                                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng                      Nested grid number.                      !
!     tile                    Domain partition.                        !
!     LBi                     I-dimension Lower bound.                 !
!     UBi                     I-dimension Upper bound.                 !
!     LBj                     J-dimension Lower bound.                 !
!     UBj                     J-dimension Upper bound.                 !
!     LBk                     J-dimension Lower bound.                 !
!     UBk                     J-dimension Upper bound.                 !
!     LBl                     L-dimension Lower bound.                 !
!     UBl                     L-dimension Upper bound.                 !
!     A                       4D field.                                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     A                       Processed 3D field.                      !
!                                                                      !
!  Routines:                                                           !
!                                                                      !
!     exchange_p4d_tile       periodic conditions at PSI-points        !
!     exchange_r4d_tile       periodic conditions at RHO-points        !
!     exchange_u4d_tile       periodic conditions at U-points          !
!     exchange_v4d_tile       periodic conditions at V-points          !
!     exchange_w4d_tile       periodic conditions at W-points          !
!                                                                      !
!  NOTE:                                                               !
!                                                                      !
!  Periodic conditions are tricky in tiled domain applications. Recall !
!  that in ROMS, we can have tiled partitions in serial and parallel   !
!  (shared- and distributed-memory) configurations. However, in serial !
!  or shared-memory applications with domain decomposition, the field  !
!  "A" to process must be a GLOBAL state array and NOT a local tiled   !
!  scratch array because it does not contain the periodic points when  !
!  NtileI>1 or NtileJ>1.                                               !
!                                                                      !
!  Contrarily, in distributed memory applications, periodicity is      !
!  possible in both state and local arrays when NtileI=1 or NtileJ=1   !
!  below. Recall that the state arrays are dimensioned to the tile     !
!  size plus halo points. Therefore, if the periodic axis is tiled     !
!  (NtileI>1 or NtileJ>1), the periodicity is applied during the halo  !
!  exchange in module "mp_exchange3d". Notice that the halo exchange   !
!  is suppressed in the I-direction in "mp_exchange3d" when NtileI=1.  !
!  Similarly, it is avoided in the J-direction if NtileJ=1. Hence, the !
!  periodic exchange is called before the halo exchange in ROMS        !
!  numerical kernel.                                                   !
!                                                                      !
!=======================================================================
!
      implicit none
      CONTAINS
!
!***********************************************************************
      SUBROUTINE exchange_p4d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj, LBk, UBk,       &
     &                              LBl, UBl,                           &
     &                              A)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk, LBl, UBl
!
      real(r8), intent(inout) :: A(LBi:,LBj:,LBk:,LBl:)
!
!  Local variable declarations.
!
      logical :: EW_exchange
      logical :: NS_exchange
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, k, l
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
!-----------------------------------------------------------------------
!  Determine processing switches.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        EW_exchange=NtileI(ng).eq.1
      ELSE
        EW_exchange=.FALSE.
      END IF
      IF (NSperiodic(ng)) THEN
        NS_exchange=NtileJ(ng).eq.1
      ELSE
        NS_exchange=.FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  East-West periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        IF (NSperiodic(ng)) THEN
          Jmin=Jstr
          Jmax=Jend
        ELSE
          Jmin=Jstr
          Jmax=JendR
        END IF
!
        IF (EW_exchange) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(Lm(ng)+1,j,k,l)=A(1,j,k,l)
                  A(Lm(ng)+2,j,k,l)=A(2,j,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO j=Jmin,Jmax
                    A(Lm(ng)+3,j,k,l)=A(3,j,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(-2,j,k,l)=A(Lm(ng)-2,j,k,l)
                  A(-1,j,k,l)=A(Lm(ng)-1,j,k,l)
                  A( 0,j,k,l)=A(Lm(ng)  ,j,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  North-South periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (NSperiodic(ng)) THEN
        IF (EWperiodic(ng)) THEN
          Imin=Istr
          Imax=Iend
        ELSE
          Imin=Istr
          Imax=IendR
        END IF
!
        IF (NS_exchange) THEN
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,Mm(ng)+1,k,l)=A(i,1,k,l)
                  A(i,Mm(ng)+2,k,l)=A(i,2,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO i=Imin,Imax
                    A(i,Mm(ng)+3,k,l)=A(i,3,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,-2,k,l)=A(i,Mm(ng)-2,k,l)
                  A(i,-1,k,l)=A(i,Mm(ng)-1,k,l)
                  A(i, 0,k,l)=A(i,Mm(ng)  ,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Boundary corners.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).and.NSperiodic(ng)) THEN
        IF (EW_exchange.and.NS_exchange) THEN
          IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,Mm(ng)+1,k,l)=A(1,1,k,l)
                A(Lm(ng)+1,Mm(ng)+2,k,l)=A(1,2,k,l)
                A(Lm(ng)+2,Mm(ng)+1,k,l)=A(2,1,k,l)
                A(Lm(ng)+2,Mm(ng)+2,k,l)=A(2,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+1,Mm(ng)+3,k,l)=A(1,3,k,l)
                  A(Lm(ng)+2,Mm(ng)+3,k,l)=A(2,3,k,l)
                  A(Lm(ng)+3,Mm(ng)+1,k,l)=A(3,1,k,l)
                  A(Lm(ng)+3,Mm(ng)+2,k,l)=A(3,2,k,l)
                  A(Lm(ng)+3,Mm(ng)+3,k,l)=A(3,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,Mm(ng)+1,k,l)=A(Lm(ng)-2,1,k,l)
                A(-1,Mm(ng)+1,k,l)=A(Lm(ng)-1,1,k,l)
                A( 0,Mm(ng)+1,k,l)=A(Lm(ng)  ,1,k,l)
                A(-2,Mm(ng)+2,k,l)=A(Lm(ng)-2,2,k,l)
                A(-1,Mm(ng)+2,k,l)=A(Lm(ng)-1,2,k,l)
                A( 0,Mm(ng)+2,k,l)=A(Lm(ng)  ,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(-2,Mm(ng)+3,k,l)=A(Lm(ng)-2,3,k,l)
                  A(-1,Mm(ng)+3,k,l)=A(Lm(ng)-1,3,k,l)
                  A( 0,Mm(ng)+3,k,l)=A(Lm(ng)  ,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,-2,k,l)=A(1,Mm(ng)-2,k,l)
                A(Lm(ng)+1,-1,k,l)=A(1,Mm(ng)-1,k,l)
                A(Lm(ng)+1, 0,k,l)=A(1,Mm(ng)  ,k,l)
                A(Lm(ng)+2,-2,k,l)=A(2,Mm(ng)-2,k,l)
                A(Lm(ng)+2,-1,k,l)=A(2,Mm(ng)-1,k,l)
                A(Lm(ng)+2, 0,k,l)=A(2,Mm(ng)  ,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+3,-2,k,l)=A(3,Mm(ng)-2,k,l)
                  A(Lm(ng)+3,-1,k,l)=A(3,Mm(ng)-1,k,l)
                  A(Lm(ng)+3, 0,k,l)=A(3,Mm(ng)  ,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,-2,k,l)=A(Lm(ng)-2,Mm(ng)-2,k,l)
                A(-2,-1,k,l)=A(Lm(ng)-2,Mm(ng)-1,k,l)
                A(-2, 0,k,l)=A(Lm(ng)-2,Mm(ng)  ,k,l)
                A(-1,-2,k,l)=A(Lm(ng)-1,Mm(ng)-2,k,l)
                A(-1,-1,k,l)=A(Lm(ng)-1,Mm(ng)-1,k,l)
                A(-1, 0,k,l)=A(Lm(ng)-1,Mm(ng)  ,k,l)
                A( 0,-2,k,l)=A(Lm(ng)  ,Mm(ng)-2,k,l)
                A( 0,-1,k,l)=A(Lm(ng)  ,Mm(ng)-1,k,l)
                A( 0, 0,k,l)=A(Lm(ng)  ,Mm(ng)  ,k,l)
              END DO
            END DO
          END IF
        END IF
      END IF
      RETURN
      END SUBROUTINE exchange_p4d_tile
!
!***********************************************************************
      SUBROUTINE exchange_r4d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj, LBk, UBk,       &
     &                              LBl, UBl,                           &
     &                              A)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk, LBl, UBl
!
      real(r8), intent(inout) :: A(LBi:,LBj:,LBk:,LBl:)
!
!  Local variable declarations.
!
      logical :: EW_exchange
      logical :: NS_exchange
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, k, l
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
!-----------------------------------------------------------------------
!  Determine processing switches.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        EW_exchange=NtileI(ng).eq.1
      ELSE
        EW_exchange=.FALSE.
      END IF
      IF (NSperiodic(ng)) THEN
        NS_exchange=NtileJ(ng).eq.1
      ELSE
        NS_exchange=.FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  East-West periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        IF (NSperiodic(ng)) THEN
          Jmin=Jstr
          Jmax=Jend
        ELSE
          Jmin=JstrR
          Jmax=JendR
        END IF
!
        IF (EW_exchange) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(Lm(ng)+1,j,k,l)=A(1,j,k,l)
                  A(Lm(ng)+2,j,k,l)=A(2,j,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO j=Jmin,Jmax
                    A(Lm(ng)+3,j,k,l)=A(3,j,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(-2,j,k,l)=A(Lm(ng)-2,j,k,l)
                  A(-1,j,k,l)=A(Lm(ng)-1,j,k,l)
                  A( 0,j,k,l)=A(Lm(ng)  ,j,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  North-South periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (NSperiodic(ng)) THEN
        IF (EWperiodic(ng)) THEN
          Imin=Istr
          Imax=Iend
        ELSE
          Imin=IstrR
          Imax=IendR
        END IF
!
        IF (NS_exchange) THEN
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,Mm(ng)+1,k,l)=A(i,1,k,l)
                  A(i,Mm(ng)+2,k,l)=A(i,2,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO i=Imin,Imax
                    A(i,Mm(ng)+3,k,l)=A(i,3,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,-2,k,l)=A(i,Mm(ng)-2,k,l)
                  A(i,-1,k,l)=A(i,Mm(ng)-1,k,l)
                  A(i, 0,k,l)=A(i,Mm(ng)  ,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Boundary corners.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).and.NSperiodic(ng)) THEN
        IF (EW_exchange.and.NS_exchange) THEN
          IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,Mm(ng)+1,k,l)=A(1,1,k,l)
                A(Lm(ng)+1,Mm(ng)+2,k,l)=A(1,2,k,l)
                A(Lm(ng)+2,Mm(ng)+1,k,l)=A(2,1,k,l)
                A(Lm(ng)+2,Mm(ng)+2,k,l)=A(2,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+1,Mm(ng)+3,k,l)=A(1,3,k,l)
                  A(Lm(ng)+2,Mm(ng)+3,k,l)=A(2,3,k,l)
                  A(Lm(ng)+3,Mm(ng)+1,k,l)=A(3,1,k,l)
                  A(Lm(ng)+3,Mm(ng)+2,k,l)=A(3,2,k,l)
                  A(Lm(ng)+3,Mm(ng)+3,k,l)=A(3,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,Mm(ng)+1,k,l)=A(Lm(ng)-2,1,k,l)
                A(-1,Mm(ng)+1,k,l)=A(Lm(ng)-1,1,k,l)
                A( 0,Mm(ng)+1,k,l)=A(Lm(ng)  ,1,k,l)
                A(-2,Mm(ng)+2,k,l)=A(Lm(ng)-2,2,k,l)
                A(-1,Mm(ng)+2,k,l)=A(Lm(ng)-1,2,k,l)
                A( 0,Mm(ng)+2,k,l)=A(Lm(ng)  ,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(-2,Mm(ng)+3,k,l)=A(Lm(ng)-2,3,k,l)
                  A(-1,Mm(ng)+3,k,l)=A(Lm(ng)-1,3,k,l)
                  A( 0,Mm(ng)+3,k,l)=A(Lm(ng)  ,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,-2,k,l)=A(1,Mm(ng)-2,k,l)
                A(Lm(ng)+1,-1,k,l)=A(1,Mm(ng)-1,k,l)
                A(Lm(ng)+1, 0,k,l)=A(1,Mm(ng)  ,k,l)
                A(Lm(ng)+2,-2,k,l)=A(2,Mm(ng)-2,k,l)
                A(Lm(ng)+2,-1,k,l)=A(2,Mm(ng)-1,k,l)
                A(Lm(ng)+2, 0,k,l)=A(2,Mm(ng)  ,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+3,-2,k,l)=A(3,Mm(ng)-2,k,l)
                  A(Lm(ng)+3,-1,k,l)=A(3,Mm(ng)-1,k,l)
                  A(Lm(ng)+3, 0,k,l)=A(3,Mm(ng)  ,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,-2,k,l)=A(Lm(ng)-2,Mm(ng)-2,k,l)
                A(-2,-1,k,l)=A(Lm(ng)-2,Mm(ng)-1,k,l)
                A(-2, 0,k,l)=A(Lm(ng)-2,Mm(ng)  ,k,l)
                A(-1,-2,k,l)=A(Lm(ng)-1,Mm(ng)-2,k,l)
                A(-1,-1,k,l)=A(Lm(ng)-1,Mm(ng)-1,k,l)
                A(-1, 0,k,l)=A(Lm(ng)-1,Mm(ng)  ,k,l)
                A( 0,-2,k,l)=A(Lm(ng)  ,Mm(ng)-2,k,l)
                A( 0,-1,k,l)=A(Lm(ng)  ,Mm(ng)-1,k,l)
                A( 0, 0,k,l)=A(Lm(ng)  ,Mm(ng)  ,k,l)
              END DO
            END DO
          END IF
        END IF
      END IF
      RETURN
      END SUBROUTINE exchange_r4d_tile
!
!***********************************************************************
      SUBROUTINE exchange_u4d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj, LBk, UBk,       &
     &                              LBl, UBl,                           &
     &                              A)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk, LBl, UBl
!
      real(r8), intent(inout) :: A(LBi:,LBj:,LBk:,LBl:)
!
!  Local variable declarations.
!
      logical :: EW_exchange
      logical :: NS_exchange
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, k, l
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
!-----------------------------------------------------------------------
!  Determine processing switches.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        EW_exchange=NtileI(ng).eq.1
      ELSE
        EW_exchange=.FALSE.
      END IF
      IF (NSperiodic(ng)) THEN
        NS_exchange=NtileJ(ng).eq.1
      ELSE
        NS_exchange=.FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  East-West periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        IF (NSperiodic(ng)) THEN
          Jmin=Jstr
          Jmax=Jend
        ELSE
          Jmin=JstrR
          Jmax=JendR
        END IF
!
        IF (EW_exchange) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(Lm(ng)+1,j,k,l)=A(1,j,k,l)
                  A(Lm(ng)+2,j,k,l)=A(2,j,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO j=Jmin,Jmax
                    A(Lm(ng)+3,j,k,l)=A(3,j,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(-2,j,k,l)=A(Lm(ng)-2,j,k,l)
                  A(-1,j,k,l)=A(Lm(ng)-1,j,k,l)
                  A( 0,j,k,l)=A(Lm(ng)  ,j,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  North-South periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (NSperiodic(ng)) THEN
        IF (EWperiodic(ng)) THEN
          Imin=Istr
          Imax=Iend
        ELSE
          Imin=Istr
          Imax=IendR
        END IF
!
        IF (NS_exchange) THEN
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,Mm(ng)+1,k,l)=A(i,1,k,l)
                  A(i,Mm(ng)+2,k,l)=A(i,2,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO i=Imin,Imax
                    A(i,Mm(ng)+3,k,l)=A(i,3,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,-2,k,l)=A(i,Mm(ng)-2,k,l)
                  A(i,-1,k,l)=A(i,Mm(ng)-1,k,l)
                  A(i, 0,k,l)=A(i,Mm(ng)  ,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Boundary corners.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).and.NSperiodic(ng)) THEN
        IF (EW_exchange.and.NS_exchange) THEN
          IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,Mm(ng)+1,k,l)=A(1,1,k,l)
                A(Lm(ng)+1,Mm(ng)+2,k,l)=A(1,2,k,l)
                A(Lm(ng)+2,Mm(ng)+1,k,l)=A(2,1,k,l)
                A(Lm(ng)+2,Mm(ng)+2,k,l)=A(2,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+2,Mm(ng)+3,k,l)=A(2,3,k,l)
                  A(Lm(ng)+3,Mm(ng)+1,k,l)=A(3,1,k,l)
                  A(Lm(ng)+3,Mm(ng)+2,k,l)=A(3,2,k,l)
                  A(Lm(ng)+3,Mm(ng)+3,k,l)=A(3,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,Mm(ng)+1,k,l)=A(Lm(ng)-2,1,k,l)
                A(-1,Mm(ng)+1,k,l)=A(Lm(ng)-1,1,k,l)
                A( 0,Mm(ng)+1,k,l)=A(Lm(ng)  ,1,k,l)
                A(-2,Mm(ng)+2,k,l)=A(Lm(ng)-2,2,k,l)
                A(-1,Mm(ng)+2,k,l)=A(Lm(ng)-1,2,k,l)
                A( 0,Mm(ng)+2,k,l)=A(Lm(ng)  ,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(-2,Mm(ng)+3,k,l)=A(Lm(ng)-2,3,k,l)
                  A(-1,Mm(ng)+3,k,l)=A(Lm(ng)-1,3,k,l)
                  A( 0,Mm(ng)+3,k,l)=A(Lm(ng)  ,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,-2,k,l)=A(1,Mm(ng)-2,k,l)
                A(Lm(ng)+1,-1,k,l)=A(1,Mm(ng)-1,k,l)
                A(Lm(ng)+1, 0,k,l)=A(1,Mm(ng)  ,k,l)
                A(Lm(ng)+2,-2,k,l)=A(2,Mm(ng)-2,k,l)
                A(Lm(ng)+2,-1,k,l)=A(2,Mm(ng)-1,k,l)
                A(Lm(ng)+2, 0,k,l)=A(2,Mm(ng)  ,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+3,-2,k,l)=A(3,Mm(ng)-2,k,l)
                  A(Lm(ng)+3,-1,k,l)=A(3,Mm(ng)-1,k,l)
                  A(Lm(ng)+3, 0,k,l)=A(3,Mm(ng)  ,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,-2,k,l)=A(Lm(ng)-2,Mm(ng)-2,k,l)
                A(-2,-1,k,l)=A(Lm(ng)-2,Mm(ng)-1,k,l)
                A(-2, 0,k,l)=A(Lm(ng)-2,Mm(ng)  ,k,l)
                A(-1,-2,k,l)=A(Lm(ng)-1,Mm(ng)-2,k,l)
                A(-1,-1,k,l)=A(Lm(ng)-1,Mm(ng)-1,k,l)
                A(-1, 0,k,l)=A(Lm(ng)-1,Mm(ng)  ,k,l)
                A( 0,-2,k,l)=A(Lm(ng)  ,Mm(ng)-2,k,l)
                A( 0,-1,k,l)=A(Lm(ng)  ,Mm(ng)-1,k,l)
                A( 0, 0,k,l)=A(Lm(ng)  ,Mm(ng)  ,k,l)
              END DO
            END DO
          END IF
        END IF
      END IF
      RETURN
      END SUBROUTINE exchange_u4d_tile
!
!***********************************************************************
      SUBROUTINE exchange_v4d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj, LBk, UBk,       &
     &                              LBl, UBl,                           &
     &                              A)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk, LBl, UBl
!
      real(r8), intent(inout) :: A(LBi:,LBj:,LBk:,LBl:)
!
!  Local variable declarations.
!
      logical :: EW_exchange
      logical :: NS_exchange
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, k, l
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
!-----------------------------------------------------------------------
!  Determine processing switches.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        EW_exchange=NtileI(ng).eq.1
      ELSE
        EW_exchange=.FALSE.
      END IF
      IF (NSperiodic(ng)) THEN
        NS_exchange=NtileJ(ng).eq.1
      ELSE
        NS_exchange=.FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  East-West periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        IF (NSperiodic(ng)) THEN
          Jmin=Jstr
          Jmax=Jend
        ELSE
          Jmin=Jstr
          Jmax=JendR
        END IF
!
        IF (EW_exchange) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(Lm(ng)+1,j,k,l)=A(1,j,k,l)
                  A(Lm(ng)+2,j,k,l)=A(2,j,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO j=Jmin,Jmax
                    A(Lm(ng)+3,j,k,l)=A(3,j,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(-2,j,k,l)=A(Lm(ng)-2,j,k,l)
                  A(-1,j,k,l)=A(Lm(ng)-1,j,k,l)
                  A( 0,j,k,l)=A(Lm(ng)  ,j,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  North-South periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (NSperiodic(ng)) THEN
        IF (EWperiodic(ng)) THEN
          Imin=Istr
          Imax=Iend
        ELSE
          Imin=IstrR
          Imax=IendR
        END IF
!
        IF (NS_exchange) THEN
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,Mm(ng)+1,k,l)=A(i,1,k,l)
                  A(i,Mm(ng)+2,k,l)=A(i,2,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO i=Imin,Imax
                    A(i,Mm(ng)+3,k,l)=A(i,3,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,-2,k,l)=A(i,Mm(ng)-2,k,l)
                  A(i,-1,k,l)=A(i,Mm(ng)-1,k,l)
                  A(i, 0,k,l)=A(i,Mm(ng)  ,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Boundary corners.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).and.NSperiodic(ng)) THEN
        IF (EW_exchange.and.NS_exchange) THEN
          IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,Mm(ng)+1,k,l)=A(1,1,k,l)
                A(Lm(ng)+1,Mm(ng)+2,k,l)=A(1,2,k,l)
                A(Lm(ng)+2,Mm(ng)+1,k,l)=A(2,1,k,l)
                A(Lm(ng)+2,Mm(ng)+2,k,l)=A(2,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+1,Mm(ng)+3,k,l)=A(1,3,k,l)
                  A(Lm(ng)+2,Mm(ng)+3,k,l)=A(2,3,k,l)
                  A(Lm(ng)+3,Mm(ng)+1,k,l)=A(3,1,k,l)
                  A(Lm(ng)+3,Mm(ng)+2,k,l)=A(3,2,k,l)
                  A(Lm(ng)+3,Mm(ng)+3,k,l)=A(3,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,Mm(ng)+1,k,l)=A(Lm(ng)-2,1,k,l)
                A(-1,Mm(ng)+1,k,l)=A(Lm(ng)-1,1,k,l)
                A( 0,Mm(ng)+1,k,l)=A(Lm(ng)  ,1,k,l)
                A(-2,Mm(ng)+2,k,l)=A(Lm(ng)-2,2,k,l)
                A(-1,Mm(ng)+2,k,l)=A(Lm(ng)-1,2,k,l)
                A( 0,Mm(ng)+2,k,l)=A(Lm(ng)  ,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(-2,Mm(ng)+3,k,l)=A(Lm(ng)-2,3,k,l)
                  A(-1,Mm(ng)+3,k,l)=A(Lm(ng)-1,3,k,l)
                  A( 0,Mm(ng)+3,k,l)=A(Lm(ng)  ,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,-2,k,l)=A(1,Mm(ng)-2,k,l)
                A(Lm(ng)+1,-1,k,l)=A(1,Mm(ng)-1,k,l)
                A(Lm(ng)+1, 0,k,l)=A(1,Mm(ng)  ,k,l)
                A(Lm(ng)+2,-2,k,l)=A(2,Mm(ng)-2,k,l)
                A(Lm(ng)+2,-1,k,l)=A(2,Mm(ng)-1,k,l)
                A(Lm(ng)+2, 0,k,l)=A(2,Mm(ng)  ,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+3,-2,k,l)=A(3,Mm(ng)-2,k,l)
                  A(Lm(ng)+3,-1,k,l)=A(3,Mm(ng)-1,k,l)
                  A(Lm(ng)+3, 0,k,l)=A(3,Mm(ng)  ,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,-2,k,l)=A(Lm(ng)-2,Mm(ng)-2,k,l)
                A(-2,-1,k,l)=A(Lm(ng)-2,Mm(ng)-1,k,l)
                A(-2, 0,k,l)=A(Lm(ng)-2,Mm(ng)  ,k,l)
                A(-1,-2,k,l)=A(Lm(ng)-1,Mm(ng)-2,k,l)
                A(-1,-1,k,l)=A(Lm(ng)-1,Mm(ng)-1,k,l)
                A(-1, 0,k,l)=A(Lm(ng)-1,Mm(ng)  ,k,l)
                A( 0,-2,k,l)=A(Lm(ng)  ,Mm(ng)-2,k,l)
                A( 0,-1,k,l)=A(Lm(ng)  ,Mm(ng)-1,k,l)
                A( 0, 0,k,l)=A(Lm(ng)  ,Mm(ng)  ,k,l)
              END DO
            END DO
          END IF
        END IF
      END IF
      RETURN
      END SUBROUTINE exchange_v4d_tile
!
!***********************************************************************
      SUBROUTINE exchange_w4d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj, LBk, UBk,       &
     &                              LBl, UBl,                           &
     &                              A)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk, LBl, UBl
!
      real(r8), intent(inout) :: A(LBi:,LBj:,LBk:,LBl:)
!
!  Local variable declarations.
!
      logical :: EW_exchange
      logical :: NS_exchange
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, k, l
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
!-----------------------------------------------------------------------
!  Determine processing switches.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        EW_exchange=NtileI(ng).eq.1
      ELSE
        EW_exchange=.FALSE.
      END IF
      IF (NSperiodic(ng)) THEN
        NS_exchange=NtileJ(ng).eq.1
      ELSE
        NS_exchange=.FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  East-West periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        IF (NSperiodic(ng)) THEN
          Jmin=Jstr
          Jmax=Jend
        ELSE
          Jmin=JstrR
          Jmax=JendR
        END IF
!
        IF (EW_exchange) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(Lm(ng)+1,j,k,l)=A(1,j,k,l)
                  A(Lm(ng)+2,j,k,l)=A(2,j,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO j=Jmin,Jmax
                    A(Lm(ng)+3,j,k,l)=A(3,j,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO j=Jmin,Jmax
                  A(-2,j,k,l)=A(Lm(ng)-2,j,k,l)
                  A(-1,j,k,l)=A(Lm(ng)-1,j,k,l)
                  A( 0,j,k,l)=A(Lm(ng)  ,j,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  North-South periodic boundary conditions.
!-----------------------------------------------------------------------
!
      IF (NSperiodic(ng)) THEN
        IF (EWperiodic(ng)) THEN
          Imin=Istr
          Imax=Iend
        ELSE
          Imin=IstrR
          Imax=IendR
        END IF
!
        IF (NS_exchange) THEN
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,Mm(ng)+1,k,l)=A(i,1,k,l)
                  A(i,Mm(ng)+2,k,l)=A(i,2,k,l)
                END DO
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  DO i=Imin,Imax
                    A(i,Mm(ng)+3,k,l)=A(i,3,k,l)
                  END DO
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                DO i=Imin,Imax
                  A(i,-2,k,l)=A(i,Mm(ng)-2,k,l)
                  A(i,-1,k,l)=A(i,Mm(ng)-1,k,l)
                  A(i, 0,k,l)=A(i,Mm(ng)  ,k,l)
                END DO
              END DO
            END DO
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Boundary corners.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).and.NSperiodic(ng)) THEN
        IF (EW_exchange.and.NS_exchange) THEN
          IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,Mm(ng)+1,k,l)=A(1,1,k,l)
                A(Lm(ng)+1,Mm(ng)+2,k,l)=A(1,2,k,l)
                A(Lm(ng)+2,Mm(ng)+1,k,l)=A(2,1,k,l)
                A(Lm(ng)+2,Mm(ng)+2,k,l)=A(2,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+1,Mm(ng)+3,k,l)=A(1,3,k,l)
                  A(Lm(ng)+2,Mm(ng)+3,k,l)=A(2,3,k,l)
                  A(Lm(ng)+3,Mm(ng)+1,k,l)=A(3,1,k,l)
                  A(Lm(ng)+3,Mm(ng)+2,k,l)=A(3,2,k,l)
                  A(Lm(ng)+3,Mm(ng)+3,k,l)=A(3,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,Mm(ng)+1,k,l)=A(Lm(ng)-2,1,k,l)
                A(-1,Mm(ng)+1,k,l)=A(Lm(ng)-1,1,k,l)
                A( 0,Mm(ng)+1,k,l)=A(Lm(ng)  ,1,k,l)
                A(-2,Mm(ng)+2,k,l)=A(Lm(ng)-2,2,k,l)
                A(-1,Mm(ng)+2,k,l)=A(Lm(ng)-1,2,k,l)
                A( 0,Mm(ng)+2,k,l)=A(Lm(ng)  ,2,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(-2,Mm(ng)+3,k,l)=A(Lm(ng)-2,3,k,l)
                  A(-1,Mm(ng)+3,k,l)=A(Lm(ng)-1,3,k,l)
                  A( 0,Mm(ng)+3,k,l)=A(Lm(ng)  ,3,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(Lm(ng)+1,-2,k,l)=A(1,Mm(ng)-2,k,l)
                A(Lm(ng)+1,-1,k,l)=A(1,Mm(ng)-1,k,l)
                A(Lm(ng)+1, 0,k,l)=A(1,Mm(ng)  ,k,l)
                A(Lm(ng)+2,-2,k,l)=A(2,Mm(ng)-2,k,l)
                A(Lm(ng)+2,-1,k,l)=A(2,Mm(ng)-1,k,l)
                A(Lm(ng)+2, 0,k,l)=A(2,Mm(ng)  ,k,l)
              END DO
            END DO
            IF (NghostPoints.eq.3) THEN
              DO l=LBl,UBl
                DO k=LBk,UBk
                  A(Lm(ng)+3,-2,k,l)=A(3,Mm(ng)-2,k,l)
                  A(Lm(ng)+3,-1,k,l)=A(3,Mm(ng)-1,k,l)
                  A(Lm(ng)+3, 0,k,l)=A(3,Mm(ng)  ,k,l)
                END DO
              END DO
            END IF
          END IF
          IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
            DO l=LBl,UBl
              DO k=LBk,UBk
                A(-2,-2,k,l)=A(Lm(ng)-2,Mm(ng)-2,k,l)
                A(-2,-1,k,l)=A(Lm(ng)-2,Mm(ng)-1,k,l)
                A(-2, 0,k,l)=A(Lm(ng)-2,Mm(ng)  ,k,l)
                A(-1,-2,k,l)=A(Lm(ng)-1,Mm(ng)-2,k,l)
                A(-1,-1,k,l)=A(Lm(ng)-1,Mm(ng)-1,k,l)
                A(-1, 0,k,l)=A(Lm(ng)-1,Mm(ng)  ,k,l)
                A( 0,-2,k,l)=A(Lm(ng)  ,Mm(ng)-2,k,l)
                A( 0,-1,k,l)=A(Lm(ng)  ,Mm(ng)-1,k,l)
                A( 0, 0,k,l)=A(Lm(ng)  ,Mm(ng)  ,k,l)
              END DO
            END DO
          END IF
        END IF
      END IF
      RETURN
      END SUBROUTINE exchange_w4d_tile
      END MODULE exchange_4d_mod
