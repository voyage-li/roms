      SUBROUTINE ana_psource (ng, tile, model)
!
!! svn $Id$
!!======================================================================
!! Copyright (c) 2002-2007 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!                                                                     !
!=======================================================================
!                                                                      !
!  This subroutine sets analytical tracer and mass point Sources       !
!  and/or Sinks.  River runoff can be consider as a point source.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_ocean
      USE mod_sources
      USE mod_stepping
!
      integer, intent(in) :: ng, tile, model

      integer :: LBi, UBi, LBj, UBj
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
      CALL ana_psource_grid (ng, model, LBi, UBi, LBj, UBj,             &
     &                       nnew(ng), knew(ng), Nsrc(ng),              &
     &                       OCEAN(ng) % zeta,                          &
     &                       OCEAN(ng) % ubar,                          &
     &                       OCEAN(ng) % vbar,                          &
#ifdef SOLVE3D
     &                       OCEAN(ng) % u,                             &
     &                       OCEAN(ng) % v,                             &
     &                       GRID(ng) % z_w,                            &
#endif
     &                       GRID(ng) % h,                              &
     &                       GRID(ng) % on_u,                           &
     &                       GRID(ng) % om_v,                           &
     &                       SOURCES(ng) % Isrc,                        &
     &                       SOURCES(ng) % Jsrc,                        &
     &                       SOURCES(ng) % Lsrc,                        &
     &                       SOURCES(ng) % Dsrc,                        &
#ifdef SOLVE3D
# if defined UV_PSOURCE || defined Q_PSOURCE
     &                       SOURCES(ng) % Qshape,                      &
     &                       SOURCES(ng) % Qsrc,                        &
# endif
# ifdef TS_PSOURCE
     &                       SOURCES(ng) % Tsrc,                        &
# endif
#endif
     &                       SOURCES(ng) % Qbar)
      RETURN
      END SUBROUTINE ana_psource
!
!***********************************************************************
      SUBROUTINE ana_psource_grid (ng, model, LBi, UBi, LBj, UBj,       &
     &                             nnew, knew, Nsrc,                    &
     &                             zeta, ubar, vbar,                    &
#ifdef SOLVE3D
     &                             u, v, z_w,                           &
#endif
     &                             h, on_u, om_v,                       &
     &                             Isrc, Jsrc, Lsrc, Dsrc,              &
#ifdef SOLVE3D
# if defined UV_PSOURCE || defined Q_PSOURCE
     &                             Qshape, Qsrc,                        &
# endif
# ifdef TS_PSOURCE
     &                             Tsrc,                                &
# endif
#endif
     &                             Qbar)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
#ifdef SEDIMENT
      USE mod_sediment
#endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, LBi, UBi, LBj, UBj
      integer, intent(in) :: nnew, knew

      integer, intent(out) :: Nsrc
!
#ifdef ASSUMED_SHAPE
      logical, intent(out) :: Lsrc(:,:)

      integer, intent(out) :: Isrc(:)
      integer, intent(out) :: Jsrc(:)

      real(r8), intent(in) :: zeta(LBi:,LBj:,:)
      real(r8), intent(in) :: ubar(LBi:,LBj:,:)
      real(r8), intent(in) :: vbar(LBi:,LBj:,:)
# ifdef SOLVE3D
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
# endif
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)

      real(r8), intent(out) :: Dsrc(:)
      real(r8), intent(out) :: Qbar(:)
# ifdef SOLVE3D
#  if defined UV_PSOURCE || defined Q_PSOURCE
      real(r8), intent(out) :: Qshape(:,:)
      real(r8), intent(out) :: Qsrc(:,:)
#  endif
#  ifdef TS_PSOURCE
      real(r8), intent(out) :: Tsrc(:,:,:)
#  endif
# endif
#else
      logical, intent(out) :: Lsrc(Msrc,NT(ng))

      integer, intent(out) :: Isrc(Msrc)
      integer, intent(out) :: Jsrc(Msrc)

      real(r8), intent(in) :: zeta(LBi:UBi,LBj:UBj,3)
      real(r8), intent(in) :: ubar(LBi:UBi,LBj:UBj,3)
      real(r8), intent(in) :: vbar(LBi:UBi,LBj:UBj,3)
# ifdef SOLVE3D
      real(r8), intent(in) :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(in) :: v(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
# endif
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)

      real(r8), intent(out) :: Dsrc(Msrc)
      real(r8), intent(out) :: Qbar(Msrc)
# ifdef SOLVE3D
#  if defined UV_PSOURCE || defined Q_PSOURCE
      real(r8), intent(out) :: Qshape(Msrc,N(ng))
      real(r8), intent(out) :: Qsrc(Msrc,N(ng))
#  endif
#  ifdef TS_PSOURCE
      real(r8), intent(out) :: Tsrc(Msrc,N(ng),NT(ng))
#  endif
# endif
#endif
!
!  Local variable declarations.
!
      integer :: is, i, j, k, ised
      real(r8) :: fac, my_area, ramp
!
!-----------------------------------------------------------------------
!  Set tracer and/or mass point sources and/or sink.
!-----------------------------------------------------------------------
!
      IF (iic(ng).eq.ntstart(ng)) THEN
!
!  Set-up point Sources/Sink number (Nsrc), direction (Dsrc), I- and
!  J-grid locations (Isrc,Jsrc), and logical switch for type of tracer
!  to apply (Lsrc). Currently, the direction can be along XI-direction
!  (Dsrc = 0) or along ETA-direction (Dsrc > 0).  The mass sources are
!  located at U- or V-points so the grid locations should range from
!  1 =< Isrc =< L  and  1 =< Jsrc =< M.
!
        Lsrc(:,:)=.FALSE.
#if defined RIVERPLUME1
        Nsrc=1
        Dsrc(Nsrc)=0.0_r8
        Isrc(Nsrc)=1
        Jsrc(Nsrc)=50
        Lsrc(Nsrc,itemp)=.TRUE.
        Lsrc(Nsrc,isalt)=.TRUE.
#elif defined RIVERPLUME2
        Nsrc=1+Lm(ng)*2
        DO is=1,(Nsrc-1)/2
          Dsrc(is)=1.0_r8
          Isrc(is)=is
          Jsrc(is)=1
          Lsrc(is,itemp)=.TRUE.
          Lsrc(is,isalt)=.TRUE.
        END DO
        DO is=(Nsrc-1)/2+1,Nsrc-1
          Dsrc(is)=1.0_r8
          Isrc(is)=is-Lm(ng)
          Jsrc(is)=Mm(ng)+1
          Lsrc(is,itemp)=.TRUE.
          Lsrc(is,isalt)=.TRUE.
        END DO
        Dsrc(Nsrc)=0.0_r8
        Isrc(Nsrc)=1
        Jsrc(Nsrc)=60
        Lsrc(Nsrc,itemp)=.TRUE.
        Lsrc(Nsrc,isalt)=.TRUE.
#elif defined SED_TEST1
        Nsrc=Mm(ng)*2
        DO is=1,Nsrc/2
          Dsrc(is)=0.0_r8
          Isrc(is)=1
          Jsrc(is)=is
        END DO
        DO is=Nsrc/2+1,Nsrc
          Dsrc(is)=0.0_r8
          Isrc(is)=Lm(ng)+1
          Jsrc(is)=is-Mm(ng)
        END DO
#else
        ana_psource.h: No values provided for Dsrc, Isrc, Jsrc, Lsrc.
#endif
      END IF
#if defined UV_PSOURCE || defined Q_PSOURCE
# ifdef SOLVE3D
!
!  If appropriate, set-up nondimensional shape function to distribute
!  mass point sources/sinks vertically.  It must add to unity!!.
!
#  if defined SED_TEST1
        DO k=1,N(ng)
          DO is=1,Nsrc
            i=Isrc(is)
            j=Jsrc(is)
            Qshape(is,k)=ABS(u(i,j,k,nnew)/ubar(i,j,knew))*             &
     &                   (z_w(i-1,Mm(ng)/2,k)-z_w(i-1,Mm(ng)/2,k-1)+    &
     &                    z_w(i  ,Mm(ng)/2,k)-z_w(i  ,Mm(ng)/2,k-1))/   &
     &                   (z_w(i-1,Mm(ng)/2,N(ng))-z_w(i-1,Mm(ng)/2,0)+  &
     &                    z_w(i  ,Mm(ng)/2,N(ng))-z_w(i  ,Mm(ng)/2,0))
          END DO
        END DO
#  elif defined RIVERPLUME2
        DO k=1,N(ng)
          DO is=1,Nsrc-1
            i=Isrc(is)
            j=Jsrc(is)
            Qshape(is,k)=ABS(v(i,j,k,nnew)/vbar(i,j,knew))*             &
     &                   (z_w(i,j-1,k)-z_w(i,j-1,k-1)+                  &
     &                    z_w(i  ,j,k)-z_w(i  ,j,k-1))/                 &
     &                   (z_w(i,j-1,N(ng))-z_w(i,j-1,0)+                &
     &                    z_w(i  ,j,N(ng))-z_w(i  ,j,0))
          END DO
          Qshape(Nsrc,k)=1.0_r8/REAL(N(ng),r8)
        END DO
#  else
        DO k=1,N(ng)
          DO is=1,Nsrc
            Qshape(is,k)=1.0_r8/REAL(N(ng),r8)
          END DO
        END DO
#  endif
# endif
!
!  Set-up vertically integrated mass transport (m3/s) of point
!  Sources/Sinks (positive in the positive U- or V-direction and
!  viceversa).
!
# if defined RIVERPLUME1
      IF ((tdays(ng)-dstart).lt.0.5_r8) THEN
        fac=1.0_r8+TANH((time(ng)-43200.0_r8)/43200.0_r8)
      ELSE
        fac=1.0_r8
      END IF
      DO is=1,Nsrc
        Qbar(is)=fac*1500.0_r8
      END DO
# elif defined RIVERPLUME2
      DO is=1,(Nsrc-1)/2                     ! North end
        i=Isrc(is)
        j=Jsrc(is)
        Qbar(is)=-0.05_r8*om_v(i,j)*(0.5_r8*(zeta(i,j-1,knew)+h(i,j-1)+ &
     &                                       zeta(i  ,j,knew)+h(i  ,j)))
      END DO
      ! South End
      DO is=(Nsrc-1)/2+1,Nsrc-1              ! South end
        i=Isrc(is)
        j=Jsrc(is)
        Qbar(is)=-0.05_r8*om_v(i,j)*(0.5_r8*(zeta(i,j-1,knew)+h(i,j-1)+ &
     &                                       zeta(i  ,j,knew)+h(i  ,j)))
      END DO
      Qbar(Nsrc)=1500.0_r8                   ! West wall
# elif defined SED_TEST1
      my_area=0.0_r8                         ! West end
      DO is=1,Nsrc/2
        i=Isrc(is)
        j=Jsrc(is)
        my_area=my_area+0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+              &
     &                          zeta(i  ,j,knew)+h(i  ,j))*on_u(i,j)
      END DO
!!    fac=-1000.0_r8*10.0_r8*1.0_r8
      fac=-500.0_r8*10.0_r8*1.0_r8
      DO is=1,Nsrc/2
        i=Isrc(is)
        j=Jsrc(is)
        Qbar(is)=fac*(0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+                &
     &                        zeta(i  ,j,knew)+h(i  ,j)))*              &
     &           on_u(i,j)/my_area
      END DO
      my_area=0.0_r8                         ! East end
      DO is=Nsrc/2+1,Nsrc
        i=Isrc(is)
        j=Jsrc(is)
        my_area=my_area+0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+              &
     &                          zeta(i  ,j,knew)+h(i  ,j))*on_u(i,j)
      END DO
!!    fac=-1000.0_r8*10.0_r8*1.0_r8
      fac=-500.0_r8*10.0_r8*1.0_r8
      DO is=Nsrc/2+1,Nsrc
        i=Isrc(is)
        j=Jsrc(is)
        Qbar(is)=fac*(0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+                &
     &                        zeta(i  ,j,knew)+h(i  ,j)))*              &
     &           on_u(i,j)/my_area
      END DO
# else
      ana_psource.h: No values provided for Qbar.
# endif
# ifdef SOLVE3D
!
!  Set-up mass transport profile (m3/s) of point Sources/Sinks.
!
      DO k=1,N(ng)
        DO is=1,Nsrc
          Qsrc(is,k)=Qbar(is)*Qshape(is,k)
        END DO
      END DO
# endif
#endif
#if defined TS_PSOURCE && defined SOLVE3D
!
!  Set-up tracer (tracer units) point Sources/Sinks.
!
# if defined RIVERPLUME1
      DO k=1,N(ng)
        DO is=1,Nsrc
          Tsrc(is,k,itemp)=T0(ng)
          Tsrc(is,k,isalt)=0.0_r8
#  ifdef SEDIMENT
          DO ised=1,NST
            Tsrc(is,k,ised+2)=0.0_r8
          END DO
#  endif
        END DO
      END DO
# elif defined RIVERPLUME2
      DO k=1,N(ng)
        DO is=1,Nsrc-1
          Tsrc(is,k,itemp)=T0(ng)
          Tsrc(is,k,isalt)=S0(ng)
        END DO
        Tsrc(Nsrc,k,itemp)=T0(ng)
        Tsrc(Nsrc,k,isalt)=0.0_r8
      END DO
# else
      ana_psource.h: No values provided for Tsrc.
# endif
#endif
      RETURN
      END SUBROUTINE ana_psource_grid