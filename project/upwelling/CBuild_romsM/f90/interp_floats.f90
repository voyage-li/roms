      MODULE interp_floats_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group         Mark Hadfield   !
!    Licensed under a MIT/X style license             John M. Klinck   !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine interpolates requested field at the float trajectory   !
!  locations.                                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number.                                   !
!     LBi        I-dimension Lower bound.                              !
!     UBi        I-dimension Upper bound.                              !
!     LBj        J-dimension Lower bound.                              !
!     UBj        J-dimension Upper bound.                              !
!     LBk        K-dimension Lower bound.                              !
!     UBk        K-dimension Upper bound.                              !
!     Lstr       Starting float index to process.                      !
!     Lend       Ending   float index to process.                      !
!     itime      Floats time level to process.                         !
!     ifield     ID of field to compute.                               !
!     isBval     Lateral boundary variable index.                      !
!     gtype      Grid type. If negative, interpolate floats slopes.    !
!     maskit     Should the field be masked? Ignored if Land/Sea       !
!                 masking is not active.                               !
!     Fspval     Special values for unbounded float.                   !
!     nudg       Vertical random walk term to be added to the field.   !
!     pm         Inverse grid spacing (1/m) in the XI-direction.       !
!     pn         Inverse grid spacing (1/m) in the ETA-direction.      !
!     Hz         Vertical thicknesses (m).                             !
!     Amask      Field Land/Sea mask.                                  !
!     A          Field to interpolate from.                            !
!     my_thread  Float parallel thread bounded switch.                 !
!     bounded    Float grid bounded status switch.                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     track      Interpolated field: track(ifield,itime,:).            !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: interp_floats
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE interp_floats (ng, LBi, UBi, LBj, UBj, LBk, UBk,       &
     &                          Lstr, Lend, itime, ifield, isBval,      &
     &                          gtype, maskit, Fspval, nudg,            &
     &                          pm, pn,                                 &
     &                          Hz,                                     &
     &                          A, my_thread, bounded, track)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
      USE mod_floats
      USE mod_scalars
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj, LBk, UBk
      integer, intent(in) :: Lstr, Lend, itime, ifield, isBval, gtype
      logical, intent(in) :: maskit
      logical, intent(in) :: my_thread(Lstr:Lend)
      logical, intent(in) :: bounded(Nfloats(ng))
      real(dp), intent(in) :: Fspval
      real(r8), intent(in) :: nudg(Lstr:Lend)
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: A(LBi:UBi,LBj:UBj,LBk:UBk)
      real(r8), intent(inout) :: track(NFV(ng),0:NFT,Nfloats(ng))
!
!  Local variable declarations.
!
      logical :: Irvar, Iuvar, Jrvar, Jvvar, Krvar, Kwvar, Lmask
      logical :: halo
      integer :: Ir, Iu, Jr, Jv, Kr, Kw, l
      integer :: i1, i2, j1, j2, k1, k2, khm, khp, vtype
      real(r8) :: p1, p2, q1, q2, r1, r2
      real(r8) :: s111, s211, s121, s221, s112, s212, s122, s222
      real(r8) :: t111, t211, t121, t221, t112, t212, t122, t222
!
!-----------------------------------------------------------------------
!  Initialize various internal variables.
!-----------------------------------------------------------------------
!
!  Determine variable type switches.
!
      vtype=ABS(gtype)
      Irvar=(vtype.eq.r2dvar).or.(vtype.eq.r3dvar).or.                  &
     &      (vtype.eq.v2dvar).or.(vtype.eq.v3dvar).or.                  &
     &      (vtype.eq.w3dvar)
      Jrvar=(vtype.eq.r2dvar).or.(vtype.eq.r3dvar).or.                  &
     &      (vtype.eq.u2dvar).or.(vtype.eq.u3dvar).or.                  &
     &      (vtype.eq.w3dvar)
      Iuvar=(vtype.eq.u2dvar).or.(vtype.eq.u3dvar)
      Jvvar=(vtype.eq.v2dvar).or.(vtype.eq.v3dvar)
      Krvar=(vtype.eq.r3dvar).or.(vtype.eq.u3dvar).or.                  &
     &      (vtype.eq.v3dvar)
      Kwvar=(vtype.eq.w3dvar)
!
!  Determine whether to allow for masking in horizontal interpolation.
!
      Lmask=.FALSE.
!
!  If not interpolating slope, set multipliers to 1.
!
      IF (gtype.ge.0) THEN
        s111=1.0_r8
        s121=1.0_r8
        s211=1.0_r8
        s221=1.0_r8
        s112=1.0_r8
        s122=1.0_r8
        s212=1.0_r8
        s222=1.0_r8
        t111=1.0_r8
        t121=1.0_r8
        t211=1.0_r8
        t221=1.0_r8
        t112=1.0_r8
        t122=1.0_r8
        t212=1.0_r8
        t222=1.0_r8
      END IF
!
!-----------------------------------------------------------------------
!  Loop through floats.
!-----------------------------------------------------------------------
!
      DO l=Lstr,Lend
        IF (my_thread(l)) THEN
          IF (.not.bounded(l)) THEN
            track(ifield,itime,l)=Fspval
          ELSE
!
!  Calculate indices and weights for vertical interpolation, if any.
!
            IF (Krvar) THEN
              Kr=INT(track(izgrd,itime,l)+0.5_r8)
              k1=MIN(MAX(Kr  ,1),N(ng))
              k2=MIN(MAX(Kr+1,1),N(ng))
              r2=REAL(k2-k1,r8)*(track(izgrd,itime,l)+                  &
     &                           0.5_r8-REAL(k1,r8))
            ELSE IF (Kwvar) THEN
              Kw=INT(track(izgrd,itime,l))
              k1=MIN(MAX(Kw  ,0),N(ng))
              k2=MIN(MAX(Kw+1,0),N(ng))
              r2=REAL(k2-k1,r8)*(track(izgrd,itime,l)-REAL(k1,r8))
            ELSE
              k1=1
              k2=1
              r2=0.0_r8
            END IF
            r1=1.0_r8-r2
!
!-----------------------------------------------------------------------
!  Interpolation on RHO-columns.
!-----------------------------------------------------------------------
!
            IF (Irvar.and.Jrvar) THEN
!
              Ir=INT(track(ixgrd,itime,l))
              Jr=INT(track(iygrd,itime,l))
!
              i1=MIN(MAX(Ir  ,0),Lm(ng)+1)
              i2=MIN(MAX(Ir+1,1),Lm(ng)+1)
              j1=MIN(MAX(Jr  ,0),Mm(ng)+1)
              j2=MIN(MAX(Jr+1,1),Mm(ng)+1)
!
              p2=REAL(i2-i1,r8)*(track(ixgrd,itime,l)-REAL(i1,r8))
              q2=REAL(j2-j1,r8)*(track(iygrd,itime,l)-REAL(j1,r8))
              p1=1.0_r8-p2
              q1=1.0_r8-q2
!
              IF (gtype.eq.-w3dvar) THEN
                khm=MIN(MAX(k1  ,1),N(ng))
                khp=MIN(MAX(k1+1,1),N(ng))
                s111=2.0_r8*pm(i1,j1)*pn(i1,j1)/                        &
     &               (Hz(i1,j1,khm)+Hz(i1,j1,khp))
                s211=2.0_r8*pm(i2,j1)*pn(i2,j1)/                        &
     &               (Hz(i2,j1,khm)+Hz(i2,j1,khp))
                s121=2.0_r8*pm(i1,j2)*pn(i1,j2)/                        &
     &               (Hz(i1,j2,khm)+Hz(i1,j2,khp))
                s221=2.0_r8*pm(i2,j2)*pn(i2,j2)/                        &
     &               (Hz(i2,j2,khm)+Hz(i2,j2,khp))
                t111=2.0_r8/(Hz(i1,j1,khm)+Hz(i1,j1,khp))
                t211=2.0_r8/(Hz(i2,j1,khm)+Hz(i2,j1,khp))
                t121=2.0_r8/(Hz(i1,j2,khm)+Hz(i1,j2,khp))
                t221=2.0_r8/(Hz(i2,j2,khm)+Hz(i2,j2,khp))
                khm=MIN(MAX(k2  ,1),N(ng))
                khp=MIN(MAX(k2+1,1),N(ng))
                s112=2.0_r8*pm(i1,j1)*pn(i1,j1)/                        &
     &               (Hz(i1,j1,khm)+Hz(i1,j1,khp))
                s212=2.0_r8*pm(i2,j1)*pn(i2,j1)/                        &
     &               (Hz(i2,j1,khm)+Hz(i2,j1,khp))
                s122=2.0_r8*pm(i1,j2)*pn(i1,j2)/                        &
     &               (Hz(i1,j2,khm)+Hz(i1,j2,khp))
                s222=2.0_r8*pm(i2,j2)*pn(i2,j2)/                        &
     &               (Hz(i2,j2,khm)+Hz(i2,j2,khp))
                t112=2.0_r8/(Hz(i1,j1,khm)+Hz(i1,j1,khp))
                t212=2.0_r8/(Hz(i2,j1,khm)+Hz(i2,j1,khp))
                t122=2.0_r8/(Hz(i1,j2,khm)+Hz(i1,j2,khp))
                t222=2.0_r8/(Hz(i2,j2,khm)+Hz(i2,j2,khp))
              END IF
!
              IF (Lmask) THEN
              ELSE
                track(ifield,itime,l)=p1*q1*r1*s111*A(i1,j1,k1)+        &
     &                                p2*q1*r1*s211*A(i2,j1,k1)+        &
     &                                p1*q2*r1*s121*A(i1,j2,k1)+        &
     &                                p2*q2*r1*s221*A(i2,j2,k1)+        &
     &                                p1*q1*r2*s112*A(i1,j1,k2)+        &
     &                                p2*q1*r2*s212*A(i2,j1,k2)+        &
     &                                p1*q2*r2*s122*A(i1,j2,k2)+        &
     &                                p2*q2*r2*s222*A(i2,j2,k2)+        &
     &                                (p1*q1*r1*t111+                   &
     &                                 p2*q1*r1*t211+                   &
     &                                 p1*q2*r1*t121+                   &
     &                                 p2*q2*r1*t221+                   &
     &                                 p1*q1*r2*t112+                   &
     &                                 p2*q1*r2*t212+                   &
     &                                 p1*q2*r2*t122+                   &
     &                                 p2*q2*r2*t222)*nudg(l)
              END IF
!
!-----------------------------------------------------------------------
!  Interpolation at horizontal velocity points.
!-----------------------------------------------------------------------
!
            ELSE
              Ir=INT(track(ixgrd,itime,l))
              Jr=INT(track(iygrd,itime,l))
              Iu=INT(track(ixgrd,itime,l)+0.5_r8)
              Jv=INT(track(iygrd,itime,l)+0.5_r8)
!
              halo=.FALSE.
!
!-----------------------------------------------------------------------
!  Interpolation at U-points.
!-----------------------------------------------------------------------
!
              IF (Iuvar) THEN
                IF (halo) THEN
                ELSE
!
!  Bilinear interpolation outside halo.
!
                  i1=MIN(MAX(Iu  ,1),Lm(ng)+1)
                  i2=MIN(MAX(Iu+1,1),Lm(ng)+1)
                  j1=MIN(MAX(Jr  ,0),Mm(ng)+1)
                  j2=MIN(MAX(Jr+1,0),Mm(ng)+1)
!
                  p2=REAL(i2-i1,r8)*                                    &
     &               (track(ixgrd,itime,l)-REAL(i1,r8)+0.5_r8)
                  q2=REAL(j2-j1,r8)*                                    &
     &               (track(iygrd,itime,l)-REAL(j1,r8))
                  p1=1.0_r8-p2
                  q1=1.0_r8-q2
!
                  IF (gtype.lt.0) THEN
                    s111=0.5_r8*(pm(i1-1,j1)+pm(i1,j1))
                    s211=0.5_r8*(pm(i2-1,j1)+pm(i2,j1))
                    s121=0.5_r8*(pm(i1-1,j2)+pm(i1,j2))
                    s221=0.5_r8*(pm(i2-1,j2)+pm(i2,j2))
                    s112=s111
                    s212=s112
                    s122=s121
                    s222=s221
                  END IF
!
                  track(ifield,itime,l)=p1*q1*r1*s111*A(i1,j1,k1)+      &
     &                                  p2*q1*r1*s211*A(i2,j1,k1)+      &
     &                                  p1*q2*r1*s121*A(i1,j2,k1)+      &
     &                                  p2*q2*r1*s221*A(i2,j2,k1)+      &
     &                                  p1*q1*r2*s112*A(i1,j1,k2)+      &
     &                                  p2*q1*r2*s212*A(i2,j1,k2)+      &
     &                                  p1*q2*r2*s122*A(i1,j2,k2)+      &
     &                                  p2*q2*r2*s222*A(i2,j2,k2)+      &
     &                                  nudg(l)
                END IF
!
!-----------------------------------------------------------------------
!  Interpolation at V-points.
!-----------------------------------------------------------------------
!
              ELSE IF (Jvvar) THEN
                IF (halo) THEN
                ELSE
!
!  Bilinear interpolation outside halo.
!
                  i1=MIN(MAX(Ir  ,0),Lm(ng)+1)
                  i2=MIN(MAX(Ir+1,1),Lm(ng)+1)
                  j1=MIN(MAX(Jv  ,1),Mm(ng)+1)
                  j2=MIN(MAX(Jv+1,1),Mm(ng)+1)
!
                  p2=REAL(i2-i1,r8)*                                    &
     &               (track(ixgrd,itime,l)-REAL(i1,r8))
                  q2=REAL(j2-j1,r8)*                                    &
     &               (track(iygrd,itime,l)-REAL(j1,r8)+0.5_r8)
                  p1=1.0_r8-p2
                  q1=1.0_r8-q2
!
                  IF (gtype.lt.0) THEN
                    s111=0.5_r8*(pn(i1,j1-1)+pn(i1,j1))
                    s211=0.5_r8*(pn(i2,j1-1)+pn(i2,j1))
                    s121=0.5_r8*(pn(i1,j2-1)+pn(i1,j2))
                    s221=0.5_r8*(pn(i2,j2-1)+pn(i2,j2))
                    s112=s111
                    s212=s112
                    s122=s121
                    s222=s221
                  END IF
!
                  track(ifield,itime,l)=p1*q1*r1*s111*A(i1,j1,k1)+      &
     &                                  p2*q1*r1*s211*A(i2,j1,k1)+      &
     &                                  p1*q2*r1*s121*A(i1,j2,k1)+      &
     &                                  p2*q2*r1*s221*A(i2,j2,k1)+      &
     &                                  p1*q1*r2*s112*A(i1,j1,k2)+      &
     &                                  p2*q1*r2*s212*A(i2,j1,k2)+      &
     &                                  p1*q2*r2*s122*A(i1,j2,k2)+      &
     &                                  p2*q2*r2*s222*A(i2,j2,k2)+      &
     &                                  nudg(l)
                END IF
              END IF
            END IF
          END IF
        END IF
      END DO
      RETURN
      END SUBROUTINE interp_floats
      END MODULE interp_floats_mod
