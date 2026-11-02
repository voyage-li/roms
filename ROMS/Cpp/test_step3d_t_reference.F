#include "cppdefs.h"
! Independent serial oracle: numerical loop bodies below are copied from
! step3d_t.F, not translated from the C++ implementation. No boundaries or
! communications are called; the fixture supplies complete predictor halos.
      SUBROUTINE reference_step3d_t(p, schemes, pm, pn, Hz, Huon, &
     &  Hvom, &
     &                             W, z_r, Akt, t, DiaTwrk) &
     &                             BIND(C, NAME='reference_step3d_t')
      USE, INTRINSIC :: iso_c_binding
      USE kokkos_step3d_t_mod, ONLY : kokkos_tracer_config
      implicit none
      TYPE(kokkos_tracer_config), INTENT(IN) :: p
      integer(C_INT), INTENT(IN) :: schemes(p%NT)
      real(C_DOUBLE), INTENT(IN) :: pm(p%LBi:p%UBi,p%LBj:p%UBj)
      real(C_DOUBLE), INTENT(IN) :: pn(p%LBi:p%UBi,p%LBj:p%UBj)
      real(C_DOUBLE), INTENT(IN) :: Hz(p%LBi:p%UBi,p%LBj:p%UBj,p%N)
      real(C_DOUBLE), INTENT(IN) :: Huon(p%LBi:p%UBi,p%LBj:p%UBj,p%N)
      real(C_DOUBLE), INTENT(IN) :: Hvom(p%LBi:p%UBi,p%LBj:p%UBj,p%N)
      real(C_DOUBLE), INTENT(IN) :: W(p%LBi:p%UBi,p%LBj:p%UBj,0:p%N)
      real(C_DOUBLE), INTENT(IN) :: z_r(p%LBi:p%UBi,p%LBj:p%UBj,p%N)
      real(C_DOUBLE), INTENT(IN) :: Akt(p%LBi:p%UBi,p%LBj:p%UBj, &
     &  0:p%N,p%NAT)
      real(C_DOUBLE), INTENT(INOUT) :: t(p%LBi:p%UBi,p%LBj:p%UBj, &
     &  p%N,3,p%NT)
      real(C_DOUBLE), INTENT(INOUT) :: DiaTwrk(p%LBi:p%UBi, &
     &  p%LBj:p%UBj, &
     &                                       p%N,p%NT,p%NDT)
      integer, parameter :: r8=C_DOUBLE
      TYPE scheme_type
        logical :: UPSTREAM3=.FALSE., AKIMA4=.FALSE.
        logical :: CENTERED4=.FALSE., SPLIT_U3=.FALSE.
      END TYPE
      TYPE domain_type
        logical :: Western_Edge(0:0), Eastern_Edge(0:0)
        logical :: Southern_Edge(0:0), Northern_Edge(0:0)
      END TYPE
      TYPE(scheme_type) :: Hadvection(p%NT,1)
      TYPE(domain_type) :: DOMAIN(1)
      logical :: EWperiodic(1), NSperiodic(1), CompositeGrid(4,1)
      integer, parameter :: iwest=1, ieast=2, isouth=3, inorth=4
      integer :: N(1), NT(1), NAT, NDT, ng, tile, nnew
      integer :: Istr, Iend, Jstr, Jend, IstrU, JstrV
      integer :: Istrm1, Iendp2, Jstrm1, Jendp2
      integer :: i, j, k, itrc, idiag, ltrc
      integer :: iTxadv, iTyadv, iThadv, iTvadv, iTvdif
      real(r8) :: dt(1), cc1, cc2, cc3
      real(r8) :: eps=1.0e-16_r8, eps1=1.0e-12_r8
      real(r8) :: cff, cff1, cff2, cff3, a1, b1, betaL, betaR, &
     &  betaD, betaU
      real(r8) :: rL, rR, rD, rU, rkaL, rkaR, rkaD, rkaU, sw, sw_xi, &
     &  sw_eta
      real(r8) :: oHz(p%LBi:p%UBi,p%LBj:p%UBj,p%N)
      real(r8) :: gradX(p%LBi:p%UBi), KaX(p%LBi:p%UBi), &
     &  oKaX(p%LBi:p%UBi)
      real(r8) :: gradE(p%LBj:p%UBj), KaE(p%LBj:p%UBj), &
     &  oKaE(p%LBj:p%UBj)
      real(r8) :: gradZ(0:p%N), KaZ(0:p%N), oKaZ(0:p%N)
      real(r8) :: FX(p%LBi:p%UBi,p%LBj:p%UBj), FE(p%LBi:p%UBi, &
     &  p%LBj:p%UBj)
      real(r8) :: curv(p%LBi:p%UBi,p%LBj:p%UBj), grad(p%LBi:p%UBi, &
     &  p%LBj:p%UBj)
      real(r8) :: CF(p%LBi:p%UBi,0:p%N), DC(p%LBi:p%UBi,0:p%N)
      real(r8) :: FC(p%LBi:p%UBi,0:p%N), BC(p%LBi:p%UBi,0:p%N)
      ng=1
      tile=0
      N=p%N
      NT=p%NT
      NAT=p%NAT
      NDT=p%NDT
      nnew=p%nnew
      dt=p%dt
      cc1=p%cc1
      cc2=p%cc2
      cc3=p%cc3
      Istr=p%Istr
      Iend=p%Iend
      Jstr=p%Jstr
      Jend=p%Jend
      IstrU=Istr
      JstrV=Jstr
      Istrm1=Istr-1
      Iendp2=Iend+2
      Jstrm1=Jstr-1
      Jendp2=Jend+2
      EWperiodic=p%periodic_ew.ne.0
      NSperiodic=p%periodic_ns.ne.0
      DOMAIN(1)%Western_Edge=p%west.ne.0
      DOMAIN(1)%Eastern_Edge=p%east.ne.0
      DOMAIN(1)%Southern_Edge=p%south.ne.0
      DOMAIN(1)%Northern_Edge=p%north.ne.0
      CompositeGrid(:,1)=[p%composite_w.ne.0,p%composite_e.ne.0, &
     &                    p%composite_s.ne.0,p%composite_n.ne.0]
      iTxadv=p%xadv
      iTyadv=p%yadv
      iThadv=p%hadv
      iTvadv=p%vadv
      iTvdif=p%vdif
      oHz=1.0_r8/Hz
      DO itrc=1,NT(ng)
        Hadvection(itrc,ng)%UPSTREAM3=schemes(itrc).eq.0
        DO k=1,N(ng)
          IF (schemes(itrc).eq.1) THEN
            DO j=Jstr,Jend
              DO i=IstrU-1,Iendp2
                cff=0.125_r8*(pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))* &
     &              dt(ng)
                cff1=cff*(oHz(i-1,j,k)+oHz(i,j,k))
                gradX(i)=t(i,j,k,3,itrc)-t(i-1,j,k,3,itrc)
                KaX(i)=1.0_r8-ABS(Huon(i,j,k)*cff1)
# ifdef MASKING
                gradX(i)=gradX(i)*umask(i,j)
                KaX(i)=KaX(i)*umask(i,j)
# endif
              END DO
              IF (.not.EWperiodic(ng)) THEN
                IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                  IF (Huon(Istr,j,k).ge.0.0_r8) THEN
                    gradX(Istr-1)=0.0_r8
                    KaX(Istr-1)=0.0_r8
                  END IF
                END IF
                IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                  IF (Huon(Iend+1,j,k).lt.0.0_r8) THEN
                    gradX(Iend+2)=0.0_r8
                    KaX(Iend+2)=0.0_r8
                  END IF
                END IF
              END IF
              DO i=Istr,Iend+1
                IF (KaX(i).le.eps1) THEN
                  oKaX(i)=0.0_r8
                ELSE
                  oKaX(i)=1.0_r8/MAX(KaX(i),eps1)
                END IF
                IF (Huon(i,j,k).ge.0.0_r8) THEN
                  IF (ABS(gradX(i)).le.eps1) THEN
                    rL=0.0_r8
                    rkaL=0.0_r8
                  ELSE
                    rL=gradX(i-1)/gradX(i)
                    rkaL=KaX(i-1)*oKaX(i)
                  END IF
                  a1= cc1*KaX(i)+cc2-cc3*oKaX(i)
                  b1=-cc1*KaX(i)+cc2+cc3*oKaX(i)
                  betaL=a1+b1*rL
                  cff=0.5_r8*MAX(0.0_r8, &
     &                           MIN(2.0_r8, 2.0_r8*rL*rkaL, betaL))* &
     &                gradX(i)*KaX(i)
# ifdef MASKING
                  ii=MAX(i-2,0)
                  cff=cff*rmask(ii,j)
# endif
                  sw_xi=t(i-1,j,k,3,itrc)+cff
                ELSE
                  IF (ABS(gradX(i)).le.eps1) THEN
                    rR=0.0_r8
                    rkaR=0.0_r8
                  ELSE
                    rR=gradX(i+1)/gradX(i)
                    rkaR=KaX(i+1)*oKaX(i)
                  END IF
                  a1= cc1*KaX(i)+cc2-cc3*oKaX(i)
                  b1=-cc1*KaX(i)+cc2+cc3*oKaX(i)
                  betaR=a1+b1*rR
                  cff=0.5_r8*MAX(0.0_r8, &
     &                           MIN(2.0_r8, 2.0_r8*rR*rkaR, betaR))* &
     &                gradX(i)*KaX(i)
# ifdef MASKING
                  ii=MIN(i+1,Lm(ng)+1)
                  cff=cff*rmask(ii,j)
# endif
                  sw_xi=t(i,j,k,3,itrc)-cff
                END IF
                FX(i,j)=sw_xi*Huon(i,j,k)
              END DO
            END DO
!
            DO i=Istr,Iend
              DO j=JstrV-1,Jendp2
                cff=0.125_r8*(pn(i,j)+pn(i,j-1))*(pm(i,j)+pm(i,j-1))* &
     &              dt(ng)
                cff1=cff*(oHz(i,j,k)+oHz(i,j-1,k))
                gradE(j)=t(i,j,k,3,itrc)-t(i,j-1,k,3,itrc)
                KaE(j)=1.0_r8-ABS(Hvom(i,j,k)*cff1)
# ifdef MASKING
                gradE(j)=gradE(j)*vmask(i,j)
                KaE(j)=KaE(j)*vmask(i,j)
# endif
              END DO
              IF (.not.NSperiodic(ng)) THEN
                IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                  IF (Hvom(i,Jstr,k).ge.0.0_r8) THEN
                    gradE(Jstr-1)=0.0_r8
                    KaE(Jstr-1)=0.0_r8
                  END IF
                END IF
                IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                  IF (Hvom(i,Jend+1,k).lt.0.0_r8) THEN
                    gradE(Jend+2)=0.0_r8
                    KaE(Jend+2)=0.0_r8
                  END IF
                END IF
              END IF
              DO j=Jstr,Jend+1
                IF (KaE(j).le.eps1) THEN
                  oKaE(j)=0.0_r8
                ELSE
                  oKaE(j)=1.0_r8/MAX(KaE(j),eps1)
                END IF
                IF (Hvom(i,j,k).ge.0.0_r8) THEN
                  IF (ABS(gradE(j)).le.eps1) THEN
                    rD=0.0_r8
                    rkaD=0.0_r8
                  ELSE
                    rD=gradE(j-1)/gradE(j)
                    rkaD=KaE(j-1)*oKaE(j)
                  END IF
                  a1= cc1*KaE(j)+cc2-cc3*oKaE(j)
                  b1=-cc1*KaE(j)+cc2+cc3*oKaE(j)
                  betaD=a1+b1*rD
                  cff=0.5_r8*MAX(0.0_r8, &
     &                           MIN(2.0_r8, 2.0_r8*rD*rkaD, betaD))* &
     &                gradE(j)*KaE(j)
# ifdef MASKING
                  jj=MAX(j-2,0)
                  cff=cff*rmask(i,jj)
# endif
                  sw_eta=t(i,j-1,k,3,itrc)+cff
                ELSE
                  IF (ABS(gradE(j)).le.eps1) THEN
                    rU=0.0_r8
                    rkaU=0.0_r8
                  ELSE
                    rU=gradE(j+1)/gradE(j)
                    rkaU=KaE(j+1)*oKaE(j)
                  END IF
                  a1= cc1*KaE(j)+cc2-cc3*oKaE(j)
                  b1=-cc1*KaE(j)+cc2+cc3*oKaE(j)
                  betaU=a1+b1*rU
                  cff=0.5*MAX(0.0_r8, &
     &                        MIN(2.0_r8, 2.0_r8*rU*rkaU, betaU))* &
     &                gradE(j)*KaE(j)
# ifdef MASKING
                  jj=MIN(j+1,Mm(ng)+1)
                  cff=cff*rmask(i,jj)
# endif
                  sw_eta=t(i,j,k,3,itrc)-cff
                END IF
                FE(i,j)=sw_eta*Hvom(i,j,k)
              END DO
            END DO
!
          ELSE
            DO j=Jstr,Jend
              DO i=Istrm1,Iendp2
                FX(i,j)=t(i  ,j,k,3,itrc)- &
     &                  t(i-1,j,k,3,itrc)
# ifdef MASKING
                FX(i,j)=FX(i,j)*umask(i,j)
# endif
              END DO
            END DO
            IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Istr-1,j)=FX(Istr,j)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Iend+2,j)=FX(Iend+1,j)
                END DO
              END IF
            END IF
!
            DO j=Jstr,Jend
              DO i=Istr-1,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FX(i+1,j)-FX(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FX(i+1,j)*FX(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or. &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FX(i+1,j)+FX(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8* &
     &                    (t(i-1,j,k,3,itrc)+ &
     &                     t(i  ,j,k,3,itrc))- &
     &                    cff1*(curv(i-1,j)*MAX(Huon(i,j,k),0.0_r8)+ &
     &                          curv(i  ,j)*MIN(Huon(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or. &
     &                   (Hadvection(itrc,ng)%CENTERED4).or. &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8* &
     &                    (t(i-1,j,k,3,itrc)+ &
     &                     t(i  ,j,k,3,itrc)- &
     &                     cff2*(grad(i  ,j)- &
     &                           grad(i-1,j)))
                END IF
              END DO
            END DO
!
            DO j=Jstrm1,Jendp2
              DO i=Istr,Iend
                FE(i,j)=t(i,j  ,k,3,itrc)- &
     &                  t(i,j-1,k,3,itrc)
# ifdef MASKING
                FE(i,j)=FE(i,j)*vmask(i,j)
# endif
              END DO
            END DO
            IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jstr-1)=FE(i,Jstr)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jend+2)=FE(i,Jend+1)
                END DO
              END IF
            END IF
!
            DO j=Jstr-1,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FE(i,j+1)-FE(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FE(i,j+1)*FE(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or. &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FE(i,j+1)+FE(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8* &
     &                    (t(i,j-1,k,3,itrc)+ &
     &                     t(i,j  ,k,3,itrc))- &
     &                    cff1*(curv(i,j-1)*MAX(Hvom(i,j,k),0.0_r8)+ &
     &                          curv(i,j  )*MIN(Hvom(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or. &
     &                   (Hadvection(itrc,ng)%CENTERED4).or. &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8* &
     &                    (t(i,j-1,k,3,itrc)+ &
     &                     t(i,j  ,k,3,itrc)- &
     &                     cff2*(grad(i,j  )- &
     &                           grad(i,j-1)))
                END IF
              END DO
            END DO
          END IF
            DO j=Jstr,Jend
              DO i=Istr,Iend
                cff=dt(ng)*pm(i,j)*pn(i,j)
                cff1=cff*(FX(i+1,j)-FX(i,j))
                cff2=cff*(FE(i,j+1)-FE(i,j))
                cff3=cff1+cff2
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff3
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTxadv)=-cff1
                DiaTwrk(i,j,k,itrc,iTyadv)=-cff2
                DiaTwrk(i,j,k,itrc,iThadv)=-cff3
# endif
              END DO
            END DO
        END DO
      END DO
      DO itrc=1,NT(ng)
        DO j=Jstr,Jend
          IF (schemes(itrc).eq.1) THEN
            DO i=Istr,Iend
              KaZ(0)=0.0_r8
              oKaZ(0)=0.0_r8
              gradZ(0)=0.0_r8
              DO k=1,N(ng)-1
                cff=pm(i,j)*pn(i,j)*dt(ng)
# ifdef WEC_VF
                KaZ(k)=1.0_r8-ABS(cff*(W(i,j,k)+W_stokes(i,j,k))/ &
     &                            (z_r(i,j,k+1)-z_r(i,j,k)))
# else
                KaZ(k)=1.0_r8-ABS(cff*W(i,j,k)/ &
     &                            (z_r(i,j,k+1)-z_r(i,j,k)))
# endif
                oKaZ(k)=1.0_r8/KaZ(k)
                gradZ(k)=t(i,j,k+1,3,itrc)-t(i,j,k,3,itrc)
              END DO
              KaZ(N(ng))=0.0_r8
              okaZ(N(ng))=0.0_r8
              gradZ(N(ng))=0.0_r8
!
              DO k=1,N(ng)-1
# ifdef WEC_VF
                cff1=W(i,j,k)+W_stokes(i,j,k)
# else
                cff1=W(i,j,k)
# endif
                IF ((k.eq.1).and.(cff1.ge.0.0_r8)) THEN
                  FC(i,k)=cff1*t(i,j,k,3,itrc)
                ELSE IF ((k.eq.N(ng)-1).and.(cff1.lt.0.0_r8)) THEN
                  FC(i,k)=cff1*t(i,j,k+1,3,itrc)
                ELSE
                  IF (cff1.ge.0) THEN
                    IF (ABS(gradZ(k)).le.eps1) THEN
                      rD=0.0_r8
                      rkaD=0.0_r8
                    ELSE
                      rD=gradZ(k-1)/gradZ(k)
                      rkaD=KaZ(k-1)*oKaZ(k)
                    END IF
                    a1= cc1*KaZ(k)+cc2-cc3*oKaZ(k)
                    b1=-cc1*KaZ(k)+cc2+cc3*oKaZ(k)
                    betaD=a1+b1*rD
                    cff=0.5_r8*MAX(0.0_r8, &
     &                             MIN(2.0_r8, 2.0_r8*rD*rkaD, &
     &  betaD))* &
     &                  gradZ(k)*KaZ(k)
                    sw=t(i,j,k,3,itrc)+cff
                  ELSE
                    IF (ABS(gradZ(k)).le.eps1) THEN
                      rU=0.0_r8
                      rkaU=0.0_r8
                    ELSE
                      rU=gradZ(k+1)/gradZ(k)
                      rkaU=KaZ(k+1)*oKaZ(k)
                    END IF
                    a1= cc1*KaZ(k)+cc2-cc3*oKaZ(k)
                    b1=-cc1*KaZ(k)+cc2+cc3*oKaZ(k)
                    betaU=a1+b1*rU
                    cff=0.5_r8*MAX(0.0_r8, &
     &                             MIN(2.0_r8, 2.0_r8*rU*rkaU, &
     &  betaU))* &
     &                  gradZ(k)*KaZ(k)
                    sw=t(i,j,k+1,3,itrc)-cff
                  END IF
                  FC(i,k)=cff1*sw
                END IF
              END DO
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE
            cff1=0.5_r8
            cff2=7.0_r8/12.0_r8
            cff3=1.0_r8/12.0_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
# ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))* &
# else
                FC(i,k)=W(i,j,k)* &
# endif
     &                  (cff2*(t(i,j,k  ,3,itrc)+ &
     &                         t(i,j,k+1,3,itrc))- &
     &                   cff3*(t(i,j,k-1,3,itrc)+ &
     &                         t(i,j,k+2,3,itrc)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
# ifdef WEC_VF
              FC(i,1)=(W(i,j,1)+W_stokes(i,j,1))* &
# else
              FC(i,1)=W(i,j,1)* &
# endif
     &                (cff1*t(i,j,1,3,itrc)+ &
     &                 cff2*t(i,j,2,3,itrc)- &
     &                 cff3*t(i,j,3,3,itrc))
# ifdef WEC_VF
              FC(i,N(ng)-1)=(W(i,j,N(ng)-1)+W_stokes(i,j,N(ng)-1))* &
# else
              FC(i,N(ng)-1)=W(i,j,N(ng)-1)* &
# endif
     &                    (cff1*t(i,j,N(ng)  ,3,itrc)+ &
     &                     cff2*t(i,j,N(ng)-1,3,itrc)- &
     &                     cff3*t(i,j,N(ng)-2,3,itrc))
              FC(i,N(ng))=0.0_r8
            END DO
          END IF
            DO i=Istr,Iend
              CF(i,0)=dt(ng)*pm(i,j)*pn(i,j)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=CF(i,0)*(FC(i,k)-FC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff1
# ifdef SPLINES_VDIFF
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*oHz(i,j,k)
# endif
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=-cff1
                DO idiag=1,NDT
                  DiaTwrk(i,j,k,itrc,idiag)=DiaTwrk(i,j,k,itrc, &
     &  idiag)* &
     &                                      oHz(i,j,k)
                END DO
# endif
              END DO
            END DO
        END DO
      END DO
      DO j=Jstr,Jend
        DO itrc=1,NT(ng)
          ltrc=MIN(NAT,itrc)
            cff1=1.0_r8/6.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=cff1*Hz(i,j,k  )- &
     &                  dt(ng)*Akt(i,j,k-1,ltrc)*oHz(i,j,k  )
                CF(i,k)=cff1*Hz(i,j,k+1)- &
     &                  dt(ng)*Akt(i,j,k+1,ltrc)*oHz(i,j,k+1)
              END DO
            END DO
            DO i=Istr,Iend
              CF(i,0)=0.0_r8
              DC(i,0)=0.0_r8
            END DO
!
!  LU decomposition and forward substitution.
!
            cff1=1.0_r8/3.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                BC(i,k)=cff1*(Hz(i,j,k)+Hz(i,j,k+1))+ &
     &                  dt(ng)*Akt(i,j,k,ltrc)*(oHz(i,j,k)+oHz(i,j,k+1))
                cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
                CF(i,k)=cff*CF(i,k)
                DC(i,k)=cff*(t(i,j,k+1,nnew,itrc)-t(i,j,k,nnew,itrc)- &
     &                       FC(i,k)*DC(i,k-1))
              END DO
            END DO
!
!  Backward substitution.
!
            DO i=Istr,Iend
              DC(i,N(ng))=0.0_r8
            END DO
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              END DO
            END DO
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                DC(i,k)=DC(i,k)*Akt(i,j,k,ltrc)
                cff1=dt(ng)*oHz(i,j,k)*(DC(i,k)-DC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+cff1
#  ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvdif)=DiaTwrk(i,j,k,itrc, &
     &  iTvdif)+ &
     &                                     cff1
#  endif
              END DO
            END DO
        END DO
      END DO
      END SUBROUTINE reference_step3d_t
