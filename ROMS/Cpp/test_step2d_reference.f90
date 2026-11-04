! Independent scalar Fortran oracle for the LF/AM3 momentum update.
! Keep the assignments and grouping of step2d_LF_AM3.h; no C++ helpers.
subroutine reference_step2d_momentum(lo,hi,jlo,jhi,istr,iend,jstr,jend, &
                                    istru,jstrv,kstp,knew,predictor,first, &
                                    dtfast,zeta,h,pm,pn,rubar,rvbar, &
                                    rhsu,rhsv,dnew,ubar,vbar) bind(C)
  use iso_c_binding
  implicit none
  integer(c_int), value :: lo,hi,jlo,jhi,istr,iend,jstr,jend,istru,jstrv
  integer(c_int), value :: kstp,knew,predictor,first
  real(c_double), value :: dtfast
  real(c_double), intent(in) :: zeta(lo:hi,jlo:jhi,3), h(lo:hi,jlo:jhi)
  real(c_double), intent(in) :: pm(lo:hi,jlo:jhi), pn(lo:hi,jlo:jhi)
  real(c_double), intent(in) :: rubar(lo:hi,jlo:jhi,2),rvbar(lo:hi,jlo:jhi,2)
  real(c_double), intent(in) :: rhsu(lo:hi,jlo:jhi),rhsv(lo:hi,jlo:jhi)
  real(c_double), intent(in) :: dnew(lo:hi,jlo:jhi)
  real(c_double), intent(inout) :: ubar(lo:hi,jlo:jhi,3),vbar(lo:hi,jlo:jhi,3)
  real(c_double) :: dstp(lo:hi,jlo:jhi),cff,cff1,cff2,cff3,fac
  integer :: i,j,ptsk
  ptsk=3-kstp
  do j=jstrv-1,jend
    do i=istru-1,iend
      dstp(i,j)=zeta(i,j,kstp)+h(i,j)
    end do
  end do
  if(first/=0.or.predictor/=0) then
    cff1=dtfast
    if(first/=0) cff1=0.5_c_double*dtfast
    do j=jstr,jend
      do i=istru,iend
        cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        fac=1.0_c_double/(dnew(i,j)+dnew(i-1,j))
        ubar(i,j,knew)=(ubar(i,j,kstp)*(dstp(i,j)+dstp(i-1,j))+ &
                       cff*cff1*rhsu(i,j))*fac
      end do
    end do
    do j=jstrv,jend
      do i=istr,iend
        cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
        fac=1.0_c_double/(dnew(i,j)+dnew(i,j-1))
        vbar(i,j,knew)=(vbar(i,j,kstp)*(dstp(i,j)+dstp(i,j-1))+ &
                       cff*cff1*rhsv(i,j))*fac
      end do
    end do
  else
    cff1=0.5_c_double*dtfast*5.0_c_double/12.0_c_double
    cff2=0.5_c_double*dtfast*8.0_c_double/12.0_c_double
    cff3=0.5_c_double*dtfast*1.0_c_double/12.0_c_double
    do j=jstr,jend
      do i=istru,iend
        cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        fac=1.0_c_double/(dnew(i,j)+dnew(i-1,j))
        ubar(i,j,knew)=(ubar(i,j,kstp)*(dstp(i,j)+dstp(i-1,j))+ &
                       cff*(cff1*rhsu(i,j)+cff2*rubar(i,j,kstp)- &
                            cff3*rubar(i,j,ptsk)))*fac
      end do
    end do
    do j=jstrv,jend
      do i=istr,iend
        cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
        fac=1.0_c_double/(dnew(i,j)+dnew(i,j-1))
        vbar(i,j,knew)=(vbar(i,j,kstp)*(dstp(i,j)+dstp(i,j-1))+ &
                       cff*(cff1*rhsv(i,j)+cff2*rvbar(i,j,kstp)- &
                            cff3*rvbar(i,j,ptsk)))*fac
      end do
    end do
  end if
end subroutine reference_step2d_momentum
