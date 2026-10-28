! Numerical check for k_step2d_pressure_grad against a Fortran reference
! computed in the same program, with the same array shapes step2d_tile
! uses (grid bounds for the inputs, private bounds for the scratch).
program test_pgrd
  use iso_c_binding
  use mod_kinds,         only : r8
  use kokkos_step2d_mod, only : kokkos_tile_bounds, k_step2d_pressure_grad
  implicit none

  integer, parameter :: LBi = 0, UBi = 10, LBj = 0, UBj = 8
  integer, parameter :: IminS = LBi - 3, ImaxS = UBi + 3
  integer, parameter :: JminS = LBj - 3, JmaxS = UBj + 3
  integer, parameter :: NDM2d = 6, M2pgrd = 4

  real(r8), allocatable, target :: h(:,:), on_u(:,:), om_v(:,:), rhoA(:,:)
  real(r8), allocatable, target :: gzeta(:,:), gzeta2(:,:), gzetaSA(:,:), zwrk(:,:)
  real(r8), allocatable, target :: rhs_u(:,:), rhs_v(:,:)
  real(r8), allocatable, target :: DiaU(:,:,:), DiaV(:,:,:)
  real(r8), allocatable :: rrhs_u(:,:), rrhs_v(:,:), rDiaU(:,:,:), rDiaV(:,:,:)

  type(kokkos_tile_bounds) :: tl
  integer :: i, j, id
  real(r8) :: cff1, cff2, g, maxerr

  allocate(h(LBi:UBi,LBj:UBj));      allocate(on_u(LBi:UBi,LBj:UBj))
  allocate(om_v(LBi:UBi,LBj:UBj));   allocate(rhoA(LBi:UBi,LBj:UBj))
  allocate(gzeta(IminS:ImaxS,JminS:JmaxS));  allocate(gzeta2(IminS:ImaxS,JminS:JmaxS))
  allocate(gzetaSA(IminS:ImaxS,JminS:JmaxS)); allocate(zwrk(IminS:ImaxS,JminS:JmaxS))
  allocate(rhs_u(IminS:ImaxS,JminS:JmaxS));   allocate(rhs_v(IminS:ImaxS,JminS:JmaxS))
  allocate(DiaU(IminS:ImaxS,JminS:JmaxS,NDM2d-1))
  allocate(DiaV(IminS:ImaxS,JminS:JmaxS,NDM2d-1))
  allocate(rrhs_u(IminS:ImaxS,JminS:JmaxS));  allocate(rrhs_v(IminS:ImaxS,JminS:JmaxS))
  allocate(rDiaU(IminS:ImaxS,JminS:JmaxS,NDM2d-1))
  allocate(rDiaV(IminS:ImaxS,JminS:JmaxS,NDM2d-1))

  do j = LBj, UBj
    do i = LBi, UBi
      h(i,j)    = 20.0_r8 + 0.5_r8*i + 0.25_r8*j
      on_u(i,j) = 1.0_r8/(1000.0_r8 + i)
      om_v(i,j) = 1.0_r8/(1100.0_r8 + j)
      rhoA(i,j) = 1.0e-3_r8*(i - j)
    end do
  end do
  do j = JminS, JmaxS
    do i = IminS, ImaxS
      gzeta(i,j)   = 0.1_r8*i - 0.2_r8*j
      gzeta2(i,j)  = 0.01_r8*(i - j)**2
      gzetaSA(i,j) = 0.3_r8*i + 0.05_r8*j
      zwrk(i,j)    = 0.02_r8*(i + j)
    end do
  end do
  rhs_u = 0.0_r8; rhs_v = 0.0_r8; rrhs_u = 0.0_r8; rrhs_v = 0.0_r8
  DiaU = 0.0_r8; DiaV = 0.0_r8; rDiaU = 0.0_r8; rDiaV = 0.0_r8

  tl%Istr = 1;  tl%Iend = UBi - 1
  tl%Jstr = 1;  tl%Jend = UBj - 1
  tl%IstrU = 1; tl%JstrV = 1
  tl%IstrR = 0; tl%JstrR = 0; tl%IendR = UBi - 1; tl%JendR = UBj - 1
  tl%Istrm1 = 0; tl%Jstrm1 = 0
  tl%Iendp1 = UBi; tl%Jendp1 = UBj
  tl%Iendp2 = UBi; tl%Jendp2 = UBj
  tl%IstrUm1 = 0; tl%JstrVm1 = 0
  tl%IstrUm2 = -1; tl%JstrVm2 = -1
  tl%UBk = 16

  g = 9.81_r8
  cff1 = 0.5_r8*g
  cff2 = 1.0_r8/3.0_r8

  ! Reference: step2d_LF_AM3.h lines 1205-1265.
  do j = tl%Jstr, tl%Jend
    do i = tl%IstrU, tl%Iend
      rrhs_u(i,j) = cff1*on_u(i,j)*                                    &
     &  ((h(i-1,j)+h(i,j))*(gzeta(i-1,j)-gzeta(i,j)) +                 &
     &   (h(i-1,j)-h(i,j))*(gzetaSA(i-1,j)+gzetaSA(i,j) +              &
     &    cff2*(rhoA(i-1,j)-rhoA(i,j))*(zwrk(i-1,j)-zwrk(i,j))) +      &
     &   (gzeta2(i-1,j)-gzeta2(i,j)))
      rDiaU(i,j,M2pgrd) = rrhs_u(i,j)
    end do
    if (j >= tl%JstrV) then
      do i = tl%Istr, tl%Iend
        rrhs_v(i,j) = cff1*om_v(i,j)*                                  &
       &  ((h(i,j-1)+h(i,j))*(gzeta(i,j-1)-gzeta(i,j)) +               &
       &   (h(i,j-1)-h(i,j))*(gzetaSA(i,j-1)+gzetaSA(i,j) +            &
       &    cff2*(rhoA(i,j-1)-rhoA(i,j))*(zwrk(i,j-1)-zwrk(i,j))) +    &
       &   (gzeta2(i,j-1)-gzeta2(i,j)))
        rDiaV(i,j,M2pgrd) = rrhs_v(i,j)
      end do
    end if
  end do

  call k_step2d_pressure_grad(1, 0, LBi, UBi, LBj, UBj,                &
                              IminS, ImaxS, JminS, JmaxS,              &
                              c_loc(h), c_loc(on_u), c_loc(om_v),      &
                              c_loc(gzeta), c_loc(gzeta2),             &
                              c_loc(gzetaSA), c_loc(rhoA),             &
                              c_loc(zwrk),                             &
                              c_loc(rhs_u), c_loc(rhs_v),              &
                              c_loc(DiaU), c_loc(DiaV),                &
                              NDM2d, M2pgrd, g, tl)

  maxerr = 0.0_r8
  do j = tl%Jstr, tl%Jend
    do i = tl%IstrU, tl%Iend
      maxerr = max(maxerr, abs(rhs_u(i,j) - rrhs_u(i,j)))
    end do
    do i = tl%Istr, tl%Iend
      if (j >= tl%JstrV) maxerr = max(maxerr, abs(rhs_v(i,j) - rrhs_v(i,j)))
    end do
  end do
  write(*,'(a,es12.4)') 'pgrd max|diff| = ', maxerr
  if (maxerr < 1.0e-14_r8) then
    write(*,'(a)') 'PASS'
  else
    write(*,'(a)') 'FAIL'
    do j = tl%Jstr, min(tl%Jstr+1, tl%Jend)
      do i = tl%IstrU, min(tl%IstrU+2, tl%Iend)
        write(*,'(a,2i3,2es22.14)') '  u ', i, j, rhs_u(i,j), rrhs_u(i,j)
      end do
    end do
  end if
end program
