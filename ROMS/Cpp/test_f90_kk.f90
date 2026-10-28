! Interoperability test for the Kokkos step2d interface.
!
! Builds TileBounds on the Fortran side, calls the C++ kernel with
! C_LOC pointers, and compares against a plain-Fortran reference done in
! the same program.  A match proves that
!   (a) the derived type layout agrees on both sides,
!   (b) the Array2d/Array3d index mapping matches Fortran's layout,
!   (c) by-value argument passing works as declared.
program test_f90_kk
  use iso_c_binding
  use mod_kinds,         only : r8
  use kokkos_step2d_mod, only : kokkos_tile_bounds, k_step2d_depth_flux
  implicit none

  integer, parameter :: LBi = 0, UBi = 10, LBj = 0, UBj = 8, UBk = 3
  integer, parameter :: IminS = LBi - 3, ImaxS = UBi + 3
  integer, parameter :: JminS = LBj - 3, JmaxS = UBj + 3

  real(r8), allocatable, target :: zeta(:,:,:), h(:,:), on_u(:,:), om_v(:,:)
  real(r8), allocatable, target :: ubar(:,:,:), vbar(:,:,:)
  real(r8), allocatable, target :: Drhs(:,:), DUon(:,:), DVom(:,:)
  real(r8), allocatable :: rDrhs(:,:), rDUon(:,:), rDVom(:,:)

  type(kokkos_tile_bounds) :: tl
  integer :: i, j, k, krhs, bad
  real(r8) :: maxerr, cff, cff1

  allocate(zeta(LBi:UBi, LBj:UBj, 0:UBk))
  allocate(h(LBi:UBi, LBj:UBj))
  allocate(on_u(LBi:UBi, LBj:UBj))
  allocate(om_v(LBi:UBi, LBj:UBj))
  allocate(ubar(LBi:UBi, LBj:UBj, 0:UBk))
  allocate(vbar(LBi:UBi, LBj:UBj, 0:UBk))
  allocate(Drhs(IminS:ImaxS, JminS:JmaxS))
  allocate(DUon(IminS:ImaxS, JminS:JmaxS))
  allocate(DVom(IminS:ImaxS, JminS:JmaxS))
  allocate(rDrhs(IminS:ImaxS, JminS:JmaxS))
  allocate(rDUon(IminS:ImaxS, JminS:JmaxS))
  allocate(rDVom(IminS:ImaxS, JminS:JmaxS))

  ! Deterministic fill, identical to the C++ standalone check.
  do k = 0, UBk
    do j = LBj, UBj
      do i = LBi, UBi
        zeta(i,j,k) = 0.1_r8*i + 0.3_r8*j + 0.7_r8*k
        ubar(i,j,k) = 0.01_r8*i - 0.02_r8*j + 0.5_r8*k
        vbar(i,j,k) = -0.03_r8*i + 0.04_r8*j + 0.2_r8*k
      end do
    end do
  end do
  do j = LBj, UBj
    do i = LBi, UBi
      h(i,j)    = 20.0_r8 + 0.5_r8*i + 0.25_r8*j
      on_u(i,j) = 1.0_r8/(1000.0_r8 + i)
      om_v(i,j) = 1.0_r8/(1100.0_r8 + j)
    end do
  end do

  Drhs = 0.0_r8; DUon = 0.0_r8; DVom = 0.0_r8

  tl%Istr = 1; tl%Iend = UBi - 1
  tl%Jstr = 1; tl%Jend = UBj - 1
  tl%IstrU = 1; tl%JstrV = 1
  tl%IstrR = 0; tl%JstrR = 0; tl%IendR = UBi - 1; tl%JendR = UBj - 1
  tl%Istrm1 = 0; tl%Jstrm1 = 0
  tl%Iendp1 = UBi; tl%Jendp1 = UBj
  tl%Iendp2 = UBi; tl%Jendp2 = UBj
  tl%IstrUm1 = 0; tl%JstrVm1 = 0
  tl%IstrUm2 = -1; tl%JstrVm2 = -1
  tl%UBk = UBk

  krhs = 1

  ! --- reference, mirroring step2d_LF_AM3.h (non-distributed branch) ---
  do j = tl%JstrVm2-1, tl%Jendp2
    do i = tl%IstrUm2-1, tl%Iendp2
      rDrhs(i,j) = zeta(i,j,krhs) + h(i,j)
    end do
  end do
  ! Written as three statements, exactly as step2d_LF_AM3.h does
  ! (cff, cff1, then the product).  Collapsing them into one expression
  ! changes the floating-point association and loses the last bit.
  do j = tl%JstrVm2-1, tl%Jendp2
    do i = tl%IstrUm2, tl%Iendp2
      cff  = 0.5_r8*on_u(i,j)
      cff1 = cff*(rDrhs(i,j) + rDrhs(i-1,j))
      rDUon(i,j) = ubar(i,j,krhs)*cff1
    end do
  end do
  do j = tl%JstrVm2, tl%Jendp2
    do i = tl%IstrUm2-1, tl%Iendp2
      cff  = 0.5_r8*om_v(i,j)
      cff1 = cff*(rDrhs(i,j) + rDrhs(i,j-1))
      rDVom(i,j) = vbar(i,j,krhs)*cff1
    end do
  end do

  ! --- Kokkos kernel ---
  call k_step2d_depth_flux(1, 0, krhs, 1, LBi, UBi, LBj, UBj,          &
                           IminS, ImaxS, JminS, JmaxS,                 &
                           c_loc(zeta), c_loc(h), c_loc(on_u),         &
                           c_loc(om_v), c_loc(ubar), c_loc(vbar),      &
                           c_loc(Drhs), c_loc(DUon), c_loc(DVom),      &
                           UBk, tl)

  ! --- compare only the region the driver wrote ---
  maxerr = 0.0_r8; bad = 0
  do j = tl%JstrVm2-1, tl%Jendp2
    do i = tl%IstrUm2-1, tl%Iendp2
      maxerr = max(maxerr, abs(Drhs(i,j) - rDrhs(i,j)))
    end do
  end do
  do j = tl%JstrVm2-1, tl%Jendp2
    do i = tl%IstrUm2, tl%Iendp2
      maxerr = max(maxerr, abs(DUon(i,j) - rDUon(i,j)))
    end do
  end do
  do j = tl%JstrVm2, tl%Jendp2
    do i = tl%IstrUm2-1, tl%Iendp2
      maxerr = max(maxerr, abs(DVom(i,j) - rDVom(i,j)))
    end do
  end do

  call report('Drhs', Drhs, rDrhs)
  call report('DUon', DUon, rDUon)
  call report('DVom', DVom, rDVom)
  write(*,'(a,es12.4)') 'max |kokkos - fortran| = ', maxerr
  if (maxerr == 0.0_r8) then
    write(*,'(a)') 'PASS: bit-identical'
  else if (maxerr < 1.0e-12_r8) then
    write(*,'(a)') 'PASS: within 1e-12'
  else
    write(*,'(a)') 'FAIL'
  end if
contains
  subroutine report(name, got, want)
    character(len=*), intent(in) :: name
    real(r8), intent(in) :: got(IminS:ImaxS, JminS:JmaxS)
    real(r8), intent(in) :: want(IminS:ImaxS, JminS:JmaxS)
    integer :: ii, jj, bi, bj
    real(r8) :: e, be
    be = 0.0_r8; bi = 0; bj = 0
    do jj = JminS, JmaxS
      do ii = IminS, ImaxS
        e = abs(got(ii,jj) - want(ii,jj))
        if (e > be) then
          be = e; bi = ii; bj = jj
        end if
      end do
    end do
    write(*,'(a,a,a,es12.4,a,i0,a,i0,a,es22.15,a,es22.15)') &
      '  ', name, ': maxerr=', be, ' at (', bi, ',', bj, &
      ') got=', got(bi,bj), ' want=', want(bi,bj)
  end subroutine
end program
