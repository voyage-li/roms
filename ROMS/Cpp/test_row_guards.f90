! Runtime checks for the descriptor-based guard on the target compiler.
program test_row_guards
  use iso_c_binding, only : c_double
  use kokkos_loops_mod, only : kk_row_contiguous
  implicit none
  real(c_double), target :: a1(10), a2(4,6), a3(4,6,5)
  real(c_double), target :: a4(4,6,5,2), a5(4,6,5,2,3)
  integer :: checks
  checks=0
  call check(kk_row_contiguous(a1),'rank 1 contiguous')
  call check(.not.kk_row_contiguous(a1(1:10:2)),'rank 1 stride')
  call check(kk_row_contiguous(a1(2:2:2)),'rank 1 singleton')
  call check(kk_row_contiguous(a1(1:0)),'rank 1 empty')
  call check(kk_row_contiguous(a2),'rank 2 contiguous')
  call check(.not.kk_row_contiguous(a2(1:4:2,:)),'rank 2 stride')
  call check(.not.kk_row_contiguous(a2(1:2,:)),'rank 2 padded columns')
  call check(kk_row_contiguous(a2(:,2:2)),'rank 2 singleton axis')
  call check(kk_row_contiguous(a3),'rank 3 contiguous')
  call check(.not.kk_row_contiguous(a3(:,1:6:2,:)),'rank 3 stride')
  call check(kk_row_contiguous(a4),'rank 4 contiguous')
  call check(.not.kk_row_contiguous(a4(:,:,1:5:2,:)),'rank 4 stride')
  call check(kk_row_contiguous(a5),'rank 5 contiguous')
  call check(.not.kk_row_contiguous(a5(1:4:2,:,:,:,:)),'rank 5 stride')
  write(*,'(a,i0)') 'row stride guard PASS checks=',checks
contains
  subroutine check(ok,label)
    logical, intent(in) :: ok
    character(len=*), intent(in) :: label
    if (.not.ok) then
      write(*,'(a)') 'FAIL: '//label
      stop 1
    end if
    checks=checks+1
  end subroutine
end program
