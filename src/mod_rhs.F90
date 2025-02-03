!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. For details, see: http://www.cecill.info.
!===============================================================================

module mod_rhs
  use mod_precision
  use mod_grid
  use mod_advection
  use mod_parameters, only: simOpts, physOpts
  use mod_forcing
  implicit none
  real(wp), parameter :: g = 9.81_wp
contains

  subroutine compute_rhs(h, u, v, rhs_h, rhs_u, rhs_v, adv_scheme, dx, dy, t)
    implicit none
    real(wp), intent(in) :: h(:,:), u(:,:), v(:,:)
    real(wp), intent(out) :: rhs_h(:,:), rhs_u(:,:), rhs_v(:,:)
    character(len=*), intent(in) :: adv_scheme
    real(wp), intent(in) :: dx, dy, t
    integer :: i, j, imax, jmax
    real(wp), allocatable :: adv_term(:,:)
    real(wp), allocatable :: forcing_tau_x(:,:), forcing_tau_y(:,:)
    integer :: local_nx_local, local_ny_local

    imax = size(h,1)
    jmax = size(h,2)
    rhs_h(:,:) = 0.0_wp
    rhs_u(:,:) = 0.0_wp
    rhs_v(:,:) = 0.0_wp

    !$acc parallel loop collapse(2) present(h, u, v, rhs_h) async(3)
    do j = 2, jmax-1
       do i = 2, imax-1
          rhs_h(i,j) = -(( u(i,j)*h(i,j) - u(i-1,j)*h(i-1,j) )/dx + &
                         ( v(i,j)*h(i,j) - v(i,j-1)*h(i,j-1) )/dy)
       end do
    end do
    !$acc wait(3)

    allocate(adv_term(size(u,1), size(u,2)))
    adv_term(:,:) = 0.0_wp
    call advection_update(u, adv_term, adv_scheme)
    !$acc parallel loop collapse(2) present(u, h, rhs_u, adv_term, v) async(4)
    do j = 2, jmax-1
       do i = 2, size(u,1)-1
          rhs_u(i,j) = adv_term(i,j) - g*(h(i+1,j) - h(i,j))/dx + &
                       physOpts%f * 0.5_wp*( v(i,j) + v(i+1,j) ) - &
                       physOpts%r * u(i,j)
       end do
    end do
    !$acc wait(4)
    deallocate(adv_term)

    allocate(adv_term(size(v,1), size(v,2)))
    adv_term(:,:) = 0.0_wp
    call advection_update(v, adv_term, adv_scheme)
    !$acc parallel loop collapse(2) present(v, h, rhs_v, adv_term, u) async(5)
    do j = 2, jmax-1
       do i = 2, size(v,1)-1
          rhs_v(i,j) = adv_term(i,j) - g*(h(i,j+1) - h(i,j))/dy - &
                       physOpts%f * 0.5_wp*( u(i,j) + u(i,j+1) ) - &
                       physOpts%r * v(i,j)
       end do
    end do
    !$acc wait(5)
    deallocate(adv_term)

    local_nx_local = imax - 2
    local_ny_local = jmax - 2
    if (trim(simOpts%test_case) == "double_gyre") then
       allocate(forcing_tau_x(local_nx_local, local_ny_local))
       allocate(forcing_tau_y(local_nx_local, local_ny_local))
       call compute_double_gyre_forcing(t, dx, local_ny_local, local_nx_local, x_offset, forcing_tau_x, forcing_tau_y)
       !$acc parallel loop collapse(2) async(6)
       do j = 2, jmax-1
         do i = 2, imax-1
           rhs_u(i,j) = rhs_u(i,j) + 0.5_wp*(forcing_tau_x(i-1,j-1) + forcing_tau_x(i,j-1))
         end do
       end do
       !$acc wait(6)
       !$acc parallel loop collapse(2) async(7)
       do j = 2, jmax-1
         do i = 2, imax-1
           rhs_v(i,j) = rhs_v(i,j) + 0.5_wp*(forcing_tau_y(i-1,j-1) + forcing_tau_y(i-1,j))
         end do
       end do
       !$acc wait(7)
       deallocate(forcing_tau_x, forcing_tau_y)
    end if

  end subroutine compute_rhs

end module mod_rhs