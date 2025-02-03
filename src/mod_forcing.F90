!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. For details, see: http://www.cecill.info.
!===============================================================================

module mod_forcing
  use mod_precision
  use mod_parameters, only: physOpts
  implicit none
contains

  subroutine compute_double_gyre_forcing(t, dx, ny, local_nx, x_offset, tau_x, tau_y)
    implicit none
    real(wp), intent(in) :: t, dx
    integer, intent(in) :: ny, local_nx, x_offset
    real(wp), intent(out) :: tau_x(:,:), tau_y(:,:)
    integer :: i, j
    real(wp) :: x, y, f_val, df_dx, pi
    pi = 4.0_wp * atan(1.0_wp)
    do j = 1, ny
       do i = 1, local_nx
          x = (i - 0.5_wp + x_offset)*dx
          y = (j - 0.5_wp)*dx
          f_val = physOpts%eps*sin(physOpts%omega*t)*x**2 + (1.0_wp - 2.0_wp*physOpts%eps*sin(physOpts%omega*t))*x
          df_dx = 2.0_wp*physOpts%eps*sin(physOpts%omega*t)*x + (1.0_wp - 2.0_wp*physOpts%eps*sin(physOpts%omega*t))
          tau_x(i,j) = -physOpts%A * sin(pi*f_val) * pi * cos(pi*y)
          tau_y(i,j) =  physOpts%A * cos(pi*f_val) * pi * sin(pi*y) * df_dx
       end do
    end do
  end subroutine compute_double_gyre_forcing

end module mod_forcing