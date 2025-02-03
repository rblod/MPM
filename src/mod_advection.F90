!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. For details, see: http://www.cecill.info.
!===============================================================================

module mod_advection
  use mod_precision
  use mod_grid
  implicit none
contains

  subroutine advection_centered2(field, adv_field)
    real(wp), intent(in) :: field(:,:)
    real(wp), intent(out) :: adv_field(:,:)
    integer :: i, j, imax, jmax
    imax = size(field, 1)
    jmax = size(field, 2)
    adv_field(:,:) = 0.0_wp
    !$acc parallel loop collapse(2) present(field, adv_field) async(1)
    do j = 2, jmax-1
       do i = 2, imax-1
         adv_field(i,j) = (field(i+1,j) - field(i-1,j))/(2.0_wp) + &
                          (field(i,j+1) - field(i,j-1))/(2.0_wp)
       end do
    end do
    !$acc wait(1)
  end subroutine advection_centered2

  subroutine advection_upwind3(field, adv_field)
    real(wp), intent(in) :: field(:,:)
    real(wp), intent(out) :: adv_field(:,:)
    integer :: i, j, imax, jmax
    imax = size(field, 1)
    jmax = size(field, 2)
    adv_field(:,:) = 0.0_wp
    !$acc parallel loop collapse(2) present(field, adv_field) async(2)
    do j = 3, jmax-1
       do i = 3, imax-1
         adv_field(i,j) = (-field(i-2,j) + 6.0_wp*field(i-1,j) - 3.0_wp*field(i,j) - 2.0_wp*field(i+1,j))/6.0_wp + &
                          (-field(i,j-2) + 6.0_wp*field(i,j-1) - 3.0_wp*field(i,j) - 2.0_wp*field(i,j+1))/6.0_wp
       end do
    end do
    !$acc wait(2)
  end subroutine advection_upwind3

  subroutine advection_update(field, adv_field, scheme)
    real(wp), intent(in) :: field(:,:)
    real(wp), intent(out) :: adv_field(:,:)
    character(len=*), intent(in) :: scheme
    if (trim(scheme) == "centered2") then
       call advection_centered2(field, adv_field)
    else if (trim(scheme) == "upwind3") then
       call advection_upwind3(field, adv_field)
    else
       call log_message(LOG_ERROR, "Unknown advection scheme: " // trim(scheme))
       stop 1
    end if
  end subroutine advection_update

end module mod_advection