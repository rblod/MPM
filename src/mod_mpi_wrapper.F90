!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. For details, see: http://www.cecill.info.
!===============================================================================

module mod_mpi_wrapper
  use mod_precision
  use mod_logging, only: LOG_ERROR, log_message
  use mpi    ! Utilisation du module MPI traditionnel pour obtenir les interfaces MPI
  implicit none
  ! Renommage de la constante pour éviter les conflits
  integer, parameter :: MY_MPI_MAX_ERROR_STRING = 256
contains

  !> @brief Checks the MPI error code and aborts if an error occurred.
  subroutine MPI_CHECK(ierr, func_name)
    integer, intent(in) :: ierr
    character(len=*), intent(in) :: func_name
    integer :: abort_ierror
    if (ierr /= MPI_SUCCESS) then
       call log_message(LOG_ERROR, "MPI error in " // trim(func_name) // ": " // trim(my_MPI_Error_string(ierr)))
       call MPI_Abort(MPI_COMM_WORLD, ierr, abort_ierror)
    end if
  end subroutine MPI_CHECK

  !> @brief Returns a descriptive string for a given MPI error code.
  function my_MPI_Error_string(ierr) result(err_str)
    integer, intent(in) :: ierr
    character(len=MY_MPI_MAX_ERROR_STRING) :: err_str_full
    character(len=:), allocatable :: err_str
    integer :: err_len, ierr2
    call MPI_Error_string(ierr, err_str_full, err_len, ierr2)
    err_str = err_str_full(1:err_len)
  end function my_MPI_Error_string

end module mod_mpi_wrapper
