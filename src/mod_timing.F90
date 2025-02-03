!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. For details, see: http://www.cecill.info.
!===============================================================================

module mod_timing
  use mod_precision
  use mpi                       ! Utilisation du module MPI traditionnel
  use mod_logging, only: LOG_INFO, log_message
  implicit none
  real(wp) :: t_start_global, t_end_global
contains

  !> @brief Starts the global timer.
  subroutine start_timer()
    integer :: ierr
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    t_start_global = MPI_Wtime()
  end subroutine start_timer

  !> @brief Stops the global timer, reduces the maximum elapsed time across all ranks, and logs it.
  subroutine stop_timer()
    real(wp) :: t_local, t_max
    integer :: ierr
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    t_end_global = MPI_Wtime()
    t_local = t_end_global - t_start_global
    call MPI_Reduce(t_local, t_max, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
    call log_message(LOG_INFO, "Maximum elapsed time across all ranks: " // trim(ftoa(t_max)) // " seconds.")
  end subroutine stop_timer

  !> @brief Converts a real number to a string.
  function ftoa(x) result(str)
    real(wp), intent(in) :: x
    character(len=32) :: str
    write(str, '(F8.4)') x
  end function ftoa

end module mod_timing
