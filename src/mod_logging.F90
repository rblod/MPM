!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. You can use, modify, and/or redistribute it under the
! terms of the CeCILL-C license. For details, see: http://www.cecill.info.
!===============================================================================

module mod_logging
  use mod_precision
  use mod_parameters, only: logOpts
  use mpi            ! Utilisation de l'ancien module MPI pour l'I/O
  implicit none

  ! Définition des niveaux de log
  integer, parameter :: LOG_TRACE = 0, LOG_DEBUG = 1, LOG_INFO = 2, LOG_WARN = 3, LOG_ERROR = 4, LOG_CRITICAL = 5

  ! Déclaration de la variable de niveau de log (non parameter)
  integer :: current_log_level

  ! Variable pour le fichier log MPI
  integer :: mpi_log_file

contains

  !> @brief Returns a timestamp string.
  function get_timestamp() result(timestamp)
    character(len=30) :: timestamp
    integer :: values(8)
    call date_and_time(values=values)
    write(timestamp, '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
         values(1), values(2), values(3), values(5), values(6), values(7)
  end function get_timestamp

  !> @brief Initializes logging: opens the log file via MPI I/O and sets current_log_level.
  subroutine init_log()
    integer :: ierr
    ! Ouvre le fichier de log en mode écriture (MPI_MODE_WRONLY + MPI_MODE_CREATE)
    call MPI_File_open(MPI_COMM_WORLD, trim(logOpts%log_file), MPI_MODE_WRONLY+MPI_MODE_CREATE, MPI_INFO_NULL, mpi_log_file, ierr)
    if (ierr /= MPI_SUCCESS) then
       print *, "Error opening log file via MPI I/O."
       stop 1
    end if
    ! Initialisation du niveau de log à partir de logOpts
    current_log_level = logOpts%log_level
  end subroutine init_log

  !> @brief Finalizes logging by closing the log file.
  subroutine finalize_log()
    integer :: ierr
    call MPI_File_close(mpi_log_file, ierr)
  end subroutine finalize_log

  !> @brief Logs a message if the log level is sufficient.
  subroutine log_message(level, msg)
    integer, intent(in) :: level
    character(len=*), intent(in) :: msg
    character(len=30) :: timestamp
    integer :: my_rank, ierr
    character(len=256) :: full_msg
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    timestamp = get_timestamp()
    if (level >= current_log_level) then
       select case (level)
       case (LOG_TRACE)
          write(full_msg, '(A, " [TRACE] ", A, " [Rank:", I0, "] ", A)') timestamp, "TRACE", my_rank, trim(msg)
       case (LOG_DEBUG)
          write(full_msg, '(A, " [DEBUG] ", A, " [Rank:", I0, "] ", A)') timestamp, "DEBUG", my_rank, trim(msg)
       case (LOG_INFO)
          write(full_msg, '(A, " [INFO ] ", A, " [Rank:", I0, "] ", A)') timestamp, "INFO", my_rank, trim(msg)
       case (LOG_WARN)
          write(full_msg, '(A, " [WARN ] ", A, " [Rank:", I0, "] ", A)') timestamp, "WARN", my_rank, trim(msg)
       case (LOG_ERROR)
          write(full_msg, '(A, " [ERROR] ", A, " [Rank:", I0, "] ", A)') timestamp, "ERROR", my_rank, trim(msg)
       case (LOG_CRITICAL)
          write(full_msg, '(A, " [CRIT ] ", A, " [Rank:", I0, "] ", A)') timestamp, "CRITICAL", my_rank, trim(msg)
       end select
       write(*,*) full_msg
       call MPI_File_write_ordered(mpi_log_file, full_msg, len_trim(full_msg), MPI_CHARACTER, MPI_STATUS_IGNORE, ierr)
    end if
  end subroutine log_message

  !> @brief Sets the current log level.
  subroutine set_log_level(level)
    integer, intent(in) :: level
    current_log_level = level
  end subroutine set_log_level

end module mod_logging
