!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. You can use, modify, and/or redistribute it under the
! terms of the CeCILL-C license. For details, see: http://www.cecill.info.
!===============================================================================

module mod_parameters
  use mod_precision
  implicit none

  ! Type for simulation options
  type :: sim_options
     integer :: nx = 400
     integer :: ny = 400
     real(wp) :: dx = 1.0_wp
     real(wp) :: dy = 1.0_wp
     real(wp) :: dt = 0.05_wp
     integer :: nsteps = 0        ! Will be computed from the dates if 0.
     character(len=20) :: adv_scheme = "upwind3"
     character(len=20) :: test_case = "double_gyre"
     integer :: procs_x = 2
     integer :: procs_y = 2
     integer :: nt = 1
     character(len=20) :: time_integrator = "rk3"
  end type sim_options

  ! Type for output options
  type :: out_options
     character(len=20) :: output_format = "netcdf"
     character(len=20) :: sim_start_date = "2025-01-01"
     character(len=20) :: sim_end_date   = "2025-01-02"
  end type out_options

  ! Type for physical options
  type :: phys_options
     real(wp) :: f = 1.0e-4_wp
     real(wp) :: r = 1.0e-3_wp
     real(wp) :: A = 0.1_wp
     real(wp) :: eps = 0.25_wp
     real(wp) :: omega = 0.62831853_wp
  end type phys_options

  ! Type for logging options
  type :: log_options
     integer :: log_level = 0    ! 0=TRACE, 1=DEBUG, 2=INFO, 3=WARN, 4=ERROR, 5=CRITICAL
     character(len=128) :: log_file = "global_log.txt"
     logical :: log_to_file = .true.
  end type log_options

  ! Déclaration des variables globales
  type(sim_options)   :: simOpts
  type(out_options)   :: outOpts
  type(phys_options)  :: physOpts
  type(log_options)   :: logOpts

  ! Déclaration des namelists avec des noms de groupe différents des types.
  namelist /sim_opts/ simOpts
  namelist /out_opts/ outOpts
  namelist /phys_opts/ physOpts
  namelist /log_opts/ logOpts

contains

  !> @brief Reads parameters from a namelist file.
  subroutine read_params(filename)
    character(len=*), intent(in) :: filename
    integer :: ios
    open(unit=10, file=filename, status='old', action='read')
    read(10, nml=sim_opts, iostat=ios)
    if (ios /= 0) then
       print *, "Error reading &sim_opts from ", trim(filename)
       stop 1
    end if
    read(10, nml=out_opts, iostat=ios)
    if (ios /= 0) then
       print *, "Error reading &out_opts from ", trim(filename)
       stop 1
    end if
    read(10, nml=phys_opts, iostat=ios)
    if (ios /= 0) then
       print *, "Error reading &phys_opts from ", trim(filename)
       stop 1
    end if
    read(10, nml=log_opts, iostat=ios)
    if (ios /= 0) then
       print *, "Error reading &log_opts from ", trim(filename)
       stop 1
    end if
    close(10)
    call process_command_line()
    call update_nsteps_from_dates()
  end subroutine read_params

  !> @brief Processes command-line arguments to override parameters.
  subroutine process_command_line()
    integer :: num_args, i
    character(len=128) :: arg
    num_args=command_argument_count()
    do i = 1, num_args
       call get_command_argument(i, arg)
       if (index(arg, "nx=") == 1) then
          read(arg(4:), *) simOpts%nx
       else if (index(arg, "ny=") == 1) then
          read(arg(4:), *) simOpts%ny
       else if (index(arg, "dt=") == 1) then
          read(arg(4:), *) simOpts%dt
       else if (index(arg, "nsteps=") == 1) then
          read(arg(8:), *) simOpts%nsteps
       else if (index(arg, "nt=") == 1) then
          read(arg(4:), *) simOpts%nt
       else if (index(arg, "log_level=") == 1) then
          read(arg(10:), *) logOpts%log_level
       end if
    end do
  end subroutine process_command_line

  !> @brief Converts a date string "YYYY-MM-DD" to the number of days since 1970-01-01.
  function date_to_days(date_str) result(days)
    character(len=*), intent(in) :: date_str
    integer :: year, month, day, a, y, m
    integer :: days
    read(date_str, '(I4,1X,I2,1X,I2)') year, month, day
    a = (14 - month) / 12
    y = year + 4800 - a
    m = month + 12*a - 3
    days = day + ((153*m + 2) / 5) + 365*y + (y/4) - (y/100) + (y/400) - 32045 - 2440588
  end function date_to_days

  !> @brief Updates simOpts%nsteps based on the difference between sim_start_date and sim_end_date.
  subroutine update_nsteps_from_dates()
    integer :: start_days, end_days, diff_days, total_seconds, computed_steps
    start_days = date_to_days(outOpts%sim_start_date)
    end_days = date_to_days(outOpts%sim_end_date)
    diff_days = end_days - start_days
    total_seconds = diff_days * 86400
    computed_steps = int(total_seconds / simOpts%dt)
    simOpts%nsteps = computed_steps
  end subroutine update_nsteps_from_dates

!> @brief Converts an integer to a string.
function itoa(i) result(str)
  integer, intent(in) :: i
  character(len=12) :: str
  write(str, '(I0)') i
end function itoa



end module mod_parameters
