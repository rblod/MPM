!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. For details, see: http://www.cecill.info.
!===============================================================================

!> @brief Main program for the shallow water model with the double gyre test case.
!>
!> Initializes MPI, performs 2D domain decomposition using MPI and coarrays,
!> allocates the grid (with ghost layers) and initializes the time dimension,
!> converts simulation start and end dates into the number of time steps,
!> initializes fields, and integrates in time using an RK3 scheme with a memory pool
!> for temporary arrays. GPU/CPU transfers are optimized using an OpenACC persistent
!> data region with update directives before and after ghost exchanges.
!> The initial CFL is computed and logged, and timing is measured for key routines.
program shallow_water
  use mod_precision
  use mod_parameters
  use mod_grid
  use mod_advection
  use mod_rhs
  use mod_io
  use mod_logging
  use mod_timing
  use mod_memory_pool
  use mpi
  implicit none

  integer :: ierr, provided, rank, step
  real(wp) :: t_sim, cfl, t1, t2
  character(len=32) :: rank_str

  call MPI_Init_thread(MPI_THREAD_MULTIPLE, provided, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
  write(rank_str, '(I0)') rank

  call log_message(LOG_INFO, "Simulation starting on rank " // trim(rank_str))
  call read_params("input.nml")
  call init_log()
  call log_message(LOG_INFO, "Parameters successfully read and nsteps updated from dates.")

  call init_grid()
  call log_message(LOG_INFO, "2D local grid allocated.")

  call initialize_grid()
  call log_message(LOG_INFO, "Grid fields initialized (h=1, u=v=0).")

  cfl = simOpts%dt * sqrt(g) / simOpts%dx
  call log_message(LOG_INFO, "Initial CFL = " // trim(adjustl(ftoa(cfl))))

  call pool_init([local_nx+2, local_ny+2, simOpts%nt])
  call log_message(LOG_INFO, "Memory pool initialized.")

  !$acc data copyin(h, u, v)
  call start_timer()
  call log_message(LOG_INFO, "Timing started at " // get_timestamp())

  do step = 1, simOpts%nsteps
     t_sim = step * simOpts%dt

     t1 = MPI_Wtime()
     call simulate_step(step, t_sim)
     t2 = MPI_Wtime()
     call log_message(LOG_DEBUG, "simulate_step took " // trim(adjustl(ftoa(t2-t1))) // " seconds at step " // trim(itoa(step)))

  !   !$acc update host(h, u, v)
 !    t1 = MPI_Wtime()
 !    call exchange_ghosts()
 !    t2 = MPI_Wtime()
  !   call log_message(LOG_DEBUG, "exchange_ghosts took " // trim(adjustl(ftoa(t2-t1))) // " seconds at step " // trim(itoa(step)))
     !$acc update device(h, u, v)

     if (mod(step, 10) == 0) then
        t1 = MPI_Wtime()
        call write_output(step, outOpts%output_format)
        t2 = MPI_Wtime()
        call log_message(LOG_DEBUG, "write_output took " // trim(adjustl(ftoa(t2-t1))) // " seconds at step " // trim(itoa(step)))
     end if
     call log_message(LOG_INFO, "Completed step " // trim(itoa(step)) // ", simulation time = " // trim(adjustl(ftoa(t_sim))) // " seconds.")
  end do
  !$acc end data

  call stop_timer()
  call log_message(LOG_INFO, "Timing stopped at " // get_timestamp())
  call pool_finalize()
  call finalize_log()
  call MPI_Finalize(ierr)
end program shallow_water


!> @brief Initializes the grid fields.
subroutine initialize_grid()
  use mod_precision
  use mod_grid
  implicit none
  integer :: i, j
  do j = 1, local_ny
     do i = 1, local_nx
        h(i,j) = 1.0_wp
     end do
  end do
  do j = 1, local_ny
     do i = 0, local_nx
        u(i,j) = 0.0_wp
     end do
  end do
  do j = 0, local_ny
     do i = 1, local_nx
        v(i,j) = 0.0_wp
     end do
  end do
end subroutine initialize_grid

!> @brief Performs one RK3 time step using the memory pool.
subroutine simulate_step(step, t_sim)
  use mod_precision
  use mod_grid
  use mod_parameters
  use mod_rhs
  use mod_memory_pool
  implicit none
  integer, intent(in) :: step
  real(wp), intent(in) :: t_sim
  real(wp) :: dt
  real(wp), pointer :: k1_h(:,:,:), k1_u(:,:,:), k1_v(:,:,:)
  real(wp), pointer :: k2_h(:,:,:), k2_u(:,:,:), k2_v(:,:,:)
  real(wp), pointer :: k3_h(:,:,:), k3_u(:,:,:), k3_v(:,:,:)

  dt = simOpts%dt

  call pool_get(k1_h)
  call pool_get(k1_u)
  call pool_get(k1_v)
  call pool_get(k2_h)
  call pool_get(k2_u)
  call pool_get(k2_v)
  call pool_get(k3_h)
  call pool_get(k3_u)
  call pool_get(k3_v)

  call compute_rhs(h(:,:), u(:,:), v(:,:), k1_h(:,:,1), k1_u(:,:,1), k1_v(:,:,1), &
                    simOpts%adv_scheme, simOpts%dx, simOpts%dy, t_sim)
  h(:,:) = h(:,:) + dt * k1_h(:,:,1)
  u(:,:) = u(:,:) + dt * k1_u(:,:,1)
  v(:,:) = v(:,:) + dt * k1_v(:,:,1)

  call compute_rhs(h(:,:), u(:,:), v(:,:), k2_h(:,:,1), k2_u(:,:,1), k2_v(:,:,1), &
                    simOpts%adv_scheme, simOpts%dx, simOpts%dy, t_sim)
  h(:,:) = 0.75_wp * h(:,:) + 0.25_wp * (h(:,:) + dt * k2_h(:,:,1))
  u(:,:) = 0.75_wp * u(:,:) + 0.25_wp * (u(:,:) + dt * k2_u(:,:,1))
  v(:,:) = 0.75_wp * v(:,:) + 0.25_wp * (v(:,:) + dt * k2_v(:,:,1))

  call compute_rhs(h(:,:), u(:,:), v(:,:), k3_h(:,:,1), k3_u(:,:,1), k3_v(:,:,1), &
                    simOpts%adv_scheme, simOpts%dx, simOpts%dy, t_sim)
  h(:,:) = (1.0_wp/3.0_wp) * h(:,:) + (2.0_wp/3.0_wp) * (h(:,:) + dt * k3_h(:,:,1))
  u(:,:) = (1.0_wp/3.0_wp) * u(:,:) + (2.0_wp/3.0_wp) * (u(:,:) + dt * k3_u(:,:,1))
  v(:,:) = (1.0_wp/3.0_wp) * v(:,:) + (2.0_wp/3.0_wp) * (v(:,:) + dt * k3_v(:,:,1))

  call pool_release(k1_h)
  call pool_release(k1_u)
  call pool_release(k1_v)
  call pool_release(k2_h)
  call pool_release(k2_u)
  call pool_release(k2_v)
  call pool_release(k3_h)
  call pool_release(k3_u)
  call pool_release(k3_v)
end subroutine simulate_step

!> @brief Converts an integer to a string.
function itoa(i) result(str)
  integer, intent(in) :: i
  character(len=12) :: str
  write(str, '(I0)') i
end function itoa

!> @brief Converts a real number to a string.
function ftoa(x) result(str)
  real(8), intent(in) :: x
  character(len=32) :: str
  write(str, '(F8.4)') x
end function ftoa
