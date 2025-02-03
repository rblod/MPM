!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. You can use, modify, and/or redistribute it under the
! terms of the CeCILL-C license. For details, see: http://www.cecill.info.
!===============================================================================

module mod_grid
  use mod_precision
  use mod_parameters, only: simOpts
  use mod_mpi_wrapper
  use mpi                   ! Utilisation du module MPI traditionnel pour éviter les ambiguïtés
  implicit none

  ! Global grid dimensions (from simOpts)
  integer :: global_nx, global_ny
  ! Local subdomain dimensions
  integer :: local_nx, local_ny
  ! Offsets dans le domaine global
  integer :: x_offset, y_offset
  ! Informations sur le rang MPI et la décomposition 2D
  integer :: my_rank, nprocs
  integer :: my2d_x, my2d_y

  ! Grilles (allocatables avec coarrays pour l'échange de ghost cells)
  real(wp), allocatable :: h(:,:)[:]   ! Dimensions : 0:local_nx+1, 0:local_ny+1
  real(wp), allocatable :: u(:,:)[:]   ! Dimensions : 0:local_nx, 1:local_ny+1
  real(wp), allocatable :: v(:,:)[:]   ! Dimensions : 1:local_nx+1, 0:local_ny

contains

  !> @brief Initializes the 2D grid.
  !>
  !> Allocates the arrays for h, u, v, and computes the local dimensions and offsets
  !> based on the global grid size (simOpts%nx, simOpts%ny) and the 2D processor layout.
  subroutine init_grid()
    integer :: ierr, rx, rem_x, ry, rem_y

    ! Get MPI rank and number of processes
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

    ! Global dimensions from simOpts
    global_nx = simOpts%nx
    global_ny = simOpts%ny

    ! Vérifier que le nombre de processus correspond à la décomposition attendue
    if (nprocs /= simOpts%procs_x * simOpts%procs_y) then
       call error_stop("nprocs does not equal procs_x * procs_y in mod_grid::init_grid")
    end if

    ! Calcul de la position dans la décomposition 2D
    my2d_x = mod(my_rank, simOpts%procs_x)
    my2d_y = my_rank / simOpts%procs_x

    ! Calcul de la taille locale en x
    rx = global_nx / simOpts%procs_x
    rem_x = mod(global_nx, simOpts%procs_x)
    if (my2d_x < rem_x) then
       local_nx = rx + 1
       x_offset = my2d_x * local_nx
    else
       local_nx = rx
       x_offset = rem_x*(rx+1) + (my2d_x - rem_x)*rx
    end if

    ! Calcul de la taille locale en y
    ry = global_ny / simOpts%procs_y
    rem_y = mod(global_ny, simOpts%procs_y)
    if (my2d_y < rem_y) then
       local_ny = ry + 1
       y_offset = my2d_y * local_ny
    else
       local_ny = ry
       y_offset = rem_y*(ry+1) + (my2d_y - rem_y)*ry
    end if

    ! Allocation des grilles avec des ghost cells (utilisation de coarrays)
    allocate(h(0:local_nx+1, 0:local_ny+1)[*])
    allocate(u(0:local_nx, 1:local_ny+1)[*])
    allocate(v(1:local_nx+1, 0:local_ny)[*])
  end subroutine init_grid

  !> @brief Stops the execution with an error message.
  subroutine error_stop(msg)
    character(len=*), intent(in) :: msg
    call log_message(LOG_ERROR, msg)
    stop 1
  end subroutine error_stop

end module mod_grid
