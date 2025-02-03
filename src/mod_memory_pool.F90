!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2023, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. You can use, modify, and/or redistribute it under the
! terms of the CeCILL-C license. For details, see: http://www.cecill.info.
!===============================================================================

module mod_memory_pool
  use mod_precision
  implicit none

  ! First, define the derived type
  type :: temp_array
     real(wp), pointer :: arr(:,:,:)
  end type temp_array

  ! Now declare the variables that use the type
  type(temp_array), allocatable :: pool(:)
  logical, allocatable :: in_use(:)
  integer :: pool_size = 0
  logical :: pool_initialized = .false.
  
contains

  !> @brief Initializes the memory pool.
  !>
  !> @param dims Array [nx, ny, nt] specifying the dimensions for temporary arrays.
  subroutine pool_init(dims)
    integer, intent(in) :: dims(3)
    integer :: i
    if (.not. pool_initialized) then
       pool_size = 6  ! initial pool size
       allocate(pool(pool_size))
       allocate(in_use(pool_size))
       in_use = .false.
       do i = 1, pool_size
          allocate(pool(i)%arr(dims(1), dims(2), dims(3)))
       end do
       pool_initialized = .true.
    end if
  end subroutine pool_init

  !> @brief Retrieves a temporary array from the pool.
  !>
  !> If all arrays are in use, the pool is expanded.
  !> @param out_arr Pointer to the retrieved temporary array.
  subroutine pool_get(out_arr)
    real(wp), pointer, intent(out) :: out_arr(:,:,:)
    integer :: i, new_size
    do i = 1, pool_size
       if (.not. in_use(i)) then
          in_use(i) = .true.
          out_arr => pool(i)%arr
          return
       end if
    end do
    new_size = pool_size + 2
    call expand_pool(new_size)
    in_use(pool_size - 2 + 1) = .true.
    out_arr => pool(pool_size - 2 + 1)%arr
  end subroutine pool_get

  !> @brief Expands the memory pool to a new size.
  !>
  !> @param new_size New total size of the pool.
  subroutine expand_pool(new_size)
    integer, intent(in) :: new_size
    integer :: old_size, i
    type(temp_array), allocatable :: new_pool(:)
    logical, allocatable :: new_in_use(:)
    old_size = pool_size
    pool_size = new_size
    allocate(new_pool(pool_size))
    allocate(new_in_use(pool_size))
    new_in_use(1:old_size) = in_use
    new_in_use(old_size+1:pool_size) = .false.
    do i = 1, old_size
       new_pool(i) = pool(i)
    end do
    do i = old_size+1, pool_size
       allocate(new_pool(i)%arr(size(pool(1)%arr,1), size(pool(1)%arr,2), size(pool(1)%arr,3)))
    end do
    deallocate(pool)
    deallocate(in_use)
    pool = new_pool
    in_use = new_in_use
  end subroutine expand_pool

  !> @brief Releases a temporary array back to the pool.
  !>
  !> @param arr Pointer to the temporary array to release.
  subroutine pool_release(arr)
    real(wp), pointer, intent(inout) :: arr(:,:,:)
    integer :: i
    do i = 1, pool_size
       if (associated(arr, pool(i)%arr)) then
          in_use(i) = .false.
          nullify(arr)
          return
       end if
    end do
    call error_stop("Attempt to release an array not in pool in pool_release.")
  end subroutine pool_release

  !> @brief Finalizes the memory pool.
  subroutine pool_finalize()
    integer :: i
    if (pool_initialized) then
       do i = 1, pool_size
          if (associated(pool(i)%arr)) deallocate(pool(i)%arr)
       end do
       deallocate(pool)
       deallocate(in_use)
       pool_initialized = .false.
       pool_size = 0
    end if
  end subroutine pool_finalize

  !> @brief Reports a memory pool error and stops the program.
  subroutine error_stop(msg)
    character(len=*), intent(in) :: msg
    print *, "Memory Pool Error: ", trim(msg)
    stop 1
  end subroutine error_stop

end module mod_memory_pool
