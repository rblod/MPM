!===============================================================================
! CeCILL-C License Header
! ------------------------------------------------------------------------------
! Copyright (c) 2025, Rachid Benshila
! This software is governed by the CeCILL-C license under French law and
! abiding by its terms. You can use, modify, and/or redistribute it under the
! terms of the CeCILL-C license. For details, see: http://www.cecill.info.
!===============================================================================

module mod_io
  use mod_precision
  use mod_parameters, only: outOpts
  use mod_grid
  use mod_logging, only: log_message, LOG_ERROR
  use mpi           ! Using the traditional MPI module to avoid ambiguities
  use netcdf
#ifdef ENABLE_ADIOS
  use adios2
#endif

  implicit none

contains



  !---------------------------------------------------------------------------
  !> @brief Writes a NetCDF output file (parallel NetCDF4).
  !>
  !> This subroutine creates a NetCDF file (with MPI I/O enabled) that contains
  !> the fields h, u, and v. In case of any error, it logs the error message and
  !> aborts the MPI execution.
  subroutine write_netcdf(step)
    integer, intent(in) :: step
    integer :: ncid, varid_h, varid_u, varid_v, dimid_x, dimid_y, retval
    character(len=128) :: filename
    integer, dimension(2) :: dims
    integer :: local_x, local_y, myrank, ierr

    ! Get current MPI rank
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    ! Determine local domain dimensions from global arrays h, u, v (assumed allocated in mod_grid)
    local_x = size(h,1) - 2
    local_y = size(h,2) - 2

    ! Create output filename using step and rank
    write(filename, '(A,I4.4,"_rank",I4.4,".nc")') "output_step", step, myrank

    retval = nf90_create(filename, NF90_CLOBBER + NF90_NETCDF4 + NF90_MPIIO, ncid, &
                         comm=MPI_COMM_WORLD, info=MPI_INFO_NULL)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "NetCDF create error: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_def_dim(ncid, "x", local_x, dimid_x)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error defining dimension x: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_def_dim(ncid, "y", local_y, dimid_y)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error defining dimension y: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    dims = [dimid_x, dimid_y]

    retval = nf90_def_var(ncid, "h", NF90_DOUBLE, dims, varid_h)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error defining variable h: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_def_var(ncid, "u", NF90_DOUBLE, [dimid_x+1, dimid_y], varid_u)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error defining variable u: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_def_var(ncid, "v", NF90_DOUBLE, [dimid_x, dimid_y+1], varid_v)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error defining variable v: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_enddef(ncid)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error ending define mode: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_put_var(ncid, varid_h, h(1:local_x,1:local_y))
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error writing h: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_put_var(ncid, varid_u, u(0:local_x,1:local_y))
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error writing u: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_put_var(ncid, varid_v, v(1:local_x,0:local_y))
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error writing v: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

    retval = nf90_close(ncid)
    if (retval /= NF90_NOERR) then
       call log_message(LOG_ERROR, "Error closing file: " // trim(nf90_strerror(retval)))
       call MPI_Abort(MPI_COMM_WORLD, retval, ierr)
    end if

  end subroutine write_netcdf

  !---------------------------------------------------------------------------
  !> @brief Writes output using ADIOS2 (if enabled).
  subroutine write_adios(step)
    integer, intent(in) :: step
#ifdef ENABLE_ADIOS
    type(adios2_engine) :: engine
    type(adios2_io) :: io
    type(adios2_adios) :: adios
    character(len=128) :: filename
    integer :: local_x, local_y, myrank, ierr
    ! For error checking from ADIOS routines (example)
    integer :: adios_err

    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    local_x = size(h,1) - 2
    local_y = size(h,2) - 2
    write(filename, '(A,I4.4,"_rank",I4.4,".bp")') "adios_output_step", step, myrank

    call adios2_init(adios)
    io = adios2_declare_io(adios, "Output")
    call adios2_set_engine(io, "BPFile")
    call adios2_set_parameter(io, "OpenTimeout", "0")
    engine = adios2_open(io, filename, adios2_mode_write)
    ! Here you should check for errors in ADIOS routines (example below):
    adios_err = engine%error_code
    if (adios_err /= 0) then
       call log_message(LOG_ERROR, "ADIOS error in adios2_open: code " // trim(itoa(adios_err)))
       call MPI_Abort(MPI_COMM_WORLD, adios_err, ierr)
    end if

    call adios2_put(engine, "h", h(1:local_x,1:local_y), adios2_mode_sync)
    call adios2_put(engine, "u", u(0:local_x,1:local_y), adios2_mode_sync)
    call adios2_put(engine, "v", v(1:local_x,0:local_y), adios2_mode_sync)
    call adios2_close(engine)
    call adios2_finalize(adios)
#else
    print *, "Warning: ADIOS support not enabled; skipping ADIOS output."
#endif
  end subroutine write_adios

  !---------------------------------------------------------------------------
  !> @brief Writes a ParaView (VTK) output file.
  subroutine write_paraview(step)
    integer, intent(in) :: step
#ifdef ENABLE_PARAVIEW
    integer :: local_x, local_y, i, j, myrank, ierr
    character(len=128) :: filename
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    local_x = size(h,1) - 2
    local_y = size(h,2) - 2
    write(filename, '(A,I4.4,"_rank",I4.4,".vti")') "output_vti_step", step, myrank

    open(unit=20, file=filename, status="replace", action="write")
    write(20,*) '<?xml version="1.0"?>'
    write(20,*) '<VTKFile type="ImageData" version="0.1" byte_order="LittleEndian">'
    write(20,*) '  <ImageData WholeExtent="0 ', global_nx-1, ' 0 ', global_ny-1, '" Origin="0 0 0" Spacing="1 1 1">'
    write(20,*) '    <Piece Extent="0 ', local_x-1, ' 0 ', local_y-1, '">'
    write(20,*) '      <PointData Scalars="h">'
    write(20,*) '        <DataArray type="Float64" Name="h" format="ascii">'
    do j = 1, local_y
       do i = 1, local_x
          write(20,*) h(i,j)
       end do
    end do
    write(20,*) '        </DataArray>'
    write(20,*) '      </PointData>'
    write(20,*) '      <CellData>'
    write(20,*) '      </CellData>'
    write(20,*) '    </Piece>'
    write(20,*) '  </ImageData>'
    write(20,*) '</VTKFile>'
    close(20)
#else
    print *, "Warning: ParaView output not enabled; skipping VTK output."
#endif
  end subroutine write_paraview

  !---------------------------------------------------------------------------
  !> @brief Chooses the output format and calls the appropriate subroutine.
  subroutine write_output(step, output_format)
    integer, intent(in) :: step
    character(len=*), intent(in) :: output_format
    integer :: ierr
    select case (trim(output_format))
    case ("netcdf")
       call write_netcdf(step)
    case ("adios")
       call write_adios(step)
    case ("paraview")
       call write_paraview(step)
    case default
       call log_message(LOG_ERROR, "Error: Unknown output format: " // trim(output_format))
       call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
    end select
  end subroutine write_output

#ifdef ENABLE_ADIOS
  !---------------------------------------------------------------------------
  !> @brief Converts an integer to a string.
  function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=12) :: s
    write(s, '(I0)') i
  end function itoa
#endif

end module mod_io