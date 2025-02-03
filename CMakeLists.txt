cmake_minimum_required(VERSION 3.10)
project(ShallowWaterModel Fortran)

# Enable Fortran 2008 standard
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -fcoarray=single -O3 -acc -Minfo=acc")

# Options for ADIOS and ParaView
option(ENABLE_ADIOS "Enable ADIOS2 support" ON)
option(ENABLE_PARAVIEW "Enable ParaView (VTK) output" ON)

if(ENABLE_ADIOS)
  add_definitions(-DENABLE_ADIOS)
  include_directories(/path/to/adios2/include)
  link_directories(/path/to/adios2/lib)
endif()

if(ENABLE_PARAVIEW)
  add_definitions(-DENABLE_PARAVIEW)
endif()

# Find MPI
find_package(MPI REQUIRED)
include_directories(${MPI_Fortran_INCLUDE_PATH})

# Find NetCDF Fortran
find_package(NetCDF REQUIRED)
include_directories(${NETCDF_Fortran_INCLUDE_DIRS})

# Automatically detect all source files with extensions .F90 and .f90
file(GLOB SOURCES "*.F90" "*.f90")

# Create the executable
add_executable(shallow_water ${SOURCES})
target_link_libraries(shallow_water ${MPI_Fortran_LIBRARIES} ${NETCDF_Fortran_LIBRARIES})
if(ENABLE_ADIOS)
  target_link_libraries(shallow_water adios2)
endif()
if(MPI_Fortran_COMPILE_DEFINITIONS)
  target_compile_definitions(shallow_water PRIVATE ${MPI_Fortran_COMPILE_DEFINITIONS})
endif()
install(TARGETS shallow_water DESTINATION bin)

# Documentation target using FORD
find_package(Ford QUIET)
if(FORD_FOUND)
  add_custom_target(doc
    COMMAND ford --config ${CMAKE_CURRENT_SOURCE_DIR}/fordconfig.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generating FORD documentation" VERBATIM
  )
else()
  message(STATUS "FORD not found. Documentation target 'doc' will not be available.")
endif()