# This CMakeLists file sets options for compiling variable_bandwidth_recursive_filter_library
# 
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build --install-prefix <dispatch_root>
#
# Date: 2025.11.10
# Author: Julian Ehlers

cmake_minimum_required(VERSION 3.20)

project(variable_bandwidth_recursive_filter_library VERSION 0.1.0
        DESCRIPTION "Fortran implementation of a variable-bandwidth infinite impulse response (IIR) filter; conforms to the dispatch API"
        LANGUAGES Fortran)

# define target
add_library(variable_bandwidth_recursive_filter SHARED)

# add source files
target_sources(variable_bandwidth_recursive_filter
  PRIVATE
    src/variable_bandwidth_recursive_filter_library.f90
    src/sos_class.f90
    src/polynomial_class.f90
    ../_common/src/hdf5_utils.f90)

# enable/disable target preprocessor
set_target_properties(variable_bandwidth_recursive_filter PROPERTIES Fortran_PREPROCESS ON)
#target_compile_definitions(variable_bandwidth_recursive_filter PRIVATE REAL64)

# set target compiler flags
cmake_path(GET CMAKE_Fortran_COMPILER STEM fortran_compiler_name)
if(APPLE OR UNIX)
  # Enable .dll export
  if (fortran_compiler_name MATCHES "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -r8 -double-size 64 -no-wrap-margin")
  else()
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -ffree-line-length-0 -fdefault-real-8 -fdefault-double-8")  
  endif()
elseif (WIN32)
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Ensure static linking to avoid requiring Fortran runtime dependencies
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8")
  elseif (fortran_compiler_name MATCHES "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /free /real-size:64 /double-size:64")
  endif()
endif()

# set target include directories
target_include_directories(variable_bandwidth_recursive_filter PRIVATE "${CMAKE_INSTALL_PREFIX}/inc/mod")
target_include_directories(variable_bandwidth_recursive_filter PRIVATE "${HDF5_ROOT}/mod/static")

# set target linker include directories
target_link_directories(variable_bandwidth_recursive_filter PRIVATE "${CMAKE_INSTALL_PREFIX}/lib")
cmake_path(APPEND HDF5_ROOT "lib" OUTPUT_VARIABLE hdf5_lib_dir)
target_link_directories(variable_bandwidth_recursive_filter PRIVATE "${hdf5_lib_dir}")

# set target link libraries
if(APPLE OR UNIX)
  target_link_libraries(variable_bandwidth_recursive_filter dispatch_types error_handling extf hdf5_utils hdf5 hdf5_fortran hdf5_tools hdf5_f90cstub aec szaec zlib-static)
elseif (WIN32)
  target_link_libraries(variable_bandwidth_recursive_filter dispatch_types error_handling extf hdf5_utils libhdf5 libhdf5_fortran libhdf5_tools libhdf5_f90cstub libaec libszaec zlib-static shlwapi)
endif()