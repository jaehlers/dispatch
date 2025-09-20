# This CMakeLists file sets options for compiling dispatch
# 
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build --install-prefix <dispatch_root>
#
# Date: 2025.01.14
# Author: Julian Ehlers

cmake_minimum_required(VERSION 3.20)

project(dispatch VERSION 0.1.0
        DESCRIPTION "DIscrete Signal Processing library dispATCHer (DISPATCH): an environment for linking signal processing routines to simulators of time-varying processes."
        LANGUAGES Fortran)

# define target
add_library(dispatch SHARED)

# add source files
target_sources(dispatch
  PRIVATE
    src/dispatch_interface.f90
    src/dispatch_library_types.f90
    src/dispatch_manager.f90
    ../_common/src/library_class.f90)

# enable/disable target preprocessor
set_target_properties(dispatch PROPERTIES Fortran_PREPROCESS ON)

# set target compiler flags
cmake_path(GET CMAKE_Fortran_COMPILER STEM fortran_compiler_name)
if(APPLE OR UNIX)
  # Enable .dll export
  if (fortran_compiler_name STREQUAL "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -r8 -double-size 64 -no-wrap-margin")
  else()
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -ffree-line-length-0 -fdefault-real-8 -fdefault-double-8")  
  endif()
elseif (WIN32)
  # set a preprocessor flag to use windows system calls for library loading
  target_compile_definitions(dispatch PRIVATE OS_WIN)
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Ensure static linking to avoid requiring Fortran runtime dependencies
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8")
  elseif (fortran_compiler_name STREQUAL "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -free -real-size:64 -double-size:64")
  endif()
endif()

# set target include directories
target_include_directories(dispatch PRIVATE "${CMAKE_INSTALL_PREFIX}/inc/mod")

# set target linker include directories
target_link_directories(dispatch PRIVATE "${CMAKE_INSTALL_PREFIX}/lib")

# set target link libraries
target_link_libraries(dispatch error_handling extf toml_f)