# This CMakeLists file sets options for compiling timing_rig
# 
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build --install-prefix <dispatch_root>
#
# Date: 2025.09.14
# Author: Julian Ehlers

cmake_minimum_required(VERSION 3.20)

project(timing_rig_program VERSION 0.1.0
        DESCRIPTION "A program for testing and timing dispatch libraries; conforms to the dispatch API"
        LANGUAGES Fortran)

# define target
add_executable(timing_rig)

# add source files
target_sources(timing_rig PRIVATE src/timing_rig.f90
                                  "C:/Program Files (x86)/Intel/oneAPI/mkl/latest/include/mkl_vsl.f90"
                                  ../_common/src/library_class.f90
                                  ../dispatch/src/dispatch_library_types.f90
                                  ../dispatch/src/dispatch_manager.f90)

# enable/disable target preprocessor
set_target_properties(timing_rig PROPERTIES Fortran_PREPROCESS ON)
target_compile_definitions(timing_rig PRIVATE REAL64)

# set target compiler flags
cmake_path(GET CMAKE_Fortran_COMPILER STEM fortran_compiler_name)
if(APPLE OR UNIX)
  # Enable .dll export
  if (fortran_compiler_name STREQUAL "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -r8 -double-size 64 -no-wrap-margin -mkl")
  else()
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIMPLICIT_DLLEXPORT -ffree-line-length-0 -fdefault-real-8 -fdefault-double-8")  
  endif()
elseif (WIN32)
  # set a preprocessor flag to use windows system calls for library loading
  target_compile_definitions(timing_rig PRIVATE OS_WIN)
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Ensure static linking to avoid requiring Fortran runtime dependencies
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8")
  elseif (fortran_compiler_name STREQUAL "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /free /real-size:64 /double-size:64 /Qmkl:sequential")
  endif()
endif()

# set target include directories
target_include_directories(timing_rig PRIVATE "${CMAKE_INSTALL_PREFIX}/inc/mod" "%MKLROOT%/include")

# set target linker include directories
target_link_directories(timing_rig PRIVATE "${CMAKE_INSTALL_PREFIX}/lib" "%MKLROOT%/lib")

# set target link libraries
target_link_libraries(timing_rig dispatch_types error_handling extf toml_f mkl_intel_lp64 mkl_sequential mkl_core)