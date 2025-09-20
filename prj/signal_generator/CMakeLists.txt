# This CMakeLists file sets options for compiling signal_renamer_library
# 
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build --install-prefix <dispatch_root>
#
# Date: 2025.01.14
# Author: Julian Ehlers

cmake_minimum_required(VERSION 3.20)

project(signal_generator_library VERSION 0.1.0
        DESCRIPTION "A component for generating signals; conforms to the dispatch API"
        LANGUAGES Fortran)

# define target
add_library(signal_generator SHARED)

# add source files
target_sources(signal_generator PRIVATE src/signal_generator_library.f90 "C:/Program Files (x86)/Intel/oneAPI/mkl/latest/include/mkl_vsl.f90")

# enable/disable target preprocessor
set_target_properties(signal_generator PROPERTIES Fortran_PREPROCESS ON)
#target_compile_definitions(signal_generator PRIVATE "")

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
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Ensure static linking to avoid requiring Fortran runtime dependencies
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8")
  elseif (fortran_compiler_name STREQUAL "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /free /real-size:64 /double-size:64 /Qmkl:sequential")
  endif()
endif()

# set target include directories
target_include_directories(signal_generator PRIVATE "${CMAKE_INSTALL_PREFIX}/inc/mod" "%MKLROOT%/include")

# set target linker include directories
target_link_directories(signal_generator PRIVATE "${CMAKE_INSTALL_PREFIX}/lib" "%MKLROOT%/lib")

# set target link libraries
target_link_libraries(signal_generator dispatch_types error_handling extf toml_f mkl_intel_lp64 mkl_sequential mkl_core)