# This CMakeLists file sets options for compiling delay_library
# 
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build --install-prefix <dispatch_root>
#
# Date: 2025.08.20
# Author: Julian Ehlers

cmake_minimum_required(VERSION 3.20)

project(delay_library VERSION 0.1.0
        DESCRIPTION "A component for delaying simulation signals a prescribed number of sample steps; conforms to the dispatch API"
        LANGUAGES Fortran)

# define target
add_library(delay SHARED)

# add source files
target_sources(delay PRIVATE src/delay_library.f90)

# enable/disable target preprocessor
set_target_properties(delay PROPERTIES Fortran_PREPROCESS ON)
#target_compile_definitions(delay PRIVATE "")

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
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Ensure static linking to avoid requiring Fortran runtime dependencies
    # not tested: set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8")
  elseif (fortran_compiler_name STREQUAL "ifx")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -free -real-size:64 -double-size:64")
  endif()
endif()

# set target include directories
target_include_directories(delay PRIVATE "${CMAKE_INSTALL_PREFIX}/inc/mod")

# set target linker include directories
target_link_directories(delay PRIVATE "${CMAKE_INSTALL_PREFIX}/lib")

# set target link libraries
target_link_libraries(delay dispatch_types error_handling extf toml_f)