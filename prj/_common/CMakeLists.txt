# This CMakeLists file sets options for compiling the dependencies common across dispatch projects
#
# The directory named "dispatch" 2 levels above this CMakeLists.txt is referred to as <dispatch_root>
# The build process has been tested on the following environments:
# - Windows 11 (Windows SDK 10.0.26100), Intel Fortran (ifx) 2025.0.4, Microsoft Visual Studio 17 2022
#
# Prerequisites:
# ==============
# - cmake (installed with Visual Studio, if you have Visual Studio)
# - cpm (https://github.com/cpm-cmake/CPM.cmake; installed in the directory above cmake.exe, see below)
# - Intel Fortran compiler (https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html)
# - Intel Math Kernel Library (https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl-download.html)
# On Windows, it is convenient to use Microsoft Visual Studio
# - Microsoft Visual Studio 2022 (e.g. https://visualstudio.microsoft.com/vs/community/)
# -- cmake is included in VS, typically at: C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\IDE\CommonExtensions\Microsoft\CMake\CMake
# 
# A typical build process would be, from the Intel OneAPI command prompt:
# cd <dispatch_root>/prj/_common:
# 1. generate the buildsystem for all dependencies:
# =================================================
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build -D HDF5_ROOT="C:/Program Files/HDF_Group/HDF5/1.14.5" -D CPM_SOURCE_CACHE=src --install-prefix <dispatch_root>
#
# 2. build all dependencies (generated as static libraries):
# ==========================================================
# open <dispatch_root>/prj/_common/build/dispatch_dependencies.sln in visual studio
# select appropriate build profile (Debug/ Release/ MinSizeRel/ RelWithDebInfo)
# build solution
#
# 3. install dependencies in <dispatch_root>/lib:
# ===============================================
# build the INSTALL project in dispatch_dependencies.sln (under CMakePredefinedTargets)
#
# Linux build process:
# ====================
# From a terminal,
# cd <dispatch_root>/prj/_common
# mkdir build
# cd build
#
# 1. generate makefiles
# =====================
# cmake .. -B build -D HDF5_ROOT="/hdf5/HDF5-1.14.5-Linux/HDF_Group/HDF5/1.14.5" -D CMP_SOURCE_CACHE=../src --install-prefix <dispatch_root> -D CMAKE_BUILD_TYPE="Debug" (or MinSizeRel, RelWithDebInfo, Release)
#
# 2. build
# ========
# make
#
# 3. install
# ==========
# make install
#
# Once these dependencies are built and installed, see <dispatch_root>/CMakeLists.txt for instructions on building dispatch and its libraries.
#
# Date: 2025.01.15
# Author: Julian Ehlers

cmake_minimum_required(VERSION 3.20)

project(dispatch_dependencies VERSION 0.1.0
        DESCRIPTION "Dependencies needed for building components of the dispatch ecosystem"
        LANGUAGES Fortran)

# add cpm for package management (assuming CPM.cmake is installed in the directory above cmake.exe)
cmake_path(GET CMAKE_COMMAND PARENT_PATH cmake_command_path)
cmake_path(GET cmake_command_path PARENT_PATH cmake_root_path)
cmake_path(APPEND cmake_root_path "CPM.cmake" OUTPUT_VARIABLE cpm_path)
include("${cpm_path}")

# add external dependencies (excluding any CMakeLists.txt files they may contain, keeping this build free of any test & example program targets that the dependencies might define)
CPMAddPackage(NAME bspline_fortran_pkg URL https://github.com/jacobwilliams/bspline-fortran/archive/refs/tags/7.4.0.zip EXCLUDE_FROM_ALL TRUE)
CPMAddPackage(NAME error_handling_pkg URL https://github.com/SINTEF/fortran-error-handling/archive/refs/tags/v0.2.0.zip EXCLUDE_FROM_ALL TRUE)
CPMAddPackage(NAME polyroots_fortran_pkg URL https://github.com/jacobwilliams/polyroots-fortran/archive/refs/tags/1.4.0.zip EXCLUDE_FROM_ALL TRUE)
CPMAddPackage(NAME slsqp_pkg URL https://github.com/jacobwilliams/slsqp/archive/refs/tags/1.5.0.zip EXCLUDE_FROM_ALL TRUE)
CPMAddPackage(NAME toml_f_pkg URL https://github.com/toml-f/toml-f/releases/download/v0.4.2/toml-f-0.4.2.tar.xz EXCLUDE_FROM_ALL TRUE)

# set the module directory for all targets
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_INSTALL_PREFIX}/inc/mod")

# define targets
add_library(bspline_fortran STATIC)
add_library(dispatch_types STATIC)
add_library(error_handling STATIC)
add_library(extf STATIC)
add_library(hdf5_utils STATIC)
add_library(polyroots_fortran STATIC)
add_library(slsqp STATIC)
add_library(toml_f STATIC)

# add source files to each target
cmake_path(APPEND bspline_fortran_pkg_SOURCE_DIR "src" "*.[fF]90" OUTPUT_VARIABLE bspline_fortran_file_pattern)
file(GLOB bspline_fortran_files "${bspline_fortran_file_pattern}")
target_sources(bspline_fortran PRIVATE ${bspline_fortran_files})
target_sources(dispatch_types PRIVATE "${CMAKE_SOURCE_DIR}/src/dispatch_types.f90" "${CMAKE_SOURCE_DIR}/src/extf.f90")
set(error_handling_sources 
  ${error_handling_pkg_SOURCE_DIR}/src/error.f90
  ${error_handling_pkg_SOURCE_DIR}/src/error_handling.f90
  ${error_handling_pkg_SOURCE_DIR}/src/internal/error_handling_impl.f90
  ${error_handling_pkg_SOURCE_DIR}/src/internal/error_stop_impl.f90)
target_sources(error_handling PRIVATE ${error_handling_sources})
target_sources(extf PRIVATE "${CMAKE_SOURCE_DIR}/src/extf.f90")
target_sources(hdf5_utils PRIVATE "${CMAKE_SOURCE_DIR}/src/hdf5_utils.f90" ${error_handling_sources} "${CMAKE_SOURCE_DIR}/src/extf.f90")
target_sources(polyroots_fortran PRIVATE ${polyroots_fortran_pkg_SOURCE_DIR}/src/polyroots_module.F90)
cmake_path(APPEND slsqp_pkg_SOURCE_DIR "src" "*.[fF]90" OUTPUT_VARIABLE slsqp_file_pattern)
file(GLOB slsqp_files "${slsqp_file_pattern}")
target_sources(slsqp PRIVATE ${slsqp_files})
cmake_path(APPEND toml_f_pkg_SOURCE_DIR "src" "*.f90" OUTPUT_VARIABLE toml_f_file_pattern)
file(GLOB_RECURSE toml_f_files "${toml_f_file_pattern}")
target_sources(toml_f PRIVATE ${toml_f_files} "${CMAKE_SOURCE_DIR}/src/tomlf_error_handler.f90")

# set target compiler flags
if(APPLE OR UNIX)
  # Enable .dll export
  if (CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC -DIMPLICIT_DLLEXPORT -r8 -double-size 64 -no-wrap-margin")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC -DIMPLICIT_DLLEXPORT -ffree-line-length-0 -fdefault-real-8 -fdefault-double-8")  
  endif()
elseif (WIN32)
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Ensure static linking to avoid requiring Fortran runtime dependencies
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -static-libgcc -static-libgfortran -static -fdefault-real-8 -fdefault-double-8")
  elseif (CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
    #set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /libs:static /free /static /real-size:64 /double-size:64")
    target_compile_options(polyroots_fortran PRIVATE /Qmkl:sequential)
    target_compile_options(slsqp PRIVATE /Qmkl:sequential)
  endif()
endif()

# set target preprocessor definitions
set_target_properties(bspline_fortran PROPERTIES Fortran_PREPROCESS ON)
set_target_properties(dispatch_types PROPERTIES Fortran_PREPROCESS ON)
set_target_properties(extf PROPERTIES Fortran_PREPROCESS ON)
set_target_properties(hdf5_utils PROPERTIES Fortran_PREPROCESS ON)
set_target_properties(polyroots_fortran PROPERTIES Fortran_PREPROCESS ON)
set_target_properties(slsqp PROPERTIES Fortran_PREPROCESS ON)
target_compile_definitions(bspline_fortran PRIVATE REAL64)
target_compile_definitions(dispatch_types PRIVATE REAL64)
target_compile_definitions(extf PRIVATE REAL64)
target_compile_definitions(hdf5_utils PRIVATE REAL64)
target_compile_definitions(polyroots_fortran PRIVATE REAL64)
target_compile_definitions(slsqp PRIVATE REAL64 HAS_BLAS)
    
# set target include directories
cmake_path(APPEND HDF5_ROOT "mod" "static" OUTPUT_VARIABLE hdf5_mod_dir)
target_include_directories(hdf5_utils PRIVATE "${hdf5_mod_dir}")

# set target link directories
cmake_path(APPEND HDF5_ROOT "lib" OUTPUT_VARIABLE hdf5_lib_dir)
target_link_directories(hdf5_utils PRIVATE "${hdf5_lib_dir}")

# set target link libraries
target_link_libraries(hdf5_utils libhdf5 libhdf5_fortran libhdf5_tools libhdf5_f90cstub libhdf5_cpp libaec libszaec zlib-static shlwapi)
target_link_libraries(polyroots_fortran mkl_blas95_ilp64 mkl_lapack95_ilp64 mkl_intel_ilp64 mkl_sequential mkl_core)
target_link_libraries(slsqp mkl_blas95_ilp64 mkl_intel_ilp64 mkl_sequential mkl_core)

# set install parameters
install(TARGETS bspline_fortran dispatch_types error_handling extf hdf5_utils polyroots_fortran slsqp toml_f
  LIBRARY DESTINATION "lib")