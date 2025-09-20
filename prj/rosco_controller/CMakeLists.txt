# This CMakeLists file sets options for compiling rosco_controller_library.dll
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
# On Linux, only command-line builds have been tested so far
# 
# Windows build process:
# ======================
# A typical build process would be, from the Intel OneAPI command prompt:
# cd <dispatch_root>
# 1. generate the buildsystem for all dependencies:
# =================================================
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build -D HDF5_ROOT="C:/Program Files/HDF_Group/HDF5/1.14.5" -D CPM_SOURCE_CACHE=src --install-prefix <dispatch_root>
#
# 2. build all dependencies (generated as static libraries):
# ==========================================================
# open <dispatch_root>/build/dispatch.sln in visual studio
# select appropriate build profile (Debug/ Release/ MinSizeRel/ RelWithDebInfo)
# build solution
#
# 3. install dependencies in <dispatch_root>/lib:
# ===============================================
# build the INSTALL project in dispatch.sln (under CMakePredefinedTargets)
#
# Linux build process:
# ====================
# From a terminal,
# cd <dispatch_root>
# 1. generate makefiles
# =====================
# cmake -B build -D HDF5_ROOT="/HDF_Group/HDF5/1.14.5" -D CMP_SOURCE_CACHE=src --install-prefix <dispatch_root> -D CMAKE_BUILD_TYPE="Debug" (or MinSizeRel, RelWithDebInfo, Release)
#
# 2. build
# ========
# make
#
# 3. install
# ==========
# make install
#
# Date: 2025.01.15
# Author: Julian Ehlers

cmake_minimum_required(VERSION 3.20)

project(rosco_controller_library VERSION 0.1.0
        DESCRIPTION "Dispatch interface to NREL's ROSCO controller DLL"
        LANGUAGES Fortran)

if (NOT CMAKE_BUILD_TYPE)
  # Sets the optimization level to -O2 and includes -g for debugging
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Choose the build type: Debug RelWithDebInfo Release" FORCE)
endif()

# add cpm for package management (assuming CPM.cmake is installed in the directory above cmake.exe)
cmake_path(GET CMAKE_COMMAND PARENT_PATH cmake_command_path)
cmake_path(GET cmake_command_path PARENT_PATH cmake_root_path)
cmake_path(APPEND cmake_root_path "CPM.cmake" OUTPUT_VARIABLE cpm_path)
include("${cpm_path}")

# add external dependencies (excluding any CMakeLists.txt files they may contain, keeping this build free of any test & example program targets that the dependencies might define)
CPMAddPackage(NAME rosco_pkg URL https://github.com/NREL/ROSCO/archive/refs/tags/v2.9.5.zip EXCLUDE_FROM_ALL TRUE)

# set the module directory for all targets
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_INSTALL_PREFIX}/inc/mod")

# define targets
add_library(rosco_controller SHARED)

# add source files
cmake_path(APPEND rosco_pkg_SOURCE_DIR "rosco" "controller" "src" OUTPUT_VARIABLE rosco_source_path)
set(rosco_sources
    "${rosco_source_path}/Constants.f90"
    "${rosco_source_path}/ControllerBlocks.f90"
    "${rosco_source_path}/Controllers.f90"
    "${rosco_source_path}/ROSCO_Types.f90"
    "${rosco_source_path}/Filters.f90"
    "${rosco_source_path}/Functions.f90"
    "${rosco_source_path}/ReadSetParameters.f90"
    "${rosco_source_path}/ROSCO_Helpers.f90"
    "${rosco_source_path}/ROSCO_IO.f90"
    "${rosco_source_path}/ZeroMQInterface.f90"
    "${rosco_source_path}/ExtControl.f90"
)

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  if (WIN32)
    set(NWTC_SYS_FILE "${rosco_source_path}/SysFiles/SysGnuWin.f90")
  elseif (APPLE OR UNIX OR CYGWIN)
    set(NWTC_SYS_FILE "${rosco_source_path}/SysFiles/SysGnuLinux.f90")
  endif ()
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "^Intel")
  if (APPLE OR UNIX)
    set(NWTC_SYS_FILE "${rosco_source_path}/SysFiles/SysIFL.f90")
  elseif (WIN32)
    set(NWTC_SYS_FILE "${rosco_source_path}/SysFiles/SysIVF.f90")
  endif (APPLE OR UNIX)
endif ()

if (NWTC_SYS_FILE)
  message("-- Setting system file as: ${NWTC_SYS_FILE}")
  list(APPEND rosco_sources ${NWTC_SYS_FILE})
else (NWTC_SYS_FILE)
  message(FATAL_ERROR "Cannot determine system file used with NWTC_Library")
endif (NWTC_SYS_FILE)

if(APPLE OR UNIX)
  # use PkgConfig to include ZMQ library if desired
  find_package(PkgConfig)
  pkg_check_modules(PC_ZeroMQ libzmq)
  if(PC_ZeroMQ_FOUND)
    # Find ZeroMQ installation
    find_path(ZeroMQ_INCLUDE_DIR
        NAMES zmq.h
        PATHS ${PC_ZeroMQ_INCLUDE_DIRS}
        )
    find_library(ZeroMQ_LIBRARY
        NAMES zmq
        PATHS ${PC_ZeroMQ_LIBRARY_DIRS}
        )
    include_directories(${ZeroMQ_INCLUDE_DIR})
  
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -L${PC_ZeroMQ_LIBRARY_DIRS} -lzmq")
  
    # Compile C-based ZeroMQ client as object library
    add_compile_options(-I${ZeroMQ_INCLUDE_DIR} -l${ZeroMQ_LIBRARY} -fPIC)
    add_library(zmq_client OBJECT "${rosco_source_path}/zmq_client.c")
  
    # Add definition
    add_definitions(-DZMQ_CLIENT="TRUE")
        set(SOURCES ${SOURCES}
    $<TARGET_OBJECTS:zmq_client>)
    target_include_directories(rosco_controller PUBLIC ${ZeroMQ_INCLUDE_DIR})
    target_link_libraries(rosco_controller PUBLIC ${ZeroMQ_LIBRARY})
  endif()
elseif (WIN32)
  # I couldn't get cmake to find PkgConfig: skip ZMQ
  message(WARNING "Unable to build against ZMQ library on Windows: excluding ZMQ from this build")
endif()

target_sources(rosco_controller PRIVATE ${rosco_sources})
target_sources(rosco_controller PRIVATE src/rosco_controller_library.f90)

# enable/disable target preprocessor
set_target_properties(rosco_controller PROPERTIES Fortran_PREPROCESS ON)
#target_compile_definitions(rosco_controller PRIVATE "")

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
target_include_directories(rosco_controller PRIVATE "${CMAKE_INSTALL_PREFIX}/inc/mod")

# set target link directories
target_link_directories(rosco_controller PRIVATE "${CMAKE_INSTALL_PREFIX}/lib")

# set target link libraries
target_link_libraries(rosco_controller dispatch_types error_handling extf toml_f)
