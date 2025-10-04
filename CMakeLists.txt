# This CMakeLists file sets options for compiling dispatch and all available dispatch libraries (components)
#
# The directory in which this CMakeLists.txt is located is referred to as <dispatch_root>
# The build process has been tested on the following environments:
# - Windows 11 (Windows SDK 10.0.26100), Intel Fortran (ifx) 2025.01, Microsoft Visual Studio 17 2022
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
# To generate a buildsystem for dispatch and its libraries:
# cd <dispatch_root>:
# cmake -G "Visual Studio 17 2022" -T fortran=ifx -A x64 -B build -D HDF5_ROOT="C:/Program Files/HDF_Group/HDF5/1.14.5" -D STDLIB_ROOT="C:/Users/jaehl/AppData/Roaming/local" -D CPM_SOURCE_CACHE=prj/_common/src --install-prefix <dispatch_root>
#
# To build dispatch and its libraries (generated as dynamic libraries, .so or .dll) and install them in <dispatch_root>/bin:
# open <dispatch_root>/prj/build/dispatch.sln
# select appropriate build profile (Debug/ Release/ MinSizeRel/ RelWithDebInfo)
# build solution

cmake_minimum_required(VERSION 3.25)

project(dispatch VERSION 0.1.0
        DESCRIPTION "DIscrete Signal Processing library dispATCHer (DISPATCH): an environment and libraries for linking signal processing routines to simulators of time-varying processes.")

cmake_path(APPEND CMAKE_INSTALL_PREFIX "bin" OUTPUT_VARIABLE dispatch_binary_path)
add_subdirectory(prj/delay)
add_subdirectory(prj/dispatch)
add_subdirectory(prj/iir_filters)
add_subdirectory(prj/monitor)
add_subdirectory(prj/n_ary_function)
add_subdirectory(prj/rosco_controller)
add_subdirectory(prj/scheduled_matrix_gain)
add_subdirectory(prj/signal_generator)
add_subdirectory(prj/signal_renamer)
add_subdirectory(prj/soft_clipper)
add_subdirectory(prj/state_estimator)
add_subdirectory(prj/timing_rig)

install(TARGETS 
                delay 
                dispatch 
                iir_filters 
                monitor 
                n_ary_function 
                rosco_controller 
                scheduled_matrix_gain
                signal_generator 
                signal_renamer 
                soft_clipper 
                state_estimator 
                timing_rig
  LIBRARY DESTINATION "${dispatch_binary_path}")