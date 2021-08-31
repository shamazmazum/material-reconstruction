cmake_minimum_required (VERSION 3.0)
cmake_policy(SET CMP0048 NEW)

project(anneal-ocl VERSION 0.1)
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")

find_package (OpenCL REQUIRED)
find_package (ProgramMap REQUIRED)
find_package (FFTW3 REQUIRED)

add_subdirectory(src)
