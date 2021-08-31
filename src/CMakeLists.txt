include_directories (
  ${FFTW3_INCLUDE_DIR}
  ${PROGRAM_MAP_INCLUDE_DIR}
  ${OpenCL_INCLUDE_DIRS})

add_library(anneal-ocl SHARED
  anneal_ocl.c)
target_link_libraries (anneal-ocl
  ${OpenCL_LIBRARIES}
  ${PROGRAM_MAP_LIBRARY}
  ${FFTW3_LIBRARY}
  m)

add_executable (example main.c)
target_link_libraries (example anneal-ocl)
