# N.B. this CMakeLists.txt is designed to be incorporated via add_subdirectory
# from a larger project, rather than acting as a standalone project
cmake_minimum_required(VERSION 2.8)


add_custom_command(OUTPUT "array-utils.F90"
                   COMMAND "python2" 
                   ARGS "gen-array-utils.py" "array-utils.tmpl.F90" "${CMAKE_CURRENT_BINARY_DIR}/array-utils.F90" 
                   DEPENDS "gen-array-utils.py" "array-utils.tmpl.F90"
                   WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" )