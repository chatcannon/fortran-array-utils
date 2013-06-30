# N.B. this CMakeLists.txt is designed to be incorporated via add_subdirectory
# from a larger project, rather than acting as a standalone project

add_custom_command(OUTPUT "extendable_arrays.F90"
                   COMMAND "python2"
                   ARGS "extend-gen.py "${CMAKE_CURRENT_BINARY_DIR}/extendable_arrays.F90"
                   DEPENDS "extend-gen.py" "extend.tmpl.F90"
                   WORKING DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})