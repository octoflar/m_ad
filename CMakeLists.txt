## @author Ralf Quast
## @date 2022
## @copyright MIT License

cmake_minimum_required(VERSION 3.5.0)
include(src/main/cmake/test.cmake)
include(src/main/cmake/unix.cmake)
include(src/main/cmake/veclib.cmake)

##
# Project
#
project(m_ad VERSION 2024.1 LANGUAGES Fortran)

veclib_required()

##
# Source directories
#
set(MAIN ${CMAKE_SOURCE_DIR}/src/main)
set(TEST ${CMAKE_SOURCE_DIR}/src/test)


##
# Configuration
#
# None.

##
# Main targets
#
# None.


##
# Test targets
#
add_unit_test(addition_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/addition_mod.F90
        ${TEST}/fortran/addition_mod_test.F90)
add_unit_test(multiplication_test
        ${multiplication_OUTPUT}
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/multiplication_mod.F90
        ${TEST}/fortran/multiplication_mod_test.F90)
add_unit_test(inverse_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/inverse_mod.F90
        ${TEST}/fortran/inverse_mod_test.F90)
target_link_libraries(inverse_test ${VECLIB})
add_unit_test(inverse_product_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/inverse_product_mod.F90
        ${TEST}/fortran/inverse_product_mod_test.F90)
target_link_libraries(inverse_product_test ${VECLIB})
add_unit_test(first_quadratic_form_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/first_quadratic_form_mod.F90
        ${TEST}/fortran/first_quadratic_form_mod_test.F90)
add_unit_test(second_quadratic_form_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/second_quadratic_form_mod.F90
        ${TEST}/fortran/second_quadratic_form_mod_test.F90)
target_link_libraries(second_quadratic_form_test ${VECLIB})
add_unit_test(second_quadratic_form_symmetric_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/second_quadratic_form_symmetric_mod.F90
        ${TEST}/fortran/second_quadratic_form_symmetric_mod_test.F90)
target_link_libraries(second_quadratic_form_symmetric_test ${VECLIB})
add_unit_test(cost_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/solver_mod.F90
        ${MAIN}/fortran/cost_mod.F90
        ${TEST}/fortran/cost_mod_test.F90)
add_unit_test(solver_test
        ${MAIN}/fortran/base_mod.F90
        ${MAIN}/fortran/math_mod.F90
        ${MAIN}/fortran/solver_mod.F90
        ${TEST}/fortran/solver_mod_test.F90)


##
# Custom targets
#
add_custom_target(tar
        COMMAND ${TAR} -f ${PROJECT_NAME}.tar.gz -C ${CMAKE_SOURCE_DIR} --exclude .git -X ${CMAKE_SOURCE_DIR}/.gitignore .
        BYPRODUCTS ${PROJECT_NAME}.tar.gz
        VERBATIM)


##
# Installation
#
# None.
