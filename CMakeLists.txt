cmake_minimum_required(VERSION 3.12)

project(qmluic VERSION 0.1)

# Make our cmake modules available to uiviewer
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

find_program(CARGO_COMMAND cargo DOC "cargo command" REQUIRED)
set(CARGO_BUILD_TARGET_DIR "${CMAKE_CURRENT_SOURCE_DIR}/target" CACHE PATH "cargo build directory")

# Help Qt Creator find our type stub
set(QML_IMPORT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/contrib/imports" CACHE STRING "" FORCE)

add_subdirectory(uiviewer)

add_custom_target(qmluic ALL
  COMMAND
    ${CARGO_COMMAND} build --workspace $<$<CONFIG:Release>:--release>
                           --target-dir "${CARGO_BUILD_TARGET_DIR}"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  USES_TERMINAL
)

configure_package_config_file(cmake/QmluicConfig.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/QmluicConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_DATADIR}/cmake/Qmluic"
)
install(PROGRAMS
  "${CARGO_BUILD_TARGET_DIR}/$<IF:$<CONFIG:Release>,release,debug>/qmluic${CMAKE_EXECUTABLE_SUFFIX}"
  TYPE BIN
)
install(FILES
  "cmake/QmluicMacros.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/QmluicConfig.cmake"
  DESTINATION "${CMAKE_INSTALL_DATADIR}/cmake/Qmluic"
)
