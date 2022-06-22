cmake_minimum_required(VERSION 3.12)

project(qmluic VERSION 0.1)

find_program(CARGO_COMMAND cargo DOC "cargo command" REQUIRED)
set(CARGO_BUILD_TARGET_DIR "${CMAKE_CURRENT_SOURCE_DIR}/target" CACHE PATH "cargo build directory")

add_subdirectory(contrib/uiviewer)

add_custom_target(qmluic ALL
  COMMAND
    ${CARGO_COMMAND} build --workspace $<$<CONFIG:Release>:--release>
                           --target-dir "${CARGO_BUILD_TARGET_DIR}"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  USES_TERMINAL
)

# TODO: generate QmluicConfig.cmake and install it
install(PROGRAMS
  "${CARGO_BUILD_TARGET_DIR}/$<IF:$<CONFIG:Release>,release,debug>/qmluic${CMAKE_EXECUTABLE_SUFFIX}"
  TYPE BIN
)
install(FILES
  "cmake/QmluicMacros.cmake"
  DESTINATION "share/cmake/Qmluic"
)
