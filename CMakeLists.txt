cmake_minimum_required(VERSION 3.12)

project(qmluic VERSION 0.1)

# Make our cmake modules available to uiviewer
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)
include(QmluicMacros)

# Help Qt Creator find our type stub
set(QML_IMPORT_PATH "${CMAKE_CURRENT_BINARY_DIR}/imports" CACHE STRING "" FORCE)

option(USE_QT6 "Use Qt6 even if Qt5 found")
if(USE_QT6)
  find_package(Qt6 REQUIRED COMPONENTS UiTools Widgets)
  set(QT_VERSION_MAJOR 6)
else()
  find_package(Qt5 5.15 COMPONENTS UiTools Widgets)
  if(Qt5_FOUND)
    set(QT_VERSION_MAJOR 5)
  else()
    find_package(Qt6 REQUIRED COMPONENTS UiTools Widgets)
    set(QT_VERSION_MAJOR 6)
  endif()
endif()

# Should be built by 'cargo build --workspace' prior to calling cmake.
add_executable(Qmluic::qmluic IMPORTED)
set_target_properties(Qmluic::qmluic PROPERTIES
  IMPORTED_LOCATION_DEBUG "${CMAKE_CURRENT_SOURCE_DIR}/target/debug/qmluic"
  IMPORTED_LOCATION_RELEASE "${CMAKE_CURRENT_SOURCE_DIR}/target/release/qmluic")
set(LOCAL_QMLUIC_EXECUTABLE
  "${CMAKE_CURRENT_SOURCE_DIR}/target/$<IF:$<CONFIG:Release>,release,debug>/qmluic")

add_subdirectory(uiviewer)

qmluic_add_qmldir(qmluic.QtWidgets Qt${QT_VERSION_MAJOR}::Widgets)

configure_package_config_file(cmake/QmluicConfig.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/QmluicConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_DATADIR}/cmake/Qmluic"
)
install(PROGRAMS "${LOCAL_QMLUIC_EXECUTABLE}" TYPE BIN)
install(FILES
  "cmake/QmluicMacros.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/QmluicConfig.cmake"
  DESTINATION "${CMAKE_INSTALL_DATADIR}/cmake/Qmluic"
)
install(FILES
  "cmake/QmluicShim.cmake"
  DESTINATION "${CMAKE_INSTALL_DATADIR}/qmluic"
)
install(DIRECTORY
  "${CMAKE_CURRENT_BINARY_DIR}/imports"
  DESTINATION "${CMAKE_INSTALL_DATADIR}/qmluic"
)
