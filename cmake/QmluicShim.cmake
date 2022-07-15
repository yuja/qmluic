# Drop-in replacement for QmluicMacros.cmake
#
# This module should be loaded only when qmluic isn't available:
#
#   find_package(Qmluic QUIET)
#   if(NOT Qmluic_FOUND)
#     include("${CMAKE_SOURCE_DIR}/cmake/QmluicShim.cmake")
#   endif()

function(qmluic_target_qml_sources target)
  cmake_parse_arguments(PARSE_ARGV 1 arg "NO_DYNAMIC_BINDING" "OUTPUT_DIRECTORY" "")
  set(qml_files ${arg_UNPARSED_ARGUMENTS})
  set(no_dynamic_binding ${arg_NO_DYNAMIC_BINDING})
  set(output_directory ${arg_OUTPUT_DIRECTORY})
  if(NOT output_directory)
    message(FATAL_ERROR "OUTPUT_DIRECTORY must be set (shim can't generate .ui from .qml)")
  endif()

  list(TRANSFORM qml_files TOLOWER OUTPUT_VARIABLE ui_files)
  list(TRANSFORM ui_files REPLACE "\.qml$" ".ui")
  list(TRANSFORM ui_files PREPEND "${output_directory}/" OUTPUT_VARIABLE abs_ui_files)

  if(no_dynamic_binding)
    set(ui_support_h_files)
    set(abs_ui_support_h_files)
  else()
    list(TRANSFORM qml_files TOLOWER OUTPUT_VARIABLE ui_support_h_files)
    list(TRANSFORM ui_support_h_files REPLACE "([^/]*)\.qml$" "uisupport_\\1.h")
    list(TRANSFORM ui_support_h_files PREPEND "${output_directory}/" OUTPUT_VARIABLE abs_ui_support_h_files)
  endif()

  target_sources(${target} PRIVATE ${qml_files})
  target_sources(${target} PRIVATE ${abs_ui_support_h_files})

  qt_wrap_ui(header_files ${abs_ui_files})
  target_sources(${target} PRIVATE ${header_files})
endfunction()
