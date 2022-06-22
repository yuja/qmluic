cmake_minimum_required(VERSION 3.12)

function(qmluic_target_qml_sources target)
  cmake_parse_arguments(PARSE_ARGV 1 arg "" "OUTPUT_DIRECTORY" "")
  set(qml_files ${arg_UNPARSED_ARGUMENTS})
  set(output_directory ${arg_OUTPUT_DIRECTORY})
  if(NOT output_directory)
    set(output_directory ${CMAKE_CURRENT_BINARY_DIR})
  endif()

  list(TRANSFORM qml_files PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/" OUTPUT_VARIABLE abs_qml_files)
  list(TRANSFORM qml_files TOLOWER OUTPUT_VARIABLE ui_files)
  list(TRANSFORM ui_files REPLACE "\.qml$" ".ui")
  list(TRANSFORM ui_files PREPEND "${output_directory}/" OUTPUT_VARIABLE abs_ui_files)

  if(NOT QMLUIC_COMMAND)
    message(FATAL_ERROR "QMLUIC_COMMAND must be set")
  endif()

  target_sources(${target} PRIVATE ${qml_files})
  add_custom_command(
    OUTPUT ${abs_ui_files}
    COMMAND ${QMLUIC_COMMAND} generate-ui -O "${output_directory}" -- ${qml_files}
    DEPENDS ${abs_qml_files}
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT "Generating UI from QML"
  )

  qt_wrap_ui(header_files ${abs_ui_files})
  target_sources(${target} PRIVATE ${header_files})
endfunction()
