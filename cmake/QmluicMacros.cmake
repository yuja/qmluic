cmake_minimum_required(VERSION 3.12)

find_program(QMLUIC_COMMAND qmluic DOC "qmluic command")

function(qmluic_target_qml_sources target)
  set(qml_files ${ARGN})
  list(TRANSFORM qml_files PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/" OUTPUT_VARIABLE abs_qml_files)
  list(TRANSFORM qml_files TOLOWER OUTPUT_VARIABLE ui_files)
  list(TRANSFORM ui_files REPLACE "\.qml$" ".ui")
  list(TRANSFORM ui_files PREPEND "${CMAKE_CURRENT_BINARY_DIR}/" OUTPUT_VARIABLE abs_ui_files)

  target_sources(${target} PRIVATE ${qml_files})
  add_custom_command(
    OUTPUT ${abs_ui_files}
    COMMAND ${QMLUIC_COMMAND} generate-ui -O "${CMAKE_CURRENT_BINARY_DIR}" -- ${qml_files}
    DEPENDS ${abs_qml_files}
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT "Generating UI from QML"
  )

  qt_wrap_ui(header_files ${abs_ui_files})
  target_sources(${target} PRIVATE ${header_files})
endfunction()
