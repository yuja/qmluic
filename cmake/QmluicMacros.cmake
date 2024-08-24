cmake_minimum_required(VERSION 3.12)

function(qmluic_target_qml_sources target)
  cmake_parse_arguments(PARSE_ARGV 1 arg "NO_DYNAMIC_BINDING" "OUTPUT_DIRECTORY" "")
  set(qml_files ${arg_UNPARSED_ARGUMENTS})
  set(no_dynamic_binding ${arg_NO_DYNAMIC_BINDING})
  set(output_directory ${arg_OUTPUT_DIRECTORY})
  if(NOT output_directory)
    set(output_directory ${CMAKE_CURRENT_BINARY_DIR})
  endif()

  set(generate_ui_opts)

  list(TRANSFORM qml_files PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/" OUTPUT_VARIABLE abs_qml_files)
  list(TRANSFORM qml_files TOLOWER OUTPUT_VARIABLE ui_files)
  list(TRANSFORM ui_files REPLACE "\.qml$" ".ui")
  list(TRANSFORM ui_files PREPEND "${output_directory}/" OUTPUT_VARIABLE abs_ui_files)

  if(no_dynamic_binding)
    set(ui_support_h_files)
    set(abs_ui_support_h_files)
    list(APPEND generate_ui_opts "--no-dynamic-binding")
  else()
    list(TRANSFORM qml_files TOLOWER OUTPUT_VARIABLE ui_support_h_files)
    list(TRANSFORM ui_support_h_files REPLACE "([^/]*)\.qml$" "uisupport_\\1.h")
    list(TRANSFORM ui_support_h_files PREPEND "${output_directory}/" OUTPUT_VARIABLE abs_ui_support_h_files)
  endif()

  target_sources(${target} PRIVATE ${qml_files})
  get_target_property(QMAKE_EXECUTABLE Qt::qmake LOCATION)
  add_custom_command(
    OUTPUT ${abs_ui_files} ${abs_ui_support_h_files}
    COMMAND
      Qmluic::qmluic generate-ui -O "${output_directory}"
                                 --qmake "${QMAKE_EXECUTABLE}"
                                 ${generate_ui_opts}
                                 -- ${qml_files}
    DEPENDS ${abs_qml_files} Qmluic::qmluic Qt::qmake
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT "Generating UI from QML"
  )
  target_sources(${target} PRIVATE ${abs_ui_support_h_files})

  qt_wrap_ui(header_files ${abs_ui_files})
  target_sources(${target} PRIVATE ${header_files})
endfunction()

# Adds custom target that creates QML type stubs.
#
# This is primarily designed for internal use, but you can use this to generate
# type stubs by yourself.
#
# Example:
#   qmluic_add_qmldir(qmluic.QtWidgets Qt5::Widgets) to
#   generate imports/qmluic/QtWidgets/{qmldir,plugins.qmltypes}
function(qmluic_add_qmldir module_name lib)
  get_target_property(lib_location ${lib} LOCATION)
  get_filename_component(lib_directory "${lib_location}" DIRECTORY)
  set(metatypes_directory "${lib_directory}/metatypes")

  # Look up metatypes.json by pattern matching. Maybe this can be resolved from
  # the INTERFACE_SOURCES property with TARGET_PROPERTY:QT_CONSUMES_METATYPES on Qt 6,
  # but doing that would be tedious.
  string(REPLACE "::" "" metatypes_prefix ${lib})
  string(TOLOWER "${metatypes_prefix}" metatypes_prefix)
  file(GLOB input_metatypes_file "${metatypes_directory}/${metatypes_prefix}_*.json")
  list(LENGTH input_metatypes_file count)
  if(count EQUAL 0)
    message(FATAL_ERROR "No metatypes.json found for ${lib}")
  elseif(NOT count EQUAL 1)
    message(FATAL_ERROR "Multiple metatypes.json found for ${lib}: ${input_metatypes_file}")
  endif()

  string(REPLACE "." "/" module_directory ${module_name})
  set(output_directory "${CMAKE_CURRENT_BINARY_DIR}/imports/${module_directory}")
  set(output_qmldir "${output_directory}/qmldir")
  set(output_qmltypes "${output_directory}/plugins.qmltypes")

  add_custom_command(
    OUTPUT "${output_qmldir}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${output_directory}"
    COMMAND ${CMAKE_COMMAND} -E echo "module ${module_name}" > "${output_qmldir}"
    COMMAND ${CMAKE_COMMAND} -E echo "typeinfo plugins.qmltypes" >> "${output_qmldir}"
    COMMAND ${CMAKE_COMMAND} -E echo "depends QtQml" >> "${output_qmldir}"
  )

  get_target_property(QMAKE_EXECUTABLE Qt::qmake LOCATION)
  add_custom_command(
    OUTPUT "${output_qmltypes}"
    COMMAND
      Qmluic::qmluic dump-metatypes --qmake "${QMAKE_EXECUTABLE}"
                                    --output-qmltypes "${output_qmltypes}"
                                    "${input_metatypes_file}"
    MAIN_DEPENDENCY "${input_metatypes_file}"
    DEPENDS Qmluic::qmluic Qt::qmake
  )

  add_custom_target("${module_name}" ALL DEPENDS "${output_qmldir}" "${output_qmltypes}")
endfunction()
