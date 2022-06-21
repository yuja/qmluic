if(USE_QT6)
  find_package(Qt6 REQUIRED COMPONENTS Widgets)
else()
  find_package(Qt5 5.15 COMPONENTS Widgets)
  if(NOT Qt5_FOUND)
    find_package(Qt6 REQUIRED COMPONENTS Widgets)
  endif()
endif()

add_executable(qmluic-examples-customwidget
  common/mydialogbuttonbox.cpp
  common/mydialogbuttonbox.h
  common/mydialogbuttonbox.ui
  main.cpp
  maindialog.cpp
  maindialog.h
  settingsform.cpp
  settingsform.h
)

target_qmlui_sources(qmluic-examples-customwidget
  common/MyDialogButtonBox.qml
  MainDialog.qml
  SettingsForm.qml
)

target_compile_definitions(qmluic-examples-customwidget
  PRIVATE
    # The following define makes your compiler emit warnings if you use
    # any feature of Qt which has been marked as deprecated
    QT_DEPRECATED_WARNINGS
    # disables all the APIs deprecated before Qt 6.0.0
    QT_DISABLE_DEPRECATED_BEFORE=0x060000
    # disables erroneous QString cast, but allows QString("...")
    QT_RESTRICTED_CAST_FROM_ASCII
)

target_include_directories(qmluic-examples-customwidget
  PRIVATE
    common
)

target_link_libraries(qmluic-examples-customwidget
  PRIVATE
    Qt::Widgets
)
