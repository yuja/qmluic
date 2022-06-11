cmake_minimum_required(VERSION 3.5)

project(uiviewer VERSION 0.1 LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt5 5.15 REQUIRED COMPONENTS UiTools Widgets)

add_executable(qmluic-uiviewer
  main.cpp
  pipeserver.cpp
  pipeserver.h
  uiviewerdialog.cpp
  uiviewerdialog.h
  uiviewerdialog.ui
)

target_compile_definitions(qmluic-uiviewer
  PRIVATE
    # The following define makes your compiler emit warnings if you use
    # any feature of Qt which has been marked as deprecated
    QT_DEPRECATED_WARNINGS
    # disables all the APIs deprecated before Qt 6.0.0
    QT_DISABLE_DEPRECATED_BEFORE=0x060000
    # disables erroneous QString cast, but allows QString("...")
    QT_RESTRICTED_CAST_FROM_ASCII
)

target_link_libraries(qmluic-uiviewer
  PRIVATE
    Qt::UiTools
    Qt::Widgets
)
