cmake_minimum_required(VERSION 3.5)

project(examples VERSION 0.1 LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Help Qt Creator find our type stub
set(QML_IMPORT_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../contrib/imports CACHE STRING "" FORCE)

option(USE_QT6 "Use Qt6 even if Qt5 found")
if(USE_QT6)
  find_package(Qt6 REQUIRED COMPONENTS Widgets)
else()
  find_package(Qt5 5.15 COMPONENTS Widgets)
  if(NOT Qt5_FOUND)
    find_package(Qt6 REQUIRED COMPONENTS Widgets)
  endif()
endif()

add_subdirectory(customwidget)

add_executable(qmluic-examples
  hgemaildialog.cpp
  hgemaildialog.h
  hgemaildialog.ui
  itemviews.cpp
  itemviews.h
  itemviews.ui
  layoutflow.cpp
  layoutflow.h
  layoutflow.ui
  main.cpp
  mainwindow.cpp
  mainwindow.h
  mainwindow.ui
  settingsdialog.cpp
  settingsdialog.h
  settingsdialog.ui
  staticitemmodel.cpp
  staticitemmodel.h
  staticitemmodel.ui
  variouslayouts.cpp
  variouslayouts.h
  variouslayouts.ui
)

# Makes .qml files appear in Qt Creator project
target_sources(qmluic-examples
  PRIVATE
    HgEmailDialog.qml
    ItemViews.qml
    LayoutFlow.qml
    MainWindow.qml
    SettingsDialog.qml
    StaticItemModel.qml
    VariousLayouts.qml
)

target_compile_definitions(qmluic-examples
  PRIVATE
    # The following define makes your compiler emit warnings if you use
    # any feature of Qt which has been marked as deprecated
    QT_DEPRECATED_WARNINGS
    # disables all the APIs deprecated before Qt 6.0.0
    QT_DISABLE_DEPRECATED_BEFORE=0x060000
    # disables erroneous QString cast, but allows QString("...")
    QT_RESTRICTED_CAST_FROM_ASCII
)

target_link_libraries(qmluic-examples
  PRIVATE
    Qt::Widgets
)
