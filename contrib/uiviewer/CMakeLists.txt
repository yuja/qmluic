cmake_minimum_required(VERSION 3.5)

project(uiviewer VERSION 0.1 LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt5 5.15 REQUIRED COMPONENTS Widgets)

add_executable(qmluic-uiviewer
  main.cpp
)

target_link_libraries(qmluic-uiviewer
  PRIVATE
    Qt::Widgets
)
