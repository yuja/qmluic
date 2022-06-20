find_package(Qt5 5.15 REQUIRED COMPONENTS Widgets)

add_executable(qmluic-examples-thg
  hgemaildialog.cpp
  hgemaildialog.h
  hgemaildialog.ui
  main.cpp
)

# Makes .qml files appear in Qt Creator project
target_sources(qmluic-examples-thg
  PRIVATE
    HgEmailDialog.qml
)

target_compile_definitions(qmluic-examples-thg
  PRIVATE
    # The following define makes your compiler emit warnings if you use
    # any feature of Qt which has been marked as deprecated
    QT_DEPRECATED_WARNINGS
    # disables all the APIs deprecated before Qt 6.0.0
    QT_DISABLE_DEPRECATED_BEFORE=0x060000
    # disables erroneous QString cast, but allows QString("...")
    QT_RESTRICTED_CAST_FROM_ASCII
)

target_link_libraries(qmluic-examples-thg
  PRIVATE
    Qt::Widgets
)
