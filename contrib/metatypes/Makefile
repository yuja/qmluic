QMAKE = qmake

QT_INSTALL_LIBS = $(shell $(QMAKE) -query QT_INSTALL_LIBS)
QT_VERSION = $(shell $(QMAKE) -query QT_VERSION)

QT_METATYPES = $(QT_INSTALL_LIBS)/metatypes

major_version = $(word 1,$(subst ., ,$(QT_VERSION)))
qtx = qt$(major_version)

.PHONY: all
all:
	cp $(QT_METATYPES)/$(qtx)core_metatypes.json .
	cp $(QT_METATYPES)/$(qtx)gui_metatypes.json .
	cp $(QT_METATYPES)/$(qtx)widgets_metatypes.json .
