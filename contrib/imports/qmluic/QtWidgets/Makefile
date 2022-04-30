JQ = jq
QMAKE = qmake

QT_INSTALL_BINS = $(shell $(QMAKE) -query QT_INSTALL_BINS)
QT_INSTALL_LIBS = $(shell $(QMAKE) -query QT_INSTALL_LIBS)
QT_VERSION = $(shell $(QMAKE) -query QT_VERSION)

QMLTYPEREGISTRAR = $(QT_INSTALL_BINS)/qmltyperegistrar
QT_METATYPES = $(QT_INSTALL_LIBS)/metatypes

major_version = $(word 1,$(subst ., ,$(QT_VERSION)))
qtx = qt$(major_version)

.PHONY: all
all: plugins.qmltypes

plugins.qmltypes: metatypes.json $(QT_METATYPES)/$(qtx)core_metatypes.json
	$(QMLTYPEREGISTRAR) \
		--import-name qmluic.QtWidgets \
		--major-version $(major_version) \
		--foreign-types $(QT_METATYPES)/$(qtx)core_metatypes.json \
		--generate-qmltypes $@ $< > /dev/null

metatypes.json: $(QT_METATYPES)/$(qtx)widgets_metatypes.json
	$(JQ) 'map(.classes |= map({"classInfos": [{"name": "QML.Element", "value": "auto"}]} + .))' $< > $@

.PHONY: clean
clean:
	$(RM) plugins.qmltypes metatypes.json
