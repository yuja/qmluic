#include <QApplication>
#include <QBuffer>
#include <QCommandLineParser>
#include <QFile>
#include <QLoggingCategory>
#include <QUiLoader>
#include <QWidget>
#include <QtDebug>
#include <memory>
#include "pipeserver.h"
#include "uiviewerdialog.h"

namespace {
std::unique_ptr<QWidget> loadUiFile(QUiLoader &loader, const QString &fileName)
{
    QFile file(fileName);
    if (!file.open(QFile::ReadOnly)) {
        qWarning() << "failed to open .ui file:" << file.fileName();
        return {};
    }
    return std::unique_ptr<QWidget>(loader.load(&file));
}

std::unique_ptr<QWidget> loadUiData(QUiLoader &loader, const QByteArray &data)
{
    auto tmp = data;
    QBuffer buf(&tmp);
    buf.open(QIODevice::ReadOnly);
    return std::unique_ptr<QWidget>(loader.load(&buf));
}
}

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationDisplayName(QApplication::translate("main", "UI Viewer"));

    QCommandLineParser parser;
    parser.addHelpOption();
    parser.addOption({ "pipe", QApplication::translate("main", "update ui via stdio") });
    parser.addPositionalArgument("file", QApplication::translate("main", ".ui file to load"),
                                 "[file]");
    parser.process(app);

#ifdef QT_NO_DEBUG
    QLoggingCategory::setFilterRules(QStringLiteral("*.debug=false\n*.info=false"));
#endif

    const auto args = parser.positionalArguments();
    if (args.size() > 1)
        return 1;

    UiViewerDialog dlg;
    QUiLoader loader;

    if (!args.isEmpty()) {
        auto w = loadUiFile(loader, args.first());
        if (w) {
            dlg.setContentWidget(std::move(w));
        }
    }

    PipeServer pipeServer;
    if (parser.isSet("pipe")) {
        QObject::connect(&pipeServer, &PipeServer::dataReceived, &dlg,
                         [&loader, &dlg](const QByteArray &data) {
                             auto w = loadUiData(loader, data);
                             if (w) {
                                 dlg.setContentWidget(std::move(w));
                             }
                         });
        QObject::connect(&app, &QApplication::aboutToQuit, &pipeServer, [&pipeServer]() {
            qInfo() << "waiting for pipe thread shutdown";
            pipeServer.wait();
        });
        pipeServer.start();
    }

    dlg.show();
    return app.exec();
}
