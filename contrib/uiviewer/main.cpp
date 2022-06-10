#include <QApplication>
#include <QCommandLineParser>
#include <QDialog>
#include <QFile>
#include <QUiLoader>
#include <QVBoxLayout>
#include <QWidget>
#include <QtDebug>
#include <memory>

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
}

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationDisplayName(QApplication::translate("main", "UI Viewer"));

    QCommandLineParser parser;
    parser.addHelpOption();
    parser.addPositionalArgument("file", QApplication::translate("main", ".ui file to load"),
                                 "[file]");
    parser.process(app);

    QDialog dlg;
    QVBoxLayout lay(&dlg);

    const auto args = parser.positionalArguments();
    if (args.size() > 1)
        return 1;

    QUiLoader loader;
    if (!args.isEmpty()) {
        auto w = loadUiFile(loader, args.first());
        if (w) {
            // TODO: auto vs fixed layout
            // TODO: if w were window?
            lay.addWidget(w.release());
        }
    }

    dlg.show();
    return app.exec();
}
