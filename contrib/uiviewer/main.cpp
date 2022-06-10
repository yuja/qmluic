#include <QApplication>
#include <QDialog>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QDialog dlg;
    dlg.show();
    return app.exec();
}
