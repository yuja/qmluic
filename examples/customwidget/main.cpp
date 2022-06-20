#include <QApplication>
#include "maindialog.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    MainDialog dialog;
    dialog.show();
    return app.exec();
}
