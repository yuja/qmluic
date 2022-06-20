#include <QApplication>
#include "hgemaildialog.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    HgEmailDialog dialog;
    dialog.show();
    return app.exec();
}
