#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QtDebug>
#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent), ui_(std::make_unique<Ui::MainWindow>())
{
    ui_->setupUi(this);
    connect(ui_->fileNameEdit, &QComboBox::currentIndexChanged, this, &MainWindow::updateView);
    updateView();
}

MainWindow::~MainWindow() = default;

void MainWindow::updateView()
{
    ui_->formStack->setCurrentIndex(ui_->fileNameEdit->currentIndex());

    QDir baseDir(QFileInfo(__FILE__).dir());
    QFile file(baseDir.filePath(ui_->fileNameEdit->currentText()));
    if (file.open(QIODevice::ReadOnly)) {
        ui_->sourceEdit->setPlainText(QString::fromUtf8(file.readAll()));
    } else {
        qWarning() << "failed to open source file:" << file.fileName() << file.errorString();
        ui_->sourceEdit->setPlainText("");
    }
}
