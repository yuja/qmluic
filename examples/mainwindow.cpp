#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QtDebug>
#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "uisupport_mainwindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
      ui_(std::make_unique<Ui::MainWindow>()),
      uiSupport_(std::make_unique<UiSupport::MainWindow>(this, ui_.get()))
{
    ui_->setupUi(this);
    uiSupport_->setup();
    connect(ui_->fileNameEdit, &QComboBox::currentIndexChanged, this,
            &MainWindow::updateSourceEdit);
    connect(ui_->action_Quit, &QAction::triggered, QCoreApplication::instance(),
            &QCoreApplication::quit, Qt::QueuedConnection);
    updateSourceEdit();
}

MainWindow::~MainWindow() = default;

void MainWindow::updateSourceEdit()
{
    QDir baseDir(QFileInfo(__FILE__).dir());
    QFile file(baseDir.filePath(ui_->fileNameEdit->currentText()));
    if (file.open(QIODevice::ReadOnly)) {
        ui_->sourceEdit->setPlainText(QString::fromUtf8(file.readAll()));
    } else {
        qWarning() << "failed to open source file:" << file.fileName() << file.errorString();
        ui_->sourceEdit->setPlainText("");
    }
}
