#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent), ui_(std::make_unique<Ui::MainWindow>())
{
    ui_->setupUi(this);
}

MainWindow::~MainWindow() = default;
