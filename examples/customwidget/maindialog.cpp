#include "maindialog.h"
#include "ui_maindialog.h"

MainDialog::MainDialog(QWidget *parent) : QDialog(parent), ui_(std::make_unique<Ui::MainDialog>())
{
    ui_->setupUi(this);
}

MainDialog::~MainDialog() = default;
