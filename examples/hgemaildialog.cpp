#include "hgemaildialog.h"
#include "ui_hgemaildialog.h"

HgEmailDialog::HgEmailDialog(QWidget *parent)
    : QDialog(parent), ui_(std::make_unique<Ui::HgEmailDialog>())
{
    ui_->setupUi(this);
}

HgEmailDialog::~HgEmailDialog() = default;
