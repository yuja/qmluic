#include "hgemaildialog.h"
#include "ui_hgemaildialog.h"
#include "uisupport_hgemaildialog.h"

HgEmailDialog::HgEmailDialog(QWidget *parent)
    : QDialog(parent),
      ui_(std::make_unique<Ui::HgEmailDialog>()),
      uiSupport_(std::make_unique<UiSupport::HgEmailDialog>(this, ui_.get()))
{
    ui_->setupUi(this);
    uiSupport_->setup();
}

HgEmailDialog::~HgEmailDialog() = default;
