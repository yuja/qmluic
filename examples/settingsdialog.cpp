#include "settingsdialog.h"
#include "ui_settingsdialog.h"
#include "uisupport_settingsdialog.h"

SettingsDialog::SettingsDialog(QWidget *parent)
    : QDialog(parent),
      ui_(std::make_unique<Ui::SettingsDialog>()),
      uiSupport_(std::make_unique<UiSupport::SettingsDialog>(this, ui_.get()))
{
    ui_->setupUi(this);
    uiSupport_->setup();
}

SettingsDialog::~SettingsDialog() = default;
