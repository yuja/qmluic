#include "settingsdialog.h"
#include "ui_settingsdialog.h"

SettingsDialog::SettingsDialog(QWidget *parent)
    : QDialog(parent), ui_(std::make_unique<Ui::SettingsDialog>())
{
    ui_->setupUi(this);
}

SettingsDialog::~SettingsDialog() = default;
